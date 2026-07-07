# Plan 10 -- history_hourly / history_daily assembly (SCOPING section 4).
#
# `history_hourly` is the QC'd, gap-filled, hourly-aggregated AWS record for a
# site -- simple in principle (read the store, aggregate hourly) since the
# heavy lifting (QC, gap-fill) already happened upstream via qc_run()/
# fill_run() before this reads the store.
#
# `history_daily` is the more interesting product (SCOPING section 4): base =
# SILO daily (gap-free by construction, since SILO interpolates/patches at
# source), overlaid with the site's own AWS daily aggregate wherever it is
# present AND QC-clean (`qc_flag != "fail"`) -- the "AWS-wins" compositing
# rule. Provenance (the `source` column) always records which leg served
# each value, so the step-change the AWS installation date introduces is
# auditable after the fact, not silently absorbed into a single blended
# series.
#
# *** IMPORTANT CAVEAT (must travel with every consumer of history_daily,
# per SCOPING section 4): the AWS installation date introduces a step change
# in the underlying instrumentation/siting -- history_daily is fit for
# operational bounds and priors (climatology, plausibility ranges, gap-fill
# donors), but it is NOT a homogenized climate record and is UNFIT for trend
# analysis across the installation date without an explicit homogenization
# step this package does not perform. ***

#' Build the `history_hourly` product for a site
#'
#' Reads the site's stored observations over `window` and aggregates them to
#' hourly (`aggregate_hourly()`). This *is* `history_hourly` (SCOPING section
#' 4): the QC'd, gap-filled, hourly-aggregated AWS record. QC (`qc_run()`)
#' and gap-fill (`fill_run()`) are expected to have already run over the
#' window before this reads the store; this function does not itself
#' orchestrate them (that is the pipeline's job, Plan 16), it only reads and
#' aggregates whatever is currently on file.
#'
#' @param store_root Root directory of the store.
#' @param site A `met_site` object.
#' @param window A list with `from`/`to` POSIXct bounds.
#' @param as_of Optional UTC POSIXct point-in-time read, threaded straight
#'   into `store_read_obs()` (Plan 17 item 9): the aggregation is a pure
#'   function of the rows read, so a point-in-time read of the store
#'   reproduces the point-in-time product.
#' @return A canonical long obs tibble at hourly resolution.
#' @keywords internal
#' @noRd
build_history_hourly <- function(store_root, site, window, as_of = NULL) {
  sid <- site_id(site)
  raw <- store_read_obs(store_root, sid, from = window$from, to = window$to, as_of = as_of)
  if (nrow(raw) == 0) {
    return(raw)
  }
  aggregate_hourly(raw, dict = met_variables())
}

# Is a row's qc_flag "clean enough" to let it win the AWS-vs-SILO
# compositing (SCOPING section 4: "QC-clean" -- present and not fail-flagged;
# "suspect" is still allowed to win, since it is still the best available
# site truth, merely flagged for caution, whereas "fail"/"missing" carries no
# usable value at all).
.is_qc_clean <- function(qc_flag) {
  !is.na(qc_flag) & qc_flag != "fail" & qc_flag != "missing"
}

# Apply the current `(variable, "silo")` calibration to SILO daily rows
# (Plan 17 item 2), keyed on `datetime_utc` as the value's own time (there is
# no separate issue/valid distinction for an already-realised daily
# observation, mirroring `.correct_apply_fitted()`, R/correct.R). A variable
# with no calibration on file is left raw, tier `"physical"` (the day-0
# floor `correct_apply()` itself falls back to) -- this is never an error,
# just a no-op until `correct_refit()` fits one. Model-only variables never
# reach SILO's own dataset in practice, but are handled the same way as
# every other serve-time correction path for consistency: tier `"raw"`.
.apply_silo_calibration <- function(store_root, site, silo) {
  if (nrow(silo) == 0) {
    silo$tier <- character(0)
    return(silo)
  }

  sid <- site_id(site)
  manifest <- tryCatch(calib_manifest(store_root, sid), error = function(e) NULL)

  out <- silo
  out$tier <- NA_character_
  for (variable in unique(silo$variable)) {
    idx <- which(silo$variable == variable)

    if (isTRUE(met_variable(variable)$measurability_class == "model_only")) {
      out$tier[idx] <- "raw"
      next
    }

    rows_manifest <- if (is.null(manifest) || nrow(manifest) == 0) {
      NULL
    } else {
      manifest[manifest$variable == variable & manifest$source == "silo", , drop = FALSE]
    }
    if (is.null(rows_manifest) || nrow(rows_manifest) == 0) {
      out$tier[idx] <- "physical"
      next
    }

    calib <- calib_read(store_root, sid, variable, "silo")
    tier <- calib$manifest$tier[[1]]
    newdata <- tibble::tibble(
      issue_time = silo$datetime_utc[idx], valid_time = silo$datetime_utc[idx],
      forecast = silo$value[idx]
    )
    out$value[idx] <- .apply_fitted_values(calib$coeffs, tier, newdata)
    out$tier[idx] <- tier
  }
  out
}

#' Build the `history_daily` product for a site (SILO base, AWS wins)
#'
#' Composites a site's daily climate record from two legs (SCOPING section
#' 4): SILO daily observations (`source == "silo"`, gap-free by
#' construction), **site-corrected against AWS** via the current
#' `(variable, "silo")` calibration (Plan 17 item 2) as the base, overlaid
#' with the site's own AWS daily aggregate (any `source` other than
#' `"silo"`, e.g. `"site_aws"`) wherever present **and QC-clean** (not
#' `fail`/`missing`-flagged) for that `(variable, day)`. Provenance
#' (`source`) always records which leg served each value; a `tier` column
#' records the correction state (SCOPING section 4): the applied SILO
#' calibration tier for SILO-served rows, or `"raw"` for AWS-served rows
#' (measured truth carries no model correction). The composited frame is
#' then run through the post-correction physical-consistency pass
#' (`.consistency_pass_long()`, Plan 17 item 4), clipping any cross-variable
#' physical impossibility the correction introduced and counting how many
#' relations needed clipping (`attr(., "n_violations")`).
#'
#' **Caveat (must travel with every consumer):** the AWS installation date
#' introduces a step change in instrumentation/siting. `history_daily` is fit
#' for operational bounds and priors (climatology, plausibility ranges,
#' gap-fill donor selection) but is **not** a homogenized climate record and
#' is **unfit for trend analysis** across the installation date without an
#' explicit homogenization step this package does not perform.
#'
#' @param store_root Root directory of the store.
#' @param site A `met_site` object.
#' @param window A list with `from`/`to` POSIXct bounds.
#' @param as_of Optional UTC POSIXct point-in-time read, threaded straight
#'   into `store_read_obs()` (Plan 17 item 9).
#' @return A canonical long obs tibble at daily resolution, one row per
#'   `(variable, day)`, `source` recording which leg (`"silo"` or the AWS
#'   source name) served the value, plus a `tier` column recording the
#'   correction state; the clipped-consistency-relation count is attached as
#'   `attr(., "n_violations")`.
#' @keywords internal
#' @noRd
build_history_daily <- function(store_root, site, window, as_of = NULL) {
  sid <- site_id(site)
  raw <- store_read_obs(store_root, sid, from = window$from, to = window$to, as_of = as_of)
  if (nrow(raw) == 0) {
    return(raw)
  }

  silo <- raw[raw$source == "silo", , drop = FALSE]
  aws <- raw[raw$source != "silo", , drop = FALSE]
  aws_clean <- aws[.is_qc_clean(aws$qc_flag), , drop = FALSE]

  silo <- .apply_silo_calibration(store_root, site, silo)
  aws_clean$tier <- "raw"

  # Composite on the SITE-LOCAL calendar date, not the UTC date: SILO rows
  # are stamped at 9am local (`.silo_day_to_utc_instant()`) and AWS daily
  # aggregates at local midnight (rain: 9am local) -- for an Australian site
  # both instants fall on the previous UTC date, and for other offsets they
  # can fall on different UTC dates, so a UTC-date key would pair the wrong
  # (or no) days across the two legs.
  tz <- site@timezone
  key <- function(df) {
    paste(df$variable, format(df$datetime_utc, "%Y-%m-%d", tz = tz), sep = "\r")
  }
  silo_key <- key(silo)
  aws_key <- key(aws_clean)

  # AWS wins wherever it is present and clean for that (variable, day); SILO
  # serves everywhere else.
  silo_kept <- silo[!(silo_key %in% aws_key), , drop = FALSE]

  composited <- vctrs::vec_rbind(silo_kept, aws_clean)
  composited <- composited[order(composited$variable, composited$datetime_utc), , drop = FALSE]

  enforced <- .consistency_pass_long(composited, c("site_id", "datetime_utc"))
  n_violations <- attr(enforced, "n_violations") %||% 0L

  # new_obs() strips every non-canonical column (including `tier`); its
  # output preserves row order/count 1:1 against its input, so the stripped
  # `tier` can be reattached by position afterward.
  out <- new_obs(enforced)
  out$tier <- enforced$tier
  attr(out, "n_violations") <- n_violations
  out
}
