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
#' @return A canonical long obs tibble at hourly resolution.
#' @keywords internal
#' @noRd
build_history_hourly <- function(store_root, site, window) {
  sid <- site_id(site)
  raw <- store_read_obs(store_root, sid, from = window$from, to = window$to)
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

#' Build the `history_daily` product for a site (SILO base, AWS wins)
#'
#' Composites a site's daily climate record from two legs (SCOPING section
#' 4): SILO daily observations (`source == "silo"`, gap-free by
#' construction) as the base, overlaid with the site's own AWS daily
#' aggregate (any `source` other than `"silo"`, e.g. `"site_aws"`) wherever
#' present **and QC-clean** (not `fail`/`missing`-flagged) for that
#' `(variable, day)`. Provenance (`source`) always records which leg served
#' each value.
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
#' @return A canonical long obs tibble at daily resolution, one row per
#'   `(variable, day)`, `source` recording which leg (`"silo"` or the AWS
#'   source name) served the value.
#' @keywords internal
#' @noRd
build_history_daily <- function(store_root, site, window) {
  sid <- site_id(site)
  raw <- store_read_obs(store_root, sid, from = window$from, to = window$to)
  if (nrow(raw) == 0) {
    return(raw)
  }

  silo <- raw[raw$source == "silo", , drop = FALSE]
  aws <- raw[raw$source != "silo", , drop = FALSE]
  aws_clean <- aws[.is_qc_clean(aws$qc_flag), , drop = FALSE]

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

  out <- vctrs::vec_rbind(silo_kept, aws_clean)
  out <- out[order(out$variable, out$datetime_utc), , drop = FALSE]
  new_obs(out)
}
