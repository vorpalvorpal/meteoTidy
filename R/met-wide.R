#' @include met-table.R met-table-hash.R read-api.R
NULL

# Plan 15 -- the one-call meteoHazard interface (SCOPING section 10): the
# wide, Open-Meteo-named section 3.1 hourly table, wrapped as a `met_table`.
# `kind = "record"` reads the curated observation record (`met_record()`,
# Plan 14) for hindcast; `kind = "forecast"` reads the archived/corrected
# forecast (`met_forecast_archive()`, Plan 14) for prediction. Both paths
# widen to one row per timestamp with one column per variable, rename the
# time index to `time` at this outer boundary only (the canonical stores
# keep `datetime_utc`/`valid_time`), and wrap the result with provenance,
# keys, and versions.

.met_wide_schema_version <- "1.0.0"

# The exact SCOPING section 3.1 wide-contract variable set (the stable shape
# meteoHazard consumes), in contract order. Plan 15: when `variables` is not
# supplied, met_wide() emits exactly this set -- absent variables appear as
# all-NA columns rather than silently narrowing the table to whatever the
# window happened to contain.
.met31_variables <- function() {
  c(
    "temperature_2m", "relative_humidity_2m", "surface_pressure",
    "pressure_msl", "precipitation", "cloud_cover", "direct_radiation",
    "diffuse_radiation", "wind_speed_10m", "wind_direction_10m",
    "wind_gusts_10m", "wind_speed_80m", "wind_direction_80m",
    "wind_speed_120m", "wind_direction_120m", "wind_speed_180m",
    "wind_direction_180m", "boundary_layer_height",
    "soil_moisture_0_to_1cm", "soil_moisture_1_to_3cm"
  )
}

# The manifest has no single global "version" concept (Plan 03's calibration
# store versions each (variable, source) pair independently) -- for a
# multi-variable wide table, the most defensible single number is the
# highest version any calibration at this site has reached, or 0L if none
# exists yet (no calibration has ever been fit).
.met_wide_calibration_manifest_version <- function(store_root, site_id) {
  manifest <- tryCatch(calib_manifest(store_root, site_id), error = function(e) NULL)
  if (is.null(manifest) || nrow(manifest) == 0) {
    return(0L)
  }
  as.integer(max(manifest$version))
}

# Widen a canonical forecast tibble to one row per `valid_time`, one column
# per variable -- the forecast analogue of `widen_obs()`.
#
# DECIDED ensemble contract (post-implementation audit, see
# IMPLEMENTER_PROMPT.md item 6): the section 3.1 wide shape is one row per
# timestamp, one column per variable (SCOPING section 3.1) -- a per-member
# trajectory table already exists (`met_forecast_archive(members = TRUE)`),
# so the wide emitter does not attempt to also be one. Per (valid_time,
# variable), this takes the **ensemble mean across member rows** when
# members are present (a deterministic single row is the mean of one, so
# this is a no-op for non-ensemble sources) -- never a first-row-wins pick,
# which would silently and arbitrarily drop every member but one. Named
# `stat` summary rows (e.g. a source's own precomputed "min"/"max") are
# excluded from the mean: mixing heterogeneous stats into an unweighted
# average would be meaningless.
.widen_forecast <- function(fc, variables) {
  base <- unique(fc["valid_time"])
  base <- base[order(base$valid_time), , drop = FALSE]

  wide <- base
  for (v in variables) {
    sub <- fc[fc$variable == v & is.na(fc$stat), c("valid_time", "value")]
    # A requested variable absent from the archive window (or an entirely
    # empty window) must yield an all-NA column, not an error --
    # stats::aggregate() aborts on zero rows ("no rows to aggregate").
    if (nrow(sub) == 0) {
      wide[[v]] <- rep(NA_real_, nrow(wide))
      next
    }
    agg <- stats::aggregate(value ~ valid_time, data = sub, FUN = mean, na.rm = TRUE)
    matched <- agg$value[match(wide$valid_time, agg$valid_time)]
    wide[[v]] <- if (length(matched) == 0) rep(NA_real_, nrow(wide)) else matched
  }

  tibble::as_tibble(wide)
}

# Restrict an archived-forecast read to the LATEST issuance per (source,
# model). met_wide(kind = "forecast") serves "the corrected forecast for
# prediction" (SCOPING section 10): with the archive-on-every-sync policy the
# store holds every past issuance overlapping a valid window, and pooling
# them into one mean would average today's forecast with progressively
# staler ones. Older issuances remain fully retrievable via
# met_forecast_archive() -- this filter is only about what the one-call wide
# table means.
.latest_issuance <- function(fc) {
  if (nrow(fc) == 0) {
    return(fc)
  }
  grp_key <- paste(fc$source, fc$model, sep = "\r")
  keep <- rep(FALSE, nrow(fc))
  for (g in unique(grp_key)) {
    in_grp <- grp_key == g
    keep[in_grp] <- fc$issue_time[in_grp] == max(fc$issue_time[in_grp])
  }
  fc[keep, , drop = FALSE]
}

# Build `met_wide()`'s output provenance, with a real per-variable
# correction tier (post-implementation audit, see IMPLEMENTER_PROMPT.md item
# 6: "once item 1 lands, thread the real per-variable correction tier
# through"). Item 1 (R/correct.R's correct_refit()) is what makes
# calib_manifest() an honest source of truth for "what tier is this
# (variable, source) actually calibrated at", so this looks each value
# column's *current* manifest tier up there instead of hardcoding `"raw"`:
#
#  - model-only variables (SCOPING section 7.3) have no site truth to
#    correct against and are always `"raw"`;
#  - a variable with no calibration on file yet is `"physical"` (the day-0
#    default `correct_apply()` itself falls back to);
#  - otherwise, the highest-version manifest row's `tier` for
#    `(variable, source)`, where `source` is the value column's own source
#    (from the underlying long table, same lookup this function always did).
.met_wide_provenance <- function(store_root, site_id, value_cols, long) {
  src <- if ("source" %in% names(long) && nrow(long) > 0) {
    long$source[match(value_cols, long$variable)]
  } else {
    rep(NA_character_, length(value_cols))
  }

  manifest <- tryCatch(calib_manifest(store_root, site_id), error = function(e) NULL)

  tier <- character(length(value_cols))
  train_overlap <- numeric(length(value_cols))
  for (i in seq_along(value_cols)) {
    v <- value_cols[[i]]
    s <- src[[i]]
    if (isTRUE(met_variable(v)$measurability_class == "model_only")) {
      tier[[i]] <- "raw"
      next
    }
    rows <- if (is.null(manifest) || nrow(manifest) == 0 || is.na(s)) {
      NULL
    } else {
      manifest[manifest$variable == v & manifest$source == s, , drop = FALSE]
    }
    if (is.null(rows) || nrow(rows) == 0) {
      tier[[i]] <- "physical"
      next
    }
    current <- rows[which.max(rows$version), , drop = FALSE]
    tier[[i]] <- current$tier[[1]]
    # SCOPING section 3.2: provenance carries the training-overlap length.
    # The manifest records the fit's training window; report it in hours.
    train_overlap[[i]] <- as.numeric(difftime(current$train_end[[1]],
                                              current$train_start[[1]],
                                              units = "hours"))
  }

  tibble::tibble(
    variable = value_cols,
    tier = tier,
    train_overlap = train_overlap,
    source = src
  )
}

#' The section 3.1 wide emitter -- the one-call meteoHazard interface
#'
#' Returns the wide, Open-Meteo-named hourly table (SCOPING section 3.1):
#' one row per timestamp, one column per variable, canonical units, `time`
#' in UTC -- wrapped as a [new_met_table()] carrying provenance, keys, and
#' versions. `kind = "record"` reads the curated observation record
#' ([met_record()]) for hindcast; `kind = "forecast"` reads the archived
#' forecast ([met_forecast_archive()]) for prediction.
#'
#' For `kind = "forecast"`, only the **latest archived issuance** per
#' `(source, model)` is served: the archive holds every past issuance
#' overlapping the window (SCOPING section 9's archive-on-every-sync
#' policy), and pooling them would average the current forecast with stale
#' ones. Ensemble members within that issuance are reported as the ensemble
#' mean; per-member trajectories remain available via
#' [met_forecast_archive()].
#'
#' @param site A single `met_site` (or a `met_sites` of length one) -- the
#'   wide table is a per-site product.
#' @param window A list with `from`/`to` UTC POSIXct bounds.
#' @param kind Either `"forecast"` or `"record"` (default `"forecast"`).
#' @param variables Optional character vector of variable names. Every named
#'   variable appears as a column even if absent from the underlying data
#'   (an all-`NA` column) -- the stable section 3.1 shape. Defaults to the
#'   full section 3.1 contract set (see SCOPING section 3.1).
#' @param now Injectable current time; see `.now()`.
#' @return A `met_table`.
#' @family met-table
#' @export
#' @examples
#' \dontrun{
#' met_wide(site, window = list(from = as.POSIXct("2026-01-01", tz = "UTC"),
#'                              to = as.POSIXct("2026-01-02", tz = "UTC")),
#'         kind = "record")
#' }
met_wide <- function(site, window, kind = c("forecast", "record"), variables = NULL,
                     now = .now()) {
  kind <- rlang::arg_match(kind)
  sites <- as_met_sites(site)
  if (length(sites@sites) != 1) {
    abort_meteo(
      c(
        "met_wide() builds a per-site table; {.arg site} has {length(sites@sites)} sites.",
        "i" = "Call it once per site (the wide table has no site_id column)."
      ),
      class = "multi_site_wide"
    )
  }
  s <- sites@sites[[1]]
  value_cols <- variables %||% .met31_variables()

  if (kind == "record") {
    long <- met_record(site, variables = variables, from = window$from, to = window$to)
    wide <- widen_obs(long, variables = value_cols)
    names(wide)[names(wide) == "datetime_utc"] <- "time"
    wide$site_id <- NULL
  } else {
    long <- met_forecast_archive(site, valid_from = window$from, valid_to = window$to)
    long <- .latest_issuance(long)
    wide <- .widen_forecast(long, variables = value_cols)
    names(wide)[names(wide) == "valid_time"] <- "time"
  }

  attr(wide$time, "tzone") <- "UTC"

  provenance <- .met_wide_provenance(site_store_root(s), site_id(s), value_cols, long)
  keys <- list(site_id = site_id(s), from = window$from, to = window$to)
  versions <- list(
    schema_version = .met_wide_schema_version,
    calibration_manifest_version = .met_wide_calibration_manifest_version(
      site_store_root(s), site_id(s)
    )
  )

  new_met_table(wide, provenance = provenance, keys = keys, versions = versions)
}
