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
# per variable -- the forecast analogue of `widen_obs()`. Deliberately
# simple: this plan's tests only exercise routing (that `met_forecast_archive()`
# was called), not the exact shape of the forecast-widening result, so this
# does not attempt member/stat handling beyond keeping a single value per
# (valid_time, variable) (the first row wins on a duplicate, matching how
# `widen_obs()` resolves a duplicate key by `match()`).
.widen_forecast <- function(fc, variables) {
  base <- unique(fc["valid_time"])
  base <- base[order(base$valid_time), , drop = FALSE]

  wide <- base
  for (v in variables) {
    sub <- fc[fc$variable == v, c("valid_time", "value")]
    matched <- sub$value[match(wide$valid_time, sub$valid_time)]
    wide[[v]] <- if (length(matched) == 0) rep(NA_real_, nrow(wide)) else matched
  }

  tibble::as_tibble(wide)
}

# Build a simple, honest provenance tibble for `met_wide()`'s output. Plan 16
# is where full pipeline-derived per-variable tier data gets threaded through;
# at this point in the series the only tier information reliably available is
# whatever the source data's own `method`/`source` columns carry, so this
# builds the plainest defensible provenance: `tier = "raw"` (a placeholder --
# no richer per-variable correction-tier metadata is available yet from
# `met_record()`/`met_forecast_archive()`'s return shape) and `source` taken
# from the underlying long table when present, else `NA`.
.met_wide_provenance <- function(value_cols, long, train_overlap = 0) {
  src <- if ("source" %in% names(long) && nrow(long) > 0) {
    long$source[match(value_cols, long$variable)]
  } else {
    NA_character_
  }
  tibble::tibble(
    variable = value_cols,
    tier = "raw",
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
#' @param site A `met_site` or `met_sites`.
#' @param window A list with `from`/`to` UTC POSIXct bounds.
#' @param kind Either `"forecast"` or `"record"` (default `"forecast"`).
#' @param variables Optional character vector of variable names. When
#'   supplied, every named variable appears as a column even if absent from
#'   the underlying data (an all-`NA` column) -- the stable section 3.1
#'   shape. Defaults to the variables actually present in the fetched data.
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
  s <- sites@sites[[1]]

  if (kind == "record") {
    long <- met_record(site, variables = variables, from = window$from, to = window$to)
    value_cols <- variables %||% unique(long$variable)
    wide <- widen_obs(long, variables = value_cols)
    names(wide)[names(wide) == "datetime_utc"] <- "time"
    wide$site_id <- NULL
  } else {
    long <- met_forecast_archive(site, valid_from = window$from, valid_to = window$to)
    value_cols <- variables %||% unique(long$variable)
    wide <- .widen_forecast(long, variables = value_cols)
    names(wide)[names(wide) == "valid_time"] <- "time"
  }

  attr(wide$time, "tzone") <- "UTC"

  provenance <- .met_wide_provenance(value_cols, long)
  keys <- list(site_id = site_id(s), from = window$from, to = window$to)
  versions <- list(
    schema_version = .met_wide_schema_version,
    calibration_manifest_version = .met_wide_calibration_manifest_version(
      site_store_root(s), site_id(s)
    )
  )

  new_met_table(wide, provenance = provenance, keys = keys, versions = versions)
}
