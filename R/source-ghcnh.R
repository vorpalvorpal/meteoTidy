# Plan 06 — source_ghcnh(): official-quality hourly obs via worldmet
# (GHCNh, `import_ghcn_hourly()`). See plans/06-acquisition-silo-ghcnh.md.
#
# `.worldmet_get()` is the package-owned seam wrapping worldmet — the ONLY
# place worldmet is called — mirroring `.weatheroz_get()`/`.http_get()`.
# Tests mock it directly (`with_mocked_ghcnh()`,
# tests/testthat/helper-station.R).

# Map a worldmet GHCNh column name to a meteoTidy dictionary variable + unit.
# `ws` is already m/s (worldmet convention, no km/h footgun), but is still
# run through `to_canonical()` as belt-and-braces per the implementer brief.
.ghcnh_variable_map <- function() {
  list(
    temperature_2m       = list(column = "air_temp", unit = "degC"),
    wind_direction_10m   = list(column = "wd",        unit = "degree"),
    wind_speed_10m       = list(column = "ws",         unit = "m/s")
  )
}

#' The worldmet (GHCNh) seam
#'
#' The package-owned wrapper around `worldmet::import_ghcn_hourly()` — the
#' only place in `meteoTidy` that calls worldmet. Every test mocks this
#' function directly (`with_mocked_ghcnh()`); the real body below is
#' exercised only outside tests, against the live GHCNh feed.
#'
#' @param station Single string, the GHCNh station id (e.g.
#'   `"ASN00072150"`).
#' @param year Integer vector of the calendar year(s) spanned by the request
#'   window.
#' @param ... Reserved.
#'
#' @return A data.frame shaped like `make_ghcnh_frame()` (see
#'   `tests/testthat/helper-station.R`): `code`, `station`, `date` (POSIXct,
#'   UTC), `air_temp`, `wd`, `ws`, `Quality_air_temp`.
#' @keywords internal
#' @noRd
.worldmet_get <- function(station, year, ...) {
  worldmet::import_ghcn_hourly(station = station, year = year)
}

# DECIDED SIMPLIFICATION (post-implementation audit, see IMPLEMENTER_PROMPT.md
# item 7): Plan 06 says qc_flag should be mapped from worldmet's quality
# field, but worldmet documents no specific "bad" Quality_air_temp code in
# general public materials, and no fixture/test exercises a non-ok GHCNh
# quality value -- there is no confirmed vocabulary to map from yet. Rather
# than guess a mapping that cannot be verified, this always returns "ok"
# (belt-and-braces default) until worldmet's quality vocabulary is confirmed,
# at which point this should become a real lookup keyed on
# `quality_air_temp` (and a fixture row exercising a non-ok code should be
# added alongside it).
.ghcnh_qc_flag <- function(quality_air_temp) {
  rep("ok", length(quality_air_temp))
}

.ghcnh_map_to_obs <- function(frame, site, variables) {
  var_map <- .ghcnh_variable_map()
  wanted <- intersect(variables, names(var_map))

  if (length(wanted) == 0 || nrow(frame) == 0) {
    return(new_obs(tibble::tibble(
      site_id = character(0),
      datetime_utc = as.POSIXct(character(0), tz = "UTC"),
      variable = character(0),
      value = double(0),
      source = character(0),
      method = character(0),
      qc_flag = character(0)
    )))
  }

  datetime_utc <- frame$date
  attr(datetime_utc, "tzone") <- "UTC"

  rows <- lapply(wanted, function(v) {
    spec <- var_map[[v]]
    raw_value <- frame[[spec$column]]
    tibble::tibble(
      site_id = site_id(site),
      datetime_utc = datetime_utc,
      variable = v,
      value = as.numeric(to_canonical(raw_value, spec$unit, v)),
      source = "ghcnh",
      method = "measured",
      qc_flag = .ghcnh_qc_flag(frame$Quality_air_temp)
    )
  })

  new_obs(do.call(rbind, rows))
}

#' A GHCNh acquisition adapter (worldmet)
#'
#' `source_ghcnh()` builds a [met_adapter()] that fetches official-quality
#' hourly observations from NCEI's Global Historical Climatology
#' Network-hourly (GHCNh) dataset via `worldmet::import_ghcn_hourly()`.
#' GHCNh is NCEI's official replacement for the legacy ISD hourly feed
#' (SCOPING §5).
#'
#' GHCNh is updated daily but publishes no real-time latency figure; it is
#' therefore treated as a **best-effort backfill source**, never the live
#' head (SCOPING §5.1/§13) — reflected in `adapter@cadence`, a structured
#' list `list(live = FALSE, lag_days = <n>)` rather than a simple string, so
#' the pipeline (Plan 16) can tell it apart from a live-head adapter
#' programmatically.
#'
#' @param source_id Single string stamped into the `source` column. Default
#'   `"ghcnh"`.
#'
#' @return A `source_ghcnh` (`met_adapter` subclass) S7 object.
#' @family adapter
#' @export
#' @examples
#' adapter <- source_ghcnh()
source_ghcnh <- S7::new_class(
  "source_ghcnh",
  package = "meteoTidy",
  parent = met_adapter,
  constructor = function(source_id = "ghcnh") {
    S7::new_object(
      met_adapter(
        source_id = source_id,
        provides = names(.ghcnh_variable_map()),
        cadence = list(live = FALSE, lag_days = 7L)
      )
    )
  }
)

# Every whole calendar year `window` spans (worldmet requests by year).
.ghcnh_years_in_window <- function(window) {
  from_year <- as.integer(format(window$from, "%Y", tz = "UTC"))
  to_year <- as.integer(format(window$to, "%Y", tz = "UTC"))
  seq.int(from_year, to_year)
}

.ghcnh_require_station <- function(site) {
  station <- site_resolved(site, c("ghcnh", "station_id"))
  if (is.null(station) || is.na(station)) {
    abort_meteo(
      c(
        "Site {.val {site_id(site)}} has no resolved GHCNh station.",
        "i" = "Call {.fn resolve_station} first."
      ),
      class = "unresolved_station"
    )
  }
  station
}

S7::method(fetch, source_ghcnh) <- function(adapter, site, variables, window, now = .now()) {
  variables <- intersect(variables, adapter@provides)
  station <- .ghcnh_require_station(site)

  frame <- .worldmet_get(station = station, year = .ghcnh_years_in_window(window))
  in_window <- frame$date >= window$from & frame$date <= window$to
  frame <- frame[in_window, , drop = FALSE]

  out <- .ghcnh_map_to_obs(frame, site, variables)
  check_fetch_result(out, adapter, variables)
}

# resolve_station() for source_ghcnh(): finds the nearest GHCNh station(s)
# to `site` from a catalogue obtained via `.worldmet_get()`. `n = 1`
# (default) sets `c("ghcnh", "station_id")` and `c("ghcnh", "distance_km")`;
# `n > 1` additionally sets `c("ghcnh", "station_ids")` to the `n` nearest
# ids in ascending distance order. See `resolve_station()`'s roxygen block
# in R/adapter.R for the user-facing doc; `n` is documented there too.
S7::method(resolve_station, source_ghcnh) <- function(adapter, site, ..., n = 1) {
  catalogue <- .worldmet_get(station = NULL, year = NULL)
  nearest <- nearest_stations(
    as.numeric(site@latitude), as.numeric(site@longitude), catalogue, n = n
  )

  site <- site_set_resolved(site, c("ghcnh", "station_id"), as.character(nearest$station_id[[1]]))
  site <- site_set_resolved(site, c("ghcnh", "distance_km"), nearest$distance_km[[1]])

  if (n > 1) {
    site <- site_set_resolved(site, c("ghcnh", "station_ids"), as.character(nearest$station_id))
  }

  site
}

#' Nearest-station completeness for a GHCNh donor
#'
#' Computes, for each requested `variable`, the fraction of hourly slots in
#' `window` (`[from, to]` inclusive) for which the site's resolved GHCNh
#' station has a non-`NA` value. This is a building block for Plan 16's
#' per-site donor-coverage audit (SCOPING §13), not the audit itself.
#'
#' @param adapter A [source_ghcnh()] object.
#' @param site A `met_site` object with a resolved GHCNh station (see
#'   [resolve_station()]).
#' @param window A list with `from`/`to`, both UTC `POSIXct` scalars.
#'
#' @return A tibble with columns `variable`, `completeness` (0..1).
#' @family adapter
#' @export
station_coverage <- function(adapter, site, window) {
  station <- .ghcnh_require_station(site)
  frame <- .worldmet_get(station = station, year = .ghcnh_years_in_window(window))

  expected_slots <- seq(from = window$from, to = window$to, by = "hour")
  n_expected <- length(expected_slots)

  var_map <- .ghcnh_variable_map()
  wanted <- intersect(adapter@provides, names(var_map))

  rows <- lapply(wanted, function(v) {
    spec <- var_map[[v]]
    in_window <- frame$date >= window$from & frame$date <= window$to
    present <- sum(!is.na(frame[[spec$column]][in_window]))
    tibble::tibble(variable = v, completeness = present / n_expected)
  })

  do.call(rbind, rows)
}

S7::method(format, source_ghcnh) <- function(x, ...) {
  c(
    sprintf("<source_ghcnh> source_id: %s", x@source_id),
    sprintf("  cadence: live = %s, lag_days = %s", x@cadence$live, x@cadence$lag_days)
  )
}

S7::method(print, source_ghcnh) <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
