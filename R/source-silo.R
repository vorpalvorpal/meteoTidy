# Plan 06 — source_silo(): daily Australian series via weatherOz
# (SILO PatchedPoint / DataDrill). See plans/06-acquisition-silo-ghcnh.md.
#
# `.weatheroz_get()` is the package-owned seam wrapping weatherOz (mirrors
# `.http_get()`/`source_rest()`'s pattern): it is the ONLY place weatherOz is
# called, so tests mock it once (`with_mocked_silo()`,
# `tests/testthat/helper-station.R`) instead of touching the network or
# weatherOz's internals.

# Map a SILO `variable_name` (weatherOz::silo_daily_values names, e.g.
# "max_temp", "radiation") to a meteoTidy dictionary variable. Only the
# subset with an obvious canonical counterpart is mapped; SILO variables with
# no dictionary equivalent (evapotranspiration variants, vapour pressure
# deficit, etc.) are left unmapped and simply pass through unrecognised if
# ever requested (`met_variable()` will abort, matching the "unrequested
# variable" contract).
.silo_variable_map <- function() {
  c(
    max_temp   = "temperature_2m",
    min_temp   = "temperature_2m",
    rain       = "precipitation",
    radiation  = "direct_radiation",
    mslp       = "pressure_msl",
    rh_tmax    = "relative_humidity_2m",
    rh_tmin    = "relative_humidity_2m"
  )
}

.silo_variable_unit <- function() {
  c(
    max_temp   = "degC",
    min_temp   = "degC",
    rain       = "mm",
    radiation  = "MJ/m2",
    mslp       = "hPa",
    rh_tmax    = "%",
    rh_tmin    = "%"
  )
}

#' The weatherOz seam
#'
#' The package-owned wrapper around `weatherOz::get_patched_point()` /
#' `weatherOz::get_data_drill()` — the only place in `meteoTidy` that calls
#' weatherOz. Every test mocks this function directly
#' (`with_mocked_silo()`); the real body below is exercised only outside
#' tests, against the live SILO API.
#'
#' @param query A list describing the request: `station_code` (PatchedPoint)
#'   or `longitude`/`latitude` (DataDrill), `start_date`, `end_date`, and
#'   `values`.
#' @param dataset One of `"patched_point"` or `"data_drill"`.
#' @param api_key Single string, the SILO API "key" (an email address,
#'   PII not a secret — SCOPING §11).
#' @param ... Reserved.
#'
#' @return A data.frame shaped like `make_silo_frame()` (see
#'   `tests/testthat/helper-station.R`): `station_code`, `station_name`,
#'   `latitude`, `longitude`, `date`, `variable_name`, `value`,
#'   `value_quality`.
#' @keywords internal
#' @noRd
.weatheroz_get <- function(query, dataset, api_key, ...) {
  raw <- if (identical(dataset, "patched_point")) {
    weatherOz::get_patched_point(
      station_code = query$station_code,
      start_date = query$start_date,
      end_date = query$end_date,
      values = query$values %||% "all",
      api_key = api_key
    )
  } else if (identical(dataset, "data_drill")) {
    weatherOz::get_data_drill(
      longitude = query$longitude,
      latitude = query$latitude,
      start_date = query$start_date,
      end_date = query$end_date,
      values = query$values %||% "all",
      api_key = api_key
    )
  } else {
    abort_meteo(
      "Unknown SILO {.arg dataset}: {.val {dataset}}.",
      class = "unknown_adapter"
    )
  }

  .silo_reshape_to_long(raw)
}

# weatherOz's get_patched_point()/get_data_drill() return one row per date
# with one column per requested value (wide, weatherOz's own variable
# codes). Reshape to the long shape `.weatheroz_get()` documents/returns
# (one row per station/date/variable), matching `make_silo_frame()`.
.silo_reshape_to_long <- function(raw) {
  value_cols <- intersect(names(.silo_variable_map()), names(raw))

  rows <- lapply(value_cols, function(v) {
    quality_col <- paste0(v, "_source")
    data.frame(
      station_code = raw$station_code,
      station_name = raw$station_name,
      latitude = raw$latitude,
      longitude = raw$longitude,
      date = as.Date(raw$date),
      variable_name = v,
      value = raw[[v]],
      value_quality = as.character(if (quality_col %in% names(raw)) raw[[quality_col]] else NA),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

#' A SILO acquisition adapter (weatherOz)
#'
#' `source_silo()` builds a [met_adapter()] that fetches daily Australian
#' climate series from SILO via `weatherOz`, either PatchedPoint (nearest BOM
#' station, patched/interpolated) or DataDrill (5x5 km gridded surface).
#'
#' The SILO "API key" is an email address (PII, not a secret — SCOPING §11):
#' it is read from the named environment variable **at fetch/resolve time
#' only**, never stored on the adapter object, and never written into any
#' returned data column.
#'
#' Each SILO daily value carries a source/quality code (see
#' [silo_qcode_map()]) that is mapped into the canonical `method`/`qc_flag`
#' columns rather than being collapsed to a blanket `"measured"`/`"ok"`
#' (SCOPING §5: "ingest SILO source/quality codes into provenance").
#'
#' SILO's daily boundary is the **9am local-clock-time** rainfall-day
#' convention (SCOPING §3): each row's `date` is mapped to 9am local clock
#' time in the site's IANA timezone, expressed as the equivalent UTC instant
#' (DST-aware — the same wall-clock 9am is a different UTC hour in summer vs
#' winter).
#'
#' @param api_key_env Single string, the name of the environment variable
#'   holding the SILO email address.
#' @param dataset One of `"patched_point"` (station-based) or `"data_drill"`
#'   (gridded).
#' @param source_id Single string stamped into the `source` column. Default
#'   `"silo"`.
#'
#' @return A `source_silo` (`met_adapter` subclass) S7 object.
#' @family adapter
#' @export
#' @examples
#' adapter <- source_silo(api_key_env = "SILO_EMAIL", dataset = "patched_point")
source_silo <- S7::new_class(
  "source_silo",
  package = "meteoTidy",
  parent = met_adapter,
  properties = list(
    api_key_env = S7::class_character,
    dataset = S7::class_character
  ),
  constructor = function(api_key_env, dataset = c("patched_point", "data_drill"),
                         source_id = "silo") {
    dataset <- rlang::arg_match(dataset)
    S7::new_object(
      met_adapter(
        source_id = source_id,
        provides = unname(.silo_variable_map()),
        cadence = "daily"
      ),
      api_key_env = api_key_env,
      dataset = dataset
    )
  }
)

# Read the SILO email at fetch/resolve time only -- never stored on the
# adapter object, never written into returned data. Deliberately does NOT
# abort when unset: `.weatheroz_get()` is the seam that actually needs the
# key (and is always mocked in tests); a live call with an empty key will
# fail loudly inside weatherOz itself, which is an acceptable place for that
# failure to surface.
.silo_read_key <- function(adapter) {
  Sys.getenv(adapter@api_key_env, unset = NA_character_)
}

# The UTC instant SILO's 9am-local-clock-time daily boundary maps to for a
# given (Date, IANA timezone) pair. R POSIXct is always an absolute instant
# internally; constructing "<date> 09:00:00" in the site's local timezone
# and then relabelling the `tzone` attribute to "UTC" (not re-parsing) gives
# the correct DST-aware UTC instant for the identical local wall-clock time.
.silo_day_to_utc_instant <- function(date, timezone) {
  local_9am <- as.POSIXct(paste(date, "09:00:00"), tz = timezone)
  attr(local_9am, "tzone") <- "UTC"
  local_9am
}

# Map one weatherOz-shaped long frame (see `.weatheroz_get()`) to canonical
# obs rows for `site`, restricted to `variables` (dictionary names).
.silo_map_to_obs <- function(frame, site, variables) {
  var_map <- .silo_variable_map()
  unit_map <- .silo_variable_unit()

  canonical_variable <- var_map[frame$variable_name]
  keep <- !is.na(canonical_variable) & canonical_variable %in% variables
  frame <- frame[keep, , drop = FALSE]
  canonical_variable <- canonical_variable[keep]

  if (nrow(frame) == 0) {
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

  datetime_utc <- .silo_day_to_utc_instant(frame$date, site@timezone)

  quality <- lapply(frame$value_quality, silo_qcode_map)
  method <- vapply(quality, `[[`, character(1), "method")
  qc_flag <- vapply(quality, `[[`, character(1), "qc_flag")

  units_for_row <- unname(unit_map[frame$variable_name])
  canonical_value <- vapply(seq_len(nrow(frame)), function(i) {
    as.numeric(to_canonical(frame$value[i], units_for_row[i], canonical_variable[i]))
  }, double(1))

  new_obs(tibble::tibble(
    site_id = site_id(site),
    datetime_utc = datetime_utc,
    variable = unname(canonical_variable),
    value = canonical_value,
    source = "silo",
    method = method,
    qc_flag = qc_flag
  ))
}

S7::method(fetch, source_silo) <- function(adapter, site, variables, window, now = .now()) {
  variables <- intersect(variables, adapter@provides)
  api_key <- .silo_read_key(adapter)

  query <- list(
    station_code = site_resolved(site, c("silo", "station")),
    longitude = as.numeric(site@longitude),
    latitude = as.numeric(site@latitude),
    start_date = as.Date(window$from),
    end_date = as.Date(window$to),
    values = "all"
  )

  frame <- .weatheroz_get(query = query, dataset = adapter@dataset, api_key = api_key)
  out <- .silo_map_to_obs(frame, site, variables)
  check_fetch_result(out, adapter, variables)
}

S7::method(resolve_station, source_silo) <- function(adapter, site, ...) {
  api_key <- .silo_read_key(adapter)
  catalogue <- .weatheroz_get(
    query = list(), dataset = adapter@dataset, api_key = api_key
  )

  nearest <- nearest_stations(
    as.numeric(site@latitude), as.numeric(site@longitude), catalogue, n = 1
  )

  if (identical(adapter@dataset, "patched_point")) {
    site_set_resolved(site, c("silo", "station"), as.character(nearest$station_id[[1]]))
  } else {
    site_set_resolved(site, c("silo", "grid"), as.character(nearest$station_id[[1]]))
  }
}

S7::method(format, source_silo) <- function(x, ...) {
  c(
    sprintf("<source_silo> source_id: %s", x@source_id),
    sprintf("  dataset: %s", x@dataset),
    sprintf("  api_key_env: %s (value not shown)", x@api_key_env)
  )
}

S7::method(print, source_silo) <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
