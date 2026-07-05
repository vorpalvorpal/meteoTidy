# Plan 07 — BOM response parsing: précis XML -> forecast + forecast_aux,
# 72-h/web-API obs JSON -> canonical obs, geohash search JSON -> geohash.
# Pure functions: no I/O, no HTTP/FTP calls (those live in R/http.R's
# `.ftp_get()`/`.http_get()` seams and are wired in by
# R/source-bom-forecast.R / R/source-bom-obs.R).

# ---- shared helpers ---------------------------------------------------

# Parse a BOM "+HH:MM"-suffixed local ISO8601 timestamp to a UTC POSIXct.
# base R's `%z` strptime specifier requires the offset WITHOUT a colon
# (e.g. "+1100"), so the colon is stripped first.
.bom_parse_offset_time <- function(x) {
  stripped <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", x)
  parsed <- as.POSIXct(stripped, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  attr(parsed, "tzone") <- "UTC"
  parsed
}

# Parse a plain "...Z"-suffixed UTC ISO8601 timestamp (no offset arithmetic
# needed; it is already UTC).
.bom_parse_utc_time <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# Parse BOM's yyyyMMddHHmmss UTC timestamp format (72-h obs JSON
# `aifstime_utc`).
.bom_parse_compact_time <- function(x) {
  as.POSIXct(x, format = "%Y%m%d%H%M%S", tz = "UTC")
}

# Accept either an already-parsed nested list (as `.http_get()`/
# `with_mocked_http()` return) or a raw JSON string (as `.ftp_get()`
# returns) and normalise both to the same nested-list shape
# (`simplifyVector = FALSE`), so every parser below only has to handle one
# representation.
.bom_as_parsed_json <- function(x) {
  if (is.character(x) && length(x) == 1) {
    return(jsonlite::fromJSON(x, simplifyVector = FALSE))
  }
  x
}

# ---- precis XML: forecast + forecast_aux -------------------------------

# Read a précis XML document (already-parsed `xml2` doc, or raw XML text) and
# return the (issue_time, first <area>, its forecast-periods) building
# blocks shared by the forecast and forecast_aux parsers.
#
# Simplification (documented, matches plan text): the fixture has exactly
# one <area>; a real implementation would match on the site's cached AAC
# code. Not required to pass the frozen tests against a single-area fixture.
.bom_precis_parts <- function(xml) {
  doc <- if (inherits(xml, "xml_document") || inherits(xml, "xml_node")) {
    xml
  } else {
    xml2::read_xml(xml)
  }

  issue_time <- .bom_parse_utc_time(
    xml2::xml_text(xml2::xml_find_first(doc, "//issue-time-utc"))
  )
  area <- xml2::xml_find_first(doc, "//area")
  periods <- xml2::xml_find_all(area, ".//forecast-period")

  list(issue_time = issue_time, periods = periods)
}

#' Parse a précis XML forecast issuance into canonical forecast rows
#'
#' Maps `<element type="air_temperature_maximum">` (the daily max
#' temperature) from each `<forecast-period>` to a `temperature_2m` forecast
#' row. Simplification (documented, matches the plan text): précis is
#' inherently daily-resolution, so the daily max is treated as a single
#' daily point value rather than being split into an hourly profile.
#' `model` is always `NA_character_` -- the edited BOM product has no model
#' name.
#'
#' @param xml An `xml2` document/node, or a raw XML string (as
#'   `.ftp_get()` returns).
#' @param site_id Single string, the `site_id` to stamp on every row.
#' @param source Single string, the `source` to stamp on every row.
#'
#' @return A canonical forecast tibble (see the internal `new_forecast()`).
#' @keywords internal
#' @noRd
bom_parse_precis_forecast <- function(xml, site_id, source = "bom_forecast") {
  parts <- .bom_precis_parts(xml)
  issue_time <- parts$issue_time
  periods <- parts$periods

  rows <- lapply(periods, function(period) {
    valid_time <- .bom_parse_offset_time(xml2::xml_attr(period, "start-time-local"))
    tmax <- xml2::xml_find_first(period, ".//element[@type='air_temperature_maximum']")
    if (is.na(tmax)) {
      return(NULL)
    }
    value <- as.numeric(xml2::xml_text(tmax))
    tibble::tibble(
      site_id = site_id,
      source = source,
      model = NA_character_,
      issue_time = issue_time,
      valid_time = valid_time,
      lead_time = as.difftime(
        as.numeric(difftime(valid_time, issue_time, units = "hours")), units = "hours"
      ),
      member = NA_integer_,
      stat = NA_character_,
      variable = "temperature_2m",
      value = value
    )
  })
  rows <- Filter(Negate(is.null), rows)

  if (length(rows) == 0) {
    return(tibble::tibble(
      site_id = character(0), source = character(0), model = character(0),
      issue_time = as.POSIXct(character(0), tz = "UTC"),
      valid_time = as.POSIXct(character(0), tz = "UTC"),
      lead_time = as.difftime(numeric(0), units = "hours"),
      member = integer(0), stat = character(0),
      variable = character(0), value = double(0)
    ))
  }

  do.call(rbind, rows)
}

#' Parse a précis XML forecast issuance into canonical forecast_aux rows
#'
#' Every `<text type="...">...</text>` child of a `<forecast-period>` becomes
#' one `forecast_aux` row: `field` is the `type` attribute (`"precis"`,
#' `"forecast"`, `"fire_danger"`, `"uv_alert"`), `value_text` is the
#' element's text content verbatim, keyed with the same `issue_time`/
#' `valid_time` as that period.
#'
#' @inheritParams bom_parse_precis_forecast
#' @return A canonical forecast_aux tibble (see the internal
#'   `new_forecast_aux()`).
#' @keywords internal
#' @noRd
bom_parse_precis_aux <- function(xml, site_id, source = "bom_forecast") {
  parts <- .bom_precis_parts(xml)
  issue_time <- parts$issue_time
  periods <- parts$periods

  rows <- lapply(periods, function(period) {
    valid_time <- .bom_parse_offset_time(xml2::xml_attr(period, "start-time-local"))
    texts <- xml2::xml_find_all(period, ".//text")
    if (length(texts) == 0) {
      return(NULL)
    }
    tibble::tibble(
      site_id = site_id,
      source = source,
      issue_time = issue_time,
      valid_time = valid_time,
      field = vapply(texts, xml2::xml_attr, character(1), attr = "type"),
      value_text = vapply(texts, xml2::xml_text, character(1))
    )
  })
  rows <- Filter(Negate(is.null), rows)

  if (length(rows) == 0) {
    return(tibble::tibble(
      site_id = character(0), source = character(0),
      issue_time = as.POSIXct(character(0), tz = "UTC"),
      valid_time = as.POSIXct(character(0), tz = "UTC"),
      field = character(0), value_text = character(0)
    ))
  }

  do.call(rbind, rows)
}

# ---- 72-h obs JSON -> canonical obs -------------------------------------

# Map one BOM 72-h obs JSON row (a named list) to canonical (variable,
# value, unit) triples, restricted to `variables` requested.
.bom_72h_row_values <- function(row, variables) {
  out <- list()
  if ("temperature_2m" %in% variables && !is.null(row$air_temp)) {
    out[["temperature_2m"]] <- list(value = as.numeric(row$air_temp), unit = "degC")
  }
  if ("wind_speed_10m" %in% variables && !is.null(row$wind_spd_kmh)) {
    out[["wind_speed_10m"]] <- list(value = as.numeric(row$wind_spd_kmh), unit = "km/h")
  }
  if ("wind_direction_10m" %in% variables && !is.null(row$wind_dir)) {
    out[["wind_direction_10m"]] <- list(
      value = compass2angle(row$wind_dir), unit = "degree"
    )
  }
  if ("relative_humidity_2m" %in% variables && !is.null(row$rel_hum)) {
    out[["relative_humidity_2m"]] <- list(value = as.numeric(row$rel_hum), unit = "%")
  }
  out
}

.bom_empty_obs <- function() {
  tibble::tibble(
    site_id = character(0), datetime_utc = as.POSIXct(character(0), tz = "UTC"),
    variable = character(0), value = double(0), source = character(0),
    method = character(0), qc_flag = character(0)
  )
}

# Shared row-list -> canonical-obs-tibble assembly for both BOM obs JSON
# shapes. `extract` is a function(row, variables) -> named list of
# list(value=, unit=), and `time_of` is function(row) -> UTC POSIXct scalar.
.bom_obs_rows_to_tibble <- function(rows, variables, extract, time_of, site_id, source) {
  pieces <- lapply(rows, function(row) {
    datetime_utc <- time_of(row)
    values <- extract(row, variables)
    if (length(values) == 0) {
      return(NULL)
    }
    tibble::tibble(
      site_id = site_id,
      datetime_utc = datetime_utc,
      variable = names(values),
      value = vapply(seq_along(values), function(i) {
        as.numeric(to_canonical(values[[i]]$value, values[[i]]$unit, names(values)[i]))
      }, double(1)),
      source = source,
      method = "measured",
      qc_flag = "ok"
    )
  })
  pieces <- Filter(Negate(is.null), pieces)
  if (length(pieces) == 0) {
    return(.bom_empty_obs())
  }
  do.call(rbind, pieces)
}

#' Parse a rolling 72-h obs JSON response into canonical obs rows
#'
#' `observations.data[]`: `aifstime_utc` (`yyyyMMddHHmmss` UTC), `air_temp`
#' (degC), `wind_spd_kmh` (km/h), `wind_dir` (compass string, mapped only
#' when `wind_direction_10m` is requested), `rel_hum` (%). `method` is
#' always `"measured"`.
#'
#' @param body A parsed JSON list (already-parsed nested list, or a raw JSON
#'   string as returned by `.ftp_get()`).
#' @param variables Character vector of requested dictionary variable names.
#' @param site_id Single string, the `site_id` to stamp on every row.
#' @param source Single string, the `source` to stamp on every row.
#'
#' @return A canonical obs tibble (see the internal `new_obs()`).
#' @keywords internal
#' @noRd
bom_parse_72h_obs <- function(body, variables, site_id, source = "bom_obs") {
  parsed <- .bom_as_parsed_json(body)
  rows <- parsed$observations$data %||% list()

  .bom_obs_rows_to_tibble(
    rows, variables,
    extract = .bom_72h_row_values,
    time_of = function(row) .bom_parse_compact_time(row$aifstime_utc),
    site_id = site_id, source = source
  )
}

# Map one BOM web-API obs JSON row (a named list) to canonical (variable,
# value, unit) triples, restricted to `variables` requested.
.bom_webapi_row_values <- function(row, variables) {
  out <- list()
  if ("temperature_2m" %in% variables && !is.null(row$temp)) {
    out[["temperature_2m"]] <- list(value = as.numeric(row$temp), unit = "degC")
  }
  wind <- row$wind
  if ("wind_speed_10m" %in% variables && !is.null(wind$speed_kilometre)) {
    out[["wind_speed_10m"]] <- list(value = as.numeric(wind$speed_kilometre), unit = "km/h")
  }
  if ("wind_direction_10m" %in% variables && !is.null(wind$direction)) {
    out[["wind_direction_10m"]] <- list(
      value = compass2angle(wind$direction), unit = "degree"
    )
  }
  if ("relative_humidity_2m" %in% variables && !is.null(row$humidity)) {
    out[["relative_humidity_2m"]] <- list(value = as.numeric(row$humidity), unit = "%")
  }
  out
}

#' Parse a web-API obs JSON response into canonical obs rows
#'
#' `data[]`: `time` (ISO8601 UTC string), `temp` (degC), nested
#' `wind.speed_kilometre` (km/h) / `wind.direction` (compass string, mapped
#' only when `wind_direction_10m` is requested), `humidity` (%). `method` is
#' always `"measured"`.
#'
#' @inheritParams bom_parse_72h_obs
#' @return A canonical obs tibble (see the internal `new_obs()`).
#' @keywords internal
#' @noRd
bom_parse_webapi_obs <- function(body, variables, site_id, source = "bom_obs") {
  parsed <- .bom_as_parsed_json(body)
  rows <- parsed$data %||% list()

  .bom_obs_rows_to_tibble(
    rows, variables,
    extract = .bom_webapi_row_values,
    time_of = function(row) .bom_parse_utc_time(row$time),
    site_id = site_id, source = source
  )
}

# ---- web-API geohash search JSON -> geohash string ----------------------

#' Parse a web-API geohash-search JSON response into a single geohash
#'
#' `data[]`: each row has `geohash`/`latitude`/`longitude`/etc. Simplification
#' (documented, matches plan text): the fixture has exactly one row; takes
#' the first row's `geohash` verbatim. A real implementation would query by
#' the site's lat/lon and pick the nearest/matching result (see
#' `nearest_stations()` in `R/station-resolve.R`) -- not required to make a
#' single-row fixture pass, so not built here to avoid over-engineering an
#' untested path.
#'
#' @param body A parsed JSON list (already-parsed nested list, or a raw JSON
#'   string).
#' @return A single string, the resolved geohash.
#' @keywords internal
#' @noRd
bom_parse_geohash_search <- function(body) {
  parsed <- .bom_as_parsed_json(body)
  rows <- parsed$data %||% list()
  if (length(rows) == 0) {
    abort_meteo(
      "BOM geohash search returned no results.",
      class = "bom_geohash_unavailable"
    )
  }
  as.character(rows[[1]]$geohash)
}
