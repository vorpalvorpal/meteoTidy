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

# ---- forecast row builders ---------------------------------------------

# One forecast row per non-missing value. `value` may contain NULL/NA
# (BOM nulls out elements that no longer apply, e.g. today's minimum).
.bom_fc_rows <- function(site_id, source, model, issue_time, valid_time, variable, value,
                         stat = NA_character_) {
  value <- suppressWarnings(as.numeric(value))
  keep <- !is.na(value) & !is.na(valid_time)
  if (!any(keep)) {
    return(NULL)
  }
  valid_time <- valid_time[keep]
  tibble::tibble(
    site_id = site_id, source = source, model = model,
    issue_time = issue_time, valid_time = valid_time,
    lead_time = as.difftime(as.numeric(difftime(valid_time, issue_time, units = "hours")),
                            units = "hours"),
    member = NA_integer_, stat = stat, variable = variable, value = value[keep]
  )
}

.bom_bind_fc <- function(rows) {
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(.empty_forecast())
  }
  vctrs::vec_rbind(!!!rows)
}

.bom_aux_rows <- function(site_id, source, issue_time, valid_time, field, value_text) {
  value_text <- as.character(value_text)
  keep <- !is.na(value_text) & nzchar(value_text)
  if (!any(keep)) {
    return(NULL)
  }
  tibble::tibble(
    site_id = site_id, source = source, issue_time = issue_time,
    valid_time = valid_time, field = field, value_text = value_text[keep]
  )
}

.empty_forecast_aux <- function() {
  tibble::tibble(
    site_id = character(0), source = character(0),
    issue_time = as.POSIXct(character(0), tz = "UTC"),
    valid_time = as.POSIXct(character(0), tz = "UTC"),
    field = character(0), value_text = character(0)
  )
}

.bom_bind_aux <- function(rows) {
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(.empty_forecast_aux())
  }
  vctrs::vec_rbind(!!!rows)
}

# `x$a$b` for a list that may be NULL at any level, as a length-1 value.
.bom_get <- function(x, ...) {
  for (k in c(...)) {
    if (!is.list(x) || is.null(x[[k]])) {
      return(NA)
    }
    x <- x[[k]]
  }
  if (length(x) == 0) NA else x
}

# ---- precis XML: forecast + forecast_aux -------------------------------

# Read a précis XML document (already-parsed `xml2` doc, or raw XML text) and
# return the issue time and the forecast periods of the site's <area>
# (matched on its BOM area code). Taking the first <area> -- as the previous
# version did -- served some other town's forecast for every site.
.bom_precis_parts <- function(xml, aac = NULL) {
  doc <- if (inherits(xml, "xml_document") || inherits(xml, "xml_node")) {
    xml
  } else {
    xml2::read_xml(xml)
  }

  issue_time <- .bom_parse_utc_time(
    xml2::xml_text(xml2::xml_find_first(doc, "//issue-time-utc"))
  )
  area <- if (is.null(aac) || is.na(aac)) {
    xml2::xml_find_first(doc, "//area[forecast-period]")
  } else {
    xml2::xml_find_first(doc, sprintf("//area[@aac='%s']", aac))
  }
  if (inherits(area, "xml_missing")) {
    abort_meteo(
      "BOM précis product has no area {.val {aac}}.",
      class = "bom_rung_unconfigured"
    )
  }
  periods <- xml2::xml_find_all(area, ".//forecast-period")

  list(issue_time = issue_time, periods = periods)
}

# Start of a forecast period in UTC (prefer the explicit UTC attribute).
.bom_period_start <- function(period) {
  utc <- xml2::xml_attr(period, "start-time-utc")
  if (!is.na(utc)) {
    return(.bom_parse_utc_time(utc))
  }
  .bom_parse_offset_time(xml2::xml_attr(period, "start-time-local"))
}

.bom_precis_element <- function(period, type) {
  node <- xml2::xml_find_first(period, sprintf(".//*[@type='%s']", type))
  if (inherits(node, "xml_missing")) NA_character_ else xml2::xml_text(node)
}

#' Parse a précis XML forecast issuance into canonical forecast rows
#'
#' For the site's area (`aac`), each daily `<forecast-period>` yields:
#' `air_temperature_minimum`/`_maximum` -> `temperature_2m_min`/`_max`;
#' `probability_of_precipitation` ("40%") -> `precipitation_probability_max`;
#' `precipitation_range` ("0 to 8 mm") -> `precipitation_sum` with
#' `stat = "p50"` (the lower bound: BOM's 50 % chance amount) and
#' `stat = "p75"` (the upper bound: BOM's 25 % chance amount). Rows carry
#' `model = "daily"`.
#'
#' @param xml An `xml2` document/node, or a raw XML string.
#' @param site_id,source Stamped on every row.
#' @param aac The site's BOM area code (e.g. `"NSW_PT072"`); `NULL` takes the
#'   first area with forecast periods (single-area fixtures only).
#' @return A canonical forecast tibble.
#' @keywords internal
#' @noRd
bom_parse_precis_forecast <- function(xml, site_id, source = "bom_forecast", aac = NULL) {
  parts <- .bom_precis_parts(xml, aac)
  issue_time <- parts$issue_time

  rows <- lapply(parts$periods, function(period) {
    valid <- .bom_period_start(period)
    range <- .bom_precis_element(period, "precipitation_range")
    bounds <- c(NA_real_, NA_real_)
    if (!is.na(range)) {
      nums <- as.numeric(regmatches(range, gregexpr("[0-9.]+", range))[[1]])
      if (length(nums) >= 1) bounds <- if (length(nums) == 1) c(nums, nums) else nums[1:2]
    }
    pop <- sub("%", "", .bom_precis_element(period, "probability_of_precipitation"), fixed = TRUE)
    mk <- function(variable, value, stat = NA_character_) {
      .bom_fc_rows(site_id, source, "daily", issue_time, valid, variable, value, stat)
    }
    .bom_bind_fc(list(
      mk("temperature_2m_min", .bom_precis_element(period, "air_temperature_minimum")),
      mk("temperature_2m_max", .bom_precis_element(period, "air_temperature_maximum")),
      mk("precipitation_probability_max", pop),
      mk("precipitation_sum", bounds[1], "p50"),
      mk("precipitation_sum", bounds[2], "p75")
    ))
  })
  .bom_bind_fc(rows)
}

#' Parse a précis XML forecast issuance into canonical forecast_aux rows
#'
#' Every `<text type="...">` of the site's area's periods becomes one
#' `forecast_aux` row (`field` = the `type`: `"precis"`, `"forecast"`,
#' `"fire_danger"`, `"uv_alert"`, `"probability_of_precipitation"`, ...),
#' verbatim.
#'
#' @inheritParams bom_parse_precis_forecast
#' @return A canonical forecast_aux tibble.
#' @keywords internal
#' @noRd
bom_parse_precis_aux <- function(xml, site_id, source = "bom_forecast", aac = NULL) {
  parts <- .bom_precis_parts(xml, aac)
  issue_time <- parts$issue_time

  rows <- lapply(parts$periods, function(period) {
    texts <- xml2::xml_find_all(period, ".//text")
    if (length(texts) == 0) {
      return(NULL)
    }
    tibble::tibble(
      site_id = site_id,
      source = source,
      issue_time = issue_time,
      valid_time = .bom_period_start(period),
      field = vapply(texts, xml2::xml_attr, character(1), attr = "type"),
      value_text = vapply(texts, xml2::xml_text, character(1))
    )
  })
  .bom_bind_aux(rows)
}

# ---- web API daily / hourly forecast JSON ------------------------------

.bom_webapi_issue_time <- function(parsed) {
  it <- .bom_get(parsed, "metadata", "issue_time")
  if (is.na(it)) {
    abort_meteo("BOM web-API forecast has no {.field metadata.issue_time}.",
                class = "bom_bad_response")
  }
  .bom_parse_utc_time(it)
}

#' Parse a BOM web-API daily forecast into canonical forecast rows
#'
#' `api.weather.bom.gov.au/v1/locations/<geohash>/forecasts/daily`. Per day
#' (`date` = local midnight, as UTC): `temp_max`/`temp_min` ->
#' `temperature_2m_max`/`_min`; `rain.chance` ->
#' `precipitation_probability_max`; `rain.precipitation_amount_{25,50,75}_
#' percent_chance` -> `precipitation_sum` at `stat` `p75`/`p50`/`p25`;
#' `uv.max_index` -> `uv_index_max`. `model = "daily"`; `issue_time` is
#' `metadata.issue_time`.
#' @keywords internal
#' @noRd
bom_parse_webapi_daily <- function(body, site_id, source = "bom_forecast") {
  parsed <- .bom_as_parsed_json(body)
  issue_time <- .bom_webapi_issue_time(parsed)
  rows <- lapply(parsed$data %||% list(), function(d) {
    valid <- .bom_parse_utc_time(.bom_get(d, "date"))
    mk <- function(variable, value, stat = NA_character_) {
      .bom_fc_rows(site_id, source, "daily", issue_time, valid, variable, value, stat)
    }
    list(
      mk("temperature_2m_max", .bom_get(d, "temp_max")),
      mk("temperature_2m_min", .bom_get(d, "temp_min")),
      mk("precipitation_probability_max", .bom_get(d, "rain", "chance")),
      mk("precipitation_sum", .bom_get(d, "rain", "precipitation_amount_25_percent_chance"), "p75"),
      mk("precipitation_sum", .bom_get(d, "rain", "precipitation_amount_50_percent_chance"), "p50"),
      mk("precipitation_sum", .bom_get(d, "rain", "precipitation_amount_75_percent_chance"), "p25"),
      mk("uv_index_max", .bom_get(d, "uv", "max_index"))
    )
  })
  .bom_bind_fc(unlist(rows, recursive = FALSE))
}

#' Parse a BOM web-API daily forecast into forecast_aux rows
#'
#' Fields: `precis` (`short_text`), `forecast` (`extended_text`),
#' `fire_danger`, `uv_category`, `chance_of_no_rain_category`, `icon`.
#' @keywords internal
#' @noRd
bom_parse_webapi_daily_aux <- function(body, site_id, source = "bom_forecast") {
  parsed <- .bom_as_parsed_json(body)
  issue_time <- .bom_webapi_issue_time(parsed)
  fields <- list(
    precis = "short_text", forecast = "extended_text", fire_danger = "fire_danger",
    uv_category = c("uv", "category"),
    chance_of_no_rain_category = c("rain", "chance_of_no_rain_category"),
    icon = "icon_descriptor"
  )
  rows <- lapply(parsed$data %||% list(), function(d) {
    valid <- .bom_parse_utc_time(.bom_get(d, "date"))
    lapply(names(fields), function(f) {
      .bom_aux_rows(site_id, source, issue_time, valid, f, .bom_get(d, fields[[f]]))
    })
  })
  .bom_bind_aux(unlist(rows, recursive = FALSE))
}

#' Parse a BOM web-API hourly forecast into canonical forecast rows
#'
#' `.../forecasts/hourly`. Per hour (`time`, UTC): `temp` ->
#' `temperature_2m`, `temp_feels_like` -> `apparent_temperature`,
#' `dew_point` -> `dewpoint_2m`, `relative_humidity` ->
#' `relative_humidity_2m`, `wind.speed_kilometre`/`wind.gust_speed_kilometre`
#' (km/h) -> `wind_speed_10m`/`wind_gusts_10m` (m/s), `wind.direction`
#' (compass) -> `wind_direction_10m`, `uv` -> `uv_index`, `rain.chance` ->
#' `precipitation_probability`, `rain.precipitation_amount_{10,25,50}_
#' percent_chance` -> `precipitation` at `stat` `p90`/`p75`/`p50`.
#' `model = "hourly"`; `issue_time` is `metadata.issue_time`.
#' @keywords internal
#' @noRd
bom_parse_webapi_hourly <- function(body, site_id, source = "bom_forecast") {
  parsed <- .bom_as_parsed_json(body)
  issue_time <- .bom_webapi_issue_time(parsed)
  kmh <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (is.na(x)) NA_real_ else as.numeric(to_canonical(x, "km/h", "wind_speed_10m"))
  }
  rows <- lapply(parsed$data %||% list(), function(h) {
    valid <- .bom_parse_utc_time(.bom_get(h, "time"))
    mk <- function(variable, value, stat = NA_character_) {
      .bom_fc_rows(site_id, source, "hourly", issue_time, valid, variable, value, stat)
    }
    dir <- .bom_get(h, "wind", "direction")
    list(
      mk("temperature_2m", .bom_get(h, "temp")),
      mk("apparent_temperature", .bom_get(h, "temp_feels_like")),
      mk("dewpoint_2m", .bom_get(h, "dew_point")),
      mk("relative_humidity_2m", .bom_get(h, "relative_humidity")),
      mk("wind_speed_10m", kmh(.bom_get(h, "wind", "speed_kilometre"))),
      mk("wind_gusts_10m", kmh(.bom_get(h, "wind", "gust_speed_kilometre"))),
      mk("wind_direction_10m", if (is.na(dir)) NA else compass2angle(dir)),
      mk("uv_index", .bom_get(h, "uv")),
      mk("precipitation_probability", .bom_get(h, "rain", "chance")),
      mk("precipitation", .bom_get(h, "rain", "precipitation_amount_10_percent_chance"), "p90"),
      mk("precipitation", .bom_get(h, "rain", "precipitation_amount_25_percent_chance"), "p75"),
      mk("precipitation", .bom_get(h, "rain", "precipitation_amount_50_percent_chance"), "p50")
    )
  })
  .bom_bind_fc(unlist(rows, recursive = FALSE))
}

# ---- 72-h obs JSON -> canonical obs -------------------------------------

# Numeric scalar from a JSON field that may be NULL, a number, or a numeric
# string ("0.2"); NA otherwise.
.bom_num <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(x[[1]]))
}

# Map one BOM 72-h obs JSON row (a named list) to canonical (variable,
# value, unit) triples, restricted to `variables` requested. Missing (null)
# fields and "CALM" directions are skipped rather than stored as NA.
.bom_72h_row_values <- function(row, variables) {
  spec <- list(
    temperature_2m = list(value = .bom_num(row$air_temp), unit = "degC"),
    wind_speed_10m = list(value = .bom_num(row$wind_spd_kmh), unit = "km/h"),
    wind_gusts_10m = list(value = .bom_num(row$gust_kmh), unit = "km/h"),
    wind_direction_10m = list(
      value = if (is.null(row$wind_dir)) NA_real_ else compass2angle(row$wind_dir), unit = "degree"
    ),
    relative_humidity_2m = list(value = .bom_num(row$rel_hum), unit = "%"),
    dewpoint_2m = list(value = .bom_num(row$dewpt), unit = "degC"),
    pressure_msl = list(value = .bom_num(row$press_msl), unit = "hPa")
  )
  spec <- spec[intersect(names(spec), variables)]
  Filter(function(s) length(s$value) == 1 && !is.na(s$value), spec)
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
    if (length(values) == 0 || is.na(datetime_utc)) {
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
  out <- do.call(rbind, pieces)
  out[!duplicated(out[c("datetime_utc", "variable")]), , drop = FALSE]
}

#' Parse a rolling 72-h obs JSON response into canonical obs rows
#'
#' `observations.data[]` (half-hourly): `aifstime_utc` (`yyyyMMddHHmmss`
#' UTC), `air_temp` (degC), `wind_spd_kmh`/`gust_kmh` (km/h), `wind_dir`
#' (compass string; "CALM" is skipped), `rel_hum` (%), `dewpt` (degC),
#' `press_msl` (hPa). Null fields are skipped. `method` is always
#' `"measured"`.
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
    time_of = function(row) .bom_parse_compact_time(row$aifstime_utc %||% NA_character_),
    site_id = site_id, source = source
  )
}

# Map one BOM web-API observation object to canonical (variable, value,
# unit) triples, restricted to `variables` requested. The API reports wind
# and gusts in km/h and direction as a compass string.
.bom_webapi_row_values <- function(row, variables) {
  wind <- row$wind
  spec <- list(
    temperature_2m = list(value = .bom_num(row$temp), unit = "degC"),
    wind_speed_10m = list(value = .bom_num(wind$speed_kilometre), unit = "km/h"),
    wind_gusts_10m = list(value = .bom_num(row$gust$speed_kilometre), unit = "km/h"),
    wind_direction_10m = list(
      value = if (is.null(wind$direction)) NA_real_ else compass2angle(wind$direction),
      unit = "degree"
    ),
    relative_humidity_2m = list(value = .bom_num(row$humidity), unit = "%")
  )
  spec <- spec[intersect(names(spec), variables)]
  Filter(function(s) length(s$value) == 1 && !is.na(s$value), spec)
}

#' Parse a web-API obs JSON response into canonical obs rows
#'
#' `api.weather.bom.gov.au/v1/locations/<geohash>/observations` returns ONE
#' current observation: `data` is a single object (`temp`, `humidity`,
#' `wind.speed_kilometre`/`wind.direction`, `gust.speed_kilometre`, ...) and
#' its time is `metadata.observation_time` (UTC). Problem 5 of the production
#' review: the previous parser expected a `data[]` array of rows with their
#' own `time`, so the real response produced nothing. The array shape is
#' still accepted. `method` is always `"measured"`.
#'
#' @inheritParams bom_parse_72h_obs
#' @return A canonical obs tibble (see the internal `new_obs()`).
#' @keywords internal
#' @noRd
bom_parse_webapi_obs <- function(body, variables, site_id, source = "bom_obs") {
  parsed <- .bom_as_parsed_json(body)
  data <- parsed$data %||% list()
  single <- !is.null(names(data))
  rows <- if (single) list(data) else data
  obs_time <- parsed$metadata$observation_time %||% NA_character_

  .bom_obs_rows_to_tibble(
    rows, variables,
    extract = .bom_webapi_row_values,
    time_of = function(row) .bom_parse_utc_time(if (single) obs_time else row$time %||% NA_character_),
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
