# Plan 04 — the response-mapping spec + apply_mapping(), the shared,
# pure-function core both source_rest() and source_file() call. Represented
# as a validated list (not S7): it is pure config data with no methods of its
# own, constructed once per adapter and passed straight through to
# apply_mapping().

#' Describe how to map a parsed response to canonical observations
#'
#' A declarative spec describing how to turn an already-parsed source
#' response (a JSON list, or a CSV data frame) into canonical long-format
#' observation rows. Built from YAML (Plan 02 site config) or written by hand
#' (as in `source_rest()`/`source_file()` examples).
#'
#' @param format Single string, `"json"` or `"csv"`.
#' @param time A list describing how to find the timestamp:
#'   - for `format = "json"`: `list(path = "hourly/time", tz = "UTC")`, where
#'     `path` is a `/`-separated walk through the parsed list.
#'   - for `format = "csv"`: `list(column = "timestamp", tz = "UTC")`.
#'   `tz` is the timezone the raw timestamps are expressed in; they are
#'   converted to UTC by [apply_mapping()].
#' @param variables A list, one entry per source field, each a list with:
#'   - `variable` — the canonical dictionary variable name.
#'   - `path` (JSON) or `column` (CSV) — where to find the raw values.
#'   - `unit` — the **source** unit (a `units`-package-parseable string);
#'     [apply_mapping()] calls `to_canonical()` to convert it.
#'   - `height` — (optional) a `units`-classed double (metres), the sensor
#'     height this field was measured at, recorded for later height
#'     correction (Plan 11); not used by `apply_mapping()` itself.
#'
#' @return A `met_mapping` object (a validated list with class
#'   `"meteoTidy::met_mapping"`).
#' @family adapter
#' @export
#' @examples
#' met_mapping(
#'   format = "json",
#'   time = list(path = "hourly/time", tz = "UTC"),
#'   variables = list(
#'     list(variable = "temperature_2m", path = "hourly/temperature_2m",
#'          unit = "degC", height = units::set_units(2, "m"))
#'   )
#' )
met_mapping <- function(format = c("json", "csv"), time, variables) {
  format <- rlang::arg_match(format)

  if (!is.list(time) || is.null(time$tz)) {
    abort_meteo(
      "{.arg time} must be a list with a {.field tz} entry (and a {.field path}/{.field column} entry).", # nolint: line_length_linter.
      class = "bad_mapping"
    )
  }
  locator_field <- if (format == "json") "path" else "column"
  if (is.null(time[[locator_field]])) {
    abort_meteo(
      "{.arg time} must have a {.field {locator_field}} entry when {.arg format} = {.val {format}}.", # nolint: line_length_linter.
      class = "bad_mapping"
    )
  }

  if (!is.list(variables) || length(variables) == 0) {
    abort_meteo("{.arg variables} must be a non-empty list.", class = "bad_mapping")
  }
  for (v in variables) {
    if (is.null(v$variable) || is.null(v$unit) || is.null(v[[locator_field]])) {
      abort_meteo(
        c(
          "Each entry in {.arg variables} must have {.field variable}, {.field unit}, and",
          "{.field {locator_field}} (for {.arg format} = {.val {format}})."
        ),
        class = "bad_mapping"
      )
    }
  }

  structure(
    list(format = format, time = time, variables = variables),
    class = c("meteoTidy::met_mapping", "list")
  )
}

# Walk a `/`-separated JSON path into a nested list, e.g.
# "hourly/temperature_2m" -> x[["hourly"]][["temperature_2m"]]. Deliberately
# simple (SCOPING §13 constrains scope): no array indices, no wildcards.
.json_path_get <- function(x, path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  for (part in parts) {
    x <- x[[part]]
    if (is.null(x)) {
      return(NULL)
    }
  }
  x
}

# Parse a single ISO-ish timestamp string against a fixed set of candidate
# formats, trying each in turn (per-element, not vector-wide -- base R's
# `tryFormats` picks ONE format for the whole vector based only on the first
# element, which silently mis-parses ragged inputs like a vector containing
# both "...T00:00" (looks like a bare date once seconds are dropped) and
# "...T01:00:00"). Returns NA if none match.
.parse_one_timestamp <- function(x, tz, formats) {
  for (fmt in formats) {
    t <- as.POSIXct(x, tz = tz, format = fmt)
    if (!is.na(t)) {
      return(t)
    }
  }
  as.POSIXct(NA_character_, tz = tz)
}

.timestamp_formats <- c(
  "%Y-%m-%dT%H:%M:%OS", "%Y-%m-%dT%H:%M",
  "%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M", "%Y-%m-%d"
)

# Resolve the raw timestamp vector for either format, as UTC POSIXct.
.mapping_resolve_time <- function(parsed, mapping) {
  raw <- if (mapping$format == "json") {
    unlist(.json_path_get(parsed, mapping$time$path), use.names = FALSE)
  } else {
    parsed[[mapping$time$column]]
  }
  if (is.null(raw) || length(raw) == 0) {
    return(as.POSIXct(character(0), tz = "UTC"))
  }

  if (inherits(raw, "POSIXct")) {
    # Already parsed (e.g. readr's col_datetime()): just re-express in the
    # declared source tz, then convert to UTC below.
    parsed_time <- as.POSIXct(format(raw, "%Y-%m-%d %H:%M:%OS6"),
                              tz = mapping$time$tz, format = "%Y-%m-%d %H:%M:%OS")
  } else {
    raw <- as.character(raw)
    parsed_time <- do.call(c, lapply(raw, .parse_one_timestamp,
                                     tz = mapping$time$tz, formats = .timestamp_formats))
  }

  if (anyNA(parsed_time)) {
    abort_meteo(
      "Could not parse one or more timestamps with the mapping's declared formats.",
      class = "bad_mapping"
    )
  }
  as.POSIXct(format(parsed_time, tz = "UTC", usetz = FALSE), tz = "UTC")
}

# Resolve one variable entry's raw values (a numeric vector, same length as
# the resolved time vector).
.mapping_resolve_values <- function(parsed, mapping, entry) {
  raw <- if (mapping$format == "json") {
    unlist(.json_path_get(parsed, entry$path), use.names = FALSE)
  } else {
    parsed[[entry$column]]
  }
  if (is.null(raw)) numeric(0) else as.double(raw)
}

#' Apply a response-mapping spec to a parsed source response
#'
#' The shared, pure-function core both [source_rest()] and [source_file()]
#' call: takes an already-parsed body (a JSON list, or a CSV data frame) and a
#' [met_mapping()] spec, and returns a canonical long observation tibble.
#' Performs no IO.
#'
#' @param parsed The parsed response: a nested list for `format = "json"`, a
#'   data frame for `format = "csv"`.
#' @param mapping A [met_mapping()] object.
#' @param site A `met_site` object (used for `site_id`).
#' @param source_id Single string, stamped into the `source` column.
#' @param now Injectable clock; unused directly but threaded through for
#'   interface consistency with the rest of the acquisition seam.
#'
#' @return A canonical long observation tibble (see the internal `new_obs()`).
#' @family adapter
#' @export
apply_mapping <- function(parsed, mapping, site, source_id, now = .now()) {
  time_utc <- .mapping_resolve_time(parsed, mapping)

  rows <- lapply(mapping$variables, function(entry) {
    raw_values <- .mapping_resolve_values(parsed, mapping, entry)
    if (length(raw_values) == 0) {
      return(tibble::tibble(
        site_id = character(0),
        datetime_utc = as.POSIXct(character(0), tz = "UTC"),
        variable = character(0),
        value = double(0),
        source = character(0),
        method = character(0),
        qc_flag = character(0)
      ))
    }
    canonical <- to_canonical(raw_values, from = entry$unit, variable = entry$variable)
    tibble::tibble(
      site_id = site_id(site),
      datetime_utc = time_utc,
      variable = entry$variable,
      value = as.double(units::drop_units(canonical)),
      source = source_id,
      method = "measured",
      qc_flag = "ok"
    )
  })

  out <- vctrs::vec_rbind(!!!rows)
  new_obs(out)
}
