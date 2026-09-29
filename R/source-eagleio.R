#' @include adapter.R http.R
NULL

# source_eagleio(): observations from an eagle.io-hosted site AWS (problem 11
# of the production review). eagle.io's historic endpoint returns a JTS
# ("JSON time series") document whose values sit in a record array
# (data[].f."<column>".v) that source_rest()'s single-path mapping cannot
# walk, and it authenticates with an `X-Api-Key` header rather than
# `Authorization:`, so it gets its own small adapter.

#' An eagle.io observation adapter
#'
#' `source_eagleio()` builds a [met_adapter()] that fetches a site weather
#' station's logged data from [eagle.io](https://eagle.io) (historic data
#' API), one eagle.io *node* (parameter) per dictionary variable -- e.g. the
#' station's 10-minute rain total and temperature. Values are returned at the
#' logger's native resolution (typically 10 minutes; aggregation to hourly
#' happens downstream), with `method = "measured"`. eagle.io timestamps are
#' true UTC.
#'
#' A station that has stopped reporting (every node returns no records for
#' the window) makes `fetch()` abort with class
#' `meteoTidy_error_source_stale`, naming the time of the node's last value
#' -- the pipeline verbs record that as this source's failure and carry on
#' with the site's other sources.
#'
#' @param nodes A named character vector (or named list) mapping dictionary
#'   variable names to eagle.io node ids, e.g.
#'   `c(precipitation = "64642e52fbaed638fdb04110",
#'   temperature_2m = "64642e52fbaed638fdb04100")`.
#' @param api_key_env Name of the environment variable holding the eagle.io
#'   API key (read at fetch time; never stored or printed).
#' @param source_id Single string stamped into the `source` column.
#' @param base_url The eagle.io API root.
#' @param stale_after_hours Hours without a new reading after which the
#'   station is reported stale (a `meteoTidy_warning_source_stale` warning;
#'   the rows are still returned). Default 6.
#' @return A `source_eagleio` (`met_adapter` subclass) S7 object.
#' @family adapter
#' @export
#' @examples
#' source_eagleio(c(precipitation = "64642e52fbaed638fdb04110"))
source_eagleio <- S7::new_class(
  "source_eagleio",
  package = "meteoTidy",
  parent = met_adapter,
  properties = list(
    nodes = S7::class_character,
    api_key_env = S7::class_character,
    base_url = S7::class_character,
    stale_after_hours = S7::class_double
  ),
  constructor = function(nodes, api_key_env = "EAGLE_API_KEY", source_id = "eagleio",
                         base_url = "https://api.eagle.io/api/v1",
                         stale_after_hours = 6) {
    nodes <- unlist(nodes)
    if (is.null(names(nodes)) || any(!nzchar(names(nodes)))) {
      abort_meteo("{.arg nodes} must be named by dictionary variable.", class = "bad_mapping")
    }
    unknown <- setdiff(names(nodes), met_variables()$variable)
    if (length(unknown) > 0) {
      abort_meteo("{.arg nodes} names unknown variable{?s} {.val {unknown}}.",
                  class = "unknown_variable")
    }
    S7::new_object(
      met_adapter(source_id = source_id, provides = names(nodes), cadence = "subdaily"),
      nodes = vapply(nodes, as.character, character(1)),
      api_key_env = api_key_env,
      base_url = sub("/+$", "", base_url),
      stale_after_hours = as.double(stale_after_hours)
    )
  }
)

.eagleio_key <- function(adapter) {
  key <- Sys.getenv(adapter@api_key_env, unset = "")
  if (!nzchar(key)) {
    abort_meteo(
      c(
        "eagle.io API key not found.",
        "i" = "Set the environment variable {.envvar {adapter@api_key_env}}."
      ),
      class = "secret_unresolved"
    )
  }
  key
}

.eagleio_iso <- function(t) {
  format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# Parse one JTS document into (datetime_utc, value, unit) for column "0".
.eagleio_parse_jts <- function(body) {
  records <- body$data %||% list()
  unit <- body$header$columns[["0"]]$units %||% NA_character_
  if (length(records) == 0) {
    return(list(time = as.POSIXct(character(0), tz = "UTC"), value = double(0), unit = unit))
  }
  ts <- vapply(records, function(r) as.character(r$ts %||% NA_character_), character(1))
  value <- vapply(records, function(r) {
    v <- r$f[["0"]]$v
    if (is.null(v) || !is.numeric(v)) NA_real_ else as.double(v)
  }, double(1))
  time <- as.POSIXct(sub("Z$", "", ts), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  keep <- !is.na(value) & !is.na(time)
  list(time = time[keep], value = value[keep], unit = unit)
}

# eagle.io unit labels -> units-package symbols.
.eagleio_unit <- function(unit, variable) {
  if (is.na(unit) || !nzchar(unit)) {
    return(canonical_unit(variable))
  }
  # eagle.io's unit labels, written as escapes to keep the code ASCII.
  known <- stats::setNames(
    c("degC", "degree", "W/m2", "m3/m3"),
    c("\u00b0C", "\u00b0", "W/m\u00b2", "m\u00b3/m\u00b3")
  )
  if (unit %in% names(known)) known[[unit]] else unit
}

# The time of a node's most recent value (NA if the lookup fails).
.eagleio_last_time <- function(adapter, node, key, now) {
  meta <- tryCatch(
    .http_get(sprintf("%s/nodes/%s?attr=currentTime", adapter@base_url, node),
              headers = list(`X-Api-Key` = key), now = now),
    error = function(e) NULL
  )
  t <- if (is.list(meta)) meta$currentTime else NULL
  if (is.null(t)) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  as.POSIXct(sub("Z$", "", t), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
}

S7::method(fetch, source_eagleio) <- function(adapter, site, variables, window, now = .now()) {
  variables <- intersect(variables, adapter@provides)
  key <- .eagleio_key(adapter)

  pieces <- lapply(variables, function(v) {
    node <- adapter@nodes[[v]]
    url <- sprintf(
      "%s/nodes/%s/historic?startTime=%s&endTime=%s&format=json",
      adapter@base_url, node, .eagleio_iso(window$from %||% (now - 86400)),
      .eagleio_iso(window$to %||% now)
    )
    parsed <- .eagleio_parse_jts(.http_get(url, headers = list(`X-Api-Key` = key), now = now))
    if (length(parsed$value) == 0) {
      return(NULL)
    }
    .flag_out_of_range(tibble::tibble(
      site_id = site_id(site),
      datetime_utc = parsed$time,
      variable = v,
      value = as.double(units::drop_units(
        to_canonical(parsed$value, .eagleio_unit(parsed$unit, v), v)
      )),
      source = adapter@source_id,
      method = "measured",
      qc_flag = "ok"
    ))
  })
  names(pieces) <- variables
  empty <- variables[vapply(pieces, is.null, logical(1))]

  if (length(variables) > 0 && length(empty) == length(variables)) {
    last <- .eagleio_last_time(adapter, adapter@nodes[[variables[[1]]]], key, now)
    since <- if (is.na(last)) "an unknown time" else format(last, "%Y-%m-%d %H:%M UTC")
    abort_meteo(
      c(
        "eagle.io station is stale: no data from {.val {adapter@source_id}} for the requested window.",
        "i" = "Last value reported {since}."
      ),
      class = "source_stale"
    )
  }
  if (length(empty) > 0) {
    warn_meteo(
      "eagle.io returned no records for {.val {empty}} ({.val {adapter@source_id}}).",
      class = "eagleio_node_empty"
    )
  }

  out <- vctrs::vec_rbind(!!!Filter(Negate(is.null), pieces))
  if (is.null(out)) {
    out <- .bom_empty_obs()
  }
  .eagleio_warn_if_stale(adapter, out, now)
  check_fetch_result(out, adapter, variables)
}

S7::method(format, source_eagleio) <- function(x, ...) {
  c(
    sprintf("<source_eagleio> source_id: %s", x@source_id),
    sprintf("  nodes: %s", paste(names(x@nodes), x@nodes, sep = " = ", collapse = ", ")),
    sprintf("  api_key_env: %s (value not shown)", x@api_key_env)
  )
}

S7::method(print, source_eagleio) <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

# A station can stop reporting while a long (e.g. 7-day daily) window still
# holds its last readings: the fetch succeeds but the station is dead.
# Warn source_stale so the sync records the source as "stale" (rows kept).
.eagleio_warn_if_stale <- function(adapter, out, now) {
  if (nrow(out) == 0) {
    return(invisible())
  }
  last <- max(out$datetime_utc)
  age <- as.numeric(difftime(now, last, units = "hours"))
  if (age > adapter@stale_after_hours) {
    warn_meteo(
      c("eagle.io station is stale: {.val {adapter@source_id}}'s newest reading is {round(age)} h old.",
        "i" = "Last value reported {format(last, '%Y-%m-%d %H:%M UTC')}."),
      class = "source_stale"
    )
  }
  invisible()
}
