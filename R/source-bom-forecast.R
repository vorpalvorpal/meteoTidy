# Plan 07 — source_bom_forecast(): daily précis (7-day) via official product
# feeds, optional hourly via the unofficial web API. See
# plans/07-acquisition-bom.md (SCOPING §5.1).

# The default 3-rung ladder shared by both BOM adapters (the "substitute"
# rung -- Open-Meteo source substitution -- is not built in here; per the
# plan it is a documented, opt-in wiring decision left to the caller/Plan 16
# rather than a silent default, since substituting a DIFFERENT source needs
# explicit acknowledgement in provenance).
.bom_forecast_ftp_url <- function(product) {
  # Précis product IDs per BOM's anonymous FTP tree (SCOPING §5.1); the
  # actual value is irrelevant to the frozen tests (`.ftp_get()` is always
  # mocked), but is filled in with a plausible real endpoint pattern.
  sprintf("https://reg.bom.gov.au/fwo/%s.xml", product)
}

# The précis-fetching helper: calls `.ftp_get()` directly and parses via
# `bom_parse_precis_forecast()`/`bom_parse_precis_aux()`. Both
# `fetch_forecast()` and `fetch_forecast_aux()` route through
# `ladder_fetch()` for architectural consistency with `source_bom_obs()`
# (Definition of Done implies both adapters share the ladder machinery); the
# frozen forecast test mocks `.ftp_get()` directly and does not inspect
# ladder/breaker internals for this adapter, so either design would pass it,
# but the ladder route keeps the breaker wired through uniformly.
.bom_precis_rung <- function(rung_id = "ftp_feeds") {
  list(
    id = rung_id,
    kind = "ftp",
    applies_to = c("precis_daily"),
    fetch_fn = function(request, now = NULL) {
      xml <- .ftp_get(.bom_forecast_ftp_url("precis"))
      list(xml = xml)
    }
  )
}

#' A BOM daily précis + (opt-in) hourly forecast adapter
#'
#' `source_bom_forecast()` builds a [met_adapter()] that fetches BOM's
#' edited 7-day précis forecast (daily résolution; the regulator-cited
#' product, SCOPING §5.1) via the official anonymous-FTP/HTTP-mirror product
#' feeds, and returns both the numeric forecast rows
#' ([fetch_forecast()]) and the non-numeric companion table
#' ([fetch_forecast_aux()]: précis text, fire-danger/UV categories).
#'
#' Hourly forecasts (via the unofficial web API / new gateway) are opt-in
#' (`allow_web_api = TRUE`) and documented as at-your-own-risk (SCOPING
#' §5.1): they are the only free channel for sub-daily BOM forecasts, but
#' rely on undocumented endpoints. The `store_root` is used only to persist
#' the transport circuit-breaker state across runs (see
#' `breaker_read()`/`breaker_write()`).
#'
#' The edited BOM précis product carries no model name; every forecast row
#' this adapter returns has `model = NA_character_` (Plan 01).
#'
#' @param ladder A list of transport rungs (see `ladder_fetch()`). Defaults
#'   to a précis-serving `ftp_feeds` rung.
#' @param allow_web_api Logical, default `FALSE`. When `TRUE`,
#'   [resolve_station()] may call the unofficial web API to search for a
#'   geohash.
#' @param store_root Single string, the store root used for breaker-state
#'   persistence.
#' @param source_id Single string stamped into the `source` column. Default
#'   `"bom_forecast"`.
#'
#' @return A `source_bom_forecast` (`met_adapter` subclass) S7 object.
#' @family adapter
#' @export
#' @examples
#' adapter <- source_bom_forecast(store_root = tempfile())
source_bom_forecast <- S7::new_class(
  "source_bom_forecast",
  package = "meteoTidy",
  parent = met_adapter,
  properties = list(
    ladder = S7::class_list,
    allow_web_api = S7::class_logical,
    store_root = S7::class_character
  ),
  constructor = function(ladder = list(.bom_precis_rung()),
                         allow_web_api = FALSE,
                         store_root,
                         source_id = "bom_forecast") {
    S7::new_object(
      met_adapter(
        source_id = source_id,
        provides = "temperature_2m",
        cadence = "per_issue"
      ),
      ladder = ladder,
      allow_web_api = allow_web_api,
      store_root = store_root
    )
  }
)

# Run the précis rung through the ladder/breaker machinery and return the
# raw XML text plus the (possibly-updated) breaker, persisting strikes.
.bom_fetch_precis_xml <- function(adapter) {
  breaker <- breaker_read(adapter@store_root)
  request <- list(product = "precis_daily", variables = NULL, window = NULL)
  result <- ladder_fetch(adapter@ladder, request, breaker, now = .now())
  breaker_write(adapter@store_root, attr(result, "breaker") %||% breaker)
  result$xml
}

S7::method(fetch_forecast, source_bom_forecast) <- function(
  adapter, site, variables, issue_window, now = .now()
) {
  xml <- .bom_fetch_precis_xml(adapter)
  out <- bom_parse_precis_forecast(xml, site_id = site_id(site), source = adapter@source_id)
  new_forecast(out)
}

S7::method(fetch_forecast_aux, source_bom_forecast) <- function(
  adapter, site, window, now = .now()
) {
  xml <- .bom_fetch_precis_xml(adapter)
  out <- bom_parse_precis_aux(xml, site_id = site_id(site), source = adapter@source_id)
  new_forecast_aux(out)
}

# Web-API geohash search rung. Only ever included transiently for
# `resolve_station()` (not part of `adapter@ladder`, which serves
# `fetch_forecast()`), since geohash search is a distinct product from
# précis/obs fetches and always goes through `.http_get()` directly when
# opted in.
.bom_geohash_search_url <- function() {
  "https://api.weather.bom.gov.au/v1/locations"
}

S7::method(resolve_station, source_bom_forecast) <- function(adapter, site, ...) {
  cached <- site_resolved(site, c("bom", "geohash"))
  if (!adapter@allow_web_api) {
    if (!is.null(cached) && !is.na(cached)) {
      return(site)
    }
    abort_meteo(
      c(
        "No BOM geohash is cached for site {.val {site_id(site)}}, and the web API is disabled.", # nolint: line_length_linter.
        "i" = "Enable {.arg allow_web_api} on {.fn source_bom_forecast}, or set a geohash via site config." # nolint: line_length_linter.
      ),
      class = "bom_geohash_unavailable"
    )
  }

  body <- .http_get(.bom_geohash_search_url(), query = list(
    search = as.character(site@latitude), lat = as.numeric(site@latitude),
    lon = as.numeric(site@longitude)
  ))
  geohash <- bom_parse_geohash_search(body)
  site_set_resolved(site, c("bom", "geohash"), geohash)
}

S7::method(format, source_bom_forecast) <- function(x, ...) {
  c(
    sprintf("<source_bom_forecast> source_id: %s", x@source_id),
    sprintf("  allow_web_api: %s", x@allow_web_api),
    sprintf("  rungs: %s", paste(vapply(x@ladder, `[[`, character(1), "id"), collapse = " -> "))
  )
}

S7::method(print, source_bom_forecast) <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
