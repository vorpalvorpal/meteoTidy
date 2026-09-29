# Plan 07 -- source_bom_forecast(): BOM's edited forecast for the site's
# location, daily (7-day précis) and hourly. See plans/07-acquisition-bom.md
# (SCOPING section 5.1).
#
# Rewritten for the production forecast archive (problem 4 of the production
# review). The previous version fetched a placeholder URL (404), parsed the
# FIRST <area> of whatever product it got (not the site's), stored only daily
# Tmax -- mislabelled as hourly `temperature_2m` -- and ignored
# `allow_web_api`. Now:
#
# * DAILY forecast, two transports in ladder order:
#     1. `ftp_feeds` -- the official précis product for the site's state
#        (e.g. IDN11060 for NSW) from BOM's anonymous product mirror,
#        matched to the site's BOM area code (`resolved: bom: aac:`). Skipped
#        (no breaker strike) when no area code is configured.
#     2. `web_api` -- api.weather.bom.gov.au daily forecast for the site's
#        6-character geohash. Opt-in (`allow_web_api = TRUE`).
# * HOURLY forecast (~3 days): only the web API serves it, so it is fetched
#   only when `allow_web_api = TRUE`.
#
# Variables map onto the dictionary (daily aggregates use the `_max`/`_min`/
# `_sum` names; "X % chance of at least A mm" is a quantile row, stat
# "p<100-X>"). Text (précis, extended forecast, fire danger, UV category) goes
# to forecast_aux. Rows carry `model = "daily"` or `model = "hourly"` so the
# two products are distinct issuances in the archive (the edited product has
# no NWP model); `issue_time` is BOM's own product issue time.

.bom_product_mirror_url <- function(product) {
  sprintf("https://reg.bom.gov.au/fwo/%s.xml", product)
}

# Précis ("city and town forecast") product per state, keyed by the AAC
# prefix. The ACT is carried in the NSW product.
.bom_precis_product_for_aac <- function(aac) {
  state <- sub("_.*$", "", aac)
  products <- c(
    NSW = "IDN11060", ACT = "IDN11060", VIC = "IDV10753", QLD = "IDQ11295",
    SA = "IDS10044", WA = "IDW14199", TAS = "IDT16710", NT = "IDD10207"
  )
  unname(products[state])
}

.bom_webapi_base <- function() {
  "https://api.weather.bom.gov.au/v1/locations"
}

# BOM's web API wants the 6-character geohash; 7 characters gives HTTP 400
# (problem 5), so anything longer is truncated.
.bom_geohash6 <- function(geohash) {
  if (is.null(geohash) || length(geohash) != 1 || is.na(geohash) || !nzchar(geohash)) {
    return(NA_character_)
  }
  substr(geohash, 1, 6)
}

.bom_require_geohash <- function(site) {
  gh <- .bom_geohash6(site_resolved(site, c("bom", "geohash")))
  if (is.na(gh)) {
    abort_meteo(
      c(
        "No BOM geohash is configured for site {.val {site_id(site)}}.",
        "i" = "Set {.code resolved: bom: geohash:} in the site YAML (6 characters)."
      ),
      class = "bom_geohash_unavailable"
    )
  }
  gh
}

# The précis rung: the site's area from its state's précis product.
.bom_precis_rung <- function(rung_id = "ftp_feeds") {
  list(
    id = rung_id,
    kind = "ftp",
    applies_to = c("precis_daily"),
    fetch_fn = function(request, now = NULL) {
      aac <- request$aac
      product <- if (is.null(aac) || is.na(aac)) NA_character_ else .bom_precis_product_for_aac(aac)
      if (is.na(product)) {
        abort_meteo(
          c(
            "No BOM précis area code (AAC) is configured for this site.",
            "i" = "Set {.code resolved: bom: aac:} (e.g. {.val NSW_PT072}) to use the official product feed."
          ),
          class = "bom_rung_unconfigured"
        )
      }
      xml <- .ftp_get(.bom_product_mirror_url(product))
      list(
        forecast = bom_parse_precis_forecast(xml, request$site_id, request$source, aac = aac),
        aux = bom_parse_precis_aux(xml, request$site_id, request$source, aac = aac)
      )
    }
  )
}

# The web-API daily rung.
.bom_webapi_daily_rung <- function() {
  list(
    id = "web_api",
    kind = "http",
    applies_to = c("precis_daily"),
    fetch_fn = function(request, now = NULL) {
      body <- .http_get(sprintf("%s/%s/forecasts/daily", .bom_webapi_base(), request$geohash),
                        headers = .bom_webapi_headers())
      list(
        forecast = bom_parse_webapi_daily(body, request$site_id, request$source),
        aux = bom_parse_webapi_daily_aux(body, request$site_id, request$source)
      )
    }
  )
}

# The web-API hourly rung.
.bom_webapi_hourly_rung <- function() {
  list(
    id = "web_api",
    kind = "http",
    applies_to = c("forecast_hourly"),
    fetch_fn = function(request, now = NULL) {
      body <- .http_get(sprintf("%s/%s/forecasts/hourly", .bom_webapi_base(), request$geohash),
                        headers = .bom_webapi_headers())
      list(forecast = bom_parse_webapi_hourly(body, request$site_id, request$source))
    }
  )
}

# BOM's web API and product mirror reject requests without a browser-like
# User-Agent.
.bom_webapi_headers <- function() {
  list(`User-Agent` = "Mozilla/5.0 (compatible; meteoTidy R package)")
}

.bom_forecast_daily_variables <- function() {
  c(
    "temperature_2m_max", "temperature_2m_min", "precipitation_sum",
    "precipitation_probability_max", "uv_index_max"
  )
}

.bom_forecast_hourly_variables <- function() {
  c(
    "temperature_2m", "apparent_temperature", "dewpoint_2m",
    "relative_humidity_2m", "wind_speed_10m", "wind_direction_10m",
    "wind_gusts_10m", "uv_index", "precipitation_probability", "precipitation"
  )
}

#' A BOM daily précis + hourly forecast adapter
#'
#' `source_bom_forecast()` builds a [met_adapter()] that archives BOM's
#' edited forecast for the site's location:
#'
#' * the **daily** 7-day forecast -- min/max temperature
#'   (`temperature_2m_min`/`temperature_2m_max`), chance of rain
#'   (`precipitation_probability_max`), rain amounts at BOM's 25/50/75 %
#'   chances (`precipitation_sum` rows with `stat = "p75"`/`"p50"`/`"p25"`:
#'   "X % chance of at least A mm" is the `100 - X` percentile), and maximum
#'   UV index (`uv_index_max`); text elements (précis, extended forecast,
#'   fire danger, UV category) are returned by [fetch_forecast_aux()] and
#'   archived to `forecast_aux`. Rows have `model = "daily"` and
#'   `valid_time` = the start of the local day.
#' * the **hourly** forecast (~3 days; web API only) -- temperature,
#'   apparent temperature, dew point, humidity, wind speed/direction/gusts,
#'   UV index, chance of rain (`precipitation_probability`) and rain amounts
#'   at BOM's 10/25/50 % chances (`precipitation` rows with `stat = "p90"`/
#'   `"p75"`/`"p50"`). Rows have `model = "hourly"`.
#'
#' `issue_time` is BOM's own issue time for the product.
#'
#' The daily forecast comes from the official précis product feed when the
#' site has a BOM area code (`resolved: bom: aac:` in site YAML, e.g.
#' `NSW_PT072` for Katoomba); otherwise, or if that feed fails, from the
#' unofficial `api.weather.bom.gov.au` web API. The web API is keyed by the
#' site's 6-character geohash (`resolved: bom: geohash:`), is **opt-in**
#' (`allow_web_api = TRUE`) and at-your-own-risk (SCOPING section 5.1), and
#' is the only channel for the hourly forecast.
#'
#' @param ladder A list of transport rungs (see `ladder_fetch()`) for the
#'   daily product. Defaults to the précis feed, then (when `allow_web_api`)
#'   the web API.
#' @param allow_web_api Logical, default `FALSE`. Enables the web API for the
#'   daily fallback, the hourly forecast, and geohash search in
#'   [resolve_station()].
#' @param store_root Single string, the store root used for breaker-state
#'   persistence.
#' @param source_id Single string stamped into the `source` column. Default
#'   `"bom_forecast"`.
#' @param products Which products to fetch: any of `"daily"`, `"hourly"`.
#'
#' @return A `source_bom_forecast` (`met_adapter` subclass) S7 object.
#' @family adapter
#' @export
#' @examples
#' adapter <- source_bom_forecast(store_root = tempfile(), allow_web_api = TRUE)
source_bom_forecast <- S7::new_class(
  "source_bom_forecast",
  package = "meteoTidy",
  parent = met_adapter,
  properties = list(
    ladder = S7::class_list,
    allow_web_api = S7::class_logical,
    store_root = S7::class_character,
    products = S7::class_character
  ),
  constructor = function(ladder = NULL,
                         allow_web_api = FALSE,
                         store_root,
                         source_id = "bom_forecast",
                         products = c("daily", "hourly")) {
    products <- match.arg(products, c("daily", "hourly"), several.ok = TRUE)
    ladder <- ladder %||% c(
      list(.bom_precis_rung()),
      if (isTRUE(allow_web_api)) list(.bom_webapi_daily_rung())
    )
    S7::new_object(
      met_adapter(
        source_id = source_id,
        provides = c(.bom_forecast_daily_variables(), .bom_forecast_hourly_variables()),
        cadence = "per_issue"
      ),
      ladder = ladder,
      allow_web_api = allow_web_api,
      store_root = store_root,
      products = products
    )
  }
)

# Run one BOM product through the ladder/breaker machinery, persisting any
# breaker strikes whether or not the fetch succeeded.
.bom_ladder_run <- function(adapter, ladder, request, now) {
  breaker <- breaker_read(adapter@store_root)
  result <- tryCatch(
    ladder_fetch(ladder, request, breaker, now = now),
    meteoTidy_error_bom_all_transports_failed = function(cnd) {
      breaker_write(adapter@store_root, cnd$breaker %||% breaker)
      rlang::cnd_signal(cnd)
    }
  )
  breaker_write(adapter@store_root, attr(result, "breaker") %||% breaker)
  result
}

.bom_forecast_request <- function(adapter, site, product) {
  list(
    product = product, variables = NULL, window = NULL,
    site_id = site_id(site), source = adapter@source_id,
    aac = site_resolved(site, c("bom", "aac")) %||% NA_character_,
    geohash = if (adapter@allow_web_api) .bom_geohash6(site_resolved(site, c("bom", "geohash"))) else NA
  )
}

# Fetch every configured product. Returns list(forecast =, aux =, errors =):
# one product failing (e.g. the hourly web API) does not discard the other.
.bom_forecast_fetch_all <- function(adapter, site, now) {
  forecast <- list()
  aux <- list()
  errors <- character(0)

  if ("daily" %in% adapter@products) {
    res <- tryCatch(
      .bom_ladder_run(adapter, adapter@ladder, .bom_forecast_request(adapter, site, "precis_daily"), now),
      meteoTidy_error = function(cnd) cnd
    )
    if (inherits(res, "condition")) {
      errors <- c(errors, paste("daily:", conditionMessage(res)))
    } else {
      forecast <- c(forecast, list(res$forecast))
      aux <- c(aux, list(res$aux))
    }
  }

  if ("hourly" %in% adapter@products && adapter@allow_web_api) {
    res <- tryCatch({
      .bom_require_geohash(site)
      .bom_ladder_run(adapter, list(.bom_webapi_hourly_rung()),
                      .bom_forecast_request(adapter, site, "forecast_hourly"), now)
    }, meteoTidy_error = function(cnd) cnd)
    if (inherits(res, "condition")) {
      errors <- c(errors, paste("hourly:", conditionMessage(res)))
    } else {
      forecast <- c(forecast, list(res$forecast))
    }
  }

  list(
    forecast = if (length(forecast)) vctrs::vec_rbind(!!!forecast) else .empty_forecast(),
    aux = if (length(aux)) vctrs::vec_rbind(!!!aux) else .empty_forecast_aux(),
    errors = errors
  )
}

.bom_abort_all_failed <- function(adapter, errors) {
  names(errors) <- rep("x", length(errors))
  errors <- gsub("([{}])", "\\1\\1", errors)
  abort_meteo(
    c("BOM forecast ({.val {adapter@source_id}}) could not be fetched.", errors),
    class = "bom_all_transports_failed"
  )
}

S7::method(fetch_forecast, source_bom_forecast) <- function(
  adapter, site, variables, issue_window, now = .now()
) {
  got <- .bom_forecast_fetch_all(adapter, site, now)
  if (nrow(got$forecast) == 0 && length(got$errors) > 0) {
    .bom_abort_all_failed(adapter, got$errors)
  }
  if (length(got$errors) > 0) {
    warn_meteo(
      c("BOM forecast partly unavailable; archived what was fetched.",
        stats::setNames(gsub("([{}])", "\\1\\1", got$errors), rep("x", length(got$errors)))),
      class = "bom_partial"
    )
  }
  out <- got$forecast
  out <- new_forecast(out[out$variable %in% variables, , drop = FALSE])
  # The companion text rows travel with the forecast so a single fetch can
  # archive both (archive_forecasts() writes this attribute to forecast_aux).
  attr(out, "aux") <- new_forecast_aux(got$aux)
  out
}

S7::method(fetch_forecast_aux, source_bom_forecast) <- function(
  adapter, site, window, now = .now()
) {
  adapter@products <- "daily"
  got <- .bom_forecast_fetch_all(adapter, site, now)
  if (length(got$errors) > 0) {
    .bom_abort_all_failed(adapter, got$errors)
  }
  new_forecast_aux(got$aux)
}

# Web-API geohash search. Only used by resolve_station(), and only when the
# web API is opted in.
.bom_geohash_search_url <- function() {
  "https://api.weather.bom.gov.au/v1/locations"
}

S7::method(resolve_station, source_bom_forecast) <- function(adapter, site, ...) {
  cached <- site_resolved(site, c("bom", "geohash"))
  if (!is.null(cached) && !is.na(cached)) {
    return(site)
  }
  if (!adapter@allow_web_api) {
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
    sprintf("  products: %s", paste(x@products, collapse = ", ")),
    sprintf("  daily rungs: %s", paste(vapply(x@ladder, `[[`, character(1), "id"), collapse = " -> "))
  )
}

S7::method(print, source_bom_forecast) <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
