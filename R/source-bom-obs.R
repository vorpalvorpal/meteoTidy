# Plan 07 — source_bom_obs(): rolling 72-h station JSON via official product
# feeds, optional web-API fallback. See plans/07-acquisition-bom.md
# (SCOPING §5.1).

.bom_obs_variable_map <- function() {
  c("temperature_2m", "wind_speed_10m", "wind_direction_10m", "relative_humidity_2m")
}

.bom_obs_ftp_url <- function(product) {
  sprintf("https://reg.bom.gov.au/fwo/%s.json", product)
}

.bom_obs_webapi_url <- function(geohash) {
  sprintf("https://api.weather.bom.gov.au/v1/locations/%s/observations", geohash)
}

# The `ftp_feeds` rung: rolling 72-h obs JSON via `.ftp_get()` +
# `bom_parse_72h_obs()`.
.bom_obs_ftp_rung <- function(request_variables_env) {
  list(
    id = "ftp_feeds",
    kind = "ftp",
    applies_to = c("obs_72h"),
    fetch_fn = function(request, now = NULL) {
      body <- .ftp_get(.bom_obs_ftp_url("IDN60901"))
      bom_parse_72h_obs(
        body, request$variables,
        site_id = request_variables_env$site_id, source = request_variables_env$source_id
      )
    }
  )
}

# The `web_api` rung: rolling obs via `.http_get()` + `bom_parse_webapi_obs()`.
# Only ever included in the ladder when `allow_web_api = TRUE`.
.bom_obs_webapi_rung <- function(request_variables_env) {
  list(
    id = "web_api",
    kind = "http",
    applies_to = c("obs_72h"),
    fetch_fn = function(request, now = NULL) {
      geohash <- request_variables_env$geohash
      body <- .http_get(.bom_obs_webapi_url(geohash))
      bom_parse_webapi_obs(
        body, request$variables,
        site_id = request_variables_env$site_id, source = request_variables_env$source_id
      )
    }
  )
}

#' A BOM rolling 72-h observation adapter
#'
#' `source_bom_obs()` builds a [met_adapter()] that fetches BOM's rolling
#' 72-hour station observation JSON via the official anonymous-FTP/
#' HTTP-mirror product feeds (SCOPING §5.1 ladder rung 1), with an opt-in
#' web-API fallback (`allow_web_api = TRUE`) for when the official feed is
#' unavailable. Every row's `method` is `"measured"`.
#'
#' The `store_root` is used only to persist the transport circuit-breaker
#' state across runs (see `breaker_read()`/`breaker_write()`).
#'
#' @param ladder A list of transport rungs (see `ladder_fetch()`). If
#'   omitted, built from `allow_web_api`: always includes an `ftp_feeds`
#'   rung, and a `web_api` rung only when `allow_web_api = TRUE`.
#' @param allow_web_api Logical, default `FALSE`. When `TRUE`, adds the
#'   unofficial web-API rung to the default ladder as a fallback.
#' @param store_root Single string, the store root used for breaker-state
#'   persistence.
#' @param source_id Single string stamped into the `source` column. Default
#'   `"bom_obs"`.
#'
#' @return A `source_bom_obs` (`met_adapter` subclass) S7 object.
#' @family adapter
#' @export
#' @examples
#' adapter <- source_bom_obs(store_root = tempfile())
source_bom_obs <- S7::new_class(
  "source_bom_obs",
  package = "meteoTidy",
  parent = met_adapter,
  properties = list(
    ladder = S7::class_list,
    allow_web_api = S7::class_logical,
    store_root = S7::class_character
  ),
  constructor = function(ladder = NULL,
                         allow_web_api = FALSE,
                         store_root,
                         source_id = "bom_obs") {
    S7::new_object(
      met_adapter(
        source_id = source_id,
        provides = .bom_obs_variable_map(),
        cadence = "hourly"
      ),
      ladder = ladder %||% list(), # populated per-fetch (needs site context); see fetch()
      allow_web_api = allow_web_api,
      store_root = store_root
    )
  }
)

# Build the ladder for one fetch() call: rung fetch_fns close over `ctx`
# (site_id/source_id/geohash), which is only known once `site` is available,
# so the ladder cannot be fully built at construction time when using the
# default (NULL) ladder. If the adapter was constructed with an explicit
# `ladder`, that is used verbatim (advanced/test usage, e.g. `fake_transport`
# rungs in the frozen ladder/breaker tests).
.bom_obs_ladder_for <- function(adapter, site) {
  if (length(adapter@ladder) > 0) {
    return(adapter@ladder)
  }

  ctx <- list(site_id = site_id(site), source_id = adapter@source_id)
  rungs <- list(.bom_obs_ftp_rung(ctx))
  if (adapter@allow_web_api) {
    ctx_web <- list(
      site_id = site_id(site), source_id = adapter@source_id,
      geohash = site_resolved(site, c("bom", "geohash"))
    )
    rungs <- c(rungs, list(.bom_obs_webapi_rung(ctx_web)))
  }
  rungs
}

S7::method(fetch, source_bom_obs) <- function(adapter, site, variables, window, now = .now()) {
  variables <- intersect(variables, adapter@provides)
  ladder <- .bom_obs_ladder_for(adapter, site)

  breaker <- breaker_read(adapter@store_root)
  request <- list(product = "obs_72h", variables = variables, window = window)
  result <- ladder_fetch(ladder, request, breaker, now = now)
  breaker_write(adapter@store_root, attr(result, "breaker") %||% breaker)

  # `check_fetch_result()` calls `new_obs()` internally, which strips any
  # column beyond the 7 canonical ones -- including the `transport`
  # provenance column `ladder_fetch()` just stamped on. Call it here for its
  # VALIDATION side-effect only (the return value is discarded), then return
  # our OWN `result` object, which still has `transport` intact. This is the
  # only way `out$transport == "ftp_feeds"` (or "web_api" on fallback)
  # survives to the caller, per the frozen test's assertion.
  check_fetch_result(result, adapter, variables)
  result
}

S7::method(resolve_station, source_bom_obs) <- function(adapter, site, ...) {
  # No frozen test exercises resolve_station() on source_bom_obs()
  # directly. The web API is keyed by geohash (SCOPING §5.1); delegate to
  # the same geohash-resolution logic as the forecast adapter for a
  # reasonable, non-duplicated implementation, wrapping it in a
  # source_bom_forecast() built from this adapter's own settings so the
  # geohash cache ends up in the same place (site@resolved$bom$geohash)
  # either way.
  delegate <- source_bom_forecast(
    allow_web_api = adapter@allow_web_api,
    store_root = adapter@store_root,
    source_id = adapter@source_id
  )
  resolve_station(delegate, site, ...)
}

S7::method(format, source_bom_obs) <- function(x, ...) {
  c(
    sprintf("<source_bom_obs> source_id: %s", x@source_id),
    sprintf("  allow_web_api: %s", x@allow_web_api)
  )
}

S7::method(print, source_bom_obs) <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
