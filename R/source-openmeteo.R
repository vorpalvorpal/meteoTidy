# Plan 05 — source_openmeteo(): one adapter covering every Open-Meteo product
# this package uses. See plans/05-acquisition-openmeteo.md for the full
# design; R/openmeteo-endpoints.R builds URLs/params, R/openmeteo-parse.R
# turns a parsed response into canonical obs/forecast rows.

#' An Open-Meteo acquisition adapter
#'
#' `source_openmeteo()` builds a [met_adapter()] that fetches from one of
#' Open-Meteo's products: Forecast, Ensemble, Historical Weather (ERA5),
#' Historical Forecast, Previous Runs, Single Runs, and Seasonal.
#'
#' ## Licensing (SCOPING §10)
#'
#' The free tier is licensed for **non-commercial use only**, at up to 10 000
#' calls/day, with no API key required. **No product in this adapter aborts
#' for lack of a key** -- the free tier technically serves every product
#' wrapped here, including Historical Weather and Ensemble. When
#' `api_key_env` is unset, `fetch()`/`fetch_forecast()` target the free host
#' and emit a one-time [inform_meteo()] reminder that the free tier is
#' non-commercial only.
#'
#' Commercial deployments need a **paid** Open-Meteo plan, and within those
#' paid plans, the Historical/Climate/Ensemble/Satellite-Radiation APIs
#' additionally require the **Professional tier or above**. That is a
#' commercial-plan boundary, not a technical key gate this adapter enforces:
#' set `api_key_env` to the name of an environment variable holding a
#' commercial key and the adapter targets the `customer-` API host and sends
#' the key; which paid plan is required for a given product/volume is the
#' caller's responsibility to arrange with Open-Meteo.
#'
#' The key is read from the named environment variable **at fetch time only**
#' -- it is never stored on the adapter object, never appears in
#' `print()`/`format()` output, and never appears in any column of a returned
#' tibble.
#'
#' @param product One of `"forecast"`, `"ensemble"`, `"historical"`,
#'   `"historical_forecast"`, `"previous_runs"`, `"single_runs"`,
#'   `"seasonal"`. Selects the endpoint and the response shape.
#' @param models Optional character vector of underlying NWP model ids (see
#'   the (non-exhaustive, extensible) roster in `R/openmeteo-endpoints.R`).
#'   `NULL` (default) lets Open-Meteo pick its default model(s) for the
#'   product.
#' @param api_key_env Optional single string: the *name* of an environment
#'   variable holding a commercial Open-Meteo API key. See Licensing above.
#' @param source_id Single string stamped into the `source` column of every
#'   returned row. Default `"openmeteo"`.
#' @param ... Reserved for future product-specific options.
#'
#' @return A `source_openmeteo` (`met_adapter` subclass) S7 object.
#' @family adapter
#' @export
#' @examples
#' adapter <- source_openmeteo(product = "forecast")
#' adapter2 <- source_openmeteo(product = "historical", api_key_env = "OPEN_METEO_KEY")
source_openmeteo <- S7::new_class(
  "source_openmeteo",
  package = "meteoTidy",
  parent = met_adapter,
  properties = list(
    product = S7::class_character,
    models = S7::class_character,
    api_key_env = S7::class_character
  ),
  constructor = function(
    product = c(
      "forecast", "ensemble", "historical", "historical_forecast",
      "previous_runs", "single_runs", "seasonal"
    ),
    models = NULL, api_key_env = NULL,
    source_id = "openmeteo", ...
  ) {
    product <- rlang::arg_match(product)
    S7::new_object(
      met_adapter(
        source_id = source_id,
        provides = met_variables()$variable,
        cadence = if (product == "seasonal") "daily" else "hourly"
      ),
      product = product,
      models = models %||% NA_character_,
      api_key_env = api_key_env %||% NA_character_
    )
  }
)

# Read the commercial key (or NULL if none configured) at fetch time only.
.openmeteo_read_key <- function(adapter) {
  if (is.na(adapter@api_key_env)) {
    return(NULL)
  }
  key <- Sys.getenv(adapter@api_key_env, unset = NA_character_)
  if (is.na(key) || !nzchar(key)) {
    return(NULL)
  }
  key
}

.openmeteo_models <- function(adapter) {
  if (length(adapter@models) == 1 && is.na(adapter@models)) NULL else adapter@models
}

# The one-time non-commercial notice: emitted whenever a request goes out on
# the free host (no key configured). Per-call (see roxygen note above and the
# implementer brief): the frozen snapshot test calls fetch() once, so a
# per-call inform satisfies it; this is not a per-session dedup.
.openmeteo_maybe_notice_free_tier <- function(has_key) {
  if (!has_key) {
    inform_meteo(
      c(
        "Using the Open-Meteo free tier.",
        "i" = "Free-tier data is licensed for {.strong non-commercial} use only (< 10,000 calls/day)." # nolint: line_length_linter.
      ),
      class = "openmeteo_free_tier"
    )
  }
  invisible(NULL)
}

S7::method(fetch, source_openmeteo) <- function(adapter, site, variables, window, now = .now()) {
  if (!identical(adapter@product, "historical")) {
    abort_meteo(
      c(
        "{.fn fetch} only supports {.val historical} for {.cls source_openmeteo}.",
        "i" = "Use {.fn fetch_forecast} for product {.val {adapter@product}}."
      ),
      class = "no_forecast_support"
    )
  }

  key <- .openmeteo_read_key(adapter)
  .openmeteo_maybe_notice_free_tier(has_key = !is.null(key))

  variables <- intersect(variables, adapter@provides)
  url <- .openmeteo_build_url(
    "historical", site, variables, window,
    api_key = key, models = .openmeteo_models(adapter)
  )
  body <- .http_get(url, query = list(), now = now)

  out <- .openmeteo_parse_obs(body, site, variables, adapter@source_id)
  check_fetch_result(out, adapter, variables)
}

S7::method(fetch_forecast, source_openmeteo) <- function(
  adapter, site, variables, issue_window, now = .now()
) {
  key <- .openmeteo_read_key(adapter)
  .openmeteo_maybe_notice_free_tier(has_key = !is.null(key))

  variables <- intersect(variables, adapter@provides)
  product <- adapter@product
  models <- .openmeteo_models(adapter)
  model_label <- if (!is.null(models)) models[[1]] else adapter@product

  url <- .openmeteo_build_url(
    product, site, variables, issue_window,
    api_key = key, models = models
  )
  body <- .http_get(url, query = list(), now = now)

  out <- switch(product,
    forecast             = .openmeteo_parse_forecast(
      body, site, variables, adapter@source_id, model_label, now
    ),
    ensemble              = .openmeteo_parse_ensemble(
      body, site, variables, adapter@source_id, model_label, now
    ),
    previous_runs         = .openmeteo_parse_previous_runs(
      body, site, variables, adapter@source_id, model_label, now
    ),
    single_runs           = .openmeteo_parse_forecast(
      body, site, variables, adapter@source_id, model_label, now
    ),
    historical_forecast   = .openmeteo_parse_historical_forecast(
      body, site, variables, adapter@source_id, model_label, now
    ),
    seasonal              = .openmeteo_parse_seasonal(
      body, site, variables, adapter@source_id, now
    ),
    abort_meteo(
      "Product {.val {product}} does not support {.fn fetch_forecast}.",
      class = "no_forecast_support"
    )
  )

  out[out$variable %in% variables, , drop = FALSE]
}

#' The attribution/credit string for an adapter's data source
#'
#' Some sources require a specific credit line to be surfaced to end users
#' (e.g. Open-Meteo's CC-BY licence). `met_attribution()` exposes it so
#' dashboards/reports can display it. The default method returns `NA`;
#' adapters that need a specific credit line override it.
#'
#' @param adapter A [met_adapter()] subclass instance.
#' @return A single string (the attribution text), or `NA_character_` if the
#'   adapter has no specific attribution requirement.
#' @family adapter
#' @export
#' @examples
#' met_attribution(source_openmeteo(product = "forecast"))
met_attribution <- S7::new_generic("met_attribution", "adapter", function(adapter) {
  S7::S7_dispatch()
})

S7::method(met_attribution, met_adapter) <- function(adapter) {
  NA_character_
}

S7::method(met_attribution, source_openmeteo) <- function(adapter) {
  "Weather data by Open-Meteo.com (CC-BY 4.0)"
}

S7::method(format, source_openmeteo) <- function(x, ...) {
  c(
    sprintf("<source_openmeteo> source_id: %s", x@source_id),
    sprintf("  product: %s", x@product),
    sprintf("  commercial: %s", !is.na(x@api_key_env)),
    if (!is.na(x@api_key_env)) sprintf("  api_key_env: %s (value not shown)", x@api_key_env)
  )
}

S7::method(print, source_openmeteo) <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
