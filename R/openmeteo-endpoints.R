# Plan 05 — Open-Meteo endpoint URLs, param builders, and model lists.
#
# Design decision (see plans/05-acquisition-openmeteo.md and the Plan 05
# implementer brief): the FULL query string is built into the request URL,
# not passed via `.http_get()`'s `query=` argument. Every capture-based test
# assertion falls back from `cap$query$X` to `cap$url` via `%||%`, and the
# licensing tests paste `unlist(cap$query)` (which drops names) together with
# `cap$url`. Building the complete, URL-encoded query string ourselves is the
# only way every literal (`"temperature_2m"`, `"wind_speed_unit"`, `"ms"`,
# `"customer-"`, the API key value, `"open-meteo.com"`) is guaranteed
# detectable. `.http_get()` is still called with `query = list()`.

# The product roster this adapter wraps. Not a technical gate on any Open-
# Meteo product Open-Meteo itself might add later -- just what Plan 05 wires
# up (SCOPING §5).
.openmeteo_products <- function() {
  c(
    "forecast", "ensemble", "historical", "historical_forecast",
    "previous_runs", "single_runs", "seasonal"
  )
}

# Per-product endpoint path (relative to the host). Open-Meteo splits
# products across subdomains; the exact subdomain choice is not load-bearing
# for the tests (which only check for "open-meteo.com" / absence of
# "customer-"), but each product genuinely lives at one of these paths on the
# real API.
.openmeteo_endpoint_path <- function(product) {
  switch(product,
    forecast             = list(subdomain = "api", path = "/v1/forecast"),
    ensemble              = list(subdomain = "ensemble-api", path = "/v1/ensemble"),
    historical            = list(subdomain = "archive-api", path = "/v1/archive"),
    historical_forecast   = list(subdomain = "historical-forecast-api", path = "/v1/forecast"),
    previous_runs         = list(subdomain = "previous-runs-api", path = "/v1/previous-runs"),
    single_runs           = list(subdomain = "api", path = "/v1/forecast"),
    seasonal              = list(subdomain = "seasonal-api", path = "/v1/seasonal"),
    abort_meteo(
      c(
        "Unknown Open-Meteo product {.val {product}}.",
        "i" = "Recognised products: {.val {.openmeteo_products()}}."
      ),
      class = "unknown_openmeteo_product"
    )
  )
}

# Which block of the response this product's data lives in: "hourly" or
# "daily". Only Seasonal uses daily cadence among the products wired up here.
.openmeteo_block_name <- function(product) {
  if (identical(product, "seasonal")) "daily" else "hourly"
}

# The underlying NWP model roster Ensemble/Seasonal/etc. can draw on. Kept
# here as the single, EXTENSIBLE source of truth -- this list is not
# presented as exhaustive; Open-Meteo's real roster is larger and changes
# over time.
.openmeteo_model_roster <- function() {
  c(
    "ecmwf_ifs025", "ecmwf_aifs025", "icon_seamless", "gfs_seamless",
    "gem_global", "ukmo_global_ensemble_20km", "bom_access_global_ensemble"
  )
}

# Build the full host for a product, selecting the free or `customer-`
# (commercial) subdomain prefix based on whether a key is present.
.openmeteo_host <- function(product, has_key) {
  spec <- .openmeteo_endpoint_path(product)
  subdomain <- if (has_key) paste0("customer-", spec$subdomain) else spec$subdomain
  sprintf("https://%s.open-meteo.com%s", subdomain, spec$path)
}

# URL-encode and join a named list of scalar character params into a query
# string, e.g. list(a = "1", b = "x y") -> "a=1&b=x%20y".
.openmeteo_build_query_string <- function(params) {
  params <- params[!vapply(params, is.null, logical(1))]
  parts <- vapply(names(params), function(nm) {
    sprintf(
      "%s=%s", utils::URLencode(nm, reserved = TRUE),
      utils::URLencode(as.character(params[[nm]]), reserved = TRUE)
    )
  }, character(1))
  paste(parts, collapse = "&")
}

# Build the complete request URL (host + path + query string) for one fetch.
# `variables` are requested under their dictionary names verbatim (Open-Meteo
# names already equal our dictionary names for the §3.1 set). Canonical units
# are always requested explicitly (the km/h wind-speed footgun, SCOPING §3.1).
.openmeteo_build_url <- function(product, site, variables, window, api_key = NULL,
                                 models = NULL) {
  block <- .openmeteo_block_name(product)
  host <- .openmeteo_host(product, has_key = !is.null(api_key))

  params <- list(
    latitude = as.numeric(units::drop_units(site_coords(site)$latitude)),
    longitude = as.numeric(units::drop_units(site_coords(site)$longitude)),
    start_date = format(window$from, "%Y-%m-%d", tz = "UTC"),
    end_date = format(window$to, "%Y-%m-%d", tz = "UTC"),
    wind_speed_unit = "ms",
    temperature_unit = "celsius",
    precipitation_unit = "mm"
  )
  params[[block]] <- paste(variables, collapse = ",")

  if (!is.null(models) && length(models) > 0) {
    params$models <- paste(models, collapse = ",")
  }
  if (!is.null(api_key)) {
    params$apikey <- api_key
  }

  query_string <- .openmeteo_build_query_string(params)
  sprintf("%s?%s", host, query_string)
}
