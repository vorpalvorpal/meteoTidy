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
#
# Live-forecast products (`forecast`, `ensemble`) request the FULL forecast
# horizon (`forecast_days`) from the current run, never a past date window:
# the archive's `issue_window` bounds issue times, not valid times, and
# reusing it as start/end dates (problem 2 of the production review) turned
# every "issuance" into past data. The historical/hindcast products keep the
# date window, which is what they are for.
.openmeteo_build_url <- function(product, site, variables, window, api_key = NULL,
                                 models = NULL, forecast_days = NULL) {
  block <- .openmeteo_block_name(product)
  host <- .openmeteo_host(product, has_key = !is.null(api_key))

  params <- list(
    latitude = as.numeric(units::drop_units(site_coords(site)$latitude)),
    longitude = as.numeric(units::drop_units(site_coords(site)$longitude))
  )
  if (product %in% .openmeteo_horizon_products()) {
    params$forecast_days <- forecast_days %||% .openmeteo_default_forecast_days(product)
  } else {
    params$start_date <- format(window$from, "%Y-%m-%d", tz = "UTC")
    params$end_date <- format(window$to, "%Y-%m-%d", tz = "UTC")
  }
  params$wind_speed_unit <- "ms"
  params$temperature_unit <- "celsius"
  params$precipitation_unit <- "mm"
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

# ---- live-forecast horizon, default models, run (init) time -----------------

# Products that are fetched as "the current run, full horizon".
.openmeteo_horizon_products <- function() {
  c("forecast", "ensemble")
}

# The longest horizon each product serves on the free API: 16 days for the
# deterministic Forecast API, 15 days for ECMWF IFS ensemble (the ensemble
# API itself allows up to 35 days for models that run that long -- override
# with `forecast_days` on source_openmeteo()).
.openmeteo_default_forecast_days <- function(product) {
  switch(product, forecast = 16L, ensemble = 15L, 16L)
}

# Default underlying models when none are configured. The Ensemble API has no
# "best_match" and returns HTTP 400 without `models` (problem 6), so it
# defaults to ECMWF IFS 0.25 deg. The deterministic Forecast API defaults to
# three NAMED global models, each archived under its own label with its real
# run time (follow-up review, item 6): "best_match" blends models, so any
# issue time given to it is a guess and it cannot be verified. Between them
# the three serve every section 3.1 variable (ECMWF IFS lacks soil moisture,
# boundary-layer height, UV and 80 m wind; ICON and GFS fill those), and
# met_wide() picks per variable in this order.
.openmeteo_default_models <- function(product) {
  switch(product,
    ensemble = "ecmwf_ifs025",
    c("ecmwf_ifs025", "gfs_global", "icon_global")
  )
}

# Default variables for the ensemble: the ensemble API serves a subset of the
# dictionary (unsupported ones come back with unit "undefined"), and every
# extra variable multiplies an already 51-member request -- large requests hit
# HTTP 429 on the free tier. These are the variables the hazard models and
# the email actually use probabilistically.
.openmeteo_ensemble_default_variables <- function() {
  c(
    "temperature_2m", "relative_humidity_2m", "precipitation",
    "wind_speed_10m", "wind_direction_10m", "wind_gusts_10m"
  )
}

# Default variables for the hourly deterministic products: every dictionary
# variable Open-Meteo serves under the same hourly name. Daily-only
# dictionary variables (temperature_2m_max, ...) are not valid hourly
# parameters and would make the request fail with HTTP 400.
.openmeteo_hourly_variables <- function() {
  c(
    .met31_variables(), "dewpoint_2m", "shortwave_radiation",
    "precipitation_probability", "apparent_temperature", "cape", "uv_index"
  )
}

# The metadata dataset name for a model, used to look up its latest run's
# initialisation time at <host>/data/<name>/static/meta.json. Ensemble
# datasets carry a suffix; unknown models fall back to their own id (and, if
# that has no metadata, to the run-cycle floor in .openmeteo_issue_time()).
.openmeteo_meta_name <- function(product, model) {
  if (identical(product, "ensemble")) {
    known <- c(
      ecmwf_ifs025 = "ecmwf_ifs025_ensemble",
      ecmwf_aifs025 = "ecmwf_aifs025_ensemble",
      gfs025 = "ncep_gefs025",
      gfs05 = "ncep_gefs05",
      icon_seamless = "dwd_icon_eps",
      gem_global = "cmc_gem_geps",
      bom_access_global_ensemble = "bom_access_global_ensemble"
    )
    return(unname(known[model]) %|NA|% paste0(model, "_ensemble"))
  }
  # Forecast API model ids whose metadata lives under a different dataset
  # name (checked live 2026-09-29).
  known <- c(
    gfs_global = "ncep_gfs025",
    gfs025 = "ncep_gfs025",
    icon_global = "dwd_icon",
    icon_eu = "dwd_icon_eu",
    icon_d2 = "dwd_icon_d2",
    gem_global = "cmc_gem_gdps",
    bom_access_global = "bom_access_global"
  )
  unname(known[model]) %|NA|% model
}

`%|NA|%` <- function(x, y) if (length(x) == 0 || is.na(x)) y else x

.openmeteo_meta_url <- function(product, model, has_key) {
  spec <- .openmeteo_endpoint_path(product)
  subdomain <- if (has_key) paste0("customer-", spec$subdomain) else spec$subdomain
  sprintf("https://%s.open-meteo.com/data/%s/static/meta.json",
          subdomain, .openmeteo_meta_name(product, model))
}

# Floor a time to the start of its 6-hourly NWP run cycle (00/06/12/18 UTC).
.floor_run_cycle <- function(t, hours = 6) {
  secs <- as.numeric(t)
  as.POSIXct(floor(secs / (hours * 3600)) * hours * 3600, origin = "1970-01-01", tz = "UTC")
}

#' The issue (run initialisation) time of the forecast about to be fetched
#'
#' Problem 3 of the production review: stamping `issue_time = now` made every
#' hourly sync look like a new issuance, so dedup never matched and the same
#' run was archived again and again. For a named model this reads the run's
#' initialisation time from Open-Meteo's model metadata
#' (`last_run_initialisation_time`). "best_match" (a blend) has no single run
#' time, and metadata can be unavailable, so the fallback is `now` floored to
#' the 6-hourly run cycle: every sync within one cycle maps to the same
#' issuance, which is stored once.
#'
#' @return A UTC POSIXct scalar (whole seconds), never later than `now`.
#' @keywords internal
#' @noRd
.openmeteo_issue_time <- function(product, model, api_key, now) {
  .openmeteo_run_meta(product, model, api_key, now)$issue_time
}

# The run about to be fetched: its issue time and, when Open-Meteo reports
# it, `data_end` (the last valid time the run is known to cover; NA when
# unknown). Used by .openmeteo_issue_time() and to skip re-downloading a run
# already archived (item 7).
.openmeteo_run_meta <- function(product, model, api_key, now) {
  if (is.null(model) || identical(model, "best_match") ||
        !(product %in% .openmeteo_horizon_products())) {
    # best_match blends several models, so it has no single run: the
    # 6-hourly cycle floor is the documented convention.
    return(list(issue_time = .floor_run_cycle(now), data_end = as.POSIXct(NA, tz = "UTC")))
  }
  # A named model must be stamped with its real run. Guessing (the clock's
  # 6 h floor) mislabelled ECMWF's 18 UTC ensemble as 06 UTC in a live run;
  # failing lets the next sync archive it correctly instead.
  meta_error <- NULL
  meta <- tryCatch(
    .http_get(.openmeteo_meta_url(product, model, has_key = !is.null(api_key)),
              query = list(), now = now),
    error = function(e) {
      meta_error <<- .one_line(conditionMessage(e))
      NULL
    }
  )
  init <- if (is.list(meta)) suppressWarnings(as.numeric(meta$last_run_initialisation_time)) else NA
  init <- if (length(init) == 1 && !is.na(init)) as.POSIXct(init, origin = "1970-01-01", tz = "UTC") else NA
  if (is.na(init) || init > now) {
    why <- gsub("([{}])", "\\1\\1", meta_error %||% "no valid last_run_initialisation_time")
    abort_meteo(
      c("Could not determine the run time of Open-Meteo model {.val {model}} ({product}); not archiving it this time.",
        x = why),
      class = "openmeteo_run_unknown"
    )
  }
  end <- if (is.list(meta)) suppressWarnings(as.numeric(meta$data_end_time)) else NA
  end <- if (length(end) == 1 && !is.na(end)) end else NA_real_
  list(issue_time = init, data_end = as.POSIXct(end, origin = "1970-01-01", tz = "UTC"))
}
