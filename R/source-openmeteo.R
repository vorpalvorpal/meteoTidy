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
#'   Each model is requested separately and archived under its own `model`
#'   label, stamped with that model's real run initialisation time from
#'   Open-Meteo's metadata. `NULL` (default) uses `c("ecmwf_ifs025",
#'   "gfs_global", "icon_global")` for `"forecast"` and `"ecmwf_ifs025"`
#'   (ECMWF IFS 0.25 deg) for `"ensemble"` -- the Ensemble API rejects
#'   requests without a model. `"best_match"` (Open-Meteo's blend) is
#'   fetched only when named here; it has no run time, so it is stamped with
#'   the 6-hourly cycle floor of the fetch time and flagged "not verifiable"
#'   in `forecast_aux` (field `issue_time_basis:best_match`). When one of
#'   several models fails, the others are still returned, with a warning of
#'   class `meteoTidy_warning_openmeteo_model_failed`.
#' @param provides Optional character vector narrowing the variables this
#'   adapter requests (e.g. from site YAML). Defaults to every hourly
#'   dictionary variable Open-Meteo serves (a smaller set for `"ensemble"`).
#' @param forecast_days Optional horizon in days for `"forecast"`/
#'   `"ensemble"`; defaults to the longest the product serves (16 / 15).
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
    api_key_env = S7::class_character,
    forecast_days = S7::class_integer
  ),
  constructor = function(
    product = c(
      "forecast", "ensemble", "historical", "historical_forecast",
      "previous_runs", "single_runs", "seasonal"
    ),
    models = NULL, api_key_env = NULL,
    source_id = "openmeteo", provides = NULL, forecast_days = NULL, ...
  ) {
    product <- rlang::arg_match(product)
    default_provides <- if (product == "ensemble") {
      .openmeteo_ensemble_default_variables()
    } else if (product == "seasonal") {
      met_variables()$variable
    } else {
      .openmeteo_hourly_variables()
    }
    provides <- .narrow_provides(default_provides, provides, source_id)
    S7::new_object(
      met_adapter(
        source_id = source_id,
        provides = provides,
        cadence = if (product == "seasonal") "daily" else "hourly"
      ),
      product = product,
      models = models %||% NA_character_,
      api_key_env = api_key_env %||% NA_character_,
      forecast_days = as.integer(forecast_days %||% NA_integer_)
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
  forecast_days <- if (is.na(adapter@forecast_days)) NULL else adapter@forecast_days

  # One request per model: each model has its own run (init) time, and a
  # multi-model request would suffix every column with the model id.
  if (product %in% .openmeteo_horizon_products()) {
    models <- models %||% .openmeteo_default_models(product)
  }
  model_list <- if (is.null(models)) list(NULL) else as.list(models)

  # Each model is isolated: one model's failure (e.g. its run time is not
  # yet known) is a warning while the others still archive; only when every
  # model fails does the source fail, with the first model's error.
  errors <- list()
  pieces <- lapply(model_list, function(model) {
    tryCatch(
      .openmeteo_fetch_one_model(adapter, site, variables, issue_window, now,
                                 product, model, key, forecast_days),
      error = function(cnd) {
        errors[[model %||% product]] <<- cnd
        NULL
      }
    )
  })
  if (length(errors) > 0 && length(errors) == length(model_list)) {
    rlang::cnd_signal(errors[[1]])
  }
  for (m in names(errors)) {
    reason <- gsub("([{}])", "\\1\\1", .one_line(conditionMessage(errors[[m]]))) # nolint: object_usage_linter. used via cli glue
    warn_meteo(
      c("Open-Meteo model {.val {m}} was not archived this time; the other models were.",
        x = reason),
      class = "openmeteo_model_failed"
    )
  }
  out <- vctrs::vec_rbind(!!!pieces)
  if (is.null(out)) {
    return(new_forecast(.empty_forecast()))
  }
  aux <- .openmeteo_issue_basis_aux(out, site, adapter@source_id)
  out <- out[out$variable %in% variables, , drop = FALSE]
  if (!is.null(aux)) {
    attr(out, "aux") <- aux
  }
  out
}

# best_match blends several models, so its issue_time (the 6-hourly cycle
# floor of the fetch time) is not a run anyone issued. Say so beside the
# rows, in forecast_aux, so verification and readers can tell (item 6).
.openmeteo_issue_basis_aux <- function(fc, site, source_id) {
  bm <- fc[fc$model %in% "best_match", , drop = FALSE]
  if (nrow(bm) == 0) {
    return(NULL)
  }
  issue <- unique(bm$issue_time)
  new_forecast_aux(tibble::tibble(
    site_id = site_id(site),
    source = source_id,
    issue_time = issue,
    valid_time = issue,
    field = "issue_time_basis:best_match",
    value_text = paste(
      "not verifiable: best_match blends several models, so it has no run time;",
      "issue_time is the fetch time floored to the 6-hourly cycle"
    )
  ))
}

.openmeteo_fetch_one_model <- function(adapter, site, variables, issue_window, now,
                                       product, model, key, forecast_days) {
  model_label <- model %||% adapter@product
  url <- .openmeteo_build_url(
    product, site, variables, issue_window,
    api_key = key,
    models = if (identical(model, "best_match")) NULL else model,
    forecast_days = forecast_days
  )
  issue_time <- if (product %in% .openmeteo_horizon_products()) {
    .openmeteo_issue_time(product, model, key, now)
  } else {
    now
  }
  body <- .http_get(url, query = list(), now = now)

  out <- switch(product,
    forecast             = .openmeteo_parse_forecast(
      body, site, variables, adapter@source_id, model_label, issue_time
    ),
    ensemble              = .openmeteo_parse_ensemble(
      body, site, variables, adapter@source_id, model_label, issue_time
    ),
    previous_runs         = .openmeteo_parse_previous_runs(
      body, site, variables, adapter@source_id, model_label, now
    ),
    single_runs           = .openmeteo_parse_forecast(
      body, site, variables, adapter@source_id, model_label, now,
      horizon_only = FALSE
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
  out
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
