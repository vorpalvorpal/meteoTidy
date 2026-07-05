# Helpers for Plan 16 — pipeline verb fixtures.
#
# The verbs sequence calls to already-tested primitives. Tests replace those
# primitives with fakes (via local_mocked_bindings) and assert orchestration:
# ordering, per-site isolation, idempotency, degradation. The two acquisition
# seams below are the package-owned entry points the verbs call per source.

# A minimal deployment config the verbs accept.
pipeline_config <- function(store_root, forecast_sources = c("openmeteo", "bom_forecast"),
                            obs_sources = c("site_aws")) {
  list(store_root = store_root,
       forecast_sources = forecast_sources,
       obs_sources = obs_sources,
       refetch_windows = list(silo = as.difftime(30, units = "days")),
       adapter_defaults = list(bom = list(allow_web_api = FALSE)))
}

# Install fakes for the acquisition seams: `.acquire_forecast()` and
# `.acquire_obs()` return canonical rows; `record` in `calls` tracks what ran.
mock_acquisition <- function(calls = new.env(),
                             forecast = new_forecast(make_forecast(n = 3)),
                             obs = new_obs(make_obs(n = 3)),
                             fail_sources = character(),
                             env = parent.frame()) {
  calls$forecast <- character()
  calls$obs <- character()
  testthat::local_mocked_bindings(
    .acquire_forecast = function(source, site, window, now = NULL, ...) {
      calls$forecast <- c(calls$forecast, source)
      if (source %in% fail_sources) abort_meteo("dead channel", class = "http_gone")
      forecast
    },
    .acquire_obs = function(source, site, window, now = NULL, ...) {
      calls$obs <- c(calls$obs, source)
      if (source %in% fail_sources) abort_meteo("dead channel", class = "http_gone")
      obs
    },
    .env = env
  )
  calls
}
