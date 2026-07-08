# Helpers for Plan 05 — Open-Meteo adapter builders + fixture loaders.
#
# Every Open-Meteo test replays a recorded/synthetic body through the Plan 04
# `.http_get()` seam (`with_mocked_http()` from helper-adapter.R). These helpers
# keep each `it()` block down to "body-in -> canonical-out assertion".

# Load one of the committed Open-Meteo fixture bodies as a parsed list.
read_om_fixture <- function(name) {
  read_json_fixture(file.path("_fixtures/openmeteo", name)) # nolint: object_usage_linter.
}

# A frozen "now" used as the forecast issue time in every forecast test, so
# issue_time / lead_time are deterministic.
om_now <- function() as.POSIXct("2026-01-01 00:00:00", tz = "UTC")

# Fetch a product end-to-end against a mocked body. `product` is passed straight
# to `source_openmeteo()`; `body` is the parsed fixture list the seam returns.
om_fetch <- function(product, body, site = make_test_site(), # nolint: object_usage_linter.
                     variables = "temperature_2m",
                     api_key_env = NULL, now = om_now(),
                     window = list(from = as.POSIXct("2020-01-01", tz = "UTC"),
                                   to = as.POSIXct("2020-01-02", tz = "UTC")),
                     capture = new.env()) {
  adapter <- source_openmeteo(product = product, api_key_env = api_key_env)
  obs_like <- product %in% c("historical")
  with_mocked_http(body, { # nolint: object_usage_linter.
    if (obs_like) {
      fetch(adapter, site, variables, window, now = now)
    } else {
      fetch_forecast(adapter, site, variables, window, now = now)
    }
  }, capture = capture)
}
