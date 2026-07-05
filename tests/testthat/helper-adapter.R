# Helpers for Plan 04 — adapter/mapping builders + HTTP mocking.

# A REST response-mapping spec (JSON) with a wind field declared in km/h so the
# unit-conversion footgun can be exercised end-to-end.
make_rest_mapping <- function() {
  met_mapping(
    format = "json",
    time = list(path = "hourly/time", tz = "UTC"),
    variables = list(
      list(variable = "temperature_2m", path = "hourly/temperature_2m",
           unit = "degC", height = units::set_units(2, "m")),
      list(variable = "wind_speed_10m", path = "hourly/wind_speed_10m",
           unit = "km/h", height = units::set_units(10, "m"))
    )
  )
}

# A CSV mapping for logger drops (`source_file()`), same variables/units.
make_csv_mapping <- function() {
  met_mapping(
    format = "csv",
    time = list(column = "timestamp", tz = "UTC"),
    variables = list(
      list(variable = "temperature_2m", column = "temp_c", unit = "degC",
           height = units::set_units(2, "m")),
      list(variable = "wind_speed_10m", column = "wind_kmh", unit = "km/h",
           height = units::set_units(10, "m"))
    )
  )
}

# Ensure the no-network guard is active for a single test (normally global).
local_no_net <- function(env = parent.frame()) {
  withr::local_envvar(METEOTIDY_NO_NET = "1", .local_envir = env)
}

# Replay a recorded API body from `_fixtures/` through the `.http_get()` seam
# instead of hitting the network. `body` is the already-parsed list a real
# `.http_get()` would have returned for that URL.
with_mocked_http <- function(body, expr, capture = new.env()) {
  fake <- function(url, headers = list(), query = list(), retry = 3, now = NULL) {
    capture$url <- url
    capture$headers <- headers
    capture$query <- query
    body
  }
  testthat::local_mocked_bindings(.http_get = fake, .env = parent.frame())
  force(expr)
}

# Load a JSON fixture body as a parsed list.
read_json_fixture <- function(path) {
  jsonlite::read_json(testthat::test_path(path), simplifyVector = FALSE)
}
