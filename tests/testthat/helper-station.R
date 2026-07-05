# Helpers for Plan 06 — SILO / GHCNh return-frame builders + station catalogues.
#
# The adapters talk to weatherOz / worldmet through the package-owned seams
# `.weatheroz_get()` and `.worldmet_get()` (mirroring Plan 04's `.http_get()`),
# so tests replay a recorded/synthetic frame by mocking that seam — never the
# network, never the third-party internals directly.

# Mock the SILO wrapper seam to return `frame` for the duration of `expr`.
with_mocked_silo <- function(frame, expr, capture = new.env()) {
  fake <- function(query, dataset, api_key, ...) {
    capture$query <- query
    capture$dataset <- dataset
    capture$api_key <- api_key
    frame
  }
  testthat::local_mocked_bindings(.weatheroz_get = fake, .env = parent.frame())
  force(expr)
}

# Mock the worldmet (GHCNh) wrapper seam.
with_mocked_ghcnh <- function(frame, expr, capture = new.env()) {
  fake <- function(station, year, ...) {
    capture$station <- station
    capture$year <- year
    frame
  }
  testthat::local_mocked_bindings(.worldmet_get = fake, .env = parent.frame())
  force(expr)
}

# A weatherOz-style PatchedPoint daily frame. `qcode` sets the per-value SILO
# source/quality code (see silo-qcode). Dates are plain Date; the adapter maps
# each SILO "day" to the 9am local-clock-time instant in UTC.
make_silo_frame <- function(dates = as.Date(c("2026-01-15", "2026-07-15")),
                            variable = "max_temp",
                            value = c(30.5, 12.0),
                            qcode = c("25", "25")) {
  data.frame(
    station_code = "072150",
    station_name = "TEST SILO",
    latitude = -34.75,
    longitude = 148.20,
    date = dates,
    variable_name = variable,
    value = value,
    value_quality = qcode,
    stringsAsFactors = FALSE
  )
}

# A worldmet-style GHCNh hourly frame (already in UTC).
make_ghcnh_frame <- function(n = 3,
                             station = "ASN00072150",
                             start = as.POSIXct("2026-06-01 00:00", tz = "UTC")) {
  data.frame(
    code = station,
    station = "TEST GHCNh",
    date = start + (seq_len(n) - 1L) * 3600,
    air_temp = seq(10, by = 0.5, length.out = n),
    wd = seq(180, by = 5, length.out = n),
    ws = seq(3, by = 0.2, length.out = n),      # m/s already (worldmet convention)
    Quality_air_temp = "0",
    stringsAsFactors = FALSE
  )
}

# A hand-built station catalogue for nearest_stations() ordering tests, with
# coordinates whose great-circle distances from the reference are known.
make_station_catalogue <- function() {
  data.frame(
    station_id = c("near", "mid", "far"),
    latitude   = c(-34.76, -34.90, -35.50),
    longitude  = c(148.21, 148.30, 149.00),
    stringsAsFactors = FALSE
  )
}
