# Helpers for the production-review regression tests (test-regress-*.R).
#
# These tests replay RECORDED real API responses (saved under _fixtures/ on
# 2026-09-29 from the live Open-Meteo, BOM, eagle.io and SILO endpoints) through
# the package's real parsing code. Only the network seam (`.http_get()` /
# `.ftp_get()`) is replaced, by a URL router, so every call a fetch makes --
# metadata lookups included -- is matched to the fixture for that URL.

# A realistic sync clock: real wall-clock times carry fractional seconds.
prod_now <- function() as.POSIXct("2026-09-29 06:22:13.482913", tz = "UTC")

# The two production sites (see the production setup in README).
make_prod_site <- function(site_id = c("kat", "blax"), sources = list(),
                           store_root = NULL, env = parent.frame()) {
  site_id <- match.arg(site_id)
  if (is.null(store_root)) {
    store_root <- withr::local_tempdir(.local_envir = env)
  }
  spec <- switch(site_id,
    kat = list(lat = -33.70, lon = 150.32, elev = 1000),
    blax = list(lat = -33.73, lon = 150.61, elev = 250)
  )
  site <- met_site(
    site_id = site_id,
    latitude = units::set_units(spec$lat, "degree"),
    longitude = units::set_units(spec$lon, "degree"),
    elevation = units::set_units(spec$elev, "m"),
    timezone = "Australia/Sydney",
    instruments = list(),
    sources = sources,
    store_root = store_root
  )
  geohash <- switch(site_id, kat = "r64bhq", blax = "r65050")
  site <- site_set_resolved(site, c("bom", "geohash"), geohash)
  site
}

fixture_path <- function(...) testthat::test_path("_fixtures", ...)

# Route every .http_get()/.ftp_get() call by URL. `routes` is a named list:
# names are regexes matched against the full URL (first match wins); values
# are either a fixture path (relative to _fixtures/), a function(url) that
# returns a body or signals a condition, or an integer HTTP status to fail
# with (404/410 -> http_gone, anything else -> http_client_error).
# Unmatched URLs fail the test. Every call is recorded in `capture$urls`.
with_routed_http <- function(routes, expr, capture = new.env()) {
  capture$urls <- character(0)
  resolve <- function(url) {
    capture$urls <- c(capture$urls, url)
    hit <- which(vapply(names(routes), function(rx) grepl(rx, url, perl = TRUE), logical(1)))
    if (length(hit) == 0) {
      stop("with_routed_http(): no route for URL ", url)
    }
    target <- routes[[hit[[1]]]]
    if (is.function(target)) {
      return(target(url))
    }
    if (is.numeric(target)) {
      cls <- if (target %in% c(404, 410)) "http_gone" else "http_client_error"
      abort_meteo(sprintf("HTTP %d (routed)", target), class = cls)
    }
    fixture_path(target)
  }
  fake_http <- function(url, headers = list(), query = list(), retry = 3, now = NULL,
                        parse = c("json", "lines", "raw")) {
    parse <- match.arg(parse)
    capture$headers <- c(capture$headers, list(headers))
    path <- resolve(url)
    if (!is.character(path) || length(path) != 1 || !file.exists(path)) {
      return(path)
    }
    switch(parse,
      json = jsonlite::read_json(path, simplifyVector = FALSE),
      lines = readLines(path, warn = FALSE),
      raw = readBin(path, "raw", file.size(path))
    )
  }
  fake_ftp <- function(url, ...) {
    path <- resolve(url)
    if (!is.character(path) || length(path) != 1 || !file.exists(path)) {
      return(path)
    }
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }
  testthat::local_mocked_bindings(.http_get = fake_http, .ftp_get = fake_ftp,
                                  .env = parent.frame())
  force(expr)
}

# Routes serving the recorded Open-Meteo responses.
openmeteo_routes <- function() {
  list(
    "ensemble-api.*/data/ecmwf_ifs025_ensemble/static/meta.json" =
      "openmeteo/meta-ecmwf_ifs025_ensemble.json",
    "/data/ecmwf_ifs025/static/meta.json" = "openmeteo/meta-ecmwf_ifs025.json",
    "ensemble-api\\.open-meteo\\.com/v1/ensemble" = "openmeteo/ensemble-ecmwf-ifs025-15d.json",
    "api\\.open-meteo\\.com/v1/forecast.*models=ecmwf_ifs025" =
      "openmeteo/forecast-ecmwf-ifs025-undefined.json",
    "api\\.open-meteo\\.com/v1/forecast" = "openmeteo/forecast-best-match-16d.json"
  )
}
