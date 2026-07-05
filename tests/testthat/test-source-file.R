# Plan 04 — source_file() (logger CSV/TSV drops).

describe("source_file()", {
  it("maps a fixture CSV to canonical rows honouring delimiter/skip/na options", {
    site <- make_test_site()
    adapter <- source_file("logger",
                           glob = test_path("_fixtures/file/logger-*.csv"),
                           make_csv_mapping())
    win <- list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                to = as.POSIXct("2026-01-02", tz = "UTC"))
    out <- fetch(adapter, site, c("temperature_2m", "wind_speed_10m"), win)
    expect_canonical_obs(out)
  })

  it("concatenates multiple glob-matched files in deterministic time order", {
    site <- make_test_site()
    adapter <- source_file("logger",
                           glob = test_path("_fixtures/file/logger-*.csv"),
                           make_csv_mapping())
    win <- list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                to = as.POSIXct("2026-12-31", tz = "UTC"))
    out <- fetch(adapter, site, "temperature_2m", win)
    t <- out$datetime_utc[out$variable == "temperature_2m"]
    expect_false(is.unsorted(t))
  })

  it("applies the same unit conversion as the REST path (shared apply_mapping)", {
    site <- make_test_site()
    adapter <- source_file("logger",
                           glob = test_path("_fixtures/file/logger-a.csv"),
                           make_csv_mapping())
    win <- list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                to = as.POSIXct("2026-12-31", tz = "UTC"))
    out <- fetch(adapter, site, "wind_speed_10m", win)
    # wind_kmh column is km/h; canonical output is m/s
    expect_true(all(out$value[out$variable == "wind_speed_10m"] < 20))
  })
})
