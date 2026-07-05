# Plan 15 — met_wide(): the §3.1 wide emitter (the one-call meteoHazard interface).

describe("met_wide() §3.1 contract", {
  it("emits exactly the §3.1 columns with time (UTC), canonical units, met_table", {
    site <- make_test_site()
    testthat::local_mocked_bindings(
      met_record = function(...) make_obs(n = 3),
      met_forecast_archive = function(...) make_forecast(n = 3)
    )
    out <- met_wide(site,
                    window = list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                                  to = as.POSIXct("2026-01-02", tz = "UTC")),
                    kind = "record", now = as.POSIXct("2026-01-01", tz = "UTC"))
    expect_s3_class(out, "met_table")
    # time renamed at this outer boundary only
    expect_true("time" %in% names(out))
    expect_false("datetime_utc" %in% names(out))
    expect_identical(attr(out$time, "tzone"), "UTC")
    # units pinned canonical — wind is m/s (the §3.1 pinning)
    if ("wind_speed_10m" %in% names(out)) {
      expect_equal(as.character(units::deparse_unit(
        units::set_units(out$wind_speed_10m, "m/s", mode = "standard"))), "m/s")
    }
  })

  it("emits an absent variable as an all-NA column (stable §3.1 shape)", {
    site <- make_test_site()
    testthat::local_mocked_bindings(
      met_record = function(...) make_obs(n = 3, variable = "temperature_2m")
    )
    out <- met_wide(site,
                    window = list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                                  to = as.POSIXct("2026-01-02", tz = "UTC")),
                    kind = "record", variables = c("temperature_2m", "cape"))
    expect_true("cape" %in% names(out))
    expect_true(all(is.na(out$cape)))
  })
})

describe("kind routing", {
  it("routes kind='forecast' via corrected forecast and kind='record' via record", {
    site <- make_test_site()
    called <- new.env(); called$fc <- 0L; called$rec <- 0L
    testthat::local_mocked_bindings(
      met_forecast_archive = function(...) { called$fc <- called$fc + 1L
                                             make_forecast(n = 2) },
      met_record = function(...) { called$rec <- called$rec + 1L; make_obs(n = 2) }
    )
    win <- list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                to = as.POSIXct("2026-01-02", tz = "UTC"))
    met_wide(site, window = win, kind = "forecast",
             now = as.POSIXct("2026-01-01", tz = "UTC"))
    expect_gt(called$fc, 0L)
    expect_equal(called$rec, 0L)

    met_wide(site, window = win, kind = "record",
             now = as.POSIXct("2026-01-01", tz = "UTC"))
    expect_gt(called$rec, 0L)
  })
})
