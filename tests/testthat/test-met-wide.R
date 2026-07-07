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
    # units pinned canonical — wind is m/s (the §3.1 pinning; deparse_unit()
    # renders m/s in exponent notation as "m s-1")
    expect_true("wind_speed_10m" %in% names(out))
    wind_ms <- units::set_units(out$wind_speed_10m, "m/s", mode = "standard")
    expect_equal(as.character(units::deparse_unit(wind_ms)), "m s-1")
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

  it("defaults to the full §3.1 contract column set", {
    site <- make_test_site()
    testthat::local_mocked_bindings(
      met_record = function(...) make_obs(n = 3, variable = "temperature_2m")
    )
    out <- met_wide(site,
                    window = list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                                  to = as.POSIXct("2026-01-02", tz = "UTC")),
                    kind = "record")
    expect_setequal(setdiff(names(out), "time"), .met31_variables())
    expect_true(all(is.na(out$boundary_layer_height)))
  })

  it("emits an absent variable as all-NA on the FORECAST path too", {
    # Regression: stats::aggregate() errors with "no rows to aggregate" on a
    # zero-row subset, so a variable missing from the archive window used to
    # abort met_wide(kind = "forecast") instead of yielding an NA column.
    root <- local_store()
    site <- make_test_site(store_root = root)
    fc <- make_forecast(n = 2, variable = "temperature_2m")
    store_write_forecast(root, new_forecast(fc))
    out <- met_wide(site,
                    window = list(from = fc$valid_time[1] - 3600,
                                  to = fc$valid_time[2] + 3600),
                    kind = "forecast", variables = c("temperature_2m", "cape"))
    expect_true(all(is.na(out$cape)))
    expect_false(anyNA(out$temperature_2m))
  })

  it("aborts on a multi-site collection (the wide table is per-site)", {
    expect_error(
      met_wide(make_test_sites(2),
               window = list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                             to = as.POSIXct("2026-01-02", tz = "UTC"))),
      class = "meteoTidy_error_multi_site_wide"
    )
  })
})

describe("kind = 'forecast' serves the latest issuance only", {
  it("ignores older archived issuances of the same (source, model)", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    old_issue <- as.POSIXct("2026-01-01 00:00", tz = "UTC")
    new_issue <- as.POSIXct("2026-01-02 00:00", tz = "UTC")
    valid <- as.POSIXct("2026-01-03 00:00", tz = "UTC")
    fc <- tibble::tibble(
      site_id = "test", source = "openmeteo", model = "test_model",
      issue_time = c(old_issue, new_issue), valid_time = valid,
      lead_time = as.difftime(c(48, 24), units = "hours"),
      member = NA_integer_, stat = NA_character_,
      variable = "temperature_2m", value = c(10, 30)
    )
    store_write_forecast(root, new_forecast(fc))
    out <- met_wide(site,
                    window = list(from = valid - 3600, to = valid + 3600),
                    kind = "forecast", variables = "temperature_2m")
    # the pooled mean (20) would mix the stale issuance into the prediction
    expect_equal(as.numeric(out$temperature_2m), 30)
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
