# Plan 04 — the adapter contract.

describe("check_fetch_result()", {
  it("accepts a valid canonical table", {
    adapter <- source_rest("aws", "https://x/{site}", make_rest_mapping())
    ok <- new_obs(make_obs(source = "aws"))
    expect_no_error(
      check_fetch_result(ok, adapter, variables = "temperature_2m")
    )
  })

  it("rejects a non-uniform source, an unrequested variable, or a non-canonical table", {
    adapter <- source_rest("aws", "https://x/{site}", make_rest_mapping())

    mixed_source <- new_obs(rbind(make_obs(source = "aws"),
                                  make_obs(source = "other")))
    expect_error(check_fetch_result(mixed_source, adapter, "temperature_2m"),
                 class = "meteoTidy_error_source_not_uniform")

    extra_var <- new_obs(rbind(make_obs(source = "aws"),
                               make_obs(source = "aws", variable = "cape")))
    expect_error(check_fetch_result(extra_var, adapter, "temperature_2m"),
                 class = "meteoTidy_error_unrequested_variable")

    expect_error(check_fetch_result(tibble::tibble(x = 1), adapter, "temperature_2m"))
  })
})

describe("fetch_forecast() default", {
  it("aborts no_forecast_support on a non-forecast adapter", {
    adapter <- source_file("logger", "*.csv", make_csv_mapping())
    site <- make_test_site()
    win <- list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                to = as.POSIXct("2026-01-02", tz = "UTC"))
    expect_error(fetch_forecast(adapter, site, "temperature_2m", win),
                 class = "meteoTidy_error_no_forecast_support")
  })
})

describe("a user-written adapter", {
  it("passes the contract if it returns canonical rows (third-party extensibility)", {
    # A trivial hand-written adapter proving §5 "contract is user-implementable".
    toy <- new_generic_toy_adapter <- source_rest(
      "toy", "https://x/{site}", make_rest_mapping()
    )
    rows <- new_obs(make_obs(source = "toy"))
    expect_no_error(check_fetch_result(rows, toy, "temperature_2m"))
  })
})
