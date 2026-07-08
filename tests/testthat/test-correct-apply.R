# Plan 11 — correct_apply() lifecycle: day-0 physical, model-only raw, and the
# forecast/record shrinkage-routing distinction.

describe("day-0 path (no calibration)", {
  it("applies only physical adjustments and stamps tier 'physical'", {
    root <- local_store() # empty calib store → day 0
    site <- make_test_site()
    store_write_obs(root, new_obs(make_obs(
      n = 3, variable = "temperature_2m",
      value = c(20, 21, 22),
      source = "openmeteo"
    )))
    out <- correct_apply(root, site, "openmeteo",
      target = "record",
      variables = "temperature_2m",
      now = as.POSIXct("2026-01-02", tz = "UTC")
    )
    expect_true(all(out$tier == "physical"))
  })
})

describe("model-only variables", {
  it("pass through raw with tier 'raw'", {
    root <- local_store()
    site <- make_test_site()
    store_write_obs(root, new_obs(make_obs(
      n = 3,
      variable = "boundary_layer_height",
      value = c(500, 600, 700),
      source = "openmeteo"
    )))
    out <- correct_apply(root, site, "openmeteo",
      target = "record",
      variables = "boundary_layer_height",
      now = as.POSIXct("2026-01-02", tz = "UTC")
    )
    expect_true(all(out$tier == "raw"))
    expect_equal(out$value, c(500, 600, 700)) # unchanged
  })
})

describe("forecast vs record routing (respects the Plan 10/12 distinction)", {
  it("routes target='forecast' through the shrinkage hook but not target='record'", {
    root <- local_store()
    site <- make_test_site()
    called <- new.env()
    called$forecast <- 0L
    called$record <- 0L
    testthat::local_mocked_bindings(
      shrink_to_climatology = function(corrected, ...) {
        called$forecast <- called$forecast + 1L
        corrected
      }
    )
    store_write_obs(root, new_obs(make_obs(
      n = 2, variable = "temperature_2m",
      value = c(20, 21), source = "openmeteo"
    )))
    correct_apply(root, site, "openmeteo",
      target = "record",
      variables = "temperature_2m",
      now = as.POSIXct("2026-01-02", tz = "UTC")
    )
    expect_equal(called$forecast, 0L) # records never shrink

    correct_apply(root, site, "openmeteo",
      target = "forecast",
      variables = "temperature_2m",
      now = as.POSIXct("2026-01-02", tz = "UTC")
    )
    expect_gt(called$forecast, 0L) # forecasts route via shrinkage
  })
})
