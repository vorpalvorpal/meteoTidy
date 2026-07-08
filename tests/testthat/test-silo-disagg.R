# Plan 10 — SILO daily→hourly disaggregation (the donor ladder's last rung).

describe("disaggregate_silo() conservation", {
  it("disaggregated hourly rain sums exactly to the daily total", {
    daily <- tibble::tibble(
      site_id = "test",
      datetime_utc = as.POSIXct("2026-01-15 00:00", tz = "UTC"),
      variable = "precipitation", value = 12.0,
      source = "silo", method = "measured", qc_flag = "ok"
    )
    # a diurnal shape from a sub-daily reference (need not integrate to 1 itself)
    shape <- rep(1, 24)
    shape[14:17] <- 4 # afternoon-weighted
    hourly <- disaggregate_silo(daily, shape = shape)
    expect_equal(sum(hourly$value), 12.0, tolerance = 1e-9)
    expect_true(all(hourly$method == "disaggregated"))
    expect_true("shape_source" %in% names(hourly) ||
                  !is.null(attr(hourly, "shape_source")))
  })

  it("disaggregates an all-dry day to all-dry hours (no invented drizzle)", {
    daily <- tibble::tibble(
      site_id = "test",
      datetime_utc = as.POSIXct("2026-01-16 00:00", tz = "UTC"),
      variable = "precipitation", value = 0.0,
      source = "silo", method = "measured", qc_flag = "ok"
    )
    hourly <- disaggregate_silo(daily, shape = rep(1, 24))
    expect_true(all(hourly$value == 0))
  })

  it("reproduces the daily min/max for temperature", {
    daily <- tibble::tibble(
      site_id = "test",
      datetime_utc = as.POSIXct("2026-01-17 00:00", tz = "UTC"),
      variable = c("temperature_2m_min", "temperature_2m_max"),
      value = c(12.0, 28.0),
      source = "silo", method = "measured", qc_flag = "ok"
    )
    shape <- (sin(seq(0, 2 * pi, length.out = 24)) + 1) / 2 # 0..1 diurnal
    hourly <- disaggregate_silo(daily, shape = shape)
    temp <- hourly$value[hourly$variable == "temperature_2m"]
    expect_equal(min(temp), 12.0, tolerance = 1e-6)
    expect_equal(max(temp), 28.0, tolerance = 1e-6)
  })
})
