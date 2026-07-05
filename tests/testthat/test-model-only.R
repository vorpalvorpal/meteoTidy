# Plan 12 — model-only experiments: all opt-in, default-off, provenance-marked.

describe("profile_rescale is off by default", {
  it("leaves model-only winds raw with tier 'raw' unless explicitly enabled", {
    upper <- make_obs(n = 3, variable = "wind_speed_80m", value = c(8, 9, 10),
                      source = "openmeteo")
    out <- model_only_correct(upper, corrected_10m = c(4, 4.5, 5),
                              raw_10m = c(4, 4.5, 5))   # profile_rescale default off
    expect_equal(out$value, c(8, 9, 10))               # unchanged
    expect_true(all(out$tier == "raw"))
  })
})

describe("profile_rescale when enabled", {
  it("damps the rescale with height (180 m rescaled less than 80 m)", {
    raw10 <- 4; corr10 <- 6                             # +50% at 10 m
    r80  <- profile_rescale(raw = 10, height = 80,
                            corrected_10m = corr10, raw_10m = raw10)
    r180 <- profile_rescale(raw = 10, height = 180,
                            corrected_10m = corr10, raw_10m = raw10)
    # both increase, but the 180 m adjustment is damped relative to 80 m
    frac80  <- (r80 - 10) / 10
    frac180 <- (r180 - 10) / 10
    expect_gt(frac80, 0)
    expect_lt(frac180, frac80)
  })

  it("caps the rescale and suppresses it under stable stratification", {
    # a huge implied ratio must be capped, not applied unbounded
    capped <- profile_rescale(raw = 10, height = 80,
                              corrected_10m = 40, raw_10m = 1)   # ×40 implied
    expect_lt(capped, 10 * 5)                           # capped well below ×40

    # stable stratification (night + low corrected 10 m wind) → suppressed (raw)
    suppressed <- profile_rescale(raw = 10, height = 80,
                                  corrected_10m = 0.3, raw_10m = 0.2,
                                  stable = TRUE)
    expect_equal(suppressed, 10)
  })
})

describe("diagnostic BLH is served alongside the raw model BLH", {
  it("returns both the raw and the recomputed BLH, not a replacement", {
    out <- diagnostic_blh(raw_blh = 500,
                          corrected_surface = list(temperature_2m = 20,
                                                   wind_speed_10m = 5))
    expect_true(all(c("raw_blh", "diagnostic_blh") %in% names(out)))
    expect_equal(out$raw_blh, 500)
  })
})

describe("radiation re-split without a pyranometer", {
  it("returns raw model values with tier 'raw'", {
    out <- radiation_resplit(direct = 600, diffuse = 200, has_pyranometer = FALSE)
    expect_equal(out$direct, 600)
    expect_equal(out$diffuse, 200)
    expect_equal(out$tier, "raw")
  })
})
