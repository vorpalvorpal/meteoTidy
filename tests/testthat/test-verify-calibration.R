# Plan 13 — calibration diagnostics: rank/PIT histogram, spread-error, Brier.

describe("rank / PIT histogram", {
  it("is ~flat for a calibrated ensemble and U-shaped when under-dispersed", {
    withr::local_seed(21)
    n <- 2000
    m <- 20
    truth <- rnorm(n)
    # calibrated: members drawn from the same predictive dist as truth
    ens_cal <- matrix(rnorm(n * m), nrow = n)
    rh_cal <- rank_histogram(ens_cal, truth)
    # under-dispersed: members too tight around 0 → truth falls in the tails
    ens_ud <- matrix(rnorm(n * m, sd = 0.2), nrow = n)
    rh_ud <- rank_histogram(ens_ud, truth)

    # flatness statistic (e.g. reduced chi-square vs uniform): calibrated ≈ 1,
    # under-dispersed ≫ 1 with mass piled in the end bins (U-shape).
    expect_lt(histogram_flatness(rh_cal), histogram_flatness(rh_ud))
    ends <- c(rh_ud[1], rh_ud[length(rh_ud)])
    expect_gt(mean(ends), mean(rh_ud[-c(1, length(rh_ud))])) # U-shape
  })
})

describe("spread-error ratio", {
  it("is ≈ 1 for a well-calibrated ensemble", {
    withr::local_seed(22)
    n <- 3000
    m <- 30
    truth <- rnorm(n)
    ens <- matrix(rnorm(n * m), nrow = n)
    ser <- spread_error_ratio(ens, truth)
    expect_equal(ser, 1, tolerance = 0.15)
  })
})

describe("Brier score + reliability for rain occurrence", {
  it("computes the Brier score correctly for a PoP fixture", {
    prob <- c(0.0, 0.5, 1.0, 0.5)
    outcome <- c(0, 1, 1, 0) # rained: 0 or 1
    bs <- brier_score(prob, outcome)
    expect_equal(bs, mean((prob - outcome)^2))
  })

  it("returns a reliability table binning forecast probability vs frequency", {
    withr::local_seed(23)
    prob <- runif(1000)
    outcome <- rbinom(1000, 1, prob) # perfectly reliable by construction
    rel <- reliability_table(prob, outcome, bins = 5)
    expect_true(all(c("bin_mid", "observed_freq") %in% names(rel)))
    # observed frequency tracks the forecast probability (reliable)
    expect_lt(mean(abs(rel$bin_mid - rel$observed_freq), na.rm = TRUE), 0.15)
  })
})
