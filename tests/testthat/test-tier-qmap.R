# Plan 12 — qmap tier: empirical quantile mapping + cross-season pooling +
# explicit constant-shift tail policy.

describe("basic distributional correction", {
  it("corrects a known distributional shift within tolerance", {
    withr::local_seed(1)
    obs <- rnorm(1000, 15, 3)
    fc <- rnorm(1000, 18, 5)                       # shifted + wider
    pairs <- tibble::tibble(forecast = fc, observation = obs,
                            season = rep(c("DJF", "MAM", "JJA", "SON"),
                                         length.out = 1000))
    coeffs <- fit_qmap(pairs)
    corrected <- apply_qmap(coeffs, pairs)
    expect_equal(median(corrected), median(obs), tolerance = 0.5)
    expect_equal(sd(corrected), sd(obs), tolerance = 0.6)
  })
})

describe("cross-season pooling", {
  it("still corrects a season that has no training data (via the pooled base)", {
    withr::local_seed(2)
    # training covers only DJF/MAM/JJA; SON is untrained
    pairs <- tibble::tibble(
      forecast = rnorm(600, 18, 4), observation = rnorm(600, 15, 3),
      season = rep(c("DJF", "MAM", "JJA"), length.out = 600)
    )
    coeffs <- fit_qmap(pairs, by = "season")
    son <- tibble::tibble(forecast = rnorm(50, 18, 4), season = "SON")
    corrected <- apply_qmap(coeffs, son)
    expect_false(anyNA(corrected))                 # pooled base fills the gap
    expect_true(all(is.finite(corrected)))
  })
})

describe("explicit tail policy", {
  it("corrects beyond the training max by a bounded constant shift, never NA", {
    withr::local_seed(3)
    pairs <- tibble::tibble(forecast = rnorm(500, 18, 4),
                            observation = rnorm(500, 15, 3),
                            season = "DJF")
    coeffs <- fit_qmap(pairs)
    extreme <- tibble::tibble(forecast = max(pairs$forecast) + 10, season = "DJF")
    out <- apply_qmap(coeffs, extreme)
    expect_false(is.na(out))
    expect_true(is.finite(out))
    # the shift is the one at the nearest trained quantile (bounded extrapolation)
    near_max <- apply_qmap(coeffs,
                           tibble::tibble(forecast = max(pairs$forecast),
                                          season = "DJF"))
    shift_extreme <- out - (max(pairs$forecast) + 10)
    shift_near <- near_max - max(pairs$forecast)
    expect_equal(shift_extreme, shift_near, tolerance = 1e-6)
  })
})
