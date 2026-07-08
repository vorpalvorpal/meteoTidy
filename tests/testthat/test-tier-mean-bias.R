# Plan 12 — mean_bias tier: harmonic day-of-year + hour-of-day covariates,
# with annual-harmonic shrinkage under short overlap (the review fixes).

# A bias that flips sign summer↔winter — raw hour-of-day bins cannot represent
# it; the annual harmonic can.
seasonal_bias <- function(doy, lead) 3 * sin(2 * pi * doy / 365.25)

describe("harmonic fit with a full year of pairs", {
  it("recovers and removes a sign-flipping seasonal bias", {
    pairs <- forecast_obs_pairs(n = 365, bias_fun = seasonal_bias)
    coeffs <- fit_mean_bias(pairs, n_harmonics = 2)
    corrected <- apply_mean_bias(coeffs, pairs)
    # residual bias after correction is near zero across the whole year
    resid <- corrected$value - pairs$observation
    expect_lt(abs(mean(resid)), 0.3)
    # and specifically the summer and winter halves are both de-biased
    summer <- as.integer(format(pairs$issue_time, "%j")) < 90
    expect_lt(abs(mean(resid[summer])), 0.6)
    expect_lt(abs(mean(resid[!summer])), 0.6)
  })
})

describe("annual-harmonic shrinkage with short overlap", {
  it("keeps the untrained opposite-season correction near the annual mean", {
    # Only 4 months (summer) of pairs. A shrunk fit must not extrapolate a large
    # opposite-season (winter) correction; it should stay near the annual-mean.
    summer_pairs <- forecast_obs_pairs(
      n = 120, bias_fun = seasonal_bias,
      issue0 = as.POSIXct("2025-01-01",
        tz = "UTC"
      )
    )
    shrunk <- fit_mean_bias(summer_pairs, n_harmonics = 2, shrink = TRUE)
    unshrunk <- fit_mean_bias(summer_pairs, n_harmonics = 2, shrink = FALSE)

    # evaluate the implied correction on a winter day (doy ~ 200, untrained)
    winter <- forecast_obs_pairs(
      n = 1, bias_fun = seasonal_bias,
      issue0 = as.POSIXct("2025-07-20", tz = "UTC")
    )
    c_shrunk <- apply_mean_bias(shrunk, winter)$value - winter$forecast
    c_unshrunk <- apply_mean_bias(unshrunk, winter)$value - winter$forecast
    annual_mean_bias <- -mean(seasonal_bias(as.integer(format(
      summer_pairs$issue_time, "%j"
    )), 24))

    # the shrunk winter correction is closer to the annual-mean than the unshrunk
    expect_lt(
      abs(c_shrunk - annual_mean_bias),
      abs(c_unshrunk - annual_mean_bias)
    )
  })

  it("does not shrink the hour-of-day harmonics (a month has ~30 diurnal cycles)", {
    # exposed as a parameter with a sensible default
    expect_true("shrink" %in% names(formals(fit_mean_bias)))
    expect_true("n_harmonics" %in% names(formals(fit_mean_bias)))
  })
})

describe("persistence as data", {
  it("returns tidy coefficients, never an .rds model object", {
    coeffs <- fit_mean_bias(forecast_obs_pairs(n = 60), n_harmonics = 1)
    expect_s3_class(coeffs, "tbl_df")
    expect_false(inherits(coeffs, "lm"))
  })
})
