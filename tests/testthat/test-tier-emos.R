# Plan 12 — emos tier: crch heteroscedastic regression per lead bucket, and the
# refusal to train on lead_time = NA (Historical-Forecast proxy) rows.

describe("crch fit per lead bucket", {
  it("yields a predictive mean + spread and improves CRPS over raw", {
    skip_if_not_installed("crch")
    skip_if_not_installed("scoringRules")
    pairs <- forecast_obs_pairs(n = 400, bias_fun = function(doy, lead) 2,
                                lead_hours = 24)
    fit <- fit_emos(pairs, lead_bucket = "d1")
    pred <- apply_emos(fit, pairs)
    expect_true(all(c("mean", "sd") %in% names(pred)))
    crps_emos <- mean(scoringRules::crps_norm(pairs$observation,
                                              pred$mean, pred$sd))
    crps_raw <- mean(abs(pairs$forecast - pairs$observation))
    expect_lt(crps_emos, crps_raw)
  })
})

describe("lead-NA rows cannot contaminate lead-aware training", {
  it("refuses to fit on Historical-Forecast lead_time = NA rows", {
    pairs <- forecast_obs_pairs(n = 100)
    pairs$lead_time <- as.difftime(rep(NA_real_, nrow(pairs)), units = "hours")
    expect_error(
      fit_emos(pairs, lead_bucket = "d1"),
      class = "meteoTidy_error_lead_unresolved"
    )
  })
})
