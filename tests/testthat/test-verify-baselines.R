# Plan 13 — persistence / climatology / raw-model baselines.

describe("persistence baseline", {
  it("carries the last observation forward", {
    obs <- c(10, 11, 12, 13)
    pers <- baseline_persistence(obs)
    # prediction for step t is obs[t-1]
    expect_equal(pers[-1], obs[-length(obs)])
  })
})

describe("climatology baseline", {
  it("uses the history_daily seasonal distribution for the target day-of-year", {
    hist <- tibble::tibble(
      datetime_utc = seq(as.POSIXct("2020-01-01", tz = "UTC"),
                         by = "day", length.out = 366 * 3),
      variable = "temperature_2m",
      # cos (not sin) so the sinusoid peaks at day-of-year 1 (Jan 1), matching
      # the southern-hemisphere-summer comment below and the target date;
      # `sin()` here would peak around day-of-year ~91 (early April), making
      # `clim$mean > 20` unreachable at any window centred on mid-January.
      value = 15 + 10 * cos(2 * pi *
                              as.integer(format(seq(as.POSIXct("2020-01-01", tz = "UTC"),
                                                    by = "day", length.out = 366 * 3), "%j")) /
                              365.25)
    )
    clim <- baseline_climatology(hist, target = as.POSIXct("2026-01-15",
                                                           tz = "UTC"),
                                 variable = "temperature_2m")
    # mid-January in the southern hemisphere is warm; near the summer peak
    expect_true(clim$mean > 20)
  })
})

describe("climatology can beat the raw model at long lead (the review point)", {
  it("shows climatology lower-error than a low-skill long-lead forecast", {
    withr::local_seed(11)
    truth <- rnorm(300, 15, 5)
    clim_pred <- rep(15, 300)                    # the climatological mean
    raw_long_lead <- rnorm(300, 15, 8)           # noisy, no skill at long lead
    err_clim <- mean(abs(clim_pred - truth))
    err_raw <- mean(abs(raw_long_lead - truth))
    expect_lt(err_clim, err_raw)                 # "correction helps" must beat this
  })
})
