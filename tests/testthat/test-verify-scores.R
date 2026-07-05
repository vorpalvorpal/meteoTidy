# Plan 13 — deterministic + probabilistic scores and skill scores.

describe("deterministic scores", {
  it("MAE and RMSE match hand-computed values on a tiny fixture", {
    fc  <- c(1, 2, 3)
    obs <- c(1, 4, 3)                         # errors: 0, 2, 0
    s <- score_deterministic(fc, obs)
    expect_equal(s$mae, mean(c(0, 2, 0)))
    expect_equal(s$rmse, sqrt(mean(c(0, 4, 0))))
  })
})

describe("CRPS", {
  it("matches scoringRules on a tiny normal-predictive fixture", {
    skip_if_not_installed("scoringRules")
    obs <- c(0, 1)
    mu <- c(0, 0); sigma <- c(1, 1)
    got <- score_crps(obs, mu, sigma)
    expect_equal(got, scoringRules::crps_norm(obs, mu, sigma), tolerance = 1e-9)
  })

  it("accumulates cumulative quantities per member, not by summing percentiles", {
    # SCOPING §4: daily percentiles can't be validly summed. A 2-member, 2-day
    # fixture where per-member accumulation and percentile-summing disagree.
    members <- matrix(c(1, 9,     # day1: m1=1, m2=9
                        9, 1),    # day2: m1=9, m2=1
                      nrow = 2, byrow = TRUE)
    per_member <- cumulative_by_member(members)   # each member summed over days
    # both members total 10 → the correct cumulative band is degenerate at 10,
    # whereas summing daily p90 (=9+9=18) would be wrong.
    expect_equal(sort(per_member), c(10, 10))
    expect_false(18 %in% per_member)
  })
})

describe("skill score vs a baseline", {
  it("is 0 at parity, positive when better, negative when worse", {
    expect_equal(skill_score(score = 2, baseline = 2), 0)
    expect_gt(skill_score(score = 1, baseline = 2), 0)   # lower score = better
    expect_lt(skill_score(score = 3, baseline = 2), 0)
  })
})
