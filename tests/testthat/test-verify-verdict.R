# Plan 13 — skill_verdict(): the promote/keep decision + shrinkage weights that
# Plans 11 and 12 consume.

describe("promote decision (feeds Plan 11 tier_select)", {
  it("is FALSE when the improvement does not survive the bootstrap", {
    scores <- tibble::tibble(
      variable = "temperature_2m", lead_bucket = "d1",
      candidate = 1.79, incumbent = 1.80
    ) # tiny gain
    boot <- tibble::tibble(
      variable = "temperature_2m", lead_bucket = "d1",
      significant = FALSE, ci_lower = -0.2, ci_upper = 0.1
    )
    v <- skill_verdict_compute(scores, boot)
    expect_false(v$promote[1])
  })

  it("is TRUE when a real out-of-sample improvement survives the bootstrap", {
    scores <- tibble::tibble(
      variable = "temperature_2m", lead_bucket = "d1",
      candidate = 1.2, incumbent = 1.8
    )
    boot <- tibble::tibble(
      variable = "temperature_2m", lead_bucket = "d1",
      significant = TRUE, ci_lower = 0.3, ci_upper = 0.7
    )
    v <- skill_verdict_compute(scores, boot)
    expect_true(v$promote[1])
  })
})

describe("shrink_weight (feeds Plan 12 shrinkage)", {
  it("is ~0 at/below climatology skill and near 1 at high skill", {
    low <- skill_verdict_compute(
      tibble::tibble(
        variable = "temperature_2m", lead_bucket = "d10",
        skill_vs_clim = -0.1
      ),
      tibble::tibble(
        variable = "temperature_2m", lead_bucket = "d10",
        significant = FALSE, ci_lower = -1, ci_upper = 1
      )
    )
    high <- skill_verdict_compute(
      tibble::tibble(
        variable = "temperature_2m", lead_bucket = "d1",
        skill_vs_clim = 0.9
      ),
      tibble::tibble(
        variable = "temperature_2m", lead_bucket = "d1",
        significant = TRUE, ci_lower = 0.5, ci_upper = 1
      )
    )
    expect_equal(low$shrink_weight[1], 0, tolerance = 1e-9)
    expect_gt(high$shrink_weight[1], 0.8)
  })
})

describe("consistency violation rate surfaced as a red flag", {
  it("carries the Plan 12 consistency-pass violation rate through the verdict", {
    scores <- tibble::tibble(
      variable = "temperature_2m", lead_bucket = "d1",
      candidate = 1.2, incumbent = 1.8,
      consistency_violation_rate = 0.04
    )
    boot <- tibble::tibble(
      variable = "temperature_2m", lead_bucket = "d1",
      significant = TRUE, ci_lower = 0.3, ci_upper = 0.7
    )
    v <- skill_verdict_compute(scores, boot)
    expect_equal(v$consistency_violation_rate[1], 0.04)
  })
})
