# Plan 09 — the individual QC rules (range/step/persistence) + registry dispatch.
# Every rule may only DOWNGRADE qc_flag (ok→suspect→fail), never upgrade.

describe("range rule", {
  it("fails a value outside the dictionary [min, max] and leaves clean data ok", {
    obs <- qc_series("relative_humidity_2m", c(45, 150, 55))  # 150% impossible
    out <- qc_range(obs, dict = met_variables())
    expect_equal(out$qc_flag, c("ok", "fail", "ok"))
  })

  it("applies to every statistical class (range is universal)", {
    reg <- qc_registry()
    range_rule <- reg[["range"]]
    expect_setequal(range_rule$applies_to, STAT_CLASS_LEVELS)
  })
})

describe("step rule", {
  it("suspects an implausible jump between consecutive samples", {
    obs <- qc_series("temperature_2m", c(15, 15.2, 40, 15.4))  # +25°C in 1h
    out <- qc_step(obs, dict = met_variables())
    expect_equal(out$qc_flag[3], "suspect")
    expect_equal(out$qc_flag[c(1, 2)], c("ok", "ok"))
  })

  it("wraps for circular variables: 350°→10° is a 20° step (clean)", {
    obs <- qc_series("wind_direction_10m", c(350, 10, 20))
    out <- qc_step(obs, dict = met_variables())
    # the angular difference is 20°/10°, NOT the raw 340° subtraction
    expect_true(all(out$qc_flag == "ok"))
  })

  it("still catches a genuine large circular jump", {
    obs <- qc_series("wind_direction_10m", c(10, 190, 15))  # ~180° reversal
    out <- qc_step(obs, dict = met_variables())
    expect_equal(out$qc_flag[2], "suspect")
  })
})

describe("persistence (flat-line) rule", {
  it("suspects a sensor stuck longer than the per-variable window", {
    obs <- qc_series("temperature_2m", rep(15.0, 8))  # 8h identical → stuck
    out <- qc_persistence(obs, dict = met_variables())
    expect_true(any(out$qc_flag == "suspect"))
  })

  it("ignores legitimately-flat zero rain (intermittent class)", {
    obs <- qc_series("precipitation", rep(0.0, 8))
    out <- qc_persistence(obs, dict = met_variables())
    expect_true(all(out$qc_flag == "ok"))     # dry spells are not stuck sensors
  })
})

describe("rule dispatch by statistical class", {
  it("does not route a model_only variable to the spatial rule (no site truth)", {
    # SCOPING §7.3: model-only variables have no site measurement to buddy-check.
    rules <- qc_rules_for_variable("boundary_layer_height")
    expect_false("spatial" %in% rules)
    # but a site-measurable variable does get the spatial rule
    expect_true("spatial" %in% qc_rules_for_variable("temperature_2m"))
  })
})
