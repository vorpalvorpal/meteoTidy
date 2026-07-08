# Plan 11 — tier_select(): the data-availability gate AND the skill gate.
# BOTH must pass to use a higher tier (the review's skill-gate fix).

describe("data-availability gate", {
  it("returns the right tier for each overlap band", {
    pass <- skill_verdict(promote = TRUE)
    expect_equal(
      tier_select(make_test_site(), "openmeteo", "temperature_2m",
        training_summary = training_summary(
          overlap_months = 0,
          n_pairs = 0
        ),
        skill_verdict = pass
      ),
      "physical"
    )
    expect_equal(
      tier_select(make_test_site(), "openmeteo", "temperature_2m",
        training_summary = training_summary(overlap_months = 3),
        skill_verdict = pass
      ),
      "mean_bias"
    )
    expect_equal(
      tier_select(make_test_site(), "openmeteo", "temperature_2m",
        training_summary = training_summary(overlap_months = 12),
        skill_verdict = pass
      ),
      "qmap"
    )
    expect_equal(
      tier_select(make_test_site(), "openmeteo", "temperature_2m",
        training_summary = training_summary(
          overlap_months = 30,
          has_archive = TRUE
        ),
        skill_verdict = pass
      ),
      "emos"
    )
  })
})

describe("skill gate blocks promotion (the review fix)", {
  it("stays at the lower tier when data allows but skill says no improvement", {
    no_gain <- skill_verdict(promote = FALSE)
    tier <- tier_select(make_test_site(), "openmeteo", "temperature_2m",
      training_summary = training_summary(
        overlap_months = 30,
        has_archive = TRUE
      ),
      skill_verdict = no_gain
    )
    # data volume alone must NOT promote to emos
    expect_true(tier_rank(tier) < tier_rank("emos"))
  })

  it("promotes when the skill verdict passes", {
    yes <- skill_verdict(promote = TRUE)
    tier <- tier_select(make_test_site(), "openmeteo", "temperature_2m",
      training_summary = training_summary(
        overlap_months = 30,
        has_archive = TRUE
      ),
      skill_verdict = yes
    )
    expect_equal(tier, "emos")
  })
})

describe("Open-Meteo daily-lead day-0 path", {
  it("can reach emos from day 0 via Previous Runs, recording pseudo-truth", {
    # SCOPING §7.2: daily-lead pairs exist from day 0; trained vs history_daily.
    summ <- training_summary(
      overlap_months = 0, n_pairs = 700,
      lead_bucket = "d1", has_archive = TRUE,
      truth_source = "history_daily"
    )
    tier <- tier_select(make_test_site(), "openmeteo", "temperature_2m",
      lead_bucket = "d1", training_summary = summ,
      skill_verdict = skill_verdict(
        promote = TRUE,
        lead_bucket = "d1"
      )
    )
    expect_equal(tier, "emos")
  })
})

describe("tier enforcement in correct_apply()", {
  it("refuses a calibration whose manifest tier disagrees with the selection", {
    root <- local_store()
    site <- make_test_site()
    # write a qmap calibration, but the selected tier will be mean_bias
    calib_write(root, "test", "temperature_2m", "openmeteo", "qmap",
      tibble::tibble(source_q = 0.5, target_q = 0.5),
      list(
        train_start = as.POSIXct("2025-01-01", tz = "UTC"),
        train_end = as.POSIXct("2025-12-31", tz = "UTC"),
        n_pairs = 100
      ),
      now = as.POSIXct("2026-01-01", tz = "UTC")
    )
    expect_error(
      correct_apply(root, site, "openmeteo",
        target = "record",
        variables = "temperature_2m",
        now = as.POSIXct("2026-01-02", tz = "UTC"),
        force_tier = "mean_bias"
      ),
      class = "meteoTidy_error_tier_mismatch"
    )
  })
})
