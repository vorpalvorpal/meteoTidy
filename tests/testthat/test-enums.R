# Plan 01 — closed enumerations.

describe("enum validators", {
  it("accept every legal level and reject an illegal one with its class", {
    expect_silent(validate_qc_flag(c("ok", "suspect", "fail", "missing")))
    expect_error(validate_qc_flag("estimated"), class = "meteoTidy_error_invalid_qc_flag")

    expect_silent(validate_method(METHOD_LEVELS))
    expect_error(validate_method("guessed"), class = "meteoTidy_error_invalid_method")

    expect_silent(validate_tier(TIER_LEVELS))
    expect_error(validate_tier("magic"), class = "meteoTidy_error_invalid_tier")

    expect_silent(validate_statistical_class(STAT_CLASS_LEVELS))
    expect_error(validate_statistical_class("nonlinear"),
                 class = "meteoTidy_error_invalid_statistical_class")

    expect_silent(validate_measurability_class(MEASURABILITY_LEVELS))
    expect_error(validate_measurability_class("guessable"),
                 class = "meteoTidy_error_invalid_measurability_class")
  })

  it("reports the offending values in the error", {
    expect_error(validate_qc_flag(c("ok", "weird")), regexp = "weird")
  })
})

describe("tier ordering", {
  it("is total and matches TIER_LEVELS order, with raw lowest and emos highest", {
    ranks <- vapply(TIER_LEVELS, tier_rank, integer(1))
    expect_identical(unname(ranks), seq_along(TIER_LEVELS))
    expect_lt(tier_rank("raw"), tier_rank("emos"))
    expect_lt(tier_rank("mean_bias"), tier_rank("qmap"))
  })
})

describe("the post-review qc_flag enum", {
  it("does not contain 'estimated' (production method is not a QC state)", {
    expect_false("estimated" %in% QC_FLAG_LEVELS)
    expect_setequal(QC_FLAG_LEVELS, c("ok", "suspect", "fail", "missing"))
  })
})
