# Plan 01 — forecast archive + forecast_aux.

describe("new_forecast()", {
  it("validates a canonical forecast tibble", {
    expect_canonical_forecast(new_forecast(make_forecast()))
  })

  it("accepts deterministic, ensemble, and summary rows by the member/stat rule", {
    deterministic <- make_forecast(member = NA_integer_, stat = NA_character_)
    ensemble <- make_forecast(member = 3L, stat = NA_character_)
    summary <- make_forecast(member = NA_integer_, stat = "p90")
    expect_no_error(new_forecast(deterministic))
    expect_no_error(new_forecast(ensemble))
    expect_no_error(new_forecast(summary))
  })

  it("aborts when member and stat are both set", {
    conflict <- make_forecast(member = 3L, stat = "mean")
    expect_error(new_forecast(conflict),
                 class = "meteoTidy_error_member_stat_conflict")
  })

  it("aborts when lead_time disagrees with valid_time - issue_time", {
    fc <- make_forecast(n = 1)
    fc$lead_time <- as.difftime(999, units = "hours")
    expect_error(new_forecast(fc), class = "meteoTidy_error_lead_inconsistent")
  })

  it("represents a per-row underlying model (seasonal splice)", {
    fc <- make_forecast(n = 4)
    fc$model <- c("ec46", "ec46", "seas5", "seas5")
    expect_no_error(new_forecast(fc))
    expect_setequal(unique(new_forecast(fc)$model), c("ec46", "seas5"))
  })
})

describe("new_forecast_aux()", {
  it("accepts précis-text rows and rejects numeric-only misuse", {
    expect_canonical_forecast_aux(new_forecast_aux(make_forecast_aux()))
    numeric_misuse <- make_forecast_aux()
    numeric_misuse$value_text <- 42  # a number where text is required
    expect_error(new_forecast_aux(numeric_misuse),
                 class = "meteoTidy_error_schema_bad_type")
  })
})
