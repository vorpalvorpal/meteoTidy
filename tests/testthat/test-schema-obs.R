# Plan 01 — canonical observation table.

describe("new_obs()", {
  it("stamps and validates a canonical observation tibble", {
    expect_canonical_obs(new_obs(make_obs()))
  })

  it("rejects an in-range violation only when the row is flagged ok", {
    bad <- make_obs(n = 1, variable = "relative_humidity_2m", value = 150)
    expect_error(new_obs(bad), class = "meteoTidy_error_range_violation")
    # the same out-of-range value is acceptable if honestly flagged fail
    ok <- make_obs(n = 1, variable = "relative_humidity_2m", value = 150,
                   qc_flag = "fail")
    expect_no_error(new_obs(ok))
  })

  it("aborts a specific class for each malformed input", {
    expect_error(new_obs(make_obs(variable = "not_a_var")),
                 class = "meteoTidy_error_unknown_variable")
    expect_error(new_obs(make_obs(qc_flag = "weird")),
                 class = "meteoTidy_error_invalid_qc_flag")

    non_utc <- make_obs()
    attr(non_utc$datetime_utc, "tzone") <- "Australia/Sydney"
    expect_error(new_obs(non_utc), class = "meteoTidy_error_non_utc_time")

    dup <- rbind(make_obs(n = 1), make_obs(n = 1))
    expect_error(new_obs(dup), class = "meteoTidy_error_duplicate_key")
  })
})

describe("widen_obs() / narrow_obs()", {
  it("is a round-trip identity on (site, time, variable, value)", {
    obs <- rbind(
      make_obs(n = 3, variable = "temperature_2m"),
      make_obs(n = 3, variable = "wind_speed_10m", value = c(1, 2, 3))
    )
    round <- narrow_obs(widen_obs(obs))
    key <- c("site_id", "datetime_utc", "variable", "value")
    a <- obs[key][order(obs$variable, obs$datetime_utc), ]
    b <- round[key][order(round$variable, round$datetime_utc), ]
    expect_equal(as.data.frame(b), as.data.frame(a), ignore_attr = TRUE)
  })

  it("emits an absent variable as an all-NA column (stable §3.1 shape)", {
    wide <- widen_obs(make_obs(variable = "temperature_2m"),
                      variables = c("temperature_2m", "cape"))
    expect_true("cape" %in% names(wide))
    expect_true(all(is.na(wide$cape)))
  })

  it("keeps the internal time column named datetime_utc (Plan 15 renames it)", {
    wide <- widen_obs(make_obs())
    expect_true("datetime_utc" %in% names(wide))
    expect_false("time" %in% names(wide))
  })
})
