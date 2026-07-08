# Plan 18 Part B — deterministic derivation fill tier (physics before donors).
#
# `fill_derive()` computes a coupled variable from co-observed inputs at the
# same site + timestamp, exactly, via the existing Magnus helpers
# (R/fill-treatments.R). Every input must be QC-clean (`qc_flag == "ok"`,
# non-NA `value`); otherwise the gap is left for the donor/model tiers.

describe("fill_derive() computes coupled variables from co-observed inputs", {
  it("computes RH from co-observed temperature + dewpoint, exactly", {
    temp <- make_obs(n = 4, variable = "temperature_2m", value = c(20, 21, 22, 23))
    dew <- make_obs(n = 4, variable = "dewpoint_2m", value = c(10, 11, 12, 13))
    rh <- series_with_gap("relative_humidity_2m", c(60, 55, NA, 50), gap_at = 3)
    obs <- vctrs::vec_rbind(temp, dew, rh)

    out <- fill_derive(obs, dict = met_variables())
    filled <- out[out$variable == "relative_humidity_2m", ]
    filled <- filled[order(filled$datetime_utc), ]

    expect_false(anyNA(filled$value))
    expect_equal(filled$value[3], .rh_from_dewpoint(22, 12))
    expect_equal(filled$method[3], "derived")
    expect_equal(filled$qc_flag[3], "ok")
    # keeps the site's own source (never a donor's)
    expect_equal(filled$source[3], "test_src")
    # untouched rows keep their measured provenance
    expect_equal(filled$method[c(1, 2, 4)], rep("measured", 3))
  })

  it("computes dewpoint from co-observed temperature + relative humidity", {
    temp <- make_obs(n = 4, variable = "temperature_2m", value = c(20, 21, 22, 23))
    rh <- make_obs(n = 4, variable = "relative_humidity_2m", value = c(55, 60, 65, 70))
    dew <- series_with_gap("dewpoint_2m", c(9, 10, NA, 12), gap_at = 3)
    obs <- vctrs::vec_rbind(temp, rh, dew)

    out <- fill_derive(obs, dict = met_variables())
    filled <- out[out$variable == "dewpoint_2m", ]
    filled <- filled[order(filled$datetime_utc), ]

    expect_false(anyNA(filled$value))
    expect_equal(filled$value[3], .dewpoint_from_rh(22, 65))
    expect_equal(filled$method[3], "derived")
    expect_equal(filled$qc_flag[3], "ok")
  })
})

describe("fill_derive() leaves a gap alone when an input is absent", {
  it("does not derive RH when the dewpoint input is itself missing", {
    temp <- make_obs(n = 4, variable = "temperature_2m", value = c(20, 21, 22, 23))
    # dewpoint is a gap at exactly the RH gap timestamp (position 3)
    dew <- series_with_gap("dewpoint_2m", c(10, 11, NA, 13), gap_at = 3)
    rh <- series_with_gap("relative_humidity_2m", c(60, 55, NA, 50), gap_at = 3)
    obs <- vctrs::vec_rbind(temp, dew, rh)

    out <- fill_derive(obs, dict = met_variables())
    filled <- out[out$variable == "relative_humidity_2m", ]
    filled <- filled[order(filled$datetime_utc), ]

    expect_true(is.na(filled$value[3])) # stays a gap
    expect_equal(filled$qc_flag[3], "missing") # untouched by derive
    expect_equal(filled$method[3], "measured")
  })

  it("does not derive from a suspect/failed input (qc_flag != ok)", {
    temp <- make_obs(n = 4, variable = "temperature_2m", value = c(20, 21, 22, 23))
    dew <- make_obs(n = 4, variable = "dewpoint_2m", value = c(10, 11, 12, 13))
    # the dewpoint at the gap timestamp is present but QC-failed
    dew$qc_flag[3] <- "fail"
    rh <- series_with_gap("relative_humidity_2m", c(60, 55, NA, 50), gap_at = 3)
    obs <- vctrs::vec_rbind(temp, dew, rh)

    out <- fill_derive(obs, dict = met_variables())
    filled <- out[out$variable == "relative_humidity_2m", ]
    filled <- filled[order(filled$datetime_utc), ]

    expect_true(is.na(filled$value[3]))
    expect_equal(filled$qc_flag[3], "missing")
  })
})

describe("derived values survive the QC/consistency invariants", {
  it("keeps derived relative humidity within [0, 100] even when supersaturated", {
    # dewpoint above temperature would give RH > 100 before clamping
    temp <- make_obs(n = 3, variable = "temperature_2m", value = c(15, 15, 15))
    dew <- make_obs(n = 3, variable = "dewpoint_2m", value = c(20, 20, 20))
    rh <- series_with_gap("relative_humidity_2m", c(90, NA, 95), gap_at = 2)
    obs <- vctrs::vec_rbind(temp, dew, rh)

    out <- fill_derive(obs, dict = met_variables())
    filled <- out[out$variable == "relative_humidity_2m", ]
    filled <- filled[order(filled$datetime_utc), ]

    expect_gte(filled$value[2], 0)
    expect_lte(filled$value[2], 100)
  })
})
