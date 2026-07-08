# Plan 10 — tier routing by gap length + variable class.

describe("micro tier (short gaps, smooth variables only)", {
  it("micro-fills a 2 h temperature gap with method 'imputed'", {
    obs <- series_with_gap("temperature_2m", c(15, NA, NA, 18), gap_at = 2:3)
    out <- fill_tier(obs, dict = met_variables())
    filled <- out[out$variable == "temperature_2m", ]
    expect_false(anyNA(filled$value))
    expect_true(all(filled$method[2:3] == "imputed"))
  })

  it("does NOT micro-interpolate a rain gap (routed to donor/model instead)", {
    obs <- series_with_gap("precipitation", c(0, NA, NA, 4), gap_at = 2:3)
    out <- fill_tier(obs, dict = met_variables())
    filled <- out[out$variable == "precipitation", ]
    # rain is intermittent: never linearly interpolated → method is not 'imputed'
    expect_false(any(filled$method[2:3] == "imputed"))
  })
})

describe("medium tier (donor)", {
  it("fills a 2-day gap from a transfer-corrected donor (method 'donor_fill')", {
    site <- make_test_site()
    gap <- series_with_gap("temperature_2m",
                           c(15, rep(NA, 47), 16),
                           gap_at = 2:48)   # ~2 days of hourly gap
    donor <- make_obs(n = 49, variable = "temperature_2m",
                      value = seq(17, 18, length.out = 49), source = "bom_obs")
    out <- fill_medium(gap, donors = list(bom = donor), site = site,
                       dict = met_variables())
    filled <- out[out$variable == "temperature_2m", ]
    expect_false(anyNA(filled$value))
    expect_true(all(filled$method[2:48] == "donor_fill"))
    expect_true(all(filled$source[2:48] == "bom_obs"))
  })
})

describe("macro tier (pre-installation → model)", {
  it("fills a pre-install span from the corrected model (method 'model_fill')", {
    gap <- series_with_gap("temperature_2m", rep(NA, 24), gap_at = 1:24)
    model <- make_obs(n = 24, variable = "temperature_2m",
                      value = seq(14, 20, length.out = 24), source = "openmeteo")
    out <- fill_macro(gap, model = model, dict = met_variables())
    filled <- out[out$variable == "temperature_2m", ]
    expect_false(anyNA(filled$value))
    expect_true(all(filled$method == "model_fill"))
  })
})

describe("model-only variables bypass the donor ladder", {
  it("fills a model-only gap with raw model, attempting no donor transfer", {
    called <- new.env()
    called$n <- 0L
    testthat::local_mocked_bindings(
      rank_donors = function(...) {
        called$n <- called$n + 1L
        list()
      }
    )
    gap <- series_with_gap("boundary_layer_height", c(500, NA, NA, 700),
                           gap_at = 2:3)
    model <- make_obs(n = 4, variable = "boundary_layer_height",
                      value = c(500, 550, 600, 700), source = "openmeteo")
    out <- fill_tier(gap, dict = met_variables(), model = model)
    filled <- out[out$variable == "boundary_layer_height", ]
    expect_true(all(filled$method == "model_fill"))
    expect_equal(called$n, 0L)                   # donor ladder never consulted
  })
})

describe("derive tier (exact physics, before donors)", {
  it("derives RH from co-observed T + dewpoint before consulting a donor", {
    site <- make_test_site()
    n <- 8
    temp_val <- seq(20, 27, length.out = n)
    dew_val <- seq(10, 17, length.out = n)
    temp <- make_obs(n = n, variable = "temperature_2m", value = temp_val)
    dew <- make_obs(n = n, variable = "dewpoint_2m", value = dew_val)
    # a >3 h RH gap (positions 3:7) so the micro tier is skipped
    rh <- series_with_gap("relative_humidity_2m",
                          c(60, 58, NA, NA, NA, NA, NA, 50), gap_at = 3:7)
    obs <- vctrs::vec_rbind(temp, dew, rh)
    donor <- make_obs(n = n, variable = "relative_humidity_2m",
                      value = rep(42, n), source = "bom_obs")

    out <- fill_tier(obs, dict = met_variables(),
                     donors = list(bom = donor), site = site)
    filled <- out[out$variable == "relative_humidity_2m", ]
    filled <- filled[order(filled$datetime_utc), ]

    expect_false(anyNA(filled$value))
    # derivation pre-empts the donor tier
    expect_true(all(filled$method[3:7] == "derived"))
    expect_true(all(filled$source[3:7] == "test_src")) # site's own source
    # exact physics, not the donor's constant 42
    expect_equal(filled$value[3:7], .rh_from_dewpoint(temp_val[3:7], dew_val[3:7]))
  })

  it("falls through to the donor tier when a derivation input is missing", {
    site <- make_test_site()
    n <- 8
    temp <- make_obs(n = n, variable = "temperature_2m",
                     value = seq(20, 27, length.out = n))
    # dewpoint is itself a gap across the whole RH gap, so RH is not derivable
    dew <- series_with_gap("dewpoint_2m",
                           c(10, 11, NA, NA, NA, NA, NA, 17), gap_at = 3:7)
    rh <- series_with_gap("relative_humidity_2m",
                          c(60, 58, NA, NA, NA, NA, NA, 50), gap_at = 3:7)
    obs <- vctrs::vec_rbind(temp, dew, rh)
    donor <- make_obs(n = n, variable = "relative_humidity_2m",
                      value = rep(42, n), source = "bom_obs")

    out <- fill_tier(obs, dict = met_variables(),
                     donors = list(bom = donor), site = site)
    filled <- out[out$variable == "relative_humidity_2m", ]
    filled <- filled[order(filled$datetime_utc), ]

    expect_true(all(filled$method[3:7] == "donor_fill"))
    expect_true(all(filled$source[3:7] == "bom_obs"))
  })
})

describe("filled rows carry a clean qc_flag, never the gap's flag", {
  it("labels a fill 'ok' so downstream method==measured filters still work", {
    obs <- series_with_gap("temperature_2m", c(15, NA, 18), gap_at = 2)
    out <- fill_tier(obs, dict = met_variables())
    filled <- out[out$variable == "temperature_2m", ]
    expect_equal(filled$qc_flag[2], "ok")        # not the inherited 'missing'
    expect_false(filled$method[2] == "measured") # but identifiable as filled
  })
})
