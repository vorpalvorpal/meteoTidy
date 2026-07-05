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
    called <- new.env(); called$n <- 0L
    testthat::local_mocked_bindings(
      rank_donors = function(...) { called$n <- called$n + 1L; list() }
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

describe("filled rows carry a clean qc_flag, never the gap's flag", {
  it("labels a fill 'ok' so downstream method==measured filters still work", {
    obs <- series_with_gap("temperature_2m", c(15, NA, 18), gap_at = 2)
    out <- fill_tier(obs, dict = met_variables())
    filled <- out[out$variable == "temperature_2m", ]
    expect_equal(filled$qc_flag[2], "ok")        # not the inherited 'missing'
    expect_false(filled$method[2] == "measured") # but identifiable as filled
  })
})
