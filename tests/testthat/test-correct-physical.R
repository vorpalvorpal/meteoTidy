# Plan 11 — day-0 physical adjustments (always applied, never fitted).

describe("log-wind profile height adjustment", {
  it("brings a known 2 m wind to 10 m for a given z0 (hand-computed)", {
    z0 <- 0.03
    v2 <- 4.0
    expected <- v2 * log(10 / z0) / log(2 / z0)
    got <- correct_physical_wind(v2, from_height = 2, to_height = 10, z0 = z0)
    expect_equal(got, expected, tolerance = 1e-9)
  })

  it("aborts missing_roughness when z0 is absent", {
    expect_error(
      correct_physical_wind(4.0, from_height = 2, to_height = 10, z0 = NA),
      class = "meteoTidy_error_missing_roughness"
    )
  })

  it("tags physical-tier wind output with tier 'physical'", {
    out <- correct_physical(
      make_obs(n = 3, variable = "wind_speed_10m", value = c(3, 4, 5)),
      site = make_test_site()
    )
    expect_true(all(out$tier == "physical"))
  })
})

describe("lapse-rate temperature adjustment", {
  it("shifts temperature by the expected amount for an elevation delta", {
    # standard environmental lapse ~ 6.5 °C/km; +100 m → −0.65 °C
    got <- correct_physical_lapse(20, elevation_delta = 100,
                                  lapse_rate = 0.0065)
    expect_equal(got, 20 - 0.65, tolerance = 1e-9)
  })

  it("exposes the lapse rate as an overridable parameter (inversion caveat)", {
    args <- names(formals(correct_physical_lapse))
    expect_true("lapse_rate" %in% args)
    # a site in an inversion regime can override the fixed rate
    got <- correct_physical_lapse(20, elevation_delta = 100, lapse_rate = 0)
    expect_equal(got, 20)
  })
})

describe("physical adjustments are the day-0 tier", {
  it("stamps tier 'physical' on all adjusted output", {
    out <- correct_physical(
      make_obs(n = 2, variable = "temperature_2m", value = c(20, 21)),
      site = make_test_site()
    )
    expect_true(all(out$tier == "physical"))
  })
})
