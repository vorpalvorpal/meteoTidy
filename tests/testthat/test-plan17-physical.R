# Plan 17 — BDD spec for the day-0 lapse adjustment gaining a real elevation
# delta (item 10).

describe("item 10: day-0 lapse uses a grid-vs-site elevation delta", {
  it("adjusts temperature by the standard lapse rate when a grid elevation is given", {
    site <- make_test_site()   # elevation 220 m
    obs <- new_obs(make_obs(n = 1, variable = "temperature_2m", value = 20))

    # site 220 m, grid 120 m -> delta 100 m -> -0.0065 * 100 = -0.65 degC
    adjusted <- correct_physical(obs, site, grid_elevation = 120)
    expect_equal(adjusted$value[[1]], 20 - 0.0065 * 100, tolerance = 1e-9)
    expect_equal(adjusted$tier[[1]], "physical")
  })

  it("is a no-op (but still tier physical) when no grid elevation is supplied", {
    site <- make_test_site()
    obs <- new_obs(make_obs(n = 1, variable = "temperature_2m", value = 20))
    out <- correct_physical(obs, site, grid_elevation = NULL)
    expect_equal(out$value[[1]], 20)
    expect_equal(out$tier[[1]], "physical")
  })
})
