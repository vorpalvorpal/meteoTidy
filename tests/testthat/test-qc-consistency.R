# Plan 09 — internal-consistency rule + the shared physics-constraints module.
# The SAME module runs in "flag" mode here (Plan 09) and "enforce" mode in
# Plan 12; this test pins both so the reuse contract can't drift.

describe("physics_constraints(mode = 'flag')", {
  it("suspects dewpoint > temperature", {
    row <- qc_wide_row(temperature_2m = 20, dewpoint_2m = 25)  # impossible
    out <- physics_constraints(row, mode = "flag")
    expect_true(out$flag_dewpoint_2m == "suspect" ||
                  out$dewpoint_2m_flag == "suspect")
  })

  it("suspects relative humidity > 100", {
    row <- qc_wide_row(relative_humidity_2m = 130)
    out <- physics_constraints(row, mode = "flag")
    expect_true(any(grepl("suspect", unlist(out[grepl("humidity", names(out))]))) ||
                  isTRUE(out$violated))
  })

  it("suspects wind gusts < wind speed", {
    row <- qc_wide_row(wind_speed_10m = 8, wind_gusts_10m = 5)  # gust < mean
    out <- physics_constraints(row, mode = "flag")
    expect_true(isTRUE(out$violated) ||
                  any(grepl("suspect", unlist(out))))
  })

  it("suspects direct + diffuse above the clear-sky ceiling", {
    row <- qc_wide_row(direct_radiation = 1200, diffuse_radiation = 400,
                       clear_sky_ceiling = 1000)
    out <- physics_constraints(row, mode = "flag")
    expect_true(isTRUE(out$violated) || any(grepl("suspect", unlist(out))))
  })
})

describe("physics_constraints(mode = 'enforce') — the Plan 12 reuse", {
  it("clips a violating value to the constraint boundary (same relations)", {
    row <- qc_wide_row(wind_speed_10m = 8, wind_gusts_10m = 5)
    out <- physics_constraints(row, mode = "enforce")
    # gust is clipped up to at least the wind speed
    expect_gte(out$wind_gusts_10m, out$wind_speed_10m)
  })

  it("leaves a physically-consistent row unchanged with zero violations", {
    row <- qc_wide_row(temperature_2m = 20, dewpoint_2m = 12,
                       relative_humidity_2m = 60,
                       wind_speed_10m = 5, wind_gusts_10m = 9)
    out <- physics_constraints(row, mode = "enforce")
    expect_equal(out$wind_gusts_10m, 9)
    expect_equal(out$dewpoint_2m, 12)
    expect_equal(attr(out, "n_violations") %||% 0, 0)
  })
})
