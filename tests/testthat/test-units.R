# Plan 01 — canonical units + the km/h footgun.

describe("to_canonical()", {
  it("converts km/h wind to canonical m/s (the §3.1 footgun)", {
    got <- to_canonical(10, "km/h", "wind_speed_10m")
    expect_equal(as.numeric(got), 2.777778, tolerance = 1e-5)
  })

  it("round-trips canonical -> other -> canonical within 1e-9", {
    canon <- to_canonical(25, "degC", "temperature_2m")
    fahr <- units::set_units(units::set_units(as.numeric(canon), "degC"), "degF")
    back <- to_canonical(as.numeric(fahr), "degF", "temperature_2m")
    expect_equal(as.numeric(back), as.numeric(canon), tolerance = 1e-9)
  })

  it("aborts when the source unit is dimensionally incompatible", {
    expect_error(
      to_canonical(20, "degC", "wind_speed_10m"),
      class = "meteoTidy_error_bad_units"
    )
  })

  it("trusts a units-carrying input but warns when it disagrees with `from`", {
    x <- units::set_units(10, "km/h")
    expect_warning(
      out <- to_canonical(x, from = "m/s", "wind_speed_10m"),
      class = "meteoTidy_warning_units_conflict"
    )
    # honours the carried km/h, ignoring the mistaken `from = m/s`
    expect_equal(as.numeric(out), 2.777778, tolerance = 1e-5)
  })
})

describe("canonical_unit()", {
  it("reports the dictionary's canonical unit for a variable", {
    expect_equal(canonical_unit("wind_speed_10m"), "m/s")
    expect_equal(canonical_unit("temperature_2m"), "degC")
  })
})
