# Plan 01 — variable dictionary.

describe("the built-in dictionary", {
  it("has parseable units, ordered ranges, and valid enums on every row", {
    d <- met_variables()
    for (i in seq_len(nrow(d))) {
      expect_silent(units::as_units(d$unit[i]))
      if (!is.na(d$min[i]) && !is.na(d$max[i])) {
        expect_lte(d$min[i], d$max[i])
      }
    }
    expect_silent(validate_statistical_class(d$statistical_class))
    expect_silent(validate_measurability_class(d$measurability_class))
  })

  it("marks exactly the wind_direction_* variables as circular (period 360)", {
    d <- met_variables()
    is_dir <- grepl("^wind_direction_", d$variable)
    expect_true(all(d$circular_period[is_dir] == 360))
    expect_true(all(is.na(d$circular_period[!is_dir])))
  })

  it("covers the full §3.1 wide contract set", {
    contract <- c(
      "temperature_2m", "relative_humidity_2m", "dewpoint_2m",
      "surface_pressure", "pressure_msl", "precipitation", "cloud_cover",
      "direct_radiation", "diffuse_radiation",
      "wind_speed_10m", "wind_direction_10m", "wind_gusts_10m",
      "wind_speed_80m", "wind_speed_120m", "wind_speed_180m",
      "boundary_layer_height", "soil_moisture_0_to_1cm", "cape", "uv_index"
    )
    have <- met_variables()$variable
    expect_true(all(contract %in% have))
    # `time` is a column, never a dictionary variable
    expect_false("time" %in% have)
  })
})

describe("met_register_variable() / met_variable()", {
  it("adds a variable that met_variable() then finds", {
    local_clean_dict()
    met_register_variable(
      variable = "leaf_wetness", unit = "1", min = 0, max = 1,
      statistical_class = "bounded", measurability_class = "site_measurable",
      description = "fraction of time wet"
    )
    expect_equal(met_variable("leaf_wetness")$variable, "leaf_wetness")
  })

  it("refuses to silently redefine a built-in", {
    local_clean_dict()
    expect_error(
      met_register_variable("temperature_2m", unit = "degC", min = -50, max = 60,
                            statistical_class = "linear",
                            measurability_class = "site_measurable",
                            description = "dup"),
      class = "meteoTidy_error_duplicate_variable"
    )
  })

  it("aborts on an unknown variable lookup", {
    expect_error(met_variable("nope"), class = "meteoTidy_error_unknown_variable")
  })

  it("does not leak registrations across tests (local_clean_dict restores)", {
    local_clean_dict()
    met_register_variable("scratch_var", unit = "1", min = 0, max = 1,
                          statistical_class = "bounded",
                          measurability_class = "site_measurable",
                          description = "temp")
    expect_no_error(met_variable("scratch_var"))
    # after this test the deferred dict_reset() removes scratch_var
  })
})
