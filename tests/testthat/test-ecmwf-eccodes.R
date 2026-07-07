# Post-audit — the eccodes CLI decode fallback (R/ecmwf-eccodes.R). The
# fixture is the same genuine CCSDS-compressed ECMWF excerpt test-grib-read.R
# uses (see helper-ecmwf.R).

describe(".micromamba_platform_tag()", {
  it("maps sysname/machine to the real micromamba release tags", {
    expect_equal(meteoTidy:::.micromamba_platform_tag("Darwin", "aarch64"), "osx-arm64")
    expect_equal(meteoTidy:::.micromamba_platform_tag("Darwin", "x86_64"), "osx-64")
    expect_equal(meteoTidy:::.micromamba_platform_tag("Linux", "x86_64"), "linux-64")
    expect_equal(meteoTidy:::.micromamba_platform_tag("Linux", "aarch64"), "linux-aarch64")
    expect_equal(meteoTidy:::.micromamba_platform_tag("Windows", "x86_64"), "win-64")
  })

  it("aborts on an unrecognised platform rather than guessing", {
    expect_error(
      meteoTidy:::.micromamba_platform_tag("Plan9", "mips"),
      class = "meteoTidy_error_eccodes_unsupported_platform"
    )
  })
})

describe(".micromamba_download_url()", {
  it("builds the documented stable micro.mamba.pm URL", {
    expect_equal(
      meteoTidy:::.micromamba_download_url("osx-arm64"),
      "https://micro.mamba.pm/api/micromamba/osx-arm64/latest"
    )
  })
})

describe(".eccodes_unit_to_udunits()", {
  it("passes Kelvin through unchanged", {
    expect_equal(meteoTidy:::.eccodes_unit_to_udunits("K"), "K")
  })

  it("normalises GRIB2's ** exponent notation to plain udunits syntax", {
    expect_equal(meteoTidy:::.eccodes_unit_to_udunits("m s**-1"), "m s-1")
  })

  it("passes NA through", {
    expect_true(is.na(meteoTidy:::.eccodes_unit_to_udunits(NA_character_)))
  })
})

describe(".eccodes_parse_nearest_json()", {
  it("parses grib_ls -j -l lat,lon,1's output into a (member, value, unit) tibble", {
    json <- '[
      {"keys": {"perturbationNumber": 1}, "method": "nearest",
       "neighbours": [{"index": 1, "latitude": 0, "longitude": 0,
                       "distance": 0, "distance_unit": "km",
                       "value": 295.406, "unit": "K"}]},
      {"keys": {"perturbationNumber": 2}, "method": "nearest",
       "neighbours": [{"index": 1, "latitude": 0, "longitude": 0,
                       "distance": 0, "distance_unit": "km",
                       "value": 295.925, "unit": "K"}]}
    ]'
    out <- meteoTidy:::.eccodes_parse_nearest_json(json)
    expect_equal(out$member, c(1L, 2L))
    expect_equal(out$value, c(295.406, 295.925))
    expect_equal(out$unit, c("K", "K"))
  })
})

describe(".have_eccodes()", {
  it("is FALSE when grib_ls cannot be resolved", {
    testthat::local_mocked_bindings(.eccodes_grib_ls_path = function() NA_character_)
    expect_false(.have_eccodes())
  })
})

describe("ecmwf_install_eccodes()", {
  it("refuses to touch the network when METEOTIDY_NO_NET=1 and eccodes isn't already available", {
    testthat::local_mocked_bindings(.have_eccodes = function() FALSE)
    withr::local_envvar(METEOTIDY_NO_NET = "1")
    expect_error(ecmwf_install_eccodes(), class = "meteoTidy_error_network_disabled")
  })

  it("is a no-op when eccodes is already available (never touches the network guard)", {
    testthat::local_mocked_bindings(.have_eccodes = function() TRUE)
    withr::local_envvar(METEOTIDY_NO_NET = "1") # would abort if the guard were reached
    expect_true(isTRUE(suppressMessages(ecmwf_install_eccodes())))
  })
})

describe("live end-to-end (real eccodes, if provisioned in this environment)", {
  it("decodes the real committed CCSDS fixture -- the exact 'terra can't, eccodes can' case this fallback exists for", { # nolint: line_length_linter.
    skip_unless_eccodes_ready()
    skip_unless_grib_ready() # only needed for grib_field_table()'s member metadata below

    ecc <- .eccodes_extract_point(ecmwf_grib_path(), lat = -34.75, lon = 148.20)
    expect_equal(nrow(ecc), 3)
    expect_setequal(ecc$member, c(1L, 2L, 3L))
    expect_true(all(ecc$unit == "K"))
    # Plausible surface temperature in Kelvin (not still whatever GDAL would
    # have auto-converted it to -- eccodes reports the file's native unit).
    expect_true(all(ecc$value > 250 & ecc$value < 320))

    field_tbl <- grib_field_table(grib_open(ecmwf_grib_path()))
    expect_setequal(field_tbl$member, ecc$member) # same messages, decoded two ways

    if (ecmwf_ccsds_supported()) {
      # Bonus cross-check when this environment's terra/GDAL *can* also
      # decode CCSDS: independent decoders should agree, aligned by member
      # (not file position/value order).
      terra_vals <- grib_extract_point(grib_open(ecmwf_grib_path()),
                                       lat = -34.75, lon = 148.20) # already degC
      terra_by_member <- terra_vals[order(field_tbl$member)]
      ecc_by_member <- ecc[order(ecc$member), , drop = FALSE]
      expect_equal(ecc_by_member$value - 273.15, terra_by_member, tolerance = 0.01)
    }
  })
})
