# Plan 18 Part A — the eccodes-only GRIB read seam (supersedes the terra/GDAL
# read path). `grib_point_table()` reads ECMWF-native identifiers (shortName,
# step, perturbationNumber, units) via one `grib_ls -j -p ... -l lat,lon,1`
# call, so the field table is deterministic and GDAL-version-independent -- no
# NCEP-table translation to drift, and no `skip_on_ci()` for GDAL-metadata
# variance.
#
# The committed fixture (small.grib2) is a genuine CCSDS-compressed ECMWF Open
# Data excerpt (3 `enfo` 2t messages, step 24h, members 1-3; see helper-ecmwf.R
# for provenance). eccodes always decodes CCSDS, so the only gate on the real
# end-to-end read is whether eccodes (grib_ls) is installed in this
# environment (skip_unless_eccodes_ready()); the contract itself is pinned
# deterministically against canned `grib_ls -j` JSON on every platform.

describe(".grib_point_table_parse()", {
  it("parses grib_ls -j JSON into the documented field table", {
    tbl <- meteoTidy:::.grib_point_table_parse(ecmwf_grib_ls_json())
    expect_true(all(c("band", "param", "unit", "step", "member", "value") %in%
                      names(tbl)))
    expect_equal(nrow(tbl), 3)
    expect_equal(tbl$band, 1:3)
    expect_true(all(tbl$param == "2t"))
    expect_true(all(tbl$unit == "K"))
    expect_true(all(tbl$step == "24"))
    expect_type(tbl$member, "integer")
    expect_setequal(tbl$member, c(1L, 2L, 3L))
    expect_type(tbl$value, "double")
    expect_true(all(is.finite(tbl$value)))
  })

  it("normalises a step reported as '24h' to plain hours '24'", {
    json <- gsub('"step":"24"', '"step":"24h"', ecmwf_grib_ls_json(), fixed = TRUE)
    tbl <- meteoTidy:::.grib_point_table_parse(json)
    expect_true(all(tbl$step == "24"))
  })

  it("maps eccodes' native unit through .eccodes_unit_to_udunits", {
    json <- gsub('"unit":"K"', '"unit":"m s**-1"', ecmwf_grib_ls_json(), fixed = TRUE)
    tbl <- meteoTidy:::.grib_point_table_parse(json)
    expect_true(all(tbl$unit == "m s-1"))
  })
})

describe("grib_point_table()", {
  it("yields the documented contract from the grib_ls seam on every platform", {
    # Mock only the grib_ls-shell seam: the real parse runs, so this pins the
    # param/unit/step/member/value contract with no eccodes, no GDAL, no skip.
    # This is the contract that was GDAL-version-fragile under terra; it must
    # now be version-independent.
    testthat::local_mocked_bindings(
      .grib_ls_json = function(path, lat, lon) ecmwf_grib_ls_json()
    )
    tbl <- grib_point_table("ignored.grib2", lat = -34.75, lon = 148.20)
    expect_true(all(tbl$param == "2t"))
    expect_true(all(tbl$unit == "K"))
    expect_true(all(tbl$step == "24"))
    expect_setequal(tbl$member, c(1L, 2L, 3L))
    expect_true(all(is.finite(tbl$value)))
  })

  it("aborts eccodes_required when grib_ls cannot be resolved", {
    testthat::local_mocked_bindings(.eccodes_grib_ls_path = function() NA_character_)
    expect_error(
      grib_point_table(ecmwf_grib_path(), lat = 0, lon = 0),
      class = "meteoTidy_error_eccodes_required"
    )
  })

  it("decodes the committed CCSDS fixture into real values (real eccodes)", {
    skip_unless_eccodes_ready()
    tbl <- grib_point_table(ecmwf_grib_path(), lat = -34.75, lon = 148.20)
    expect_equal(nrow(tbl), 3)
    expect_true(all(tbl$param == "2t"))
    expect_true(all(tbl$unit == "K"))
    expect_setequal(tbl$member, c(1L, 2L, 3L))
    # plausible surface temperature in Kelvin -- eccodes reports the file's
    # native unit, and does not auto-convert to Celsius the way GDAL did.
    expect_true(all(tbl$value > 250 & tbl$value < 320))
  })
})
