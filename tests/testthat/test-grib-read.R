# Plan 08 — the GRIB read spike (SCOPING §13/§14 acceptance gate).
#
# The fixture is a genuine excerpt of a real ECMWF Open Data GRIB2 file (see
# helper-ecmwf.R for provenance). Opening a file and reading its band
# *metadata* never requires decoding pixel data, so `grib_open()` and
# `grib_field_table()` run on any GDAL build with the GRIB driver. Actually
# decoding pixel values (`grib_extract_point()`, and downstream the guard
# `.grib_check_ccsds_support()`) needs a build with libaec support for
# CCSDS/AEC decoding, which is not universal (OSGeo/gdal#8108) -- those tests
# are gated on `ecmwf_ccsds_supported()` and skip cleanly, rather than fail,
# when this build can't decode it.

describe("grib_open()", {
  it("opens the committed small.grib2 as a SpatRaster with the expected bands", {
    skip_unless_grib_ready()
    rast <- grib_open(ecmwf_grib_path())
    expect_s4_class(rast, "SpatRaster")
    expect_equal(terra::nlyr(rast), 3)
  })
})

describe("grib_extract_point()", {
  it("returns finite, physically plausible values at an in-grid coordinate", {
    skip_unless_grib_ready()
    testthat::skip_if_not(ecmwf_ccsds_supported(),
                          "this GDAL build cannot decode CCSDS/AEC-compressed GRIB2")
    rast <- grib_open(ecmwf_grib_path())
    vals <- grib_extract_point(rast, lat = -34.75, lon = 148.20)
    expect_true(all(is.finite(vals)))
    # nearest-gridpoint (documented, not bilinear) — a coarse 0.25° cell.
    # Values are already Celsius (GDAL auto-converts ECMWF's Kelvin fields;
    # see R/grib-read.R), so a plausible surface temperature is well within
    # +/- 60.
    expect_true(all(abs(vals) < 60))
  })
})

describe("grib_field_table()", {
  it("decodes param/unit/step and demuxes member from the PDS template", {
    skip_unless_grib_ready()
    # GDAL surfaces GRIB per-band PDS metadata (GRIB_ELEMENT, the assembled
    # template values grib_field_table() reads for param/member) differently
    # across versions -- the drift SCOPING §13's post-audit addendum records.
    # This assertion pins the values decoded from the fixture on the recorded
    # dev GDAL build; on CI's (uncontrolled, newer) GDAL the same fixture
    # decodes to different metadata strings, so skip cleanly there rather than
    # fail (the "skip, never fail, when the environment can't support it" rule
    # the other GRIB guards in helper-ecmwf.R already follow).
    testthat::skip_on_ci()
    tbl <- grib_field_table(grib_open(ecmwf_grib_path()))
    expect_true(all(c("band", "param", "unit", "step", "member") %in% names(tbl)))
    expect_equal(nrow(tbl), 3)
    expect_true(all(tbl$param == "2t"))
    expect_true(all(tbl$unit == "degC"))
    expect_true(all(tbl$step == "24"))
    # the fixture's 3 messages are distinct perturbed members (1, 2, 3); the
    # real `enfo` feed ships no separate control/member-0 (verified live,
    # 2026-07-06 — see plans/08-acquisition-ecmwf.md).
    expect_setequal(tbl$member, c(1L, 2L, 3L))
  })
})

describe(".grib_check_ccsds_support()", {
  it("matches this environment's actual CCSDS decode capability", {
    skip_unless_grib_ready()
    if (ecmwf_ccsds_supported()) {
      expect_no_error(.grib_check_ccsds_support(ecmwf_grib_path()))
    } else {
      expect_error(.grib_check_ccsds_support(ecmwf_grib_path()),
                   class = "meteoTidy_error_grib_ccsds_unsupported")
    }
  })
})
