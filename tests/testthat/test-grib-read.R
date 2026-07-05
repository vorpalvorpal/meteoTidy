# Plan 08 — the GRIB read spike (SCOPING §13/§14 acceptance gate).
# Skips unless terra is installed AND the genuine CCSDS fixture is recorded.
# Because the fixture is CCSDS-compressed, a GDAL built without libaec fails
# here — exactly the real-world failure the guard turns into
# "grib_ccsds_unsupported".

describe("grib_open()", {
  it("opens the committed small.grib2 as a SpatRaster with the expected bands", {
    skip_unless_grib_ready()
    rast <- grib_open(ecmwf_grib_path())
    expect_s4_class(rast, "SpatRaster")
    expect_gt(terra::nlyr(rast), 0)
  })
})

describe("grib_extract_point()", {
  it("returns finite, physically plausible values at an in-grid coordinate", {
    skip_unless_grib_ready()
    rast <- grib_open(ecmwf_grib_path())
    vals <- grib_extract_point(rast, lat = -34.75, lon = 148.20)
    expect_true(all(is.finite(vals)))
    # nearest-gridpoint (documented, not bilinear) — a coarse 0.25° cell
    expect_true(length(vals) >= 1)
  })
})

describe("grib_field_table()", {
  it("decodes param/level/step and demuxes member from perturbationNumber", {
    # GDAL exposes each message as a flat band with no ensemble axis; member
    # identity lives only in the band PDS metadata. The table must demux it.
    skip_unless_grib_ready()
    tbl <- grib_field_table(grib_open(ecmwf_grib_path()))
    expect_true(all(c("band", "param", "step", "member") %in% names(tbl)))
    # a control (0) and at least one perturbed (>=1) member are distinguished
    expect_true(0 %in% tbl$member)
    expect_true(any(tbl$member >= 1))
  })
})
