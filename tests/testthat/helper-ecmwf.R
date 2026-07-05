# Helpers for Plan 08 — ECMWF Open Data (GRIB2) fixtures + guards.
#
# The committed fixture (small.grib2/small.index) is a genuine excerpt of a
# live ECMWF Open Data file: 3 real `enfo` (medium-range ensemble) messages
# for `2t` (members 1-3, step 24h), range-downloaded from
# https://data.ecmwf.int/forecasts/ on 2026-07-06 and re-indexed with offsets
# recalculated for the small concatenated file. See
# plans/08-acquisition-ecmwf.md for the full provenance record.

ecmwf_grib_path <- function() {
  testthat::test_path("_fixtures/ecmwf/small.grib2")
}
ecmwf_index_path <- function() {
  testthat::test_path("_fixtures/ecmwf/small.index")
}
ecmwf_index_lines <- function() {
  readLines(ecmwf_index_path(), warn = FALSE)
}

# Skip a GRIB test unless terra AND the real fixture are both available.
# Opening a GRIB file and reading its band *metadata* never requires decoding
# pixel data (see R/grib-read.R's header note), so this gate alone is enough
# for `grib_open()`/`grib_field_table()` tests, which work regardless of
# whether the local GDAL build can decode CCSDS/AEC compression.
skip_unless_grib_ready <- function() {
  testthat::skip_if_not_installed("terra")
  testthat::skip_if_not(file.exists(ecmwf_grib_path()),
                        "genuine ECMWF GRIB2 fixture not recorded yet")
}

# Whether *this* GDAL build can actually decode the fixture's CCSDS/AEC
# compressed pixel data (as opposed to just its headers/metadata). Real ECMWF
# GRIB2 messages use this compression, and GDAL needs libaec support to read
# it (OSGeo/gdal#8108) -- many CRAN binary builds do not have it. Tests that
# need real decoded values are gated on this (skipping, not failing, when
# unsupported); tests of the *guard* (`.grib_check_ccsds_support()`) and of
# `fetch_forecast()`'s degradation behaviour instead assert whichever outcome
# this probe says is true, so they are meaningful either way this comes out.
ecmwf_ccsds_supported <- function() {
  skip_unless_grib_ready()
  tryCatch({
    vals <- grib_extract_point(grib_open(ecmwf_grib_path()), lat = 0, lon = 0)
    length(vals) > 0 && all(is.finite(vals))
  }, error = function(e) FALSE, warning = function(w) FALSE)
}
