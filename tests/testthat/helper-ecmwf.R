# Helpers for Plan 08 — ECMWF Open Data (GRIB2) fixtures + guards.

# The committed genuine CCSDS GRIB2 fixture (recorded, not synthesisable). The
# spike tests skip cleanly until it and terra are both present.
ecmwf_grib_path <- function() {
  testthat::test_path("_fixtures/ecmwf/small.grib2")
}
ecmwf_index_path <- function() {
  testthat::test_path("_fixtures/ecmwf/small.index")
}
ecmwf_index_lines <- function() {
  readLines(ecmwf_index_path(), warn = FALSE)
}

# Skip a GRIB test unless terra AND the real CCSDS fixture are both available.
skip_unless_grib_ready <- function() {
  testthat::skip_if_not_installed("terra")
  testthat::skip_if_not(file.exists(ecmwf_grib_path()),
                        "genuine CCSDS ECMWF GRIB2 fixture not recorded yet")
}
