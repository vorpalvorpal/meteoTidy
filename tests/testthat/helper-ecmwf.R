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

# Canned `grib_ls -j -p shortName,step,perturbationNumber -l lat,lon,1` output
# for the committed 3-message 2t fixture (members 1-3, step 24h, native Kelvin).
# Lets `grib_point_table()`'s JSON parse and the `fetch_forecast()` read path be
# exercised deterministically on every platform, with no real eccodes install
# and no GDAL -- exactly the GDAL-version-fragile contract Plan 18 Part A moved
# off terra to make version-independent.
ecmwf_grib_ls_json <- function() {
  paste0(
    "[",
    '{"keys":{"shortName":"2t","step":"24","perturbationNumber":1},',
    '"method":"nearest","neighbours":[{"index":1,"latitude":-34.75,',
    '"longitude":148.2,"distance":0,"distance_unit":"km","value":295.406,"unit":"K"}]},',
    '{"keys":{"shortName":"2t","step":"24","perturbationNumber":2},',
    '"method":"nearest","neighbours":[{"index":1,"latitude":-34.75,',
    '"longitude":148.2,"distance":0,"distance_unit":"km","value":295.925,"unit":"K"}]},',
    '{"keys":{"shortName":"2t","step":"24","perturbationNumber":3},',
    '"method":"nearest","neighbours":[{"index":1,"latitude":-34.75,',
    '"longitude":148.2,"distance":0,"distance_unit":"km","value":294.8,"unit":"K"}]}',
    "]"
  )
}

# A `grib_point_table()` stand-in for the committed 2t fixture: the
# deterministic field table `fetch_forecast()` would get from eccodes, without
# needing eccodes installed. Native Kelvin (eccodes does not auto-convert to
# Celsius the way GDAL's GRIB driver did).
mock_grib_point_table_2t <- function() {
  function(path, lat, lon) {
    tibble::tibble(
      band = 1:3, param = "2t", unit = "K", step = "24",
      member = c(1L, 2L, 3L), value = c(295.406, 295.925, 294.8)
    )
  }
}

# A `.http_get()` mock for the range-download seam that hands back the
# committed fixture bytes, honouring the requested Range header (real ECMWF
# HTTP 206 responses return exactly the requested byte span -- VERIFIED live
# 2026-07-06, see R/ecmwf-eccodes.R's header notes). Ignoring Range here (the
# naive "just return the whole file" mock) concatenates N whole-file copies
# into one local file whenever more than one message is selected, producing
# duplicate messages the moment a real decode actually succeeds (terra or
# eccodes) -- a real bug this fixed, not a hypothetical one.
mock_ecmwf_http_get <- function() {
  function(url, headers = list(), parse = "json", ...) {
    if (grepl("index", url)) {
      return(ecmwf_index_lines())
    }
    all_bytes <- readBin(ecmwf_grib_path(), "raw", file.info(ecmwf_grib_path())$size)
    range <- headers$Range
    if (is.null(range)) {
      return(all_bytes)
    }
    m <- regmatches(range, regexec("bytes=(\\d+)-(\\d+)", range))[[1]]
    all_bytes[(as.integer(m[2]) + 1):(as.integer(m[3]) + 1)]
  }
}

# Skip a test unless a real, usable eccodes install is present (either
# provisioned via ecmwf_install_eccodes() or already on PATH). The eccodes GRIB
# reader (R/grib-read.R) is otherwise unit-tested against canned `grib_ls -j`
# JSON (ecmwf_grib_ls_json()); this gate is for the genuine, end-to-end "does
# grib_ls actually decode our real CCSDS fixture" tests -- skip cleanly, never
# fail, when the environment doesn't have eccodes installed.
skip_unless_eccodes_ready <- function() {
  testthat::skip_if_not(.have_eccodes(), "eccodes (grib_ls) is not available in this environment")
}
