# Plan 08 — the GRIB read seam (SCOPING §13/§14 "spike").
#
# Kept in its own file so this isolable, higher-risk unit is easy to find and
# reason about. Every function here is a thin wrapper around `terra`
# (Suggests-only; every entry point that reaches this file is guarded by
# `.have_terra()`/`rlang::check_installed("terra", ...)` upstream in
# R/source-ecmwf.R -- these functions themselves assume terra is already
# available, so they stay simple and directly testable).
#
# VERIFIED 2026-07-06 against a genuine CCSDS-compressed ECMWF Open Data GRIB2
# file (range-downloaded live from https://data.ecmwf.int/forecasts/, the
# `enfo` medium-range ensemble stream; see tests/testthat/_fixtures/ecmwf/
# small.grib2 and its provenance note). Two things the plan's original
# implementation got wrong, corrected here:
#  - `terra::metags()` is NOT how GDAL's native per-band GRIB metadata is
#    exposed (it is terra's *user*-tag store, always empty for a freshly
#    opened file). The real accessor is `terra::meta(rast, layers = TRUE)`,
#    which returns one 2-column (key, value) character matrix per layer.
#  - GDAL's GRIB driver auto-converts ECMWF's Kelvin temperature fields to
#    Celsius on read (confirmed: `GRIB_UNIT` reads `"[C]"` for `2t`, `"[m/s]"`
#    for `10u`/`10v` -- already SI, no conversion needed). `grib_field_table()`
#    therefore reports the unit GDAL actually produced, per band, rather than
#    a param-name lookup table assuming raw GRIB units -- callers must convert
#    *from that reported unit*, not from a hardcoded guess.

#' Open a GRIB2 file as a `SpatRaster`
#'
#' Thin wrapper around `terra::rast()`: terra/GDAL's GRIB driver exposes each
#' GRIB message (one field, one level, one step, one ensemble member) as a
#' single raster band, so a multi-message GRIB2 file opens directly as a
#' multi-layer `SpatRaster` with no further plumbing needed here. Kept as a
#' one-line wrapper (rather than inlining `terra::rast()` at call sites) so it
#' is a single, directly mockable/testable seam.
#'
#' Does **not** perform the CCSDS/libaec capability check itself (see
#' `.grib_check_ccsds_support()`) -- that check deliberately lives in the
#' `source_ecmwf()` fetch path so this function stays a thin, unconditional
#' wrapper that test code can call directly against the committed fixture.
#' Opening a file and reading its band *metadata* does not require decoding
#' pixel data, so `grib_open()` and `grib_field_table()` succeed even on a
#' GDAL build that lacks libaec; only actual value decoding (see
#' `grib_extract_point()`) needs it.
#'
#' @param path Single string, path to a `.grib2` file.
#' @return A `terra::SpatRaster`, one layer per GRIB message.
#' @keywords internal
#' @noRd
grib_open <- function(path) {
  terra::rast(path)
}

#' Extract nearest-gridpoint values at a point from every band of a raster
#'
#' Extracts the value of every layer of `rast` at the grid cell nearest
#' `(lon, lat)`, via `terra::extract(..., method = "simple")` -- terra's
#' nearest-neighbour extraction. **Deliberately not bilinear**: ECMWF's
#' 0.25 deg open-data grid is coarse (~28 km at the equator), and bilinear
#' interpolation would smear values across the land/sea (or orographic)
#' boundary of neighbouring cells in a way that is not obviously more correct
#' for a single-point extraction than just taking the nearest cell. Nearest-
#' gridpoint is the simpler, defensible choice for this resolution (SCOPING
#' §13/§14; documented as a deliberate decision, not an oversight).
#'
#' This is the one function in this file that requires actual pixel decoding
#' (not just header/metadata reads), so it is the one that fails with a raw
#' GDAL error on a build without libaec support for CCSDS/AEC-compressed
#' messages -- see `.grib_check_ccsds_support()`, which calls this function to
#' detect exactly that.
#'
#' @param rast A `terra::SpatRaster`, as returned by `grib_open()`.
#' @param lat,lon Single doubles, the site's latitude/longitude in degrees.
#' @return A plain numeric vector, one value per layer of `rast` (ID and
#'   coordinate columns that `terra::extract()` adds are dropped).
#' @keywords internal
#' @noRd
grib_extract_point <- function(rast, lat, lon) {
  pt <- matrix(c(lon, lat), ncol = 2)
  ex <- terra::extract(rast, pt, method = "simple")
  # terra::extract() returns a data.frame with a leading ID column; drop it
  # and return a plain numeric vector, one value per band.
  ex <- ex[, setdiff(names(ex), "ID"), drop = FALSE]
  as.numeric(unlist(ex[1, ], use.names = FALSE))
}

# GDAL's GRIB driver attaches per-band metadata as a (key, value) character
# matrix, readable via `terra::meta(rast, layers = TRUE)[[i]]` for band `i`.
# VERIFIED tag names (real ECMWF `enfo` file, 2026-07-06), all present on
# every band tested:
#   GRIB_ELEMENT              - NCEP-style element code, e.g. "TMP" (2t),
#                               "UGRD" (10u), "VGRD" (10v) -- NOT the GRIB2/
#                               ECMWF shortName; must be translated (see
#                               `.grib_element_to_param()`).
#   GRIB_SHORT_NAME           - despite the name, this is a level descriptor
#                               (e.g. "2-HTGL", "10-HTGL"), not the parameter
#                               shortName -- do not use it for param ID.
#   GRIB_UNIT                 - bracketed unit GDAL decoded the values into,
#                               e.g. "[C]", "[m/s]" (GDAL auto-converts
#                               ECMWF's Kelvin temperatures to Celsius).
#   GRIB_FORECAST_SECONDS     - lead time in seconds (-> step in hours).
#   GRIB_PDS_PDTN             - Product Definition Template Number; ensemble
#                               templates (individual ensemble forecast and
#                               its statistically-processed variants) are
#                               1, 11, 33, 41.
#   GRIB_PDS_TEMPLATE_ASSEMBLED_VALUES - a space-separated numeric dump of the
#                               PDS template's fields. For the ensemble
#                               templates above, the **last two** values are,
#                               in order, `perturbationNumber` and
#                               `numberOfForecastsInEnsemble` (verified against
#                               3 real ECMWF messages: values ending
#                               `"... 1 51"`, `"... 2 51"`, `"... 3 51"` for
#                               members 1/2/3 of a 51-member ensemble). There
#                               is no separately named tag for perturbation
#                               number on this GDAL build.
# `.grib_band_tag()` looks a tag up (case-insensitive) in that matrix;
# `.grib_band_member()` uses the verified positional convention above and
# falls back to NA_integer_ (rather than erroring) for any template number it
# does not recognise, since a hard failure here would be worse than an
# honestly-unknown member for an otherwise-usable field table.

.grib_band_tag <- function(tags, candidates) {
  if (is.null(tags) || !is.matrix(tags) || nrow(tags) == 0) {
    return(NA_character_)
  }
  hit <- which(toupper(tags[, 1]) %in% toupper(candidates))
  if (length(hit) == 0) {
    return(NA_character_)
  }
  unname(tags[hit[1], 2])
}

# NCEP-style GRIB_ELEMENT -> ECMWF Open Data index `param` shortName. Only the
# params `.ecmwf_param_lookup()` (R/source-ecmwf.R) requests are mapped; an
# unrecognised element is passed through unchanged (documented, not an error)
# so `grib_field_table()` stays usable for inspection even on an unmapped
# field.
.grib_element_to_param <- function(element) {
  if (is.na(element)) {
    return(NA_character_)
  }
  switch(element,
    TMP = "2t",
    UGRD = "10u",
    VGRD = "10v",
    element
  )
}

# GDAL reports units bracketed, e.g. "[C]", "[m/s]"; translate to a
# `units`-package-compatible string for `to_canonical()` (R/units.R). "C" ->
# "degC" is the one translation needed so far (udunits also accepts "C", but
# "degC" matches this package's dictionary convention, e.g.
# `canonical_unit("temperature_2m")`); anything else is passed through with
# the brackets stripped.
.grib_unit_to_udunits <- function(raw) {
  if (is.na(raw)) {
    return(NA_character_)
  }
  stripped <- gsub("^\\[|\\]$", "", raw)
  if (identical(stripped, "C")) "degC" else stripped
}

# Ensemble PDS templates for which the last-two-values convention (see above)
# is verified. Anything else -> member NA rather than a guessed offset.
.grib_ensemble_pdtns <- c(1L, 11L, 33L, 41L)

.grib_band_member <- function(tags) {
  pdtn <- suppressWarnings(as.integer(.grib_band_tag(tags, "GRIB_PDS_PDTN")))
  if (is.na(pdtn) || !(pdtn %in% .grib_ensemble_pdtns)) {
    return(NA_integer_)
  }
  assembled <- .grib_band_tag(tags, "GRIB_PDS_TEMPLATE_ASSEMBLED_VALUES")
  if (is.na(assembled)) {
    return(NA_integer_)
  }
  nums <- suppressWarnings(as.numeric(strsplit(trimws(assembled), "\\s+")[[1]]))
  if (length(nums) < 2) {
    return(NA_integer_)
  }
  suppressWarnings(as.integer(nums[length(nums) - 1]))
}

#' Decode a per-band field table from a GRIB `SpatRaster`
#'
#' Builds a tibble describing every band (GRIB message) of `rast`: which
#' parameter and unit it arrived in, its forecast step, and its ensemble
#' member. GDAL exposes each GRIB message as a **flat band** -- there is no
#' labelled ensemble dimension -- so ensemble member identity is demuxed from
#' the band's PDS metadata (`terra::meta(rast, layers = TRUE)`) rather than
#' read off an axis; see `.grib_band_member()` above for exactly which
#' metadata fields are used and how they were verified against a real file.
#'
#' Reading this metadata does not require decoding pixel data, so this
#' function works even on a GDAL build without libaec support (unlike
#' `grib_extract_point()`).
#'
#' @param rast A `terra::SpatRaster`, as returned by `grib_open()`.
#' @return A tibble with columns `band` (integer), `param` (character, the
#'   ECMWF index shortName translated from `GRIB_ELEMENT`, e.g. `"2t"`),
#'   `unit` (character, the `units`-package unit GDAL actually decoded values
#'   into, e.g. `"degC"`), `step` (character, forecast lead in hours), and
#'   `member` (integer, `NA` if undetermined).
#' @keywords internal
#' @noRd
grib_field_table <- function(rast) {
  n <- terra::nlyr(rast)
  band_tags <- terra::meta(rast, layers = TRUE)
  rows <- lapply(seq_len(n), function(i) {
    tags <- band_tags[[i]]

    element <- .grib_band_tag(tags, "GRIB_ELEMENT")
    param <- .grib_element_to_param(element)
    if (is.na(param)) {
      param <- names(rast)[i]
    }

    unit <- .grib_unit_to_udunits(.grib_band_tag(tags, "GRIB_UNIT"))

    step_secs <- .grib_band_tag(tags, "GRIB_FORECAST_SECONDS")
    step <- if (!is.na(step_secs)) {
      as.character(suppressWarnings(as.numeric(step_secs)) / 3600)
    } else {
      NA_character_
    }

    member <- .grib_band_member(tags)

    tibble::tibble(
      band = i,
      param = param,
      unit = unit,
      step = step,
      member = member
    )
  })

  vctrs::vec_rbind(!!!rows)
}

# CCSDS/AEC capability guard (SCOPING §13/§14). Real ECMWF Open Data GRIB2
# messages use CCSDS/AEC compression, which GDAL can only decode when built
# with libaec; older/non-libaec GDAL builds fail at **value read** time (see
# upstream GDAL issue "grib driver: cannot read AEC/CCSDS compressed
# messages", tracker id 8108) -- opening the file and reading its band
# metadata (`grib_open()`, `grib_field_table()`) succeeds regardless, since
# those never touch the packed data section. The only reliable check is
# therefore to attempt a real point extraction (`grib_extract_point()`), not
# merely to open the file.
#
# VERIFIED 2026-07-06: on the CRAN macOS binary of terra 1.8.70 (bundled
# GDAL 3.8.5), this genuinely fails against a real CCSDS-compressed ECMWF
# message with `"g2_unpack7: Data Representation Template 5.42 decoding
# requires building against libaec"` -- exactly the failure this guard turns
# into `"grib_ccsds_unsupported"`, confirming both that the check is
# necessary and that this function's tryCatch correctly catches it.
.grib_check_ccsds_support <- function(fixture_path = NULL) {
  fixture_path <- fixture_path %||% tryCatch(
    testthat::test_path("_fixtures/ecmwf/small.grib2"),
    error = function(e) NA_character_
  )
  if (is.na(fixture_path) || !file.exists(fixture_path)) {
    # No fixture to check against (e.g. running outside the test tree, or the
    # fixture genuinely hasn't been recorded yet); nothing more we can verify
    # here without a real download, so this is a silent no-op rather than a
    # false-positive failure.
    return(invisible(TRUE))
  }

  ok <- tryCatch({
    rast <- grib_open(fixture_path)
    vals <- grib_extract_point(rast, lat = 0, lon = 0)
    length(vals) > 0 && all(is.finite(vals))
  }, error = function(e) FALSE, warning = function(w) FALSE)

  if (!ok) {
    abort_meteo(
      c(
        "GDAL could not read a CCSDS/AEC-compressed ECMWF GRIB2 message.",
        "x" = "This usually means the installed GDAL build lacks libaec support (OSGeo/gdal#8108).", # nolint: line_length_linter.
        "i" = "Rebuild/upgrade GDAL with libaec, or use {.fn source_openmeteo} with {.code product = \"seasonal\"} as a no-GRIB alternative." # nolint: line_length_linter.
      ),
      class = "grib_ccsds_unsupported"
    )
  }
  invisible(TRUE)
}
