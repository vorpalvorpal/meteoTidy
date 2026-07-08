# Plan 18 Part A -- the eccodes-only GRIB read seam (supersedes the terra/GDAL
# read path; SCOPING §13's 2026-07-08 note).
#
# GRIB2 is read entirely through eccodes (ECMWF's own library, provisioned by
# ecmwf_install_eccodes(), R/ecmwf-eccodes.R). One
# `grib_ls -j -p shortName,step,perturbationNumber -l lat,lon,1 <file>` call
# decodes CCSDS/AEC packing natively AND reports ECMWF-native identifiers
# (shortName, step, perturbationNumber, units) directly -- with no NCEP-table
# translation to drift across GDAL versions, which is the version-coupling that
# made the former terra/GDAL band-metadata path fragile (its `GRIB_ELEMENT`
# string differed between GDAL 3.8.5 and newer builds). terra/GDAL is no longer
# used anywhere in the package.
#
# eccodes is therefore a HARD requirement for source_ecmwf(): absence aborts
# `eccodes_required`, pointing at ecmwf_install_eccodes(). There is no
# "this build can't decode CCSDS" branch any more -- eccodes always decodes it.

# Run `grib_ls -j -p shortName,step,perturbationNumber -l lat,lon,1 <path>` and
# return its JSON output as a single string. The one seam that actually shells
# out (mocked in the deterministic field-table tests). Aborts `eccodes_required`
# when no `grib_ls` is resolvable, `eccodes_decode_failed` when it runs but errors.
.grib_ls_json <- function(path, lat, lon) {
  grib_ls <- .eccodes_grib_ls_path()
  if (is.na(grib_ls)) {
    abort_meteo(
      c(
        "eccodes is required to read ECMWF Open Data GRIB2 files, but {.code grib_ls} was not found.", # nolint: line_length_linter.
        "i" = "Run {.fn ecmwf_install_eccodes} to provision it (a one-time, cached step).",
        "i" = "Alternatively, use {.code source_openmeteo(product = \"seasonal\")} for a no-GRIB seasonal splice." # nolint: line_length_linter.
      ),
      class = "eccodes_required"
    )
  }
  args <- c(
    "-j", "-p", "shortName,step,perturbationNumber",
    "-l", sprintf("%.6f,%.6f,1", lat, lon),
    path
  )
  out <- system2(grib_ls, args, stdout = TRUE, stderr = TRUE)
  exit_status <- attr(out, "status") %||% 0L
  if (!identical(exit_status, 0L)) {
    abort_meteo(
      c(
        "eccodes ({.code grib_ls}) failed to decode {.file {path}}.",
        "x" = paste(utils::tail(out, 5), collapse = "\n")
      ),
      class = "eccodes_decode_failed"
    )
  }
  paste(out, collapse = "\n")
}

# Normalise an eccodes `step` key to plain whole hours: "24" -> "24", "24h" ->
# "24", and a range like "0-24" -> its end ("24", the valid step).
.grib_normalise_step <- function(step) {
  s <- gsub("h$", "", as.character(step))
  if (grepl("-", s)) {
    s <- sub(".*-", "", s)
  }
  s
}

# Parse `grib_ls -j ... -l lat,lon,1` JSON (a top-level array, one element per
# GRIB message) into the field table. Kept separate from the shell-out so the
# param/unit/step/member/value contract is unit-testable against canned JSON on
# every platform, with no eccodes install and no GDAL.
.grib_point_table_parse <- function(json_text) {
  parsed <- jsonlite::fromJSON(json_text, simplifyVector = FALSE)
  rows <- lapply(seq_along(parsed), function(i) {
    msg <- parsed[[i]]
    neighbour <- msg$neighbours[[1]]
    member <- suppressWarnings(as.integer(msg$keys$perturbationNumber %||% NA_integer_))
    tibble::tibble(
      band = i,
      param = as.character(msg$keys$shortName),
      unit = .eccodes_unit_to_udunits(as.character(neighbour$unit)),
      step = .grib_normalise_step(msg$keys$step),
      member = member,
      value = as.numeric(neighbour$value)
    )
  })
  vctrs::vec_rbind(!!!rows)
}

#' Read a GRIB2 file's per-message field table + nearest-gridpoint value (eccodes)
#'
#' One row per GRIB message, in file order: `band` (integer), `param` (eccodes
#' `shortName`, e.g. `"2t"`/`"10u"`/`"10v"`), `unit` (udunits string via
#' `.eccodes_unit_to_udunits()` -- eccodes reports the file's **native** unit,
#' e.g. Kelvin for temperature, and does **not** auto-convert to Celsius the way
#' GDAL's GRIB driver did, so callers must convert from this reported unit),
#' `step` (character, forecast lead in whole hours), `member` (integer
#' `perturbationNumber`; `NA` if not applicable), and `value` (double,
#' nearest-gridpoint at `(lat, lon)`, in `unit`).
#'
#' Replaces the former terra-based `grib_open()` / `grib_field_table()` /
#' `grib_extract_point()` trio with a single eccodes `grib_ls` call, decoding
#' CCSDS/AEC natively. Aborts `eccodes_required` when eccodes is not installed
#' (see [ecmwf_install_eccodes()]).
#'
#' @param path Single string, path to a local `.grib2` file.
#' @param lat,lon Single doubles, the site's latitude/longitude in degrees.
#' @return A tibble with columns `band`, `param`, `unit`, `step`, `member`,
#'   `value` (see Description).
#' @keywords internal
#' @noRd
grib_point_table <- function(path, lat, lon) {
  .grib_point_table_parse(.grib_ls_json(path, lat, lon))
}
