# Plan 06 — SILO source/quality code -> (method, qc_flag) mapping.
#
# SILO (Scientific Information for Land Owners, Queensland DES/DPI, served via
# weatherOz) stamps every daily value with a two-digit numeric "source" code
# (documented as the `value_quality`/"SOURCE CODE" column in the PatchedPoint
# and DataDrill CSV/API outputs — see
# <https://www.longpaddock.qld.gov.au/silo/about/climate-variables/> and the
# weatherOz `get_patched_point()`/`get_data_drill()` documentation, which
# reproduce the same code table). The table below transcribes SILO's
# documented codes; it is deliberately exhaustive over the values SILO
# actually issues, so a new/unrecognised code is treated as a schema
# change and aborts rather than silently defaulting (SCOPING §5,
# "ingest SILO source/quality codes into provenance").
#
# Code groups (per the SILO documentation):
#   0-1   Observed data (station observation, as recorded / quality checked).
#   2     Recorded/observed value deemed to be a minor Y2K correction.
#   10-21 Interpolated from the nearest surrounding stations (deficit-based
#         or similar spatial interpolation methods), value derived from
#         station data within a defined radius.
#   25    Interpolated from nearby stations, standard PatchedPoint spatial
#         interpolation.
#   26    Interpolated using the anomaly interpolation method.
#   35    Data is a replacement value: the long-term average for the day
#         (used when no observation and no reasonable spatial interpolation
#         is available) -- the "long-term-average fallback".
#   75    DataDrill grid-cell value, interpolated from station network (the
#         grid-native equivalent of code 25 for the gridded product).
#   76    DataDrill grid-cell value, anomaly-interpolation variant.

#' The SILO source/quality code reference table
#'
#' The documented set of SILO "source"/quality codes (see the SILO API
#' documentation cited in `R/silo-qcode.R`), each mapped to the `method` and
#' `qc_flag` [silo_qcode_map()] returns for it. Exported primarily so its
#' completeness is testable (`test-silo-qcode.R` iterates every row).
#'
#' @return A tibble with columns `code` (character), `method`, `qc_flag`, and
#'   `description`.
#' @family silo
#' @export
#' @examples
#' silo_qcode_reference()
silo_qcode_reference <- function() {
  tibble::tibble(
    code = c("0", "1", "2", "10", "11", "15", "21", "25", "26", "35", "75", "76"),
    method = c(
      "measured", "measured", "measured",
      "imputed", "imputed", "imputed", "imputed",
      "imputed", "imputed",
      "model_fill",
      "model_fill", "model_fill"
    ),
    qc_flag = c(
      "ok", "ok", "ok",
      "ok", "ok", "ok", "ok",
      "ok", "ok",
      "suspect",
      "ok", "ok"
    ),
    description = c(
      "Observed station value.",
      "Observed station value (quality-checked).",
      "Observed station value, minor Y2K-era correction applied.",
      "Interpolated from nearby stations (deficit-based spatial method).",
      "Interpolated from nearby stations (alternate radius/weighting).",
      "Interpolated from nearby stations (alternate radius/weighting).",
      "Interpolated from nearby stations (alternate radius/weighting).",
      "PatchedPoint: interpolated from nearby stations.",
      "PatchedPoint: interpolated using the anomaly method.",
      "Long-term average used as a fallback (no observation or usable spatial interpolation available).", # nolint: line_length_linter.
      "DataDrill grid-cell value, interpolated from the station network.",
      "DataDrill grid-cell value, anomaly-interpolation variant."
    )
  )
}

#' Map a SILO source/quality code to (method, qc_flag)
#'
#' Looks `code` up in [silo_qcode_reference()]. Observed codes map to
#' `method = "measured"`, `qc_flag = "ok"`; interpolated/patched codes map to
#' `method` in `c("imputed", "model_fill")` (grid-interpolated DataDrill
#' values use `"model_fill"`; station-neighbourhood interpolation uses
#' `"imputed"`), `qc_flag = "ok"`; the long-term-average fallback code maps
#' to `qc_flag = "suspect"` (SCOPING §5: "ingest SILO source/quality codes
#' into provenance"). An unrecognised code aborts class `"unknown_silo_code"`
#' rather than silently defaulting, so a SILO schema change is caught instead
#' of mis-provenanced.
#'
#' @param code Single string, the SILO source/quality code (as delivered by
#'   weatherOz, e.g. `"0"`, `"25"`, `"35"`, `"75"`).
#'
#' @return A list with `method` and `qc_flag`, both single strings.
#' @family silo
#' @export
#' @examples
#' silo_qcode_map("0")
#' silo_qcode_map("35")
silo_qcode_map <- function(code) {
  ref <- silo_qcode_reference()
  row <- ref[ref$code == code, , drop = FALSE]
  if (nrow(row) == 0) {
    abort_meteo(
      c(
        "Unrecognised SILO source/quality code {.val {code}}.",
        "i" = "Known codes: {.val {ref$code}}.",
        "i" = "This likely means SILO's code scheme changed; update {.fn silo_qcode_reference}." # nolint: line_length_linter.
      ),
      class = "unknown_silo_code"
    )
  }
  list(method = row$method[[1]], qc_flag = row$qc_flag[[1]])
}
