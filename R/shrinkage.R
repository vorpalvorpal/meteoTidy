# Plan 12 -- lead-dependent shrinkage toward climatology, forecast path only
# (SCOPING section 7.1, plans/12-correction-fitted-tiers.md). The review's
# core statistical fix: empirical QM/mean-bias are variance-preserving, so
# applied unshrunk at long lead they keep the full forecast variance when the
# skilful thing to do is shrink toward climatology as skill decays. `weight`
# (Plan 13's skill verdict, once it exists) drives how much of the raw
# correction is trusted vs how much climatology is substituted.
#
# This is the real implementation of the function Plan 11 stubbed as a
# `(corrected, ...)` identity placeholder in `R/correct.R`; that placeholder
# is now removed and `correct_apply()` calls this real, differently-shaped
# `(corrected, climatology, weight)` primitive instead.

#' Blend a corrected value toward climatology
#'
#' `weight * corrected + (1 - weight) * climatology`, vectorized. At
#' `weight = 1` this is the corrected value unchanged (full trust in the
#' correction); at `weight = 0` it is climatology outright (no skill beyond
#' climatology); intermediate weights blend linearly.
#'
#' @param corrected Numeric vector, the fitted-tier-corrected value(s).
#' @param climatology Numeric vector (recycled against `corrected`), the
#'   climatological value(s) to shrink toward.
#' @param weight Numeric scalar or vector in `[0, 1]`, the trust weight on
#'   `corrected` (typically Plan 13's verified-skill-derived shrink weight
#'   per lead bucket).
#' @return A numeric vector, the blended value(s).
#' @keywords internal
#' @noRd
shrink_to_climatology <- function(corrected, climatology, weight) {
  weight * corrected + (1 - weight) * climatology
}

#' Target-aware wrapper around `shrink_to_climatology()`
#'
#' Realised-series (record) corrections never shrink -- gap-fill and record
#' correction have no forecast-skill decay to guard against (Plan 10's
#' contract, carried forward from `R/transfer.R`). Forecast corrections
#' route through `shrink_to_climatology()`.
#'
#' @param corrected Numeric vector, the fitted-tier-corrected value(s).
#' @param climatology Numeric vector, the climatological value(s).
#' @param weight Numeric scalar or vector in `[0, 1]`, the shrink weight
#'   (ignored when `target = "record"`).
#' @param target Either `"record"` (returned unchanged) or `"forecast"`
#'   (shrunk toward climatology).
#' @return A numeric vector.
#' @keywords internal
#' @noRd
apply_correction_shrinkage <- function(corrected, climatology, weight,
                                       target = c("record", "forecast")) {
  target <- rlang::arg_match(target)
  if (target == "record") {
    return(corrected)
  }
  shrink_to_climatology(corrected, climatology, weight)
}
