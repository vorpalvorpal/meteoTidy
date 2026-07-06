# Plan 13 -- the skill verdict: promote/keep decision (Plan 11 `tier_select`)
# and per-lead shrinkage weights (Plan 12 `shrinkage`), SCOPING section 7.1 /
# section 7.4. Named `skill_verdict_compute()`, not `skill_verdict()`, to
# avoid colliding with the identically-named test-helper builder in
# `tests/testthat/helper-correct.R` used throughout Plans 11/12's
# already-passing tests as a stand-in for this function's output shape.

#' Compute the skill verdict from scores and a bootstrap result
#'
#' Joins `scores` and `bootstrap` on `(variable, lead_bucket)` and derives,
#' per row:
#'
#' - `promote` -- `TRUE` iff the candidate's out-of-sample score is actually
#'   better than the incumbent's (`candidate < incumbent`, lower-is-better
#'   error-type scores) **and** that improvement survives the block
#'   bootstrap (`bootstrap$significant`). Either condition alone is not
#'   enough (SCOPING section 7.1's skill gate).
#' - `shrink_weight` -- when `scores$skill_vs_clim` is present, the skill
#'   score against climatology clamped to `[0, 1]`: `skill_vs_clim <= 0`
#'   (no better than climatology) gives weight `0` (fall back to
#'   climatology entirely); high skill approaches weight `1` (trust the
#'   correction fully). Absent `skill_vs_clim`, `NA_real_`.
#' - `consistency_violation_rate` -- passed through unchanged from `scores`
#'   if present (Plan 12's `consistency_pass()` violation rate, a red flag
#'   surfaced here per SCOPING section 7.1); `NA_real_` otherwise.
#'
#' @param scores A tibble with columns `variable`, `lead_bucket`, and any of
#'   `candidate`, `incumbent` (both error-type scores, e.g. RMSE),
#'   `skill_vs_clim`, `consistency_violation_rate`.
#' @param bootstrap A tibble with columns `variable`, `lead_bucket`,
#'   `significant`, `ci_lower`, `ci_upper` (see `block_bootstrap_ci()`),
#'   matched to `scores` by `(variable, lead_bucket)`.
#' @return A tibble, one row per `scores` row, with columns `variable`,
#'   `lead_bucket`, `promote`, `shrink_weight`, `consistency_violation_rate`
#'   -- the same column names `tier_select()` (Plan 11) and
#'   `shrink_to_climatology()`/`apply_correction_shrinkage()` (Plan 12)
#'   expect, so a row of this output is usable as their `skill_verdict`
#'   argument directly.
#' @keywords internal
#' @noRd
skill_verdict_compute <- function(scores, bootstrap) {
  join_cols <- c("variable", "lead_bucket")
  boot_cols <- bootstrap[c(join_cols, "significant", "ci_lower", "ci_upper")]
  merged <- merge(scores, boot_cols, by = join_cols, all.x = TRUE, sort = FALSE)
  # merge() does not preserve row order; restore `scores`' original order.
  ord_key <- do.call(paste, c(scores[join_cols], sep = "\r"))
  merged_key <- do.call(paste, c(merged[join_cols], sep = "\r"))
  merged <- merged[match(ord_key, merged_key), , drop = FALSE]

  has_candidate <- all(c("candidate", "incumbent") %in% names(merged))
  better <- if (has_candidate) merged$candidate < merged$incumbent else FALSE
  significant <- isTRUE_vec(merged$significant)
  promote <- better & significant

  skill_vs_clim <- merged[["skill_vs_clim"]]
  shrink_weight <- if (!is.null(skill_vs_clim)) {
    pmax(0, pmin(1, skill_vs_clim))
  } else {
    NA_real_
  }

  consistency_violation_rate <- merged[["consistency_violation_rate"]]
  if (is.null(consistency_violation_rate)) {
    consistency_violation_rate <- NA_real_
  }

  tibble::tibble(
    variable = merged$variable,
    lead_bucket = merged$lead_bucket,
    promote = promote,
    shrink_weight = shrink_weight,
    consistency_violation_rate = consistency_violation_rate
  )
}
