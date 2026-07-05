# Plan 12 -- post-correction physical-consistency pass (SCOPING section 6
# review fix). After univariate fitted-tier corrections, cross-variable
# physical relations (gusts >= wind, dewpoint <= temperature, RH <= 100,
# direct+diffuse <= clear-sky ceiling) can be violated even though each
# variable was individually well-corrected. This is a thin wrapper around
# Plan 09's shared `physics_constraints()` module in `mode = "enforce"`:
# violations are clipped to the constraint boundary and counted, so Plan 13
# can surface a rising violation rate as a red flag.

#' Run the post-correction physical-consistency pass
#'
#' Enforces the shared physical-consistency relations (`R/physics-constraints.R`,
#' the same module Plan 09's QC engine uses in `mode = "flag"`) on a wide,
#' single-row-per-timestamp corrected frame, clipping any violation to its
#' constraint boundary.
#'
#' @param wide A wide tibble (one row per `(site_id, datetime_utc)`, columns
#'   named after dictionary variables) of corrected values.
#' @return A list with `result` (the clipped wide tibble) and `n_violations`
#'   (integer count of relations that needed clipping, `0` when nothing did).
#' @keywords internal
#' @noRd
consistency_pass <- function(wide) {
  enforced <- physics_constraints(wide, mode = "enforce")
  n_violations <- attr(enforced, "n_violations") %||% 0L
  attr(enforced, "n_violations") <- NULL
  list(result = enforced, n_violations = n_violations)
}
