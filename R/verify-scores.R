# Plan 13 -- deterministic + probabilistic scores and skill scores
# (SCOPING section 7.4). Pure scoring primitives: no store IO, no rolling
# origin here (that is R/verify.R). Kept small and hand-verifiable against
# tiny fixtures per plans/13-verification.md's test requirements.

#' Deterministic forecast error scores
#'
#' MAE and RMSE of `fc` against `obs`, elementwise.
#'
#' @param fc Numeric vector of forecasts/corrected values.
#' @param obs Numeric vector of observations, same length as `fc`.
#' @return A list with `mae` (`mean(abs(fc - obs))`) and `rmse`
#'   (`sqrt(mean((fc - obs)^2))`).
#' @keywords internal
#' @noRd
score_deterministic <- function(fc, obs) {
  err <- fc - obs
  list(mae = mean(abs(err)), rmse = sqrt(mean(err^2)))
}

#' CRPS for a normal predictive distribution
#'
#' Thin wrapper around `scoringRules::crps_norm()` -- the Continuous Ranked
#' Probability Score for a Gaussian predictive distribution `N(mu, sigma)`
#' evaluated at each observation in `obs` (SCOPING section 7.4, section 4).
#'
#' @param obs Numeric vector of observations.
#' @param mu Numeric vector of predictive means (recycled against `obs`).
#' @param sigma Numeric vector of predictive standard deviations (recycled
#'   against `obs`).
#' @return A numeric vector of CRPS values, one per element of `obs`.
#' @keywords internal
#' @noRd
score_crps <- function(obs, mu, sigma) {
  scoringRules::crps_norm(obs, mu, sigma)
}

#' Per-member cumulative accumulation
#'
#' For a cumulative quantity (e.g. multi-day rainfall totals from an
#' ensemble), daily percentiles cannot validly be summed across days (SCOPING
#' section 4): the correct accumulation is per member first, then quantiled.
#' This sums each member's trajectory across cases (rows).
#'
#' @param members A numeric matrix, rows = cases (e.g. days), columns =
#'   ensemble members.
#' @return A numeric vector of length `ncol(members)`: each member's total
#'   across all rows.
#' @keywords internal
#' @noRd
cumulative_by_member <- function(members) {
  colSums(members)
}

#' Skill score relative to a baseline
#'
#' `1 - score / baseline`, so `0` at parity with the baseline, positive when
#' `score` is better (lower, for an error-type score) than `baseline`, and
#' negative when worse.
#'
#' @param score Numeric, the candidate score (e.g. RMSE).
#' @param baseline Numeric, the baseline's score.
#' @return A numeric skill score.
#' @keywords internal
#' @noRd
skill_score <- function(score, baseline) {
  1 - score / baseline
}
