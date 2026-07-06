# Plan 13 -- calibration diagnostics: PIT/rank histogram, spread-error ratio,
# Brier score + reliability (SCOPING section 7.4). CRPS alone conflates
# sharpness and reliability, so these are computed alongside it.

#' Rank (PIT) histogram for an ensemble forecast
#'
#' For each case (row), ranks the observed `truth` among that row's ensemble
#' members, giving a rank in `1..(m + 1)`; returns the histogram (bin counts)
#' of those ranks across all cases. A flat histogram indicates a calibrated
#' ensemble; a U-shape (mass piled in the end bins) indicates
#' under-dispersion; an inverted-U (hump in the middle) indicates
#' over-dispersion.
#'
#' @param ensemble_matrix A numeric matrix, `n` rows (cases) x `m` columns
#'   (ensemble members).
#' @param truth Numeric vector of length `n`, the observed value for each
#'   case.
#' @return An integer vector of length `m + 1`: bin counts of the rank of
#'   `truth` among its ensemble.
#' @keywords internal
#' @noRd
rank_histogram <- function(ensemble_matrix, truth) {
  m <- ncol(ensemble_matrix)
  n <- nrow(ensemble_matrix)
  ranks <- integer(n)
  for (i in seq_len(n)) {
    ranks[i] <- rank(c(truth[i], ensemble_matrix[i, ]), ties.method = "random")[1]
  }
  tabulate(ranks, nbins = m + 1L)
}

#' Flatness statistic for a rank/PIT histogram
#'
#' A Pearson chi-square-style goodness-of-fit statistic against a uniform
#' expected count: `sum((rank_hist - mean(rank_hist))^2) / mean(rank_hist)`.
#' Larger values indicate a less-flat (less calibrated) histogram; a
#' perfectly flat histogram scores `0`.
#'
#' @param rank_hist An integer/numeric vector of bin counts (see
#'   `rank_histogram()`).
#' @return A single numeric flatness statistic.
#' @keywords internal
#' @noRd
histogram_flatness <- function(rank_hist) {
  sum((rank_hist - mean(rank_hist))^2) / mean(rank_hist)
}

#' Spread-error ratio for an ensemble forecast
#'
#' Ensemble spread (finite-ensemble-corrected root-mean row variance, using
#' the standard `(m + 1) / m` small-sample correction for the
#' spread-skill relationship) divided by the RMSE of the ensemble mean
#' against `truth`. Close to `1` for a well-calibrated ensemble; `< 1`
#' indicates under-dispersion, `> 1` over-dispersion.
#'
#' @param ensemble_matrix A numeric matrix, `n` rows (cases) x `m` columns
#'   (ensemble members).
#' @param truth Numeric vector of length `n`, the observed value for each
#'   case.
#' @return A single numeric ratio.
#' @keywords internal
#' @noRd
spread_error_ratio <- function(ensemble_matrix, truth) {
  m <- ncol(ensemble_matrix)
  row_var <- apply(ensemble_matrix, 1, stats::var)
  row_mean <- rowMeans(ensemble_matrix)
  spread <- sqrt((1 + 1 / m) * mean(row_var))
  rmse <- sqrt(mean((row_mean - truth)^2))
  spread / rmse
}

#' Brier score for a probabilistic binary forecast
#'
#' `mean((prob - outcome)^2)` -- the mean squared error of a probability
#' forecast (e.g. probability of precipitation) against the 0/1 outcome.
#'
#' @param prob Numeric vector of forecast probabilities in `[0, 1]`.
#' @param outcome Numeric/integer vector of 0/1 realised outcomes, same
#'   length as `prob`.
#' @return A single numeric Brier score.
#' @keywords internal
#' @noRd
brier_score <- function(prob, outcome) {
  mean((prob - outcome)^2)
}

#' Reliability table for a probabilistic binary forecast
#'
#' Bins `prob` into `bins` equal-width bins over `[0, 1]` and, for each bin,
#' reports the bin midpoint and the observed frequency of `outcome == 1`
#' within that bin. A reliable forecast has `observed_freq` close to
#' `bin_mid` in every bin.
#'
#' @param prob Numeric vector of forecast probabilities in `[0, 1]`.
#' @param outcome Numeric/integer vector of 0/1 realised outcomes, same
#'   length as `prob`.
#' @param bins Integer number of equal-width bins. Default `5`.
#' @return A tibble with one row per bin: `bin_mid` (numeric bin midpoint)
#'   and `observed_freq` (mean `outcome` within the bin; `NA` for an empty
#'   bin).
#' @keywords internal
#' @noRd
reliability_table <- function(prob, outcome, bins = 5) {
  breaks <- seq(0, 1, length.out = bins + 1)
  bin_id <- cut(prob, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  mids <- (breaks[-length(breaks)] + breaks[-1]) / 2

  observed_freq <- vapply(seq_len(bins), function(b) {
    in_bin <- !is.na(bin_id) & bin_id == b
    if (!any(in_bin)) {
      return(NA_real_)
    }
    mean(outcome[in_bin])
  }, numeric(1))

  tibble::tibble(bin_mid = mids, observed_freq = observed_freq)
}
