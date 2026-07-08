# Plan 13 -- moving-block bootstrap for CIs on score differences (SCOPING
# section 7.4). Verification series (score differences over time) are
# autocorrelated, so a naive iid bootstrap understates uncertainty; a
# moving-block bootstrap preserves short-range dependence within each
# resampled block.

#' Moving-block bootstrap confidence interval for the mean of a series
#'
#' Resamples overlapping blocks of length `block_len` (with replacement)
#' from `x`, concatenates enough blocks to cover `length(x)`, and computes
#' the mean of each of `R` bootstrap replicates. The `conf` empirical
#' quantiles of that replicate-mean distribution are returned as the CI;
#' `block_len = 1` degenerates to an ordinary iid bootstrap (no
#' autocorrelation preserved), which is deliberately narrower on an
#' autocorrelated series than a genuine block bootstrap.
#'
#' @param x Numeric vector, the score-difference series (e.g. corrected -
#'   incumbent, or model - baseline).
#' @param block_len Integer block length (number of consecutive
#'   observations per block).
#' @param R Integer number of bootstrap replicates.
#' @param seed Integer seed, set internally so results are reproducible for
#'   a given `seed`.
#' @param conf Confidence level for the interval. Default `0.95`.
#' @return A list with `ci` (a length-2 numeric vector, lower/upper) and
#'   `significant` (logical: `TRUE` iff the CI excludes zero, i.e. both
#'   bounds share the same sign).
#' @keywords internal
#' @noRd
block_bootstrap_ci <- function(x, block_len, R, seed, conf = 0.95) {
  n <- length(x)
  block_len <- max(1L, as.integer(block_len))
  n_blocks <- ceiling(n / block_len)
  starts_max <- max(1L, n - block_len + 1L)

  withr::local_seed(seed)

  rep_means <- numeric(R)
  for (r in seq_len(R)) {
    starts <- sample.int(starts_max, n_blocks, replace = TRUE)
    idx <- unlist(lapply(starts, function(s) s:min(s + block_len - 1L, n)))
    idx <- idx[seq_len(n)]
    rep_means[r] <- mean(x[idx])
  }

  alpha <- 1 - conf
  ci <- stats::quantile(rep_means, probs = c(alpha / 2, 1 - alpha / 2), names = FALSE)
  significant <- (ci[1] > 0 && ci[2] > 0) || (ci[1] < 0 && ci[2] < 0)

  list(ci = ci, significant = significant)
}
