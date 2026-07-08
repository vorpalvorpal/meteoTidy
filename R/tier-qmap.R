# Plan 12 -- qmap correction tier: empirical quantile mapping with two review
# fixes (plans/12-correction-fitted-tiers.md):
#
#  - cross-season pooling: a group (e.g. season) with too little/no training
#    data borrows the unconditional pooled map rather than going uncorrected;
#  - an explicit tail policy: beyond the training quantile support, extrapolate
#    by a CONSTANT ADDITIVE SHIFT (the shift implied at the nearest trained
#    quantile), never `stats::approx(rule = 2)`'s clamped-output behaviour,
#    which would make the correction insensitive to how far out-of-range the
#    input is.
#
# `fit_qmap()` returns tidy data (never an `.rds` model object): one row per
# fitted group (plus a `"__pooled__"` row for the unconditional map) with the
# quantile grid and the tail-shift constants baked in.

.qmap_probs <- function() {
  seq(0, 1, by = 0.01)
}

# Fit one empirical QM map (probs / source(forecast) quantiles / target
# (observation) quantiles) plus its tail-shift constants, from a single
# vector of forecast/observation pairs.
.qmap_fit_one <- function(forecast, observation, group) {
  probs <- .qmap_probs()
  source_q <- stats::quantile(forecast, probs = probs, na.rm = TRUE, names = FALSE)
  target_q <- stats::quantile(observation, probs = probs, na.rm = TRUE, names = FALSE)

  # The shift implied at the boundary quantiles: how much the mapping adjusts
  # the most extreme trained forecast value. Used to extrapolate beyond the
  # training support by a constant additive shift rather than clamping.
  shift_hi <- target_q[[length(target_q)]] - source_q[[length(source_q)]]
  shift_lo <- target_q[[1]] - source_q[[1]]

  tibble::tibble(
    group = group,
    n = length(forecast),
    probs = list(probs),
    source_quantiles = list(source_q),
    target_quantiles = list(target_q),
    shift_hi = shift_hi,
    shift_lo = shift_lo
  )
}

#' Fit the qmap correction tier (empirical quantile mapping)
#'
#' Fits an empirical quantile-mapping transform from `pairs$forecast` onto
#' `pairs$observation`. Always fits an unconditional **pooled** map over all
#' rows; when `by` names a grouping column, additionally fits one map per
#' group. A group with too few observations is still represented in the
#' returned tibble, but `apply_qmap()` falls back to the pooled map for any
#' group whose own map does not exist or is too sparse to trust (the
#' cross-season-pooling review fix -- an untrained season is corrected via
#' the pooled base rather than left unmapped).
#'
#' @param pairs A tibble with `forecast`, `observation`, and (if `by` is
#'   supplied) the named grouping column.
#' @param by Optional single column name (e.g. `"season"`) to fit per-group
#'   maps in addition to the pooled map. `NULL` (default) fits only the
#'   pooled map.
#' @return A tidy tibble, one row per fitted map (`group == "__pooled__"`
#'   for the unconditional map, plus one row per level of `by` when
#'   supplied), with list-columns `probs`/`source_quantiles`/
#'   `target_quantiles` and scalar `shift_hi`/`shift_lo` tail-shift
#'   constants. Never an `.rds` model object.
#' @keywords internal
#' @noRd
fit_qmap <- function(pairs, by = NULL) {
  pooled <- .qmap_fit_one(pairs$forecast, pairs$observation, group = "__pooled__")

  if (is.null(by)) {
    out <- pooled
  } else {
    groups <- unique(pairs[[by]])
    per_group <- lapply(groups, function(g) {
      rows <- pairs[[by]] == g
      .qmap_fit_one(pairs$forecast[rows], pairs$observation[rows], group = as.character(g))
    })
    out <- vctrs::vec_rbind(pooled, !!!per_group)
  }

  attr(out, "by") <- by
  out
}

# Minimum group sample size below which apply_qmap() falls back to the
# pooled map rather than trusting a sparse/absent per-group fit.
.qmap_min_group_n <- 20L

# Map a single numeric vector of forecast values through one fitted map
# (a one-row slice of fit_qmap()'s output), applying the constant-shift tail
# policy beyond the trained quantile support.
.qmap_apply_one <- function(map_row, values) {
  probs <- map_row$probs[[1]]
  source_q <- map_row$source_quantiles[[1]]
  target_q <- map_row$target_quantiles[[1]]

  rank <- stats::approx(x = source_q, y = probs, xout = values, rule = 2, ties = "ordered")$y
  mapped <- stats::approx(x = probs, y = target_q, xout = rank, rule = 2, ties = "ordered")$y

  max_src <- source_q[[length(source_q)]]
  min_src <- source_q[[1]]
  above <- values > max_src
  below <- values < min_src
  mapped[above] <- values[above] + map_row$shift_hi
  mapped[below] <- values[below] + map_row$shift_lo
  mapped
}

#' Apply a fitted qmap calibration
#'
#' Applies the empirical quantile-mapping transform from `fit_qmap()` to
#' `newdata$forecast`. When the fit has per-group maps (`by` was supplied),
#' each row is corrected via its own group's map, falling back to the
#' pooled map when the row's group has no fitted map or too few training
#' observations (`< 20`) to trust -- the cross-season-pooling fix. Beyond
#' the training quantile support, the correction is a bounded constant
#' shift (never `NA`, never unbounded).
#'
#' @param coeffs A tibble from `fit_qmap()`.
#' @param newdata A tibble with a `forecast` column (and, if the fit used
#'   `by`, the same grouping column).
#' @return A plain numeric vector of corrected values (not a tibble).
#' @keywords internal
#' @noRd
apply_qmap <- function(coeffs, newdata) {
  by <- attr(coeffs, "by")
  pooled_row <- coeffs[coeffs$group == "__pooled__", , drop = FALSE]

  if (is.null(by) || !(by %in% names(newdata))) {
    return(.qmap_apply_one(pooled_row, newdata$forecast))
  }

  groups <- newdata[[by]]
  out <- rep(NA_real_, nrow(newdata))
  for (g in unique(groups)) {
    rows <- groups == g
    group_row <- coeffs[coeffs$group == as.character(g), , drop = FALSE]
    use_row <- if (nrow(group_row) == 1 && group_row$n[[1]] >= .qmap_min_group_n) {
      group_row
    } else {
      pooled_row
    }
    out[rows] <- .qmap_apply_one(use_row, newdata$forecast[rows])
  }
  out
}
