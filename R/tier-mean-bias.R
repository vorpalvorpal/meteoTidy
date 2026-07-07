# Plan 12 -- mean_bias correction tier: harmonic day-of-year + hour-of-day
# covariates (the review's fix to a raw hour-of-day-bin fit, which cannot
# represent a bias that flips sign summer<->winter), plus annual-harmonic
# shrinkage when the training overlap is short (a partial-year fit
# unregularised extrapolates the annual sinusoid into unobserved seasons and
# can produce a larger, wrong-signed correction there than a plain constant
# -- SCOPING section 7.1's review fix, plans/12-correction-fitted-tiers.md).
#
# `fit_mean_bias()`/`apply_mean_bias()` persist as tidy data (a coefficients
# tibble), never an `lm` model object, matching the "calibrations as data"
# contract shared with the Plan 03 calib store.

# Build the harmonic design-matrix columns (sin/cos doy + sin/cos hod) for a
# vector of POSIXct times. `n_harmonics` only modulates the number of ANNUAL
# (day-of-year) harmonics; hour-of-day always gets exactly one harmonic (a
# month of data already contains ~30 full diurnal cycles, so it needs no
# shrinkage and no extra flexibility -- plans/12-correction-fitted-tiers.md).
.mean_bias_harmonics <- function(time, n_harmonics) {
  doy <- as.numeric(format(time, "%j")) + as.numeric(format(time, "%H")) / 24
  hod <- as.numeric(format(time, "%H")) + as.numeric(format(time, "%M")) / 60

  out <- list()
  for (k in seq_len(n_harmonics)) {
    out[[paste0("sin_doy", k)]] <- sin(2 * pi * k * doy / 365.25)
    out[[paste0("cos_doy", k)]] <- cos(2 * pi * k * doy / 365.25)
  }
  out[["sin_hod"]] <- sin(2 * pi * hod / 24)
  out[["cos_hod"]] <- cos(2 * pi * hod / 24)
  tibble::as_tibble(out)
}

# The seasonal/diurnal bias of a forecast is a property of the time the
# value is ABOUT (valid time), not when it was issued -- for a daily-lead
# forecast those differ by the lead, so keying the harmonics on issue_time
# would fit the diurnal term on the wrong clock (Plan 17 item 7). `df` may be
# a pairs tibble (has both) or a record-correction newdata frame (issue_time
# only, since a record correction's "issue" and "valid" instant are the
# same observation time) -- fall back to issue_time when valid_time is absent.
.mean_bias_time <- function(df) {
  if ("valid_time" %in% names(df)) df$valid_time else df$issue_time
}

# The seasonal-coverage fraction of `time`: the day-of-year span actually
# observed, relative to a full annual cycle. A full year (or more) gives
# 1; a single day gives ~1/365.25.
.mean_bias_coverage_fraction <- function(time) {
  doy <- as.numeric(format(time, "%j"))
  span <- max(doy) - min(doy) + 1
  min(1, span / 365.25)
}

# Damping factor applied to the annual-harmonic amplitudes as a function of
# seasonal coverage. At full-year coverage (fraction 1) damping is 1 (no
# shrinkage). At partial coverage the annual harmonic is damped roughly in
# proportion to how much of the cycle was actually observed, so a fit on a
# few months contributes only a mild tilt rather than extrapolating a full
# unregularised sinusoid into untrained seasons.
.mean_bias_damping <- function(coverage_fraction) {
  min(1, coverage_fraction * 2)
}

#' Fit the mean-bias correction tier (harmonic day-of-year + hour-of-day)
#'
#' Fits `resid = observation - forecast` as a harmonic regression on
#' day-of-year (up to `n_harmonics` annual harmonics) plus a single
#' hour-of-day harmonic. Returns a tidy tibble of fitted coefficients (never
#' an `lm` object) so the calibration can be persisted as data via the
#' Plan 03 calib store.
#'
#' **Annual-harmonic shrinkage.** When `shrink = TRUE` (the default) and the
#' training pairs cover less than a full annual cycle, the day-of-year
#' harmonic amplitudes are damped toward zero in proportion to the observed
#' seasonal coverage fraction (a full year gives damping 1, i.e. no
#' shrinkage). This is the review's fix for a one/two-season fit that would
#' otherwise extrapolate a wrong-signed correction into the untrained
#' opposite season (plans/12-correction-fitted-tiers.md). Hour-of-day
#' harmonics are never shrunk -- even a single month contains dozens of full
#' diurnal cycles.
#'
#' @param pairs A `forecast_obs_pairs()`-shaped tibble: `issue_time`,
#'   `forecast`, `observation` columns (see `tests/testthat/helper-correct.R`),
#'   and, when available, `valid_time` -- the harmonics are keyed on
#'   `valid_time` (the time the value is ABOUT), falling back to `issue_time`
#'   only when `valid_time` is absent.
#' @param n_harmonics Number of annual (day-of-year) harmonics to fit.
#'   Default `2`.
#' @param shrink Whether to shrink the annual-harmonic amplitudes toward
#'   zero when the training overlap is short. Default `TRUE`.
#' @return A one-row tibble of named coefficients (`(Intercept)`,
#'   `sin_doy1`, `cos_doy1`, ..., `sin_hod`, `cos_hod`), plus `n_harmonics`
#'   and `coverage_fraction` bookkeeping columns.
#' @keywords internal
#' @noRd
fit_mean_bias <- function(pairs, n_harmonics = 2, shrink = TRUE) {
  time <- .mean_bias_time(pairs)
  resid <- pairs$observation - pairs$forecast
  harmonics <- .mean_bias_harmonics(time, n_harmonics)

  design <- harmonics
  design$resid <- resid
  fit <- stats::lm(resid ~ ., data = design)

  coefs <- stats::coef(fit)
  coefs[is.na(coefs)] <- 0 # perfectly collinear terms (e.g. no hour variation)

  coverage_fraction <- .mean_bias_coverage_fraction(time)
  if (shrink) {
    damping <- .mean_bias_damping(coverage_fraction)
    annual_names <- grep("^(sin|cos)_doy", names(coefs), value = TRUE)
    coefs[annual_names] <- coefs[annual_names] * damping
  }

  coeffs_list <- as.list(coefs)
  names(coeffs_list) <- names(coefs)
  coeffs_list$n_harmonics <- n_harmonics
  coeffs_list$coverage_fraction <- coverage_fraction
  tibble::as_tibble(coeffs_list)
}

#' Apply a fitted mean-bias calibration
#'
#' Evaluates the harmonic fit from `fit_mean_bias()` at each row's
#' `valid_time` (falling back to `issue_time` when absent) and returns
#' `newdata` with a `value` column: the corrected forecast
#' (`forecast + predicted_bias`).
#'
#' @param coeffs A coefficients tibble from `fit_mean_bias()`.
#' @param newdata A tibble with `forecast` and, ideally, `valid_time`
#'   columns (`issue_time` as a fallback).
#' @return `newdata` with a `value` column added (the corrected forecast).
#' @keywords internal
#' @noRd
apply_mean_bias <- function(coeffs, newdata) {
  n_harmonics <- coeffs$n_harmonics[[1]]
  harmonics <- .mean_bias_harmonics(.mean_bias_time(newdata), n_harmonics)

  intercept <- if ("(Intercept)" %in% names(coeffs)) coeffs[["(Intercept)"]][[1]] else 0
  pred_bias <- rep(intercept, nrow(harmonics))
  for (col in names(harmonics)) {
    coef_val <- if (col %in% names(coeffs)) coeffs[[col]][[1]] else 0
    pred_bias <- pred_bias + coef_val * harmonics[[col]]
  }

  newdata$value <- newdata$forecast + pred_bias
  newdata
}
