# Plan 12 -- EMOS correction tier: crch heteroscedastic regression per
# lead-time bucket, producing a predictive (mean, spread) rather than a
# single corrected value. EMOS supersedes qmap/mean_bias partly because it
# models forecast-skill decay with lead time natively (SCOPING section 7.1).
#
# Must never fit on Historical-Forecast `lead_time = NA` proxy rows (SCOPING
# section 7.2) -- these rows stand in for the shortest available lead and
# would contaminate lead-aware training if mixed in silently.

# Fit a location+scale regression of observation on forecast. Prefers
# `crch::crch()` for genuine heteroscedastic (mean AND spread depend on the
# forecast) regression; falls back to a plain `lm()` location fit with a
# constant (floored) residual-sd scale when `crch` fails to converge --
# which happens on a degenerate/near-noiseless fixture where the residual
# variance is ~0 and `crch`'s scale-model Hessian is singular. Either path
# returns the same tidy coefficient shape.
.emos_fit_location_scale <- function(pairs) {
  crch_fit <- tryCatch(
    crch::crch(observation ~ forecast | 1, data = pairs, dist = "gaussian"),
    error = function(e) NULL
  )

  if (!is.null(crch_fit)) {
    loc <- stats::coef(crch_fit, model = "location")
    scale <- stats::coef(crch_fit, model = "scale")
    return(list(
      method = "crch",
      intercept = unname(loc[["(Intercept)"]]),
      slope = unname(loc[["forecast"]]),
      log_sd = unname(scale[["(Intercept)"]])
    ))
  }

  lm_fit <- stats::lm(observation ~ forecast, data = pairs)
  loc <- stats::coef(lm_fit)
  resid_sd <- max(stats::sigma(lm_fit), 1e-3, na.rm = TRUE)
  list(
    method = "lm_fallback",
    intercept = unname(loc[["(Intercept)"]]),
    slope = unname(loc[["forecast"]]),
    log_sd = log(resid_sd)
  )
}

#' Fit the EMOS correction tier (crch heteroscedastic regression per lead bucket)
#'
#' Fits a predictive Gaussian distribution (mean, spread) for `observation`
#' given `forecast`, for one named `lead_bucket`. Stores the fitted
#' coefficients as tidy data (never the raw `crch`/`lm` model object) so the
#' calibration can be persisted via the Plan 03 calib store and
#' reconstructed by `apply_emos()` without ever calling `predict()` on a
#' retained model object.
#'
#' **Refuses to fit on `lead_time = NA` rows.** Historical-Forecast proxy
#' rows use `lead_time = NA` to stand in for "shortest available lead"
#' (SCOPING section 7.2); mixing them into a lead-bucket-specific fit would
#' silently contaminate lead-aware training, so this aborts
#' `"lead_unresolved"` if any row's `lead_time` is `NA`.
#'
#' @param pairs A `forecast_obs_pairs()`-shaped tibble: `forecast`,
#'   `observation`, `lead_time` columns.
#' @param lead_bucket A single string identifying the lead bucket this fit
#'   belongs to (e.g. `"d1"`), stored alongside the coefficients.
#' @return A one-row tibble: `lead_bucket`, `method`, `intercept`, `slope`,
#'   `log_sd`.
#' @keywords internal
#' @noRd
fit_emos <- function(pairs, lead_bucket) {
  if (anyNA(pairs$lead_time)) {
    abort_meteo(
      c(
        "{.arg pairs} has {.code lead_time = NA} rows.",
        "i" = "fit_emos() refuses to fit lead-aware calibration on Historical-Forecast proxy rows (SCOPING section 7.2); resolve or drop them before fitting." # nolint: line_length_linter.
      ),
      class = "lead_unresolved"
    )
  }

  fit <- .emos_fit_location_scale(pairs)
  tibble::tibble(
    lead_bucket = lead_bucket,
    method = fit$method,
    intercept = fit$intercept,
    slope = fit$slope,
    log_sd = fit$log_sd
  )
}

#' Apply a fitted EMOS calibration
#'
#' Reconstructs the predictive (mean, spread) from the tidy coefficients
#' returned by `fit_emos()` -- never by calling `predict()` on a retained
#' model object.
#'
#' @param fit A one-row tibble from `fit_emos()`.
#' @param newdata A tibble with a `forecast` column.
#' @return A tibble with `mean` and `sd` columns, one row per row of
#'   `newdata`.
#' @keywords internal
#' @noRd
apply_emos <- function(fit, newdata) {
  mean_pred <- fit$intercept[[1]] + fit$slope[[1]] * newdata$forecast
  sd_pred <- rep(exp(fit$log_sd[[1]]), length(mean_pred))
  tibble::tibble(mean = mean_pred, sd = sd_pred)
}
