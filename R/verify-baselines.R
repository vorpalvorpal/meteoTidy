# Plan 13 -- baselines: raw model, persistence, climatology (SCOPING section
# 7.4). Without these, "correction beats raw" can't be told apart from "a
# lead where climatology already wins" (the review point exercised in
# test-verify-baselines.R). The raw-model baseline is just the uncorrected
# `forecast` column already present in a pairs tibble, so it needs no helper
# here.

#' Persistence baseline: last observation carried forward
#'
#' The naive forecast for step `t` is `obs[t - 1]`. There is no prior
#' observation for the first element, so it is `NA_real_` (documented,
#' rather than guessed as e.g. `obs[1]`, since there genuinely is no prior
#' value to persist).
#'
#' @param obs Numeric vector of observations, in time order.
#' @return A numeric vector the same length as `obs`; element 1 is
#'   `NA_real_`, element `t > 1` is `obs[t - 1]`.
#' @keywords internal
#' @noRd
baseline_persistence <- function(obs) {
  n <- length(obs)
  if (n == 0) {
    return(numeric(0))
  }
  c(NA_real_, obs[-n])
}

#' Climatology baseline from a `history_daily`-shaped long tibble
#'
#' The climatological mean (and spread) for `target`'s day-of-year, pooled
#' over a +/- `window_days` window around that day-of-year across all years
#' in `hist` (a window rather than an exact day-of-year match gives a more
#' robust estimate from a few years of daily history; `window_days = 7` is a
#' documented default). Day-of-year distance wraps across the year boundary
#' (`366 - |d|`) so a target near Dec 31/Jan 1 pools correctly.
#'
#' When `hour_window` is supplied, the pool is *additionally* restricted to
#' rows within +/- `hour_window` hours of `target`'s hour-of-day (wrapping
#' across midnight, `24 - |dh|`), so a sub-daily climatology resolves the
#' diurnal cycle instead of collapsing every hour to the daily mean. This
#' only makes sense on an hourly `hist` (the `history_hourly` product); on a
#' daily `hist` the hour cell is (near-)empty and `n` comes back small.
#' Hour-of-day is taken in UTC; for a single fixed site UTC hour maps to a
#' fixed local hour (a ~1 h seasonal wobble across DST, immaterial at the
#' +/-1 h window the serve path uses).
#'
#' @param hist A long tibble with columns `datetime_utc`, `variable`,
#'   `value`, spanning multiple years (`history_daily`, or `history_hourly`
#'   when `hour_window` is used; Plan 10).
#' @param target A UTC POSIXct instant; its day-of-year (and, when
#'   `hour_window` is set, its hour-of-day) is used.
#' @param variable Variable name to filter `hist` on.
#' @param window_days Half-width, in days, of the day-of-year pooling
#'   window. Default `7`.
#' @param hour_window Optional half-width, in hours, of an additional
#'   hour-of-day pooling window. `NULL` (default) pools by day-of-year only
#'   (unchanged behaviour for existing callers).
#' @return A list with `mean`, `sd`, and `n` (the pooled sample size).
#' @keywords internal
#' @noRd
baseline_climatology <- function(hist, target, variable, window_days = 7, hour_window = NULL) {
  h <- hist[hist$variable == variable, , drop = FALSE]
  target_doy <- as.integer(format(target, "%j"))
  h_doy <- as.integer(format(h$datetime_utc, "%j"))
  d <- abs(h_doy - target_doy)
  d <- pmin(d, 366L - d)
  keep <- d <= window_days
  if (!is.null(hour_window)) {
    target_hod <- as.integer(format(target, "%H", tz = "UTC"))
    h_hod <- as.integer(format(h$datetime_utc, "%H", tz = "UTC"))
    dh <- abs(h_hod - target_hod)
    dh <- pmin(dh, 24L - dh)
    keep <- keep & dh <= hour_window
  }
  sel <- h[keep, , drop = FALSE]
  list(mean = mean(sel$value), sd = stats::sd(sel$value), n = nrow(sel))
}
