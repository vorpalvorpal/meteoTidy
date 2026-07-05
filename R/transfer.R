# Plan 10 -- the shared transfer engine (SCOPING section 6).
#
# `fit_transfer()`/`apply_transfer()` are the single statistical primitive
# both gap-fill (this plan) and forecast correction (Plan 12) build on: fit a
# bias-correction transform from a `source_series` onto a `target_series`
# over their shared/overlapping window, then apply it to (possibly a
# different slice of) the source series.
#
# The engine assumes both series are REALISED observations -- there is no
# lead time, no forecast skill decay, and therefore no shrinkage anywhere in
# this file. `apply_transfer()`'s signature is a frozen contract Plan 12
# relies on: it wraps this engine to add lead-dependent shrinkage for
# forecasts, but the primitive itself must never grow a lead/weight/shrink
# argument (see test-transfer.R's "skill-decay-free invariant" block). This
# is the review's "gap-fill and forecast correction are the opposite
# direction, not the same problem" distinction made concrete: applying the
# same fitted transfer to an early or a late row of the same source series
# must yield an identical correction.

# Join `source` and `target` single-variable long obs tibbles on
# `datetime_utc` (their shared window), returning a data frame with
# `source_value`/`target_value` columns, one row per timestamp present in
# both.
.transfer_overlap <- function(source, target) {
  m <- match(source$datetime_utc, target$datetime_utc)
  has_match <- !is.na(m)
  data.frame(
    datetime_utc = source$datetime_utc[has_match],
    source_value = source$value[has_match],
    target_value = target$value[m[has_match]]
  )
}

# Fit a constant additive offset: target = source - offset, i.e.
# offset = mean(source) - mean(target) over the overlap, so that
# `source - offset` recovers `target` in expectation.
.fit_mean_bias <- function(overlap) {
  offset <- mean(overlap$source_value, na.rm = TRUE) - mean(overlap$target_value, na.rm = TRUE)
  list(method = "mean_bias", offset = offset)
}

.apply_mean_bias <- function(transfer, values) {
  values - transfer$offset
}

# Fit an empirical quantile-mapping transform: record the source and target
# empirical quantiles (at a fixed probability grid) over the overlap.
# Applying the transform interpolates a new value's rank in the source
# quantile grid, then maps that rank onto the target quantile grid via
# `stats::approx()` -- a standard hand-rolled empirical QM (no `qmap`
# dependency needed; house style prefers avoiding a new heavy dependency when
# a `stats::approx()`-based implementation is simple and sufficient, see
# plans/10-curation-gapfill.md).
.qmap_probs <- function() {
  seq(0, 1, by = 0.01)
}

.fit_qmap <- function(overlap) {
  probs <- .qmap_probs()
  source_q <- stats::quantile(overlap$source_value, probs = probs, na.rm = TRUE, names = FALSE)
  target_q <- stats::quantile(overlap$target_value, probs = probs, na.rm = TRUE, names = FALSE)
  list(
    method = "qmap",
    probs = probs,
    source_quantiles = source_q,
    target_quantiles = target_q
  )
}

.apply_qmap <- function(transfer, values) {
  # Rank each value against the fitted source quantile grid (0..1), then
  # map that rank onto the target quantile grid. Values outside the fitted
  # source range extrapolate via the endpoint rule (`rule = 2`: clamp to the
  # nearest fitted quantile) rather than producing NA.
  rank <- stats::approx(
    x = transfer$source_quantiles, y = transfer$probs,
    xout = values, rule = 2, ties = "ordered"
  )$y
  stats::approx(
    x = transfer$probs, y = transfer$target_quantiles,
    xout = rank, rule = 2, ties = "ordered"
  )$y
}

#' Fit a transfer (bias-correction) transform between two realised series
#'
#' Fits a transform that maps `source_series` onto `target_series` over their
#' shared/overlapping window (matched on `datetime_utc`). Both arguments are
#' single-variable canonical long obs tibbles. This is the shared primitive
#' behind gap-fill's donor bias-correction and Plan 12's forecast correction
#' tiers; **it assumes both series are realised observations, over the same
#' physical window, with no forecast lead time involved** -- there is no
#' skill-decay or shrinkage concept here (Plan 12 adds that as a wrapper).
#'
#' @param source A single-variable canonical long obs tibble (the series
#'   being corrected, e.g. a donor station).
#' @param target A single-variable canonical long obs tibble (the series
#'   being corrected towards, e.g. the site's own record).
#' @param method Either `"mean_bias"` (a constant additive offset) or
#'   `"qmap"` (empirical quantile mapping, correcting the full distribution
#'   shape, not just the mean).
#' @param by Optional grouping column name(s) in both series (e.g. an hour-
#'   of-day block) to fit the transform conditionally. `NULL` (default) fits
#'   one unconditional transform over the whole overlap.
#' @param treatment Optional name of the per-variable statistical space the
#'   transform should be fit in (see `R/fill-treatments.R`); `NULL` (default)
#'   fits directly on `value`. Reserved for callers that have already
#'   converted `source`/`target` into the right space themselves.
#' @return A plain list of fitted parameters (never an `.rds` model object):
#'   `method`, plus `offset` (mean_bias) or `probs`/`source_quantiles`/
#'   `target_quantiles` (qmap).
#' @family transfer
#' @export
#' @examples
#' target <- data.frame(
#'   site_id = "test",
#'   datetime_utc = seq(as.POSIXct("2026-01-01", tz = "UTC"), by = "hour", length.out = 10),
#'   variable = "temperature_2m", value = seq(10, 19),
#'   source = "site", method = "measured", qc_flag = "ok"
#' )
#' source_series <- target
#' source_series$value <- target$value + 2
#' tr <- fit_transfer(source_series, target, method = "mean_bias")
#' apply_transfer(tr, source_series)
fit_transfer <- function(source, target, method = c("mean_bias", "qmap"),
                         by = NULL, treatment = NULL) {
  method <- rlang::arg_match(method)
  overlap <- .transfer_overlap(source, target)

  if (nrow(overlap) == 0) {
    abort_meteo(
      c(
        "{.arg source} and {.arg target} have no overlapping timestamps.",
        "i" = "fit_transfer() needs at least one shared datetime_utc to fit against."
      ),
      class = "transfer_no_overlap"
    )
  }

  transfer <- switch(method,
    mean_bias = .fit_mean_bias(overlap),
    qmap = .fit_qmap(overlap)
  )
  transfer$by <- by
  transfer$treatment <- treatment
  transfer
}

#' Apply a fitted transfer to a source series
#'
#' Applies a transform fitted by [fit_transfer()] to `source_series` (which
#' may be a different slice, or a different but comparable series, than the
#' one the transform was fit on). Returns `source_series` with `value`
#' corrected; every other column is unchanged.
#'
#' **Frozen contract:** this function has no `lead`/`lead_time`/`weight`/
#' `shrink`/`shrinkage` argument, and never will -- the transfer engine is
#' skill-decay-free by design (SCOPING section 6). Applying the same fitted
#' transfer to an early or a late row of the same source series yields an
#' identical correction; Plan 12 adds lead-dependent shrinkage as a wrapper
#' around this primitive, never inside it.
#'
#' @param transfer A transform fitted by [fit_transfer()].
#' @param source_series A single-variable canonical long obs tibble to
#'   correct.
#' @return `source_series` with `value` replaced by the corrected values.
#' @family transfer
#' @export
#' @examples
#' target <- data.frame(
#'   site_id = "test",
#'   datetime_utc = seq(as.POSIXct("2026-01-01", tz = "UTC"), by = "hour", length.out = 10),
#'   variable = "temperature_2m", value = seq(10, 19),
#'   source = "site", method = "measured", qc_flag = "ok"
#' )
#' source_series <- target
#' source_series$value <- target$value + 2
#' tr <- fit_transfer(source_series, target, method = "mean_bias")
#' apply_transfer(tr, source_series)
apply_transfer <- function(transfer, source_series) {
  values <- switch(transfer$method,
    mean_bias = .apply_mean_bias(transfer, source_series$value),
    qmap = .apply_qmap(transfer, source_series$value)
  )
  source_series$value <- values
  source_series
}
