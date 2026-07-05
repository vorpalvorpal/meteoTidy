# Plan 09 -- spatial/buddy check against donor stations (the review's
# highest-value QC addition; the strongest available detector of slow sensor
# drift, which range/step/persistence all miss).
#
# `obs` is a single-variable, single-site long series (one row per
# datetime_utc); `donors` is a list of single-variable long series tibbles,
# one per donor station, at the SAME variable and (ideally) overlapping
# timestamps. For each timestamp, the donor consensus is the median of
# whatever donor values are available; the spread is the MAD (median
# absolute deviation) of those donor values, a robust spread estimator that
# is not thrown off by one bad donor the way an SD would be. The site value
# is flagged "suspect" when it deviates from the donor median by more than
# `k` MADs (k = 3.5, matching the conventional "modified z-score" outlier
# threshold, e.g. Iglewicz & Hoaglin 1993) -- comfortably wide enough not to
# fire on ordinary station-to-station noise, but tight enough to catch a
# slow additive drift once it has accumulated (see test-qc-spatial.R).
#
# Requires >= 2 usable donors at a timestamp (a single donor cannot
# distinguish "the site drifted" from "the one donor drifted"); with fewer,
# the rule is a no-op for that timestamp and logs "insufficient donors"
# rather than erroring.
#
# Never applies to a `measurability_class == "model_only"` variable (SCOPING
# section 7.3: no site truth exists for e.g. boundary_layer_height, so there
# is nothing to buddy-check against) -- this is enforced as a hard abort,
# not a silent no-op, so a caller that mistakenly tries never gets a falsely
# reassuring "ok" result.

.qc_spatial_k <- function() {
  3.5
}

# Robust MAD (median absolute deviation), scaled to be consistent with the
# standard deviation for normally-distributed data (the conventional
# `constant = 1.4826` scaling; see `stats::mad()`, which we reuse directly).
.qc_mad <- function(x) {
  stats::mad(x, na.rm = TRUE)
}

#' Spatial/buddy check against donor stations
#'
#' @param obs A single-variable canonical long series for the site under
#'   test (see `qc_series()` in `tests/testthat/helper-qc.R`).
#' @param donors A list of single-variable long series tibbles, one per donor
#'   station (see `qc_donor()`).
#' @param site A `met_site` object (used only for variable/measurability
#'   lookups here; spatial geometry, e.g. elevation bias adjustment, is left
#'   as a documented refinement -- see Details).
#' @return `obs` with `qc_flag` downgraded to `"suspect"` where the site
#'   deviates from the donor consensus by more than `k` MADs, and `qc_log`
#'   rows attached via `attr(out, "qc_log")` (including an "insufficient
#'   donors" note where fewer than two donors are usable).
#' @keywords internal
#' @noRd
qc_spatial <- function(obs, donors, site) {
  variable <- unique(obs$variable)
  if (length(variable) == 1) {
    row <- met_variable(variable)
    if (isTRUE(row$measurability_class == "model_only")) {
      abort_meteo(
        c(
          "{.fn qc_spatial} cannot buddy-check {.val {variable}}: it is a {.val model_only} variable.", # nolint: line_length_linter.
          "i" = "Model-only variables have no site truth to compare against neighbours (SCOPING section 7.3)." # nolint: line_length_linter.
        ),
        class = "spatial_not_applicable"
      )
    }
  }

  usable_donors <- Filter(function(d) nrow(d) > 0, donors)
  if (length(usable_donors) < 2) {
    log_rows <- .qc_log_rows(
      obs, seq_len(nrow(obs)), "spatial", "ok",
      "insufficient donors (< 2 usable) - spatial check skipped"
    )
    return(.qc_log_attach(obs, log_rows))
  }

  donor_long <- vctrs::vec_rbind(!!!usable_donors)
  k <- .qc_spatial_k()

  bad <- integer(0)
  for (i in seq_len(nrow(obs))) {
    t <- obs$datetime_utc[i]
    at_t <- donor_long$value[donor_long$datetime_utc == t]
    at_t <- at_t[!is.na(at_t)]
    if (length(at_t) < 2) next

    center <- stats::median(at_t)
    spread <- .qc_mad(at_t)
    if (spread == 0 || is.na(spread)) next

    deviation <- abs(obs$value[i] - center) / spread
    if (!is.na(deviation) && deviation > k) {
      bad <- c(bad, i)
    }
  }

  out <- .qc_downgrade(obs, bad, "suspect")
  log_rows <- .qc_log_rows(
    obs, bad, "spatial", "suspect",
    sprintf("site value deviates from donor consensus by > %.1f MAD", k)
  )
  .qc_log_attach(out, log_rows)
}
