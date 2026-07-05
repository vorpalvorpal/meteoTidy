# Plan 11 -- tier_select(): the data-availability gate AND the skill gate
# (SCOPING section 7.1, with the review's skill-gate fix). BOTH gates must
# pass to use a higher tier than the data-availability gate alone would
# reach: data volume alone never demonstrates a complex method stopped
# overfitting; only Plan 13's out-of-sample skill verdict can promote past
# the incumbent.

# Month/pair thresholds for the data-availability gate. Chosen to land the
# four concrete cases in test-tier-select.R:
#   overlap_months = 0             -> below mean_bias_months -> "physical"
#   overlap_months = 3             -> in [mean_bias_months, qmap_months)  -> "mean_bias"
#   overlap_months = 12            -> in [qmap_months, emos_months)       -> "qmap"
#   overlap_months = 30, archive   -> >= emos_months & has_archive        -> "emos"
# matching the plan's stated bands ("<overlap for mean_bias" / "1-6mo" /
# "6mo-2yr" / ">=2yr+archive").
.tier_thresholds <- function() {
  list(mean_bias_months = 1, qmap_months = 6, emos_months = 24)
}

# SCOPING section 7.2: for Open-Meteo daily-lead forecasts, forecast/truth
# pairs exist from day 0 via the Previous Runs archive (trained against
# `history_daily` pseudo-truth), not from waiting for real-time overlap to
# accrue. So for this path the availability gate keys off `n_pairs`/
# `has_archive` rather than `overlap_months`. ~365 daily pairs (a year) is
# the "roughly 2 years of daily pairs" threshold's conservative floor for
# reaching `emos` immediately; test-tier-select.R exercises `n_pairs = 700`
# (~2 years) with `overlap_months = 0`, which would otherwise gate to
# "physical".
.daily_lead_pseudo_truth_pairs_floor <- function() {
  365
}

# The data-availability gate alone: which tier does training_summary's
# overlap/pairs/archive info support, ignoring the skill gate entirely?
.tier_availability_gate <- function(training_summary) {
  thresholds <- .tier_thresholds()

  is_daily_lead_pseudo_truth <-
    isTRUE(training_summary$truth_source == "history_daily") &&
    isTRUE(training_summary$n_pairs >= .daily_lead_pseudo_truth_pairs_floor()) &&
    isTRUE(training_summary$has_archive)
  if (is_daily_lead_pseudo_truth) {
    return("emos") # SCOPING section 7.2: day-0 daily-lead pairs via Previous Runs
  }

  months <- training_summary$overlap_months
  if (months >= thresholds$emos_months && isTRUE(training_summary$has_archive)) {
    return("emos")
  }
  if (months >= thresholds$qmap_months) {
    return("qmap")
  }
  if (months >= thresholds$mean_bias_months) {
    return("mean_bias")
  }
  "physical"
}

#' Select the correction tier for a site, source, variable, and lead bucket
#'
#' Chooses one of [TIER_LEVELS] via two gates that **both** must pass to use
#' a tier higher than `"physical"` (SCOPING section 7.1, with the review's
#' skill-gate fix):
#'
#' 1. **Data-availability gate** -- from `training_summary`'s
#'    `overlap_months`/`n_pairs`/`has_archive`/`truth_source`: below one
#'    month of overlap stays at `"physical"`; 1-6 months reaches
#'    `"mean_bias"`; 6 months-2 years reaches `"qmap"`; 2+ years with an
#'    archive reaches `"emos"`. **Special case (SCOPING section 7.2):** for
#'    Open-Meteo daily-lead forecasts, forecast/truth pairs exist from day 0
#'    via the Previous Runs archive trained against `history_daily`
#'    pseudo-truth -- so a large `n_pairs` (>= ~365, roughly a year of daily
#'    pairs) with `has_archive = TRUE` and
#'    `truth_source == "history_daily"` reaches `"emos"` immediately, even
#'    with `overlap_months == 0`.
#' 2. **Skill gate** -- Plan 13's `skill_verdict$promote` must be `TRUE` to
#'    actually use the tier the data-availability gate reached. If `FALSE`,
#'    the selection stays one rung below the data-gate's answer (the
#'    "incumbent" tier) rather than dropping all the way to `"physical"`:
#'    "data volume alone never demonstrates the complex method stopped
#'    overfitting" reads as "don't promote past what's already proven", not
#'    "erase all progress". (Plan 12's re-fitting logic is expected to care
#'    about this distinction -- capping at the incumbent rather than
#'    wiping the slate.)
#'
#' @param site A [met_site()] object (accepted for interface symmetry with
#'   the rest of the correction lifecycle; not currently used in the gate
#'   arithmetic, which is entirely summarised by `training_summary`).
#' @param source Data source name, e.g. `"openmeteo"`.
#' @param variable Variable name.
#' @param lead_bucket Optional lead-time bucket (e.g. `"d1"`); `NA` (default)
#'   for observation-only (no-lead) corrections.
#' @param training_summary A one-row tibble with columns `overlap_months`,
#'   `n_pairs`, `has_archive`, `truth_source` (see
#'   `tests/testthat/helper-correct.R`'s `training_summary()`).
#' @param skill_verdict A one-row tibble with a `promote` logical column (see
#'   `tests/testthat/helper-correct.R`'s `skill_verdict()`).
#' @return A single string, one of [TIER_LEVELS].
#' @keywords internal
#' @noRd
tier_select <- function(site, source, variable, lead_bucket = NA,
                        training_summary, skill_verdict) {
  gate_tier <- .tier_availability_gate(training_summary)

  if (isTRUE(skill_verdict$promote)) {
    return(gate_tier)
  }

  # Skill gate failed: cap at the rung below the data-gate's answer (the
  # incumbent), never promote past it, but do not wipe out all progress by
  # forcing "physical" outright.
  gate_rank <- tier_rank(gate_tier)
  fallback_rank <- max(gate_rank - 1L, 1L)
  TIER_LEVELS[[fallback_rank]]
}
