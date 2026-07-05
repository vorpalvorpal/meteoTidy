# Plan 11 -- the correction lifecycle: correct_apply() (daily; applies the
# current calibration) and the correct_refit() skeleton (monthly; delegates
# fitting to Plan 12). Reads/writes the Plan 03 calibration store instead of
# a watermark for its main branching logic -- the tier a variable is
# corrected at depends on what has been fitted and manifest-recorded for
# that (site, variable, source), not on how much time has passed.
#
# The real `shrink_to_climatology()` (a 3-arg `(corrected, climatology,
# weight)` blend primitive) now lives in `R/shrinkage.R`, replacing this
# file's former placeholder identity pass-through. `correct_apply()` still
# calls it directly (by name) from its own `target == "forecast"` branch
# below -- see `.correct_forecast_climatology()` and the call site inside
# `correct_apply()` -- rather than routing through
# `apply_correction_shrinkage()`, so `test-correct-apply.R`'s
# `local_mocked_bindings(shrink_to_climatology = ...)` mock (which replaces
# the whole function for that test and only asserts call counts on the
# record/forecast branches) keeps working unchanged.

# Placeholder climatology lookup for the forecast-shrinkage path. Plan 13
# does not yet exist (the real per-lead-bucket verified-skill weight is its
# scope), so there is no climatology series or skill-derived weight to read
# yet either. Until Plan 13 wires those in, this returns `obs$value`
# unchanged as "climatology" and a weight of `1` ("trust the correction
# fully") -- i.e. shrink_to_climatology() is a no-op in practice today. This
# is a known, documented gap: Plan 13 replaces both the climatology lookup
# and the weight with real verified-skill-derived values.
.correct_forecast_climatology <- function(corrected, leads) {
  shrink_to_climatology(corrected$value, climatology = corrected$value, weight = 1)
}

# Read a site's current observations for (source, variable) from the store.
# store_read_obs() (Plan 03) does not filter on `source` directly, so filter
# after reading. No window narrowing beyond `variables`/`source`: the daily
# correct_apply() job is expected to be called over whatever window the
# caller (Plan 16) has already selected via other means; this plan reads
# everything currently in the store for the requested variable and source.
.correct_read_source_obs <- function(store_root, site_id, source, variable) {
  obs <- store_read_obs(store_root, site_id, variables = variable)
  if (nrow(obs) == 0) {
    return(obs)
  }
  obs[obs$source == source, , drop = FALSE]
}

# Apply the day-0 physical tier to `obs` for one variable and stamp
# provenance. Used both when no calibration exists yet (day 0) and as the
# uncorrected fallback tier.
.correct_apply_physical <- function(obs, site) {
  correct_physical(obs, site = site)
}

# Apply a fitted (non-physical) tier's calibration to `obs`. Plan 11 does
# not implement the fitted-tier apply functions themselves (Plan 12's
# scope); this is the seam Plan 12 wires its per-tier apply_* functions
# into. Until Plan 12 lands, fall back to a mean_bias-shaped apply using the
# shared transfer engine's offset (R/transfer.R) for tiers whose coeffs
# tibble carries an `offset` column, and otherwise pass values through
# unchanged with the manifest's recorded tier stamped -- this plan's tests
# never reach this branch (day 0, no calibration written, is what
# test-correct-apply.R exercises), so it is deliberately minimal.
.correct_apply_fitted <- function(obs, coeffs, tier) {
  if ("offset" %in% names(coeffs) && nrow(coeffs) > 0) {
    obs$value <- obs$value - coeffs$offset[[1]]
  }
  obs$tier <- tier
  obs
}

#' Apply the current correction to a site's observations (daily lifecycle)
#'
#' For each requested `variable`: reads the site's current `(source,
#' variable)` observations from the store, then:
#'
#' - **Model-only variables** (`met_variable(variable)$measurability_class
#'   == "model_only"`, SCOPING section 7.3) pass through unchanged with tier
#'   `"raw"` -- no calibration lookup at all.
#' - **No calibration exists yet (day 0)** -- applies
#'   `correct_physical()` and stamps tier `"physical"`.
#' - **A calibration exists** -- reads it via `calib_manifest()`/
#'   `calib_read()` (Plan 03) and applies it (delegating to Plan 12's
#'   per-tier apply function; see `.correct_apply_fitted()`).
#'
#' `target` distinguishes correcting a **forecast** (routes the corrected
#' values through `shrink_to_climatology()`, Plan 12's lead-dependent
#' shrinkage hook) from a **donor/record** correction (no shrinkage) -- the
#' same distinction Plan 10's transfer engine draws between gap-fill and
#' forecast correction.
#'
#' `force_tier` is a testing/override seam (not part of the documented
#' public signature): when supplied, it is treated as "what `tier_select()`
#' would have chosen" -- the real tier-selection process is skipped -- and
#' compared against the current calibration's manifest `tier`. A mismatch
#' aborts `"tier_mismatch"`: the tier `correct_apply()` uses is *enforced*,
#' not advisory (SCOPING section 7.1) -- it refuses to apply a calibration
#' whose stored tier disagrees with the tier-selection process's answer.
#'
#' @param store_root Root directory of the store.
#' @param site A [met_site()] object.
#' @param source Data source name, e.g. `"openmeteo"`.
#' @param target Either `"record"` (a donor/record correction, no shrinkage)
#'   or `"forecast"` (routes through `shrink_to_climatology()`).
#' @param variables Character vector of variables to correct.
#' @param leads Reserved for lead-bucket-scoped forecast correction (Plan
#'   12); unused here.
#' @param now Injectable current time; see `.now()`.
#' @param force_tier Optional single string, one of [TIER_LEVELS]. Testing
#'   seam: treated as the tier-selection answer for the enforcement check
#'   against the current calibration's manifest tier, bypassing the real
#'   `tier_select()` call.
#' @return A tibble of corrected observations with a `tier` column stamped.
#' @keywords internal
#' @noRd
correct_apply <- function(store_root, site, source, target = c("record", "forecast"),
                          variables, leads = NULL, now = .now(), force_tier = NULL) {
  target <- rlang::arg_match(target)
  sid <- site_id(site)

  results <- lapply(variables, function(variable) {
    dict_row <- met_variable(variable)
    is_model_only <- isTRUE(dict_row$measurability_class == "model_only")

    manifest <- calib_manifest(store_root, sid)
    manifest_rows <- manifest[manifest$variable == variable & manifest$source == source, ,
                              drop = FALSE]
    has_calib <- !is_model_only && nrow(manifest_rows) > 0

    manifest_tier <- NULL
    if (has_calib) {
      calib <- calib_read(store_root, sid, variable, source)
      manifest_tier <- calib$manifest$tier[[1]]
    }

    # Tier enforcement (SCOPING section 7.1: the choice is enforced, not
    # advisory) fires whenever a calibration exists and a selected tier is
    # available to compare against -- independent of whether there happen
    # to be any observation rows to correct yet.
    if (!is.null(force_tier) && has_calib && !identical(force_tier, manifest_tier)) {
      abort_meteo(
        c(
          "The selected tier {.val {force_tier}} disagrees with the calibration manifest's tier {.val {manifest_tier}}.", # nolint: line_length_linter.
          "i" = "correct_apply() refuses to apply a calibration whose recorded tier does not match the tier-selection process's answer." # nolint: line_length_linter.
        ),
        class = "tier_mismatch"
      )
    }

    obs <- .correct_read_source_obs(store_root, sid, source, variable)
    if (nrow(obs) == 0) {
      obs$tier <- character(0)
      return(obs)
    }

    if (is_model_only) {
      obs$tier <- "raw"
      return(obs)
    }

    if (!has_calib) {
      return(.correct_apply_physical(obs, site))
    }

    .correct_apply_fitted(obs, calib$coeffs, manifest_tier)
  })

  corrected <- vctrs::vec_rbind(!!!results)

  if (target == "forecast") {
    # Plan 12's real shrink_to_climatology(corrected, climatology, weight)
    # blend primitive (R/shrinkage.R). No climatology series or verified
    # skill weight exists yet (Plan 13's scope) -- see
    # .correct_forecast_climatology()'s documented gap above -- so this
    # calls shrink_to_climatology() with a placeholder weight = 1 ("trust
    # the correction fully") until Plan 13 supplies the real weight per
    # lead bucket.
    corrected$value <- .correct_forecast_climatology(corrected, leads = leads)
  }

  corrected
}

#' Refit correction calibrations for a site (monthly lifecycle skeleton)
#'
#' **Skeleton.** This plan implements the orchestration shape and the
#' `physical`-tier day-0 path; the actual per-tier fitting functions (mean-
#' bias, qmap, EMOS, MBC) are Plan 12's scope, and the skill verdict this
#' function would gate the write on is Plan 13's scope. No test exercises
#' `correct_refit()` directly (Plan 11's test files only exercise
#' `correct_apply()`/`tier_select()`/the physical adjustments) -- this
#' function documents the intended monthly job shape so Plan 12/13 have a
#' concrete seam to plug into, without faking coverage that doesn't exist.
#'
#' Intended flow once Plan 12/13 land:
#' 1. Assemble training pairs by joining the Plan 03 forecast archive with
#'    curated observations (SCOPING section 4) -- no training-pairs-assembly
#'    function exists yet in this codebase; Plan 12/16 supply it.
#' 2. Summarise the pairs into a `training_summary` and call `tier_select()`.
#' 3. Delegate fitting to Plan 12's per-tier `fit_*()` functions for the
#'    selected tier.
#' 4. Request Plan 13's skill verdict for the freshly-fit calibration.
#' 5. Write the new calibration + manifest bump (`calib_write()`, Plan 03)
#'    **only if** the skill gate passes; otherwise keep the incumbent
#'    calibration as current.
#'
#' @param store_root Root directory of the store.
#' @param site A [met_site()] object.
#' @param source Data source name.
#' @param variables Character vector of variables to refit.
#' @param now Injectable current time; see `.now()`.
#' @return Invisibly, `NULL`. No calibration is written by this skeleton.
#' @keywords internal
#' @noRd
correct_refit <- function(store_root, site, source, variables, now = .now()) {
  # TODO(Plan 12/13): assemble training pairs, call tier_select(), delegate
  # to Plan 12's fit_*() functions, request Plan 13's skill verdict, and
  # calib_write() only on a passing verdict. Not implemented: no training-
  # pairs-assembly function or fitted-tier fit_*() functions exist yet.
  invisible(NULL)
}
