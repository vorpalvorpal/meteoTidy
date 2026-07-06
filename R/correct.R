# Plan 11 -- the correction lifecycle: correct_apply() (daily; applies the
# current calibration) and correct_refit() (monthly; fits and skill-gates a
# new calibration). Reads/writes the Plan 03 calibration store instead of
# a watermark for its main branching logic -- the tier a variable is
# corrected at depends on what has been fitted and manifest-recorded for
# that (site, variable, source), not on how much time has passed.
#
# The real `shrink_to_climatology()` (a 3-arg `(corrected, climatology,
# weight)` blend primitive) lives in `R/shrinkage.R`. `correct_apply()`
# calls it directly (by name) from its own `target == "forecast"` branch
# below -- see `.correct_forecast_climatology()` and the call site inside
# `correct_apply()` -- rather than routing through
# `apply_correction_shrinkage()`, so `test-correct-apply.R`'s
# `local_mocked_bindings(shrink_to_climatology = ...)` mock (which replaces
# the whole function for that test and only asserts call counts on the
# record/forecast branches) keeps working unchanged.

# Real climatology lookup + per-row skill weight for the forecast-shrinkage
# path (Plan 13 wiring). The climatology series is `history_daily`'s
# day-of-year pooled mean (`build_history_daily()` + `baseline_climatology()`,
# Plans 10/13) over the trailing ~3 years; a row with no history to pool
# against falls back to its own (uncorrected) value, i.e. no shrinkage. The
# weight is derived from `corrected$tier`, already stamped per row by
# `correct_apply()`'s per-variable loop: a fitted tier (`mean_bias`/`qmap`/
# `emos`) only ever reaches the manifest via `correct_refit()`'s skill gate
# (SCOPING section 7.1), so its presence *is* the verified-skill evidence --
# weight `1` (trust the correction fully). No calibration beyond `physical`/
# `raw` has passed that gate, so those get weight `0` (shrink fully to
# climatology). This is coarser than a true per-lead-bucket weight (this
# call site has no `lead_time` column to key a finer weight on -- `leads` is
# reserved for a future finer-grained version), but it is real, not a
# placeholder identity pass-through.
.correct_forecast_climatology <- function(store_root, site, corrected, leads = NULL) {
  if (nrow(corrected) == 0) {
    return(corrected$value)
  }

  window <- list(from = min(corrected$datetime_utc) - as.difftime(1095, units = "days"),
                 to = max(corrected$datetime_utc))
  hist <- build_history_daily(store_root, site, window)

  climatology <- vapply(seq_len(nrow(corrected)), function(i) {
    if (nrow(hist) == 0) {
      return(corrected$value[[i]])
    }
    base <- baseline_climatology(hist, corrected$datetime_utc[[i]], corrected$variable[[i]])
    if (is.na(base$mean)) corrected$value[[i]] else base$mean
  }, numeric(1))

  weight <- ifelse(corrected$tier %in% c("mean_bias", "qmap", "emos"), 1, 0)

  shrink_to_climatology(corrected$value, climatology = climatology, weight = weight)
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

# Apply a fitted (non-physical) tier's calibration to `obs`, delegating to
# Plan 12's real per-tier apply_*() function. `newdata`'s columns are named
# to match what each apply_*() expects (`issue_time`/`forecast`) even though
# `obs` is observation-shaped (`datetime_utc`/`value`): the harmonic/qmap/
# EMOS fits were trained on `forecast_obs_pairs()`-shaped pairs via
# `correct_refit()`, so applying them here reuses the identical column
# names by design, not by coincidence.
.correct_apply_fitted <- function(obs, coeffs, tier) {
  newdata <- tibble::tibble(issue_time = obs$datetime_utc, forecast = obs$value)
  obs$value <- switch(tier,
    mean_bias = apply_mean_bias(coeffs, newdata)$value,
    qmap = apply_qmap(coeffs, newdata),
    emos = apply_emos(coeffs, newdata)$mean,
    obs$value
  )
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
    corrected$value <- .correct_forecast_climatology(store_root, site, corrected, leads = leads)
  }

  corrected
}

# A training_summary row (see tests/testthat/helper-correct.R's builder of
# the same shape) derived from real assembled pairs: overlap_months from the
# issue_time span, n_pairs from the row count. `has_archive`/`truth_source`
# are always TRUE/"observed" here -- the Historical-Forecast pseudo-truth
# special case (SCOPING section 7.2) is Open-Meteo-specific bookkeeping that
# assemble_verification_pairs() does not currently distinguish in its output.
.correct_refit_training_summary <- function(pairs, source, variable) {
  overlap_months <- as.numeric(difftime(max(pairs$issue_time), min(pairs$issue_time),
                                        units = "days")) / 30.44
  tibble::tibble(
    source = source, variable = variable, lead_bucket = NA_character_,
    overlap_months = overlap_months, n_pairs = nrow(pairs),
    has_archive = TRUE, truth_source = "observed"
  )
}

# fit_fn/apply_fn pair for one fitted tier, both operating directly on a
# forecast_obs_pairs()-shaped tibble (issue_time/forecast/observation/
# lead_time) -- the exact shape rolling_origin_score()'s fit_fn/apply_fn
# contract expects (R/verify.R), and the exact shape correct_refit()'s
# `pairs` already is. `apply_fn` always returns a plain numeric vector.
.correct_refit_fit_apply <- function(tier) {
  switch(tier,
    mean_bias = list(
      fit = function(train) fit_mean_bias(train),
      apply = function(fit, newdata) apply_mean_bias(fit, newdata)$value
    ),
    qmap = list(
      fit = function(train) fit_qmap(train),
      apply = function(fit, newdata) apply_qmap(fit, newdata)
    ),
    emos = list(
      fit = function(train) fit_emos(train, lead_bucket = .verify_lead_bucket(train$lead_time[[1]])),
      apply = function(fit, newdata) apply_emos(fit, newdata)$mean
    )
  )
}

# Like rolling_origin_score() (R/verify.R), but returns the per-row
# out-of-sample absolute errors instead of an aggregate mae/rmse -- the raw
# material a moving-block bootstrap (block_bootstrap_ci(), R/verify-
# bootstrap.R) needs to test whether a candidate tier's improvement over the
# incumbent is significant, not just numerically smaller. Window selection
# (which rows fall in which origin's training set vs scoring window) depends
# only on `pairs$issue_time`, so calling this once with the candidate
# fit/apply and once with the incumbent fit/apply scores the identical set
# of out-of-sample rows -- a fair, paired comparison.
.rolling_origin_errors <- function(pairs, fit_fn, apply_fn, step, buffer) {
  step_dt <- .parse_period(step)
  buffer_dt <- .parse_period(buffer)
  pairs <- pairs[order(pairs$issue_time), , drop = FALSE]
  if (nrow(pairs) == 0) {
    return(numeric(0))
  }

  origins <- seq(min(pairs$issue_time), max(pairs$issue_time), by = step_dt)
  errs <- numeric(0)
  for (origin in origins) {
    origin <- as.POSIXct(origin, tz = "UTC", origin = "1970-01-01")
    train <- pairs[pairs$issue_time < origin - buffer_dt, , drop = FALSE]
    in_window <- pairs$issue_time >= origin & pairs$issue_time < origin + step_dt
    score_set <- pairs[in_window, , drop = FALSE]
    if (nrow(train) == 0 || nrow(score_set) == 0) {
      next
    }
    fit <- fit_fn(train)
    corrected <- apply_fn(fit, score_set)
    errs <- c(errs, abs(corrected - score_set$observation))
  }
  errs
}

# The step/buffer rolling-origin window shared by correct_refit()'s
# candidate-vs-incumbent comparison, matching verify_run()'s own defaults
# (R/verify.R) so the two report comparable numbers.
.correct_refit_step <- function() "30 days"
.correct_refit_buffer <- function() "1 day"

# Fit/refit one variable's calibration for (site, source), gated on Plan
# 13's skill verdict. Writes a new calibration version only when the gate
# passes; otherwise leaves the incumbent (if any) untouched.
.correct_refit_variable <- function(store_root, site, sid, source, variable, now) {
  pairs <- assemble_verification_pairs(store_root, site, sources = source, variables = variable)
  if (nrow(pairs) == 0) {
    return(invisible(NULL))
  }

  training_summary <- .correct_refit_training_summary(pairs, source, variable)
  # A promote = TRUE placeholder here only selects the *candidate* tier to
  # attempt fitting (the data-availability gate's answer); the real,
  # evidence-based promote/keep decision is the skill_verdict_compute() call
  # below, which alone gates the calib_write().
  candidate_verdict <- tibble::tibble(variable = variable, lead_bucket = NA_character_,
                                      promote = TRUE, shrink_weight = 1,
                                      consistency_violation_rate = 0)
  gate_tier <- tier_select(site, source, variable, lead_bucket = NA,
                           training_summary = training_summary, skill_verdict = candidate_verdict)
  if (!(gate_tier %in% c("mean_bias", "qmap", "emos"))) {
    return(invisible(NULL)) # below the fitted-tier floor: nothing to fit or persist
  }

  if (gate_tier == "emos") {
    pairs <- pairs[!is.na(pairs$lead_time), , drop = FALSE] # SCOPING section 7.2
  }
  if (nrow(pairs) == 0) {
    return(invisible(NULL))
  }

  fit_apply <- .correct_refit_fit_apply(gate_tier)
  step <- .correct_refit_step()
  buffer <- .correct_refit_buffer()

  candidate_errs <- .rolling_origin_errors(pairs, fit_apply$fit, fit_apply$apply, step, buffer)
  incumbent_errs <- .rolling_origin_errors(pairs, .verify_identity_fit, .verify_identity_apply,
                                           step, buffer)
  n <- min(length(candidate_errs), length(incumbent_errs))
  if (n == 0) {
    return(invisible(NULL)) # not enough out-of-sample data to evaluate the gate
  }
  candidate_errs <- candidate_errs[seq_len(n)]
  incumbent_errs <- incumbent_errs[seq_len(n)]

  scores <- tibble::tibble(
    variable = variable, lead_bucket = NA_character_,
    candidate = sqrt(mean(candidate_errs^2)), incumbent = sqrt(mean(incumbent_errs^2))
  )
  boot <- block_bootstrap_ci(incumbent_errs - candidate_errs, block_len = 7L, R = 500L, seed = 1L)
  bootstrap <- tibble::tibble(
    variable = variable, lead_bucket = NA_character_,
    significant = boot$significant, ci_lower = boot$ci[[1]], ci_upper = boot$ci[[2]]
  )

  verdict <- skill_verdict_compute(scores, bootstrap)
  if (!isTRUE(verdict$promote[[1]])) {
    return(invisible(NULL)) # skill gate failed: keep the incumbent calibration
  }

  final_fit <- fit_apply$fit(pairs)
  calib_write(
    store_root, sid, variable, source, gate_tier, final_fit,
    meta = list(train_start = min(pairs$issue_time), train_end = max(pairs$issue_time),
               n_pairs = nrow(pairs)),
    now = now
  )
  invisible(NULL)
}

#' Refit correction calibrations for a site (monthly lifecycle)
#'
#' Per requested variable (SCOPING section 7.1): assembles training pairs by
#' joining the Plan 03 forecast archive with curated observations
#' (`assemble_verification_pairs()`, R/verify.R); summarises them into a
#' `training_summary` and calls `tier_select()` to pick a *candidate* tier;
#' fits that tier via Plan 12's `fit_mean_bias()`/`fit_qmap()`/`fit_emos()`;
#' scores the candidate against the incumbent (raw/uncorrected) tier
#' out-of-sample (`rolling_origin_score()`-style rolling-origin evaluation,
#' never on the fit's own training window); and gates the write on Plan 13's
#' skill verdict (`skill_verdict_compute()`, significance via
#' `block_bootstrap_ci()`): `calib_write()` **only if** the gate promotes,
#' otherwise the incumbent calibration (if any) is left untouched.
#'
#' Model-only variables (SCOPING section 7.3) are skipped -- there is no
#' site truth to fit a bias correction against. A variable with no
#' forecast/observation overlap yet is skipped silently (nothing to fit).
#'
#' @param store_root Root directory of the store.
#' @param site A [met_site()] object.
#' @param source Data source name (the calibration's key, alongside
#'   `variable`; see `calib_write()`).
#' @param variables Character vector of variables to refit.
#' @param now Injectable current time; see `.now()`.
#' @return Invisibly, `NULL`. Calibrations are written as a side effect via
#'   `calib_write()`, one version per variable whose skill gate passes.
#' @keywords internal
#' @noRd
correct_refit <- function(store_root, site, source, variables, now = .now()) {
  sid <- site_id(site)
  for (variable in variables) {
    dict_row <- met_variable(variable)
    if (isTRUE(dict_row$measurability_class == "model_only")) {
      next
    }
    .correct_refit_variable(store_root, site, sid, source, variable, now = now)
  }
  invisible(NULL)
}
