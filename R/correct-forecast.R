#' @include correct.R verify.R consistency-pass.R
NULL

# Plan 17 item 1 -- serve-time FORECAST correction. Correction is applied at
# *serve* time (met_wide(), R/met-wide.R), never materialised into the raw
# forecast archive (see plans/17-correction-serve-wiring.md's "target
# architecture" note: the archive stays canonical/immutable, and
# reproducibility comes from the manifest version stamped into met_wide()'s
# `versions` attribute, not a stored "corrected" copy).

#' Apply the current calibration to a forecast tibble at serve time
#'
#' Per `(variable, source)` present in `fc`:
#'
#' - **model-only variables** (SCOPING section 7.3) pass through unchanged,
#'   tier `"raw"` -- there is no site truth to correct a model-only quantity
#'   against.
#' - **no calibration on file yet** for `(variable, source)` -- unchanged,
#'   tier `"physical"` (a forecast has no site instrument to height/lapse-
#'   correct against, so the day-0 physical tier is the identity here).
#' - **a calibration exists** -- applies the manifest's current tier via
#'   `.apply_fitted_values()` (R/correct.R), the same apply path
#'   `correct_apply()` uses for record correction, keyed on `valid_time`
#'   (the time the forecast value is ABOUT; Plan 17 item 7).
#'
#' The corrected values are then shrunk toward climatology per lead bucket
#' with a verified skill weight (`serve_shrink_weight()`), and finally passed
#' through the post-correction physical-consistency pass
#' (`.consistency_pass_long()`, R/consistency-pass.R), which clips
#' cross-variable physical impossibilities (e.g. gusts below mean wind) and
#' counts how many relations needed clipping.
#'
#' @param store_root Root directory of the store.
#' @param site A [met_site()] object.
#' @param fc A canonical forecast tibble (deterministic, per-member, and
#'   `stat` rows), as read from the archive.
#' @param now Injectable current time; see `.now()`.
#' @return `fc` with `value` corrected and a per-row `tier` column added; the
#'   count of clipped consistency-pass relations is attached as
#'   `attr(., "n_violations")`.
#' @keywords internal
#' @noRd
correct_forecast <- function(store_root, site, fc, now = .now()) {
  sid <- site_id(site)

  if (nrow(fc) == 0) {
    fc$tier <- character(0)
    attr(fc, "n_violations") <- 0L
    return(fc)
  }

  manifest <- tryCatch(calib_manifest(store_root, sid), error = function(e) NULL)

  out <- fc
  out$tier <- NA_character_
  keys <- unique(paste(fc$variable, fc$source, sep = "\r"))
  for (key in keys) {
    parts <- strsplit(key, "\r", fixed = TRUE)[[1]]
    variable <- parts[[1]]
    source <- parts[[2]]
    idx <- which(fc$variable == variable & fc$source == source)

    if (isTRUE(met_variable(variable)$measurability_class == "model_only")) {
      out$tier[idx] <- "raw"
      next
    }

    rows_manifest <- if (is.null(manifest) || nrow(manifest) == 0) {
      NULL
    } else {
      manifest[manifest$variable == variable & manifest$source == source, , drop = FALSE]
    }
    if (is.null(rows_manifest) || nrow(rows_manifest) == 0) {
      out$tier[idx] <- "physical"
      next
    }

    calib <- calib_read(store_root, sid, variable, source)
    tier <- calib$manifest$tier[[1]]
    newdata <- tibble::tibble(
      issue_time = fc$issue_time[idx], valid_time = fc$valid_time[idx],
      forecast = fc$value[idx]
    )
    out$value[idx] <- .apply_fitted_values(calib$coeffs, tier, newdata)
    out$tier[idx] <- tier
  }

  out <- .correct_forecast_shrink(store_root, site, sid, out)
  .consistency_pass_long(out, c("site_id", "source", "model", "issue_time", "valid_time",
                                "member", "stat"))
}

# Shrink the corrected forecast toward climatology per row, weighted by
# `serve_shrink_weight()`'s verified per-lead skill (Plan 17 item 1b). Reads
# the verification report once per unique (source, variable, lead_bucket,
# tier) combination rather than once per row -- a forecast frame typically
# has many rows sharing the same combination, and each `serve_shrink_weight()`
# call reads the stored report from disk.
#
# ONLY FITTED-TIER rows (`mean_bias`/`qmap`/`emos`) are shrunk. Shrinkage
# toward climatology exists to guard a *fitted* correction against verified
# skill decay at long lead (SCOPING section 7.1: "the QM tier therefore
# blends toward climatology") -- it is not a way to second-guess an
# uncorrected forecast. A `physical` (day-0, no calibration) or `raw`
# (model-only) tier IS the model forecast, and must be served as-is
# (SCOPING section 7.3: "never worse than the model"); shrinking it toward
# climatology would silently replace an actual weather forecast with a
# climatological average wherever `history_daily` exists -- exactly the
# normal post-`met_backfill()` state for any not-yet-calibrated variable.
.correct_forecast_shrink <- function(store_root, site, sid, out) {
  shrinkable <- out$tier %in% c("mean_bias", "qmap", "emos")
  if (!any(shrinkable)) {
    return(out)
  }

  sub <- out[shrinkable, , drop = FALSE]
  lead_bucket <- .verify_lead_bucket(sub$lead_time)
  climatology <- .climatology_series(store_root, site, sub$valid_time, sub$variable, sub$value)

  combo_key <- paste(sub$source, sub$variable, lead_bucket, sub$tier, sep = "\r")
  weight <- numeric(nrow(sub))
  for (combo in unique(combo_key)) {
    idx <- which(combo_key == combo)
    parts <- strsplit(combo, "\r", fixed = TRUE)[[1]]
    weight[idx] <- serve_shrink_weight(store_root, sid, parts[[1]], parts[[2]], parts[[3]],
                                       tier = parts[[4]])
  }

  out$value[shrinkable] <- shrink_to_climatology(sub$value, climatology = climatology,
                                                 weight = weight)
  out
}

#' Verified per-lead shrink weight for serve-time forecast correction
#'
#' Reads the stored verification report (`read_verification_report()`). If it
#' holds both a `tier`-matching row and a `"climatology"` baseline row for
#' `(source, variable, lead_bucket)`, the weight is the skill score of the
#' fitted tier against climatology (`skill_score()`, R/verify-scores.R),
#' clamped to `[0, 1]`: high skill vs. climatology trusts the correction
#' (weight -> 1); no skill over climatology shrinks fully to climatology
#' (weight 0). If the report has no such rows yet (no `verify_run()` has
#' landed, or no matching history), falls back to weight `1` for a fitted
#' tier -- trust the correction until verification says otherwise. Only
#' fitted tiers ever reach this function (`.correct_forecast_shrink()` gates
#' on the applied tier); the `else 0` branch is a defensive default, never
#' used to shrink an uncorrected `physical`/`raw` forecast.
#'
#' @param store_root Root directory of the store.
#' @param site_id Site identifier.
#' @param source Data source name.
#' @param variable Variable name.
#' @param lead_bucket A lead-bucket label (see `.verify_lead_bucket()`,
#'   R/verify.R).
#' @param tier The tier actually applied to this row (see `correct_forecast()`).
#' @return A single numeric weight in `[0, 1]`.
#' @keywords internal
#' @noRd
serve_shrink_weight <- function(store_root, site_id, source, variable, lead_bucket, tier) {
  report <- read_verification_report(store_root, site_id)
  sub <- report[report$source == source & report$variable == variable &
                  report$lead_bucket == lead_bucket, , drop = FALSE]
  tier_row <- sub[sub$tier == tier, , drop = FALSE]
  clim_row <- sub[sub$tier == "climatology", , drop = FALSE]

  if (nrow(tier_row) > 0 && nrow(clim_row) > 0 &&
        is.finite(tier_row$rmse[[1]]) && is.finite(clim_row$rmse[[1]])) {
    return(max(0, min(1, skill_score(tier_row$rmse[[1]], clim_row$rmse[[1]]))))
  }

  if (tier %in% c("mean_bias", "qmap", "emos")) 1 else 0
}
