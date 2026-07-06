#' @include pipeline.R correct.R verify.R store.R dict.R
NULL

# Plan 16 -- met_refit(): the monthly refit lifecycle (SCOPING section 9).
# Per site: correct_refit() (Plan 11/12's fitting, gated on Plan 13's skill
# verdict -- the manifest is only bumped when the gate passes), verify_run()
# for the verification report, and store_compact() (Plan 03 partition
# compaction). Idempotent: a second run in the same "month" (i.e. over
# unchanged inputs) re-verifies but only re-writes calibrations if the
# underlying data changed enough to pass the gate again.
#
# Design note on correct_refit()'s call cardinality (see plans/16's derived
# constraints): correct_refit(store_root, site, source, variables, now)
# fits/refits *observation*-side calibrations for one (site, source) pair --
# it is not itself a per-forecast-source operation. met_refit() therefore
# iterates `config$obs_sources` (the site's own observation sources, the
# things actually being calibrated against forecasts/model output) rather
# than `config$forecast_sources`. Refitting a *forecast* source's own
# correction (e.g. recalibrating Open-Meteo's bias against site truth) is a
# distinct, not-yet-fully-wired concern -- correct_apply()'s `source`
# argument already lets any given source be corrected once a manifest entry
# exists, but the *fitting* trigger for forecast sources belongs to a future
# refinement of this monthly job, not this plan's tested scope.

#' Refit correction calibrations and run verification (monthly)
#'
#' Per site (SCOPING section 9): calls `correct_refit()` once per
#' configured observation source (`config$obs_sources`) to fit/refit
#' candidate calibration tiers and bump the calibration manifest **only**
#' when Plan 13's skill gate passes (`correct_refit()` itself performs the
#' verdict-gated `calib_write()`); runs `verify_run()` over
#' `config$forecast_sources` for the verification report; and compacts the
#' store's Parquet partitions (`store_compact()`, Plan 03).
#'
#' Idempotent: a second call over the same inputs and clock re-verifies but
#' only re-writes a calibration if the skill gate passes again; compaction
#' never changes which rows are readable, only the number of underlying
#' part-files.
#'
#' @inheritParams met_sync_live
#' @return A tibble with columns `site_id`, `status`, `message`.
#' @family pipeline
#' @export
#' @examples
#' \dontrun{
#' met_refit(site, config = my_pipeline_config)
#' }
met_refit <- function(sites, now = .now(), config) {
  status <- for_each_site(sites, function(site) {
    store_root <- config$store_root
    variables <- met_variables()$variable

    for (source in config$obs_sources) {
      correct_refit(store_root, site, source = source, variables = variables, now = now)
    }

    verify_run(store_root, site, sources = config$forecast_sources, now = now)
    store_compact(store_root)

    list(status = "ok", message = NA_character_)
  }, on_error = "isolate")

  status$refit_status <- vapply(status$result, function(r) r$status %||% NA_character_,
                                character(1))
  status$refit_message <- vapply(status$result, function(r) r$message %||% NA_character_,
                                 character(1))
  status$message <- ifelse(status$status == "ok", status$refit_message, status$message)
  status$status <- ifelse(status$status == "ok", status$refit_status, status$status)
  status$refit_status <- NULL
  status$refit_message <- NULL
  status$result <- NULL
  status
}
