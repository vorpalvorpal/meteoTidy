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
# fits/refits a calibration for one (site, source) pair by joining that
# source's ARCHIVED forecasts against curated observations
# (`assemble_verification_pairs(sources = source)`, R/verify.R) -- so
# `source` names whichever side of the join has archived forecast rows to
# calibrate. Plan 17 item 3: in a realistic deployment, obs sources
# (`site_aws`/`silo`) and forecast sources (`openmeteo`/`bom_forecast`) are
# disjoint, so restricting to `config$obs_sources` alone (the pre-Plan-17
# behaviour) made that join empty for every forecast source -- no forecast
# calibration was ever fit in production. `config$obs_sources` stays in the
# iteration for the SILO daily-QM record correction (Plan 17 item 2), whose
# "source" is an obs source; `config$forecast_sources` is added so archived
# forecast sources actually get calibrated. `unique()` de-duplicates any
# source configured as both (uncommon, but harmless either way).

#' Refit correction calibrations and run verification (monthly)
#'
#' Per site (SCOPING section 9): calls `correct_refit()` once per configured
#' source (`config$obs_sources` union `config$forecast_sources`, Plan 17
#' item 3) to fit/refit candidate calibration tiers and bump the calibration
#' manifest **only** when Plan 13's skill gate passes (`correct_refit()`
#' itself performs the verdict-gated `calib_write()`); runs `verify_run()` over
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

    for (source in unique(c(config$obs_sources, config$forecast_sources))) {
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
