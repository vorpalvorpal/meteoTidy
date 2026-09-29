#' @include pipeline.R archive-forecasts.R qc.R fill.R store-watermark.R
NULL

# Plan 16 -- met_sync_live(): the hourly, best-effort near-real-time sync
# (SCOPING section 9/5.1). Per site: fetch the live obs head from each
# configured obs source (GHCNh deliberately excluded -- its ~1-week lag,
# Plan 06 cadence metadata -- makes it unsuitable for a live window), QC +
# fill once over the whole window, archive current forecast issuances, and
# advance the live watermark. A dead acquisition source degrades that site to
# status "degraded" rather than crashing the run or the other sites
# (for_each_site()'s own "isolate" mode is reserved for genuine bugs in the
# per-site function, not this expected failure mode -- see R/pipeline.R).
#
# Plan 17 item 1c: correction is applied at SERVE time (met_wide(),
# R/correct-forecast.R), never here. The `correct_apply()` calls this
# function used to make (both the obs `target = "record"` call inside the
# per-source loop and the forecast `target = "forecast"` loop below) computed
# a result that nothing downstream ever read -- live sync's job is to
# acquire, QC, fill, and archive; a consumer reading `met_wide()`/
# `build_history_daily()` gets the CURRENT calibration applied fresh, so a
# monthly refit takes effect immediately rather than waiting for the next
# sync to re-apply a stale one. See plans/17-correction-serve-wiring.md's
# "target architecture" note for the full rationale (a flagged deviation
# from SCOPING section 9, which lists "apply calibrations" as live-sync work).

# How far back of `now` the "live window" reaches. Not pinned by any test to
# an exact duration; a few hours is enough to comfortably re-poll the most
# recent near-real-time head on an hourly cadence without re-scanning a full
# day on every run.
.live_window <- function(now) {
  list(from = now - as.difftime(6, units = "hours"), to = now)
}

# Write one obs source's fetched rows (shared by the live and daily syncs).
# Returns the number of rows fetched.
.sync_write_obs <- function(store_root, obs, now) {
  if (nrow(obs) > 0) {
    store_write_obs(store_root, obs, now = now, mode = "supersede")
    if ("transport" %in% names(obs)) {
      transport_cols <- c("site_id", "datetime_utc", "variable", "source", "transport")
      obs_transport_write(store_root, obs[transport_cols], now = now)
    }
  }
  nrow(obs)
}

# Run a non-acquisition step (QC, fill, history) so its failure is reported
# rather than unwinding the site. Returns NULL or an error message.
.sync_step <- function(name, expr) {
  tryCatch({
    force(expr)
    NULL
  }, error = function(cnd) sprintf("%s: %s", name, .one_line(conditionMessage(cnd))))
}

# Run the live sync for one site. Returns list(status, message, sources):
# every source is isolated (a dead one is recorded, never thrown).
.met_sync_live_site <- function(site, now, config) {
  store_root <- config$store_root
  window <- .live_window(now)

  obs_status <- lapply(config$obs_sources, function(source) {
    .run_source("obs", source, {
      .sync_write_obs(store_root, .acquire_obs(source, site, window, now = now), now)
    })
  })

  # Hoisted out of the per-source loop (Plan 17 item 11): QC/fill see the
  # same fully-written window whether run once here or once per source.
  step_errors <- c(
    .sync_step("qc", qc_run(store_root, site, variables = NULL, now = now)),
    .sync_step("fill", fill_run(store_root, site, variables = NULL, now = now))
  )

  fc_status <- archive_forecasts(store_root, site, config$forecast_sources, now = now)
  store_set_watermark(store_root, site_id(site), "observations", "live", now)

  sources <- vctrs::vec_rbind(.empty_source_status(), !!!obs_status,
                              .as_source_status(fc_status, "forecast"))
  .site_status(sources, step_errors)
}

#' Sync the live (near-real-time) observation and forecast head
#'
#' Hourly, best-effort (SCOPING section 5.1). Per site: fetches each
#' configured obs source over a short live window, runs `qc_run()`/
#' `fill_run()` once over the whole window, archives current forecast
#' issuances (`archive_forecasts()`), and advances the live watermark
#' (`"observations"`/`"live"`). Correction is applied at SERVE time, not
#' here (Plan 17: `met_wide()`/`build_history_daily()` apply the current
#' calibration fresh on every read; see `R/correct-forecast.R` and
#' `plans/17-correction-serve-wiring.md`'s "target architecture" note).
#'
#' GHCNh is never fetched here: its ~1-week lag (Plan 06 cadence metadata)
#' makes it unsuitable for a live head; it participates in
#' `met_sync_daily()`'s history products instead. A dead acquisition source
#' marks that site's status `"degraded"` (not an error) and the run
#' continues with the other configured sources; other sites are unaffected
#' (`for_each_site(on_error = "isolate")`).
#'
#' Multi-site, incremental (the live watermark), and idempotent: re-running
#' with the same inputs and clock does not duplicate observation rows or
#' double-advance the watermark (Plan 03's supersede/dedup policies).
#'
#' @param sites A `met_site` or `met_sites` collection.
#' @param now Injectable current time; see `.now()`.
#' @param config A pipeline configuration list (see `plans/14-*`/
#'   `tests/testthat/helper-pipeline.R`'s `pipeline_config()`): at least
#'   `store_root`, `obs_sources`, `forecast_sources`.
#'   Optional `lock_timeout` (seconds, default 600): how long to wait for
#'   another process's write lock on `store_root`.
#' @param fail_on When to signal an error (class
#'   `meteoTidy_error_sync_failed`) after all the work is done, so a
#'   scheduler running `Rscript` sees a non-zero exit status:
#'   * `"none"` (default): never.
#'   * `"failed"` (**recommended for production**): any source `"failed"`
#'     or any site `"error"`. A `"stale"` source (a station that has stopped
#'     reporting, e.g. an offline logger) is logged but does not fail the
#'     run, so one silent station does not fail every hourly run while a dead
#'     feed still does.
#'   * `"any"`: any site not fully `"ok"`, including a stale source.
#'   * `"all"`: only when every site failed outright.
#'
#'   The condition carries the status table as `cnd$status`.
#' @return A tibble with one row per site: `site_id`; `status` -- `"ok"`
#'   (every source succeeded), `"degraded"` (some source or step failed),
#'   `"failed"` (every source failed) or `"error"` (an unexpected error);
#'   `message` (what failed); and `sources`, a list-column of per-source
#'   tibbles (`kind`, `source`, `status` = `"ok"`/`"failed"`/`"stale"`, `n`
#'   rows fetched, `message`). One line per site is also written to stderr
#'   via [message()] (silence with `options(meteoTidy.sync_log = FALSE)`).
#'
#' @section Isolation:
#' Every source -- each obs source and each forecast source -- runs
#' isolated: a dead source is recorded in `sources` and the site's other
#' sources, QC/fill and archiving still run. The whole per-site sync holds
#' an exclusive lock on `store_root`, so overlapping runs (e.g. a slow
#' hourly sync still going when the next starts) serialise instead of
#' writing duplicate rows.
#' @family pipeline
#' @export
#' @examples
#' \dontrun{
#' met_sync_live(site, config = my_pipeline_config)
#' }
met_sync_live <- function(sites, now = .now(), config,
                          fail_on = c("none", "failed", "any", "all")) {
  fail_on <- rlang::arg_match(fail_on)
  .run_sync_verb("met_sync_live", sites, config, fail_on, function(site) {
    .met_sync_live_site(site, now = now, config = config)
  })
}
