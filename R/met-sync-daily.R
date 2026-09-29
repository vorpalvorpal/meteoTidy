#' @include pipeline.R archive-forecasts.R qc.R fill.R history-products.R store-watermark.R
NULL

# Plan 16 -- met_sync_daily(): the daily sync (SCOPING section 9). Per site:
# archive every configured forecast source's current issuances (incl.
# seasonal, if configured); re-fetch each obs source over its refetch
# window (so upstream revisions -- e.g. SILO patched-point updates -- are
# picked up and superseded rather than duplicated, Plan 03/06); QC + fill;
# extend history_hourly/history_daily; advance daily watermarks per source.

# Default refetch window when config$refetch_windows[[source]] is absent:
# a conservative week, wide enough to catch typical short-lag revisions
# without re-fetching a source's entire history on every daily run. SILO's
# much longer revision lag is handled by pipeline_config()'s explicit
# `refetch_windows$silo = 30 days` (tests/testthat/helper-pipeline.R).
.default_refetch_window <- function() {
  as.difftime(7, units = "days")
}

.refetch_window_for <- function(config, source) {
  config$refetch_windows[[source]] %||% .default_refetch_window()
}

# Run the daily sync for one site. Every source is isolated (problem 7 of
# the production review: archive_forecasts() used to run first, unwrapped,
# so one dead forecast feed skipped every obs source, QC/fill and history).
.met_sync_daily_site <- function(site, now, config) {
  store_root <- config$store_root
  sid <- site_id(site)

  obs_status <- lapply(config$obs_sources, function(source) {
    refetch <- .refetch_window_for(config, source)
    window <- store_effective_fetch_window(store_root, sid, "observations", source,
                                           refetch = refetch, now = now)
    # No watermark yet (fresh store): adapters cannot fetch an open-ended
    # window, so a first run covers the refetch window; backfilling full
    # history is met_backfill()'s job.
    window$from <- window$from %||% (now - refetch)
    .run_source("obs", source, {
      n <- .sync_write_obs(store_root, .acquire_obs(source, site, window, now = now), now)
      store_set_watermark(store_root, sid, "observations", source, now)
      n
    })
  })

  history_window <- list(from = now - as.difftime(30, units = "days"), to = now)
  step_errors <- c(
    .sync_step("qc", qc_run(store_root, site, variables = NULL, now = now)),
    .sync_step("fill", fill_run(store_root, site, variables = NULL, now = now)),
    .sync_step("history_hourly", build_history_hourly(store_root, site, history_window)),
    .sync_step("history_daily", build_history_daily(store_root, site, history_window))
  )

  fc_status <- archive_forecasts(store_root, site, config$forecast_sources, now = now)

  sources <- vctrs::vec_rbind(.empty_source_status(), !!!obs_status,
                              .as_source_status(fc_status, "forecast"))
  .site_status(sources, step_errors)
}

#' Sync forecasts and extend curated history products (daily)
#'
#' Per site (SCOPING section 9): archives every configured forecast source's
#' current issuances (`archive_forecasts()`, including seasonal products
#' when configured); re-fetches each configured obs source over its
#' **refetch window** (`config$refetch_windows[[source]]`, e.g. SILO's 30
#' days, so upstream revisions supersede rather than duplicate -- Plan
#' 03/06); runs `qc_run()`/`fill_run()`; extends `history_hourly`/
#' `history_daily` (`build_history_hourly()`/`build_history_daily()`, Plan
#' 10); and advances each obs source's daily watermark.
#'
#' Multi-site, incremental, and idempotent, mirroring `met_sync_live()`. A
#' dead obs source degrades that site's status to `"degraded"` rather than
#' propagating; other sites are unaffected.
#'
#' @inheritParams met_sync_live
#' @return A per-site status tibble; see [met_sync_live()] (same columns,
#'   logging, `fail_on` behaviour and per-source isolation).
#' @family pipeline
#' @export
#' @examples
#' \dontrun{
#' met_sync_daily(site, config = my_pipeline_config)
#' }
met_sync_daily <- function(sites, now = .now(), config,
                           fail_on = c("none", "failed", "any", "all")) {
  fail_on <- rlang::arg_match(fail_on)
  .run_sync_verb("met_sync_daily", sites, config, fail_on, function(site) {
    .met_sync_daily_site(site, now = now, config = config)
  })
}
