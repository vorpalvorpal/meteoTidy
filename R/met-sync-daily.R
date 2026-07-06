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

# Run the daily sync for one site.
.met_sync_daily_site <- function(site, now, config) {
  store_root <- config$store_root
  sid <- site_id(site)
  degraded <- FALSE
  messages <- character(0)

  archive_forecasts(store_root, site, config$forecast_sources, now = now)

  for (source in config$obs_sources) {
    refetch <- .refetch_window_for(config, source)
    window <- store_effective_fetch_window(store_root, sid, "observations", source,
                                           refetch = refetch, now = now)

    result <- rlang::try_fetch(
      {
        obs <- .acquire_obs(source, site, window, now = now)
        if (nrow(obs) > 0) {
          store_write_obs(store_root, obs, now = now, mode = "supersede")
        }
        NULL
      },
      error = function(cnd) cnd
    )
    if (!is.null(result)) {
      degraded <- TRUE
      messages <- c(messages, sprintf("%s: %s", source, conditionMessage(result)))
      next
    }

    store_set_watermark(store_root, sid, "observations", source, now)
  }

  qc_run(store_root, site, variables = NULL, now = now)
  fill_run(store_root, site, variables = NULL, now = now)

  history_window <- list(from = now - as.difftime(30, units = "days"), to = now)
  build_history_hourly(store_root, site, history_window)
  build_history_daily(store_root, site, history_window)

  list(
    status = if (degraded) "degraded" else "ok",
    message = if (length(messages) > 0) paste(messages, collapse = "; ") else NA_character_
  )
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
#' @return A tibble with columns `site_id`, `status`, `message`.
#' @family pipeline
#' @export
#' @examples
#' \dontrun{
#' met_sync_daily(site, config = my_pipeline_config)
#' }
met_sync_daily <- function(sites, now = .now(), config) {
  status <- for_each_site(sites, function(site) {
    .met_sync_daily_site(site, now = now, config = config)
  }, on_error = "isolate")

  status$daily_status <- vapply(status$result, function(r) r$status %||% NA_character_,
                                character(1))
  status$daily_message <- vapply(status$result, function(r) r$message %||% NA_character_,
                                 character(1))
  status$message <- ifelse(status$status == "ok", status$daily_message, status$message)
  status$status <- ifelse(status$status == "ok", status$daily_status, status$status)
  status$daily_status <- NULL
  status$daily_message <- NULL
  status$result <- NULL
  status
}
