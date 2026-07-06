#' @include pipeline.R archive-forecasts.R qc.R fill.R correct.R store-watermark.R
NULL

# Plan 16 -- met_sync_live(): the hourly, best-effort near-real-time sync
# (SCOPING section 9/5.1). Per site: fetch the live obs head from each
# configured obs source (GHCNh deliberately excluded -- its ~1-week lag,
# Plan 06 cadence metadata -- makes it unsuitable for a live window), QC +
# fill just that window, apply current calibrations, archive current
# forecast issuances, and advance the live watermark. A dead acquisition
# source degrades that site to status "degraded" rather than crashing the
# run or the other sites (for_each_site()'s own "isolate" mode is reserved
# for genuine bugs in the per-site function, not this expected failure mode
# -- see R/pipeline.R).

# How far back of `now` the "live window" reaches. Not pinned by any test to
# an exact duration; a few hours is enough to comfortably re-poll the most
# recent near-real-time head on an hourly cadence without re-scanning a full
# day on every run.
.live_window <- function(now) {
  list(from = now - as.difftime(6, units = "hours"), to = now)
}

# Run the live sync for one site. Returns a list(status=, message=) rather
# than throwing on an expected acquisition failure -- degraded, not error.
.met_sync_live_site <- function(site, now, config) {
  store_root <- config$store_root
  window <- .live_window(now)
  degraded <- FALSE
  messages <- character(0)

  for (source in config$obs_sources) {
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

    qc_run(store_root, site, variables = NULL, now = now)
    fill_run(store_root, site, variables = NULL, now = now)
    correct_apply(store_root, site, source = source, target = "record",
                  variables = adapters_for_site(site)[[source]]@provides, now = now)
  }

  # Apply current calibrations to the forecast head too (SCOPING section 9:
  # "apply current calibrations ... target = 'forecast' for the forecast
  # head"). Best-effort: a forecast-correction failure does not itself
  # degrade the site beyond whatever the obs loop already found, since the
  # obs head is the load-bearing near-real-time product.
  for (source in config$forecast_sources) {
    rlang::try_fetch(
      correct_apply(store_root, site, source = source, target = "forecast",
                    variables = adapters_for_site(site)[[source]]@provides, now = now),
      error = function(cnd) NULL
    )
  }

  archive_forecasts(store_root, site, config$forecast_sources, now = now)
  store_set_watermark(store_root, site_id(site), "observations", "live", now)

  list(
    status = if (degraded) "degraded" else "ok",
    message = if (length(messages) > 0) paste(messages, collapse = "; ") else NA_character_
  )
}

#' Sync the live (near-real-time) observation and forecast head
#'
#' Hourly, best-effort (SCOPING section 5.1). Per site: fetches each
#' configured obs source over a short live window, runs `qc_run()`/
#' `fill_run()` over that window, applies current calibrations
#' (`correct_apply()`, `target = "record"` for observations and
#' `target = "forecast"` for the forecast head), archives current forecast
#' issuances (`archive_forecasts()`), and advances the live watermark
#' (`"observations"`/`"live"`).
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
#' @return A tibble with columns `site_id`, `status` (`"ok"` or
#'   `"degraded"`/`"error"`), `message`.
#' @family pipeline
#' @export
#' @examples
#' \dontrun{
#' met_sync_live(site, config = my_pipeline_config)
#' }
met_sync_live <- function(sites, now = .now(), config) {
  status <- for_each_site(sites, function(site) {
    .met_sync_live_site(site, now = now, config = config)
  }, on_error = "isolate")

  status$degraded_status <- vapply(status$result, function(r) r$status %||% NA_character_,
                                   character(1))
  status$degraded_message <- vapply(status$result, function(r) r$message %||% NA_character_,
                                    character(1))

  status$message <- ifelse(status$status == "ok", status$degraded_message, status$message)
  status$status <- ifelse(status$status == "ok", status$degraded_status, status$status)
  status$degraded_status <- NULL
  status$degraded_message <- NULL
  status$result <- NULL
  status
}
