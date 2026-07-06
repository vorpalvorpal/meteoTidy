#' @include pipeline.R store-forecast.R
NULL

# Plan 16 -- archive_forecasts(): the shared archive-on-every-sync helper
# (SCOPING section 9). Called by both met_sync_live() and met_sync_daily():
# for each configured forecast source, fetch the current issuance window and
# write it via store_write_forecast(), which dedups on
# (source, model, issue_time) (Plan 03) -- so re-archiving an unchanged
# issuance is a no-op and the whole pipeline stays idempotent.

# A short, recent issue window to poll on every sync. No test pins the exact
# window-derivation logic (test-archive-forecasts.R only asserts dedup and
# gap-note behaviour), so this keeps it simple: the last few days up to
# `now`, wide enough to comfortably re-poll a source's most recent issuance
# cycle without re-fetching its entire history on every call.
.archive_forecast_window <- function(now) {
  list(from = now - as.difftime(3, units = "days"), to = now)
}

# Per-source gap-note text for a missed issuance (constraint 5): BOM
# issuances cannot be backfilled once missed (no historical-issuance API);
# Open-Meteo (and any other source using Previous/Single-Runs) self-heals a
# missed issuance later via those endpoints. Anything else gets a neutral
# note. Kept as a small lookup table rather than real gap-detection logic,
# which is out of this plan's tested scope -- `missed` is a caller-asserted
# flag, not something this function detects on its own.
.forecast_gap_policy <- function(source) {
  if (identical(source, "bom_forecast")) {
    return("Missed BOM issuance cannot be backfilled (no historical-issuance API).")
  }
  if (identical(source, "openmeteo")) {
    return("Missed Open-Meteo issuance self-heals via Previous/Single Runs.")
  }
  sprintf("Missed %s issuance: gap semantics not documented for this source.", source)
}

#' Archive current forecast issuances for a site (dedup on every sync)
#'
#' For each `source` in `sources`, fetches the current issuance window via
#' `.acquire_forecast()` and writes it with `store_write_forecast()`, which
#' deduplicates on `(source, model, issue_time)` (Plan 03) -- re-archiving an
#' issuance already on file is a no-op, so calling this repeatedly with the
#' same upstream issuances never grows the store.
#'
#' `missed = TRUE` is a caller-asserted "an issuance was missed" flag (not
#' something this function detects itself): it skips fetching/writing
#' entirely and instead returns a summary noting whether `source`'s gap can
#' be backfilled later (`bom_forecast`: no; `openmeteo`: self-heals via
#' Previous/Single Runs -- SCOPING section 9).
#'
#' @param store_root Root directory of the store.
#' @param site A `met_site` object.
#' @param sources Character vector of forecast source names to archive.
#' @param now Injectable current time; see `.now()`.
#' @param missed Logical; when `TRUE`, report gap semantics for `sources`
#'   instead of fetching/archiving.
#' @return A summary tibble with (at least) `source` and `note` columns.
#' @keywords internal
#' @noRd
archive_forecasts <- function(store_root, site, sources, now = .now(), missed = FALSE) {
  if (isTRUE(missed)) {
    return(tibble::tibble(
      source = sources,
      note = vapply(sources, .forecast_gap_policy, character(1))
    ))
  }

  window <- .archive_forecast_window(now)

  rows <- lapply(sources, function(source) {
    fc <- .acquire_forecast(source, site, window, now = now)
    if (nrow(fc) > 0) {
      store_write_forecast(store_root, fc, now = now)
    }
    tibble::tibble(source = source, note = "archived", n = nrow(fc))
  })

  vctrs::vec_rbind(!!!rows)
}
