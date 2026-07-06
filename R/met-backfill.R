#' @include pipeline.R archive-forecasts.R correct.R source-ghcnh.R dict.R site-list.R
NULL

# Plan 16 -- met_backfill(): the ad-hoc, day-0 bootstrap verb (SCOPING
# section 7.2/9). Per site: full historical obs pulls (SILO/ERA5/Open-Meteo
# Previous-Runs -- whatever the site's `config$obs_sources` configure),
# optional historical AWS export ingestion, initial calibration fits, and a
# donor-coverage audit (station_coverage(), Plan 06) so the operator sees
# coverage gaps before relying on the donor ladder. Also documents which
# forecast sources' gaps can/cannot be backfilled (BOM: no; Open-Meteo:
# self-heals via Previous/Single Runs).

# A long historical window for day-0 obs pulls. Not pinned by any test to
# an exact duration (the acquisition seam is always mocked); ten years is a
# reasonable "give me everything you have" default for SILO/ERA5-shaped
# long climate records.
.backfill_history_window <- function(now) {
  list(from = now - as.difftime(3650, units = "days"), to = now)
}

# Build (or reuse) a source_ghcnh adapter for the donor-coverage audit.
# Falls back to constructing one directly if the site has no "ghcnh" source
# configured -- the audit is meant to run even for sites that have not yet
# wired GHCNh into their sources list, since its whole point is to flag
# missing donor coverage before the operator relies on it.
#
# Station resolution (`resolve_station()`) is deliberately NOT called here:
# it requires a live GHCNh station catalogue (`.worldmet_get()`), which is
# real network IO outside this plan's scope -- `station_coverage()` is the
# tested seam (always mocked in Plan 16's own tests; Plan 06 tests the real
# implementation against its own resolved-station fixtures).
.backfill_ghcnh_adapter <- function(site) {
  adapters <- adapters_for_site(site)
  adapters[["ghcnh"]] %||% source_ghcnh(source_id = "ghcnh")
}

.backfill_coverage_audit <- function(site, window) {
  adapter <- .backfill_ghcnh_adapter(site)
  station_coverage(adapter, site, window)
}

# One site's day-0 bootstrap. Returns a summary list consumed by
# met_backfill()'s outer row-binding.
.met_backfill_site <- function(site, now, config, aws_export) {
  store_root <- config$store_root
  window <- .backfill_history_window(now)
  variables <- met_variables()$variable

  for (source in config$obs_sources) {
    obs <- .acquire_obs(source, site, window, now = now)
    if (nrow(obs) > 0) {
      store_write_obs(store_root, obs, now = now, mode = "supersede")
    }
  }

  if (!is.null(aws_export)) {
    ingest_aws_export(store_root, site, aws_export, now = now)
  }

  for (source in config$obs_sources) {
    correct_refit(store_root, site, source = source, variables = variables, now = now)
  }

  coverage <- .backfill_coverage_audit(site, window)

  forecast_gaps <- if (length(config$forecast_sources) > 0) {
    archive_forecasts(store_root, site, config$forecast_sources, now = now, missed = TRUE)
  } else {
    tibble::tibble(source = character(0), note = character(0))
  }

  tibble::tibble(
    site_id = site_id(site),
    coverage = list(coverage),
    forecast_gaps = list(forecast_gaps)
  )
}

#' Day-0 bootstrap: full history, AWS export ingestion, initial fits, and a
#' donor-coverage audit
#'
#' Ad hoc (SCOPING section 7.2/9). Per site: pulls full historical
#' observations for each configured obs source (SILO/ERA5/Open-Meteo
#' Previous-Runs, whatever `config$obs_sources` configures) via
#' `.acquire_obs()`; ingests a historical AWS logger export when
#' `aws_export` is supplied (`ingest_aws_export()`); makes initial
#' calibration fits (`correct_refit()`); and runs the **per-site
#' donor-coverage audit** (`station_coverage()`, Plan 06) so the operator
#' sees coverage gaps (e.g. a variable with no nearby GHCNh donor) before
#' relying on the gap-fill donor ladder (SCOPING section 13).
#'
#' Also documents forecast-gap backfillability for each configured forecast
#' source (`archive_forecasts(..., missed = TRUE)`): BOM forecast gaps
#' cannot be backfilled; Open-Meteo gaps self-heal via Previous/Single Runs.
#'
#' @inheritParams met_sync_live
#' @param aws_export Optional single string, a path/glob to a historical AWS
#'   logger CSV export to ingest via `ingest_aws_export()`. `NULL` (default)
#'   skips AWS ingestion.
#' @return A tibble with one row per site: `site_id`, `coverage` (a
#'   list-column, each element the `station_coverage()`-shaped donor audit
#'   tibble for that site), `forecast_gaps` (a list-column, each element the
#'   `archive_forecasts(missed = TRUE)` gap-backfillability summary for that
#'   site).
#' @family pipeline
#' @export
#' @examples
#' \dontrun{
#' met_backfill(site, config = my_pipeline_config, aws_export = "logger-a.csv")
#' }
met_backfill <- function(sites, now = .now(), config, aws_export = NULL) {
  sites <- as_met_sites(sites)
  rows <- lapply(sites@sites, function(site) {
    .met_backfill_site(site, now = now, config = config, aws_export = aws_export)
  })
  vctrs::vec_rbind(!!!rows)
}
