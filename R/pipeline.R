#' @include adapter.R site-list.R site.R store-obs.R source-file.R adapter-mapping.R
NULL

# Plan 16 -- shared orchestration helpers: the per-site loop with error
# isolation (`for_each_site()`) and the two acquisition seams
# (`.acquire_obs()`/`.acquire_forecast()`) every verb calls per configured
# source. This file introduces no new science: it composes adapters
# (Plans 04-08), curation (09-10), correction (11-12), verification (13),
# and storage (03) primitives that already exist and are already tested.

#' Run a function over every site, isolating per-site failures
#'
#' The shared per-site loop every pipeline verb (`met_sync_live()`,
#' `met_sync_daily()`, `met_refit()`, `met_backfill()`) is built on. `sites`
#' is normalised via `as_met_sites()`, so a single `met_site` or a
#' `met_sites` collection are both accepted.
#'
#' Under the default `on_error = "isolate"` (SCOPING section 5.1
#' degradation), an error raised by `fn(site)` is caught and recorded as
#' status `"error"` for that site; the loop continues to the remaining
#' sites. Under `on_error = "stop"`, the first error propagates uncaught.
#'
#' This is a lower-level primitive than a verb's own "degraded" status
#' (see `met_sync_live()`): a verb is expected to catch its *own* known
#' failure modes (a dead acquisition source) internally and report
#' `"degraded"` without ever throwing, so `for_each_site()`'s `"error"` path
#' is reserved for genuine, unanticipated bugs in `fn` itself.
#'
#' @param sites A `met_site`, a `met_sites`, or a plain list of `met_site`.
#' @param fn A function of one argument, a single `met_site`. Its return
#'   value becomes the `result` list-column for that site's row.
#' @param on_error Either `"isolate"` (default; catch and continue) or
#'   `"stop"` (re-raise the first error).
#' @param ... Unused; reserved.
#' @return A tibble with columns `site_id`, `status` (`"ok"` or `"error"`),
#'   `message` (the error's `conditionMessage()`, `NA` on success), and
#'   `result` (a list-column of `fn(site)`'s return value, `NULL` on error).
#' @keywords internal
#' @noRd
for_each_site <- function(sites, fn, on_error = c("isolate", "stop"), ...) {
  on_error <- rlang::arg_match(on_error)
  sites <- as_met_sites(sites)

  ids <- character(0)
  statuses <- character(0)
  messages <- character(0)
  results <- list()

  for (site in sites@sites) {
    sid <- site_id(site)

    if (on_error == "stop") {
      value <- fn(site)
      ids <- c(ids, sid)
      statuses <- c(statuses, "ok")
      messages <- c(messages, NA_character_)
      results[[length(results) + 1]] <- value
      next
    }

    cnd <- rlang::catch_cnd(value <- fn(site), classes = "error")
    if (is.null(cnd)) {
      ids <- c(ids, sid)
      statuses <- c(statuses, "ok")
      messages <- c(messages, NA_character_)
      results[[length(results) + 1]] <- value
    } else {
      ids <- c(ids, sid)
      statuses <- c(statuses, "error")
      messages <- c(messages, conditionMessage(cnd))
      results[[length(results) + 1]] <- NULL
    }
  }

  tibble::tibble(
    site_id = ids, status = statuses, message = messages,
    result = results
  )
}

#' Acquire observations for one configured source (internal seam)
#'
#' Looks up `source` in `adapters_for_site(site)` and calls `fetch()` on it.
#' This is the package-owned entry point every verb calls per obs source;
#' tests mock this exact function (see `tests/testthat/helper-pipeline.R`'s
#' `mock_acquisition()`) rather than the underlying adapters, so the verbs
#' can be tested without any network fixture replay.
#'
#' @param source Source name (must be a key of `adapters_for_site(site)`).
#' @param site A `met_site` object.
#' @param window A list with `from`/`to`, both UTC `POSIXct` scalars.
#' @param now Injectable current time; see `.now()`.
#' @param variables Character vector of variables to request; `NULL`
#'   (default) requests everything the adapter `provides`.
#' @return A canonical long observation tibble (see `new_obs()`).
#' @keywords internal
#' @noRd
.acquire_obs <- function(source, site, window, now = .now(), variables = NULL) {
  adapters <- adapters_for_site(site)
  adapter <- adapters[[source]]
  if (is.null(adapter)) {
    abort_meteo(
      c(
        "Source {.val {source}} is not configured for site {.val {site_id(site)}}.",
        "i" = "Configured sources: {.val {names(adapters)}}."
      ),
      class = "unknown_adapter"
    )
  }
  fetch(adapter, site, variables %||% adapter@provides, window, now = now)
}

#' Acquire forecasts for one configured source (internal seam)
#'
#' Like `.acquire_obs()`, but calls `fetch_forecast()`.
#'
#' @inheritParams .acquire_obs
#' @param window A list with `from`/`to`, both UTC `POSIXct` scalars,
#'   bounding the forecast issue times of interest.
#' @return A canonical forecast tibble (see `new_forecast()`).
#' @keywords internal
#' @noRd
.acquire_forecast <- function(source, site, window, now = .now(), variables = NULL) {
  adapters <- adapters_for_site(site)
  adapter <- adapters[[source]]
  if (is.null(adapter)) {
    abort_meteo(
      c(
        "Source {.val {source}} is not configured for site {.val {site_id(site)}}.",
        "i" = "Configured sources: {.val {names(adapters)}}."
      ),
      class = "unknown_adapter"
    )
  }
  fetch_forecast(adapter, site, variables %||% adapter@provides, window, now = now)
}

#' Ingest a historical AWS logger export into the store (internal seam)
#'
#' Builds a `source_file()` adapter pointed at `path` and writes the fetched
#' rows to the store. Used by `met_backfill()` for day-0 ingestion of a
#' site's pre-existing historical AWS CSV exports (SCOPING section 7.2).
#'
#' @param store_root Root directory of the store.
#' @param site A `met_site` object.
#' @param path Single string, a file path or glob matching one or more CSV
#'   exports.
#' @param source_id Single string stamped into the `source` column
#'   (default `"site_aws"`).
#' @param mapping A `met_mapping()` describing how to parse `path`. Defaults
#'   to a minimal single-column `temperature_2m` mapping matching the
#'   package's test fixture logger export shape; callers with a differently
#'   shaped export should supply their own mapping.
#' @param now Injectable current time; see `.now()`.
#' @param ... Passed on to `source_file()` (e.g. `delim`, `has_header`).
#' @return Invisibly, the fetched observation tibble that was written.
#' @keywords internal
#' @noRd
ingest_aws_export <- function(store_root, site, path, source_id = "site_aws",
                              mapping = NULL, now = .now(), ...) {
  mapping <- mapping %||% met_mapping(
    format = "csv",
    time = list(column = "timestamp", tz = "UTC"),
    variables = list(
      list(variable = "temperature_2m", column = "temp_c", unit = "degC")
    )
  )

  adapter <- source_file(
    source_id = source_id, glob = path, mapping = mapping, ...
  )

  fetched <- fetch(adapter, site, adapter@provides,
                   window = list(from = NULL, to = now), now = now)
  store_write_obs(store_root, fetched, now = now, mode = "supersede")
  invisible(fetched)
}
