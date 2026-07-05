# Plan 10 -- fill_run(): gap detection, tier routing, watermark, idempotency.
#
# Mirrors qc_run()'s (Plan 09) shape exactly: read the fill watermark (a
# store watermark keyed by `source = "fill"`), apply a small look-back so a
# donor arriving late can still supersede an earlier fill on the recent tail,
# read the window, detect gaps (NA/`missing`-flagged rows) per variable, run
# them through `fill_tier()`, write back only the rows that actually changed
# via `store_write_obs(mode = "supersede")` (so a later, better donor
# supersedes an earlier fill while the earlier fill stays retrievable for
# audit -- exactly the observation store's existing revision policy, no new
# mechanism needed), and advance the watermark.

# How far before the watermark to re-scan on every run, so a better donor
# that arrives later can still retrigger (and supersede) a fill made from a
# worse donor on the recent tail. Mirrors `.qc_lookback()`'s reasoning
# (R/qc.R): not pinned to a specific duration by any test, so a value
# generous enough to cover typical adapter revision lag without re-scanning
# excessive history.
.fill_lookback <- function() {
  as.difftime(7, units = "days")
}

#' Run the gap-fill engine over a site's observation window
#'
#' Reads the fill watermark for `site` (a store watermark keyed by `source =
#' "fill"`), applies a small look-back (`.fill_lookback()`) so a better donor
#' arriving after an earlier fill can still supersede it, reads the
#' resulting window's observations, detects gaps per variable (`NA`/
#' `"missing"`-flagged `value` rows), fills them via `fill_tier()`, writes
#' back only the rows whose value/method actually changed via
#' `store_write_obs(mode = "supersede")` (so an earlier fill stays
#' retrievable via `include_superseded = TRUE` when a later run replaces it
#' with a better one), and advances the watermark to `now`.
#'
#' Incremental: only the window from `watermark - lookback` to `now` is
#' scanned. Idempotent: re-running over the same window with the same inputs
#' reproduces identical filled values and does not create duplicate rows
#' (the store's supersede path already de-duplicates identical
#' value/qc_flag/method rows -- see `store_write_obs()`, Plan 03).
#'
#' @param store_root Root directory of the store.
#' @param site A `met_site` object.
#' @param variables Character vector of variables to fill; `NULL` (default)
#'   means every variable present in the window.
#' @param now Injectable current time; see `.now()`.
#' @param donors A named list of single-variable long donor obs tibbles (see
#'   `fill_medium()`), or `NULL` (default) if none are available yet (the
#'   micro tier and, if `model` is supplied, the macro tier still apply).
#' @param model A single long obs tibble (a model series, e.g.
#'   `source = "openmeteo"`) covering the window, for the macro tier and for
#'   `model_only` variables; `NULL` (default) if none is available.
#' @return Invisibly, a list `(n_filled)` summarising the run.
#' @family fill
#' @export
#' @examples
#' \dontrun{
#' root <- withr::local_tempdir()
#' site <- met_site(
#'   site_id = "example",
#'   latitude = units::set_units(-34.75, "degree"),
#'   longitude = units::set_units(148.20, "degree"),
#'   elevation = units::set_units(220, "m"),
#'   timezone = "Australia/Sydney",
#'   instruments = list(),
#'   sources = list(),
#'   store_root = root
#' )
#' fill_run(root, site)
#' }
fill_run <- function(store_root, site, variables = NULL, now = .now(),
                     donors = NULL, model = NULL) {
  sid <- site_id(site)
  watermark <- store_get_watermark(store_root, sid, "observations", "fill")
  from <- if (is.na(watermark)) NULL else watermark - .fill_lookback()

  window <- store_read_obs(store_root, sid, variables = variables, from = from, to = now)
  if (nrow(window) == 0) {
    store_set_watermark(store_root, sid, "observations", "fill", now)
    return(invisible(list(n_filled = 0L)))
  }

  filled <- fill_tier(window, dict = met_variables(), model = model, donors = donors, site = site)
  filled <- filled[order(filled$variable, filled$datetime_utc), , drop = FALSE]
  window_ord <- window[order(window$variable, window$datetime_utc), , drop = FALSE]

  value_changed <- xor(is.na(filled$value), is.na(window_ord$value)) |
    (!is.na(filled$value) & !is.na(window_ord$value) & filled$value != window_ord$value)
  method_changed <- filled$method != window_ord$method
  flag_changed <- filled$qc_flag != window_ord$qc_flag
  changed <- value_changed | method_changed | flag_changed

  n_filled <- sum(changed)
  if (n_filled > 0) {
    store_write_obs(store_root, filled[changed, , drop = FALSE], now = now, mode = "supersede")
  }

  store_set_watermark(store_root, sid, "observations", "fill", now)
  invisible(list(n_filled = n_filled))
}
