# Plan 07 — circuit-breaker state for the BOM transport ladder.
#
# Persistence lives under `<store_root>/bom-breaker.json`, a new file (does
# not reuse R/store-watermark.R's own JSON persistence machinery, since the
# schema differs: this state is keyed by rung id, not by site_id/table/
# source). The breaker is a plain list, kept functional throughout: every
# mutator (`breaker_strike()`, `breaker_reset()`) returns a NEW breaker value
# rather than mutating in place, matching house style. Only `breaker_write()`
# touches disk.

# Path to the single breaker-state file under a store root.
.breaker_path <- function(store_root) {
  file.path(store_root, "bom-breaker.json")
}

#' Read the BOM transport circuit-breaker state
#'
#' Reads the persisted per-rung strike counts from `<store_root>/
#' bom-breaker.json`. If the file does not exist yet (first run), returns an
#' empty breaker state — every rung starts at 0 strikes
#' (`breaker_strikes()` returns `0` for any unknown rung id).
#'
#' @param store_root Single string, the store root directory.
#' @return A `bom_breaker` list (opaque; use the `breaker_*()` accessors),
#'   with one element `rungs`, a named list keyed by rung id, each holding
#'   `strikes` (integer) and `last_failure` (ISO8601 UTC string or `NA`).
#' @keywords internal
#' @noRd
breaker_read <- function(store_root) {
  path <- .breaker_path(store_root)
  if (!file.exists(path)) {
    return(structure(list(rungs = list()), class = "bom_breaker"))
  }
  raw <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE, simplifyVector = FALSE)
  rungs <- raw$rungs %||% list()
  structure(list(rungs = rungs), class = "bom_breaker")
}

#' Persist the BOM transport circuit-breaker state
#'
#' Writes `breaker` to `<store_root>/bom-breaker.json`, atomically (temp file
#' + rename), so a crash mid-write never leaves a corrupt file.
#'
#' @param store_root Single string, the store root directory.
#' @param breaker A `bom_breaker` list, as returned by `breaker_read()` /
#'   `breaker_strike()` / `breaker_reset()`.
#' @return `path`, invisibly.
#' @keywords internal
#' @noRd
breaker_write <- function(store_root, breaker) {
  path <- .breaker_path(store_root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp-", Sys.getpid())
  jsonlite::write_json(list(rungs = breaker$rungs), tmp, auto_unbox = TRUE, pretty = TRUE)
  file.rename(tmp, path)
  invisible(path)
}

#' Record a strike against a rung (functional)
#'
#' Increments the persistent-failure strike count for `rung_id` and stamps
#' `last_failure = now`. Does not write to disk; the caller persists the
#' result with `breaker_write()`.
#'
#' @param breaker A `bom_breaker` list.
#' @param rung_id Single string, the transport rung id (e.g. `"ftp_feeds"`).
#' @param now Injectable clock; the failure time to record.
#' @return A new `bom_breaker` list with the strike recorded.
#' @keywords internal
#' @noRd
breaker_strike <- function(breaker, rung_id, now) {
  current <- breaker_strikes(breaker, rung_id)
  breaker$rungs[[rung_id]] <- list(
    strikes = current + 1L,
    last_failure = format(now, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
  )
  breaker
}

#' Reset a rung's strike count to zero (functional)
#'
#' @inheritParams breaker_strike
#' @return A new `bom_breaker` list with `rung_id`'s strike count zeroed.
#' @keywords internal
#' @noRd
breaker_reset <- function(breaker, rung_id) {
  breaker$rungs[[rung_id]] <- list(strikes = 0L, last_failure = NA_character_)
  breaker
}

#' Current strike count for a rung
#'
#' @inheritParams breaker_strike
#' @return A single integer, `0` if `rung_id` has no recorded strikes.
#' @keywords internal
#' @noRd
breaker_strikes <- function(breaker, rung_id) {
  entry <- breaker$rungs[[rung_id]]
  if (is.null(entry) || is.null(entry$strikes)) {
    return(0L)
  }
  as.integer(entry$strikes)
}

#' Is a rung currently tripped?
#'
#' @inheritParams breaker_strike
#' @param threshold Integer, the strike count at/above which a rung is
#'   considered tripped. Default `3`.
#' @return A single logical.
#' @keywords internal
#' @noRd
breaker_tripped <- function(breaker, rung_id, threshold = 3) {
  breaker_strikes(breaker, rung_id) >= threshold
}
