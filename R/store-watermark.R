# Plan 03 — watermarks: per-site JSON tracking, per (table, source), the UTC
# instant through which data has been processed. Drives the re-fetch window
# that implements the observation-revision policy (SCOPING §9).

# Path to the single watermark file for one site.
.watermark_path <- function(store_root, site_id) {
  file.path(store_root, "watermarks", paste0("site_id=", site_id), "watermarks.json")
}

# Read all watermark rows for a site as a list of
# list(table=, source=, watermark= <ISO8601 string>). Empty list if the file
# does not exist yet.
.read_watermarks <- function(store_root, site_id) {
  path <- .watermark_path(store_root, site_id)
  if (!file.exists(path)) {
    return(list())
  }
  jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
}

.write_watermarks <- function(store_root, site_id, entries) {
  path <- .watermark_path(store_root, site_id)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp-", Sys.getpid())
  jsonlite::write_json(entries, tmp, auto_unbox = TRUE, pretty = TRUE)
  file.rename(tmp, path)
  invisible(path)
}

#' Get the stored watermark for (site, table, source)
#'
#' @param store_root Root directory of the store.
#' @param site_id Site identifier.
#' @param table Logical table name the watermark tracks (e.g.
#'   `"observations"`).
#' @param source Data source name (e.g. `"silo"`).
#' @return A UTC `POSIXct` scalar, or `NA` (POSIXct) if unset.
#' @keywords internal
#' @noRd
store_get_watermark <- function(store_root, site_id, table, source) {
  entries <- .read_watermarks(store_root, site_id)
  for (e in entries) {
    if (identical(e$table, table) && identical(e$source, source)) {
      return(as.POSIXct(e$watermark, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OSZ"))
    }
  }
  as.POSIXct(NA_character_, tz = "UTC")
}

#' Set the watermark for (site, table, source)
#'
#' Writes/replaces the watermark entry atomically (temp file + rename).
#'
#' @inheritParams store_get_watermark
#' @param t A UTC `POSIXct` scalar, the new watermark instant.
#' @return `t`, invisibly.
#' @keywords internal
#' @noRd
store_set_watermark <- function(store_root, site_id, table, source, t) {
  entries <- .read_watermarks(store_root, site_id)
  stamp <- format(t, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
  matched <- FALSE
  for (i in seq_along(entries)) {
    if (identical(entries[[i]]$table, table) && identical(entries[[i]]$source, source)) {
      entries[[i]]$watermark <- stamp
      matched <- TRUE
      break
    }
  }
  if (!matched) {
    entries[[length(entries) + 1]] <- list(table = table, source = source, watermark = stamp)
  }
  .write_watermarks(store_root, site_id, entries)
  invisible(t)
}

#' Compute the effective fetch window for a sync
#'
#' Implements the observation-revision re-fetch window (SCOPING §9):
#' `from = watermark - refetch`, `to = now`. When there is no watermark yet,
#' `from = NULL` signals "fetch full history".
#'
#' @inheritParams store_get_watermark
#' @param refetch A `difftime`, how far back of the watermark to re-fetch (to
#'   pick up upstream revisions).
#' @param now Injectable current time; see `.now()`.
#' @return A list with elements `from` (POSIXct or `NULL`) and `to`
#'   (POSIXct).
#' @keywords internal
#' @noRd
store_effective_fetch_window <- function(store_root, site_id, table, source,
                                         refetch, now = .now()) {
  wm <- store_get_watermark(store_root, site_id, table, source)
  from <- if (is.na(wm)) NULL else wm - refetch
  list(from = from, to = now)
}
