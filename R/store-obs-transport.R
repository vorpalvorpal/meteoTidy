# Plan 17 item 8 -- the obs_transport companion table: records which BOM
# ladder rung (`ftp_feeds` / `web_api`) served each observation (SCOPING
# section 5.1: "every stored value's provenance records the transport that
# served it"). `new_obs()` (R/schema-obs.R) strips any column beyond the 7
# canonical ones -- including `transport` -- before a fetch result reaches
# the observation store, so this companion table is the only place that
# provenance survives. Mirrors R/qc-log.R's pattern exactly: the same
# `.write_part()`/`.atomic_rewrite_partition()`-backed primitives from
# R/store.R, append-only writes, dedup-on-read keeping the latest write.

.obs_transport_dir <- function(store_root, site_id) {
  file.path(store_root, "obs_transport", paste0("site_id=", site_id))
}

# An empty, correctly-typed obs_transport tibble (used when no rows exist,
# and as the read-back shape when nothing has been written yet).
.obs_transport_empty <- function() {
  tibble::tibble(
    site_id = character(0),
    datetime_utc = as.POSIXct(character(0), tz = "UTC"),
    variable = character(0),
    source = character(0),
    transport = character(0),
    ingested_at = as.POSIXct(character(0), tz = "UTC")
  )
}

.obs_transport_col_order <- function() {
  c("site_id", "datetime_utc", "variable", "source", "transport")
}

# Deduplicate an obs_transport tibble (with an `ingested_at` bookkeeping
# column) on (site_id, datetime_utc, variable, source), keeping the row with
# the greatest `ingested_at` for each key. Mirrors `.qc_log_dedup()`
# (R/qc-log.R) adapted for this schema.
.obs_transport_dedup <- function(df) {
  if (nrow(df) == 0) {
    return(.obs_transport_empty())
  }
  key <- paste(
    df$site_id, format(df$datetime_utc, "%Y-%m-%dT%H:%M:%OS6", tz = "UTC"),
    df$variable, df$source,
    sep = "\r"
  )
  ord <- order(key, df$ingested_at, decreasing = c(FALSE, TRUE), method = "radix")
  df <- df[ord, , drop = FALSE]
  key <- key[ord]
  df <- df[!duplicated(key), , drop = FALSE]
  df <- df[order(df$datetime_utc, df$variable, df$source), , drop = FALSE]
  tibble::as_tibble(df[c(.obs_transport_col_order(), "ingested_at")])
}

#' Append rows to a site's obs_transport log
#'
#' Writes `rows` (columns `site_id`, `datetime_utc`, `variable`, `source`,
#' `transport`) as a new part-file under the site's `obs_transport`
#' partition, stamped with `ingested_at = now`. Does not itself deduplicate
#' on write (append-only, mirroring `qc_log_write()`); `obs_transport_read()`
#' performs the dedup on read, keeping the latest `ingested_at` per
#' `(site_id, datetime_utc, variable, source)` key.
#'
#' @param store_root Root directory of the store.
#' @param df A data frame with the obs_transport columns (see Description).
#'   Zero rows is a valid (silent) no-op.
#' @param now Injectable current time; see `.now()`.
#' @return `store_root`, invisibly.
#' @keywords internal
#' @noRd
obs_transport_write <- function(store_root, df, now = .now()) {
  if (is.null(df) || nrow(df) == 0) {
    return(invisible(store_root))
  }
  df <- tibble::as_tibble(df[.obs_transport_col_order()])
  df$ingested_at <- now

  for (sid in unique(df$site_id)) {
    dir <- .obs_transport_dir(store_root, sid)
    .write_part(dir, df[df$site_id == sid, , drop = FALSE])
  }
  invisible(store_root)
}

#' Read a site's obs_transport log (deduplicated, windowed)
#'
#' Reads every part-file under the site's `obs_transport` partition,
#' deduplicates on `(site_id, datetime_utc, variable, source)` keeping the
#' latest `ingested_at` per key, and filters to `datetime_utc` in
#' `[from, to]`.
#'
#' @param store_root Root directory of the store.
#' @param site_id Site identifier.
#' @param from,to `POSIXct` bounds (inclusive) on `datetime_utc`.
#' @return A tibble with columns `site_id`, `datetime_utc`, `variable`,
#'   `source`, `transport`; zero rows (typed) if nothing has been written
#'   yet or nothing falls in range.
#' @keywords internal
#' @noRd
obs_transport_read <- function(store_root, site_id, from, to) {
  dir <- .obs_transport_dir(store_root, site_id)
  if (!dir.exists(dir)) {
    return(.obs_transport_empty()[.obs_transport_col_order()])
  }
  files <- list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
  if (length(files) == 0) {
    return(.obs_transport_empty()[.obs_transport_col_order()])
  }
  raw <- tibble::as_tibble(do.call(rbind, lapply(files, arrow::read_parquet)))
  deduped <- .obs_transport_dedup(raw)
  in_range <- deduped$datetime_utc >= from & deduped$datetime_utc <= to
  deduped[in_range, .obs_transport_col_order(), drop = FALSE]
}
