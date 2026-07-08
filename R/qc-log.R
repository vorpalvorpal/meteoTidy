# Plan 09 -- the qc_log companion table: an auditable per-rule record of every
# QC decision (SCOPING section 3.2 audit spirit). Schema: (site_id,
# datetime_utc, variable, rule, outcome, detail). Stored as a
# site_id-partitioned Parquet dataset under
# <store_root>/qc_log/site_id=<id>/part-*.parquet, using the same
# `.write_part()`/`.atomic_rewrite_partition()` primitives as
# R/store-obs.R and R/store-calib.R (Plan 03) rather than inventing a new IO
# mechanism.
#
# Deduplication: qc_run() must be idempotent, so re-running the same rule
# over the same window must not create duplicate log rows. Dedup key is
# (site_id, datetime_utc, variable, rule), keeping the row from the LATEST
# write (by `logged_at`) -- a later run's verdict supersedes an earlier run's
# for the same decision point, matching the observation store's
# supersede-on-revision spirit without needing a full revision history for
# what is, after all, just an audit log.

.qc_log_dir <- function(store_root, site_id) {
  file.path(store_root, "qc_log", paste0("site_id=", site_id))
}

# An empty, correctly-typed qc_log tibble (used when no rows are logged, and
# as the read-back shape when nothing has been written yet).
.qc_log_empty <- function() {
  tibble::tibble(
    site_id = character(0),
    datetime_utc = as.POSIXct(character(0), tz = "UTC"),
    variable = character(0),
    rule = character(0),
    outcome = character(0),
    detail = character(0)
  )
}

.qc_log_col_order <- function() {
  c("site_id", "datetime_utc", "variable", "rule", "outcome", "detail")
}

# Deduplicate a qc_log tibble (with a `logged_at` bookkeeping column) on
# (site_id, datetime_utc, variable, rule), keeping the row with the greatest
# `logged_at` for each key. Drops `logged_at` from the returned columns.
.qc_log_dedup <- function(df) {
  if (nrow(df) == 0) {
    return(.qc_log_empty())
  }
  key <- paste(
    df$site_id, format(df$datetime_utc, "%Y-%m-%dT%H:%M:%OS6", tz = "UTC"),
    df$variable, df$rule,
    sep = "\r"
  )
  ord <- order(key, df$logged_at, decreasing = c(FALSE, TRUE), method = "radix")
  df <- df[ord, , drop = FALSE]
  key <- key[ord]
  df <- df[!duplicated(key), , drop = FALSE]
  df <- df[order(df$datetime_utc, df$variable, df$rule), , drop = FALSE]
  tibble::as_tibble(df[.qc_log_col_order()])
}

#' Append rows to a site's qc_log
#'
#' Writes `rows` (columns `site_id`, `datetime_utc`, `variable`, `rule`,
#' `outcome`, `detail`) as a new part-file under the site's `qc_log`
#' partition, stamped with `logged_at = now`. Does not itself deduplicate on
#' write (append-only, mirroring `store_write_obs(mode = "append")`);
#' `qc_log_read()` performs the dedup on read, keeping the latest `logged_at`
#' per `(site_id, datetime_utc, variable, rule)` key, so repeated `qc_run()`
#' invocations never accumulate duplicate readable rows.
#'
#' @param store_root Root directory of the store.
#' @param rows A data frame with the qc_log columns (see Description). Zero
#'   rows is a valid (silent) no-op.
#' @param now Injectable current time; see `.now()`.
#' @return `store_root`, invisibly.
#' @keywords internal
#' @noRd
qc_log_write <- function(store_root, rows, now = .now()) {
  if (is.null(rows) || nrow(rows) == 0) {
    return(invisible(store_root))
  }
  rows <- tibble::as_tibble(rows[.qc_log_col_order()])
  rows$logged_at <- now

  for (sid in unique(rows$site_id)) {
    dir <- .qc_log_dir(store_root, sid)
    .write_part(dir, rows[rows$site_id == sid, , drop = FALSE])
  }
  invisible(store_root)
}

#' Read a site's qc_log (deduplicated)
#'
#' Reads every part-file under the site's `qc_log` partition and
#' deduplicates on `(site_id, datetime_utc, variable, rule)`, keeping the
#' latest `logged_at` per key, so a caller always sees each rule's most
#' recent verdict for a given row exactly once (idempotency: re-running
#' `qc_run()` over the same window must not change `nrow(qc_log_read(...))`).
#'
#' @param store_root Root directory of the store.
#' @param site_id Site identifier.
#' @return A tibble with columns `site_id`, `datetime_utc`, `variable`,
#'   `rule`, `outcome`, `detail`; zero rows (typed) if nothing has been
#'   logged yet.
#' @keywords internal
#' @noRd
qc_log_read <- function(store_root, site_id) {
  dir <- .qc_log_dir(store_root, site_id)
  if (!dir.exists(dir)) {
    return(.qc_log_empty())
  }
  files <- list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
  if (length(files) == 0) {
    return(.qc_log_empty())
  }
  raw <- tibble::as_tibble(do.call(rbind, lapply(files, arrow::read_parquet)))
  .qc_log_dedup(raw)
}
