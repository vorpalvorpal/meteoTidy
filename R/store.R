# Plan 03 — storage layer: layout constants, dataset path builders, and
# partition compaction shared by R/store-obs.R, R/store-forecast.R,
# R/store-calib.R, and R/store-watermark.R.
#
# Layout (SCOPING §8):
#   <store_root>/observations/site_id=<id>/year=<yyyy>/part-*.parquet
#   <store_root>/forecasts/source=<src>/site_id=<id>/issue_date=<yyyy-mm-dd>/part-*.parquet
#   <store_root>/forecast_aux/source=<src>/site_id=<id>/issue_date=<yyyy-mm-dd>/part-*.parquet
#   <store_root>/calibrations/site_id=<id>/manifest.json
#   <store_root>/calibrations/site_id=<id>/<variable>-<source>-v<ver>.parquet
#   <store_root>/watermarks/site_id=<id>/watermarks.json
#
# Partition columns (`year`, `issue_date`) are always DERIVED from UTC
# timestamps at write time -- callers never set them directly.

# The tables that live under a store_root as hive-partitioned Parquet
# datasets (calibrations/watermarks are not "tables" in this sense: they are
# per-site JSON + individually-named Parquet files, handled separately).
.store_tables <- function() {
  c("observations", "forecasts", "forecast_aux")
}

# Directory holding a table's partitioned dataset, e.g.
# <store_root>/observations
.table_dir <- function(store_root, table) {
  file.path(store_root, table)
}

# Build (and, if `create = TRUE`, create) the partition directory for one row
# of partition values. `parts` is a named list in the order the partition
# should nest, e.g. list(site_id = "abc", year = 2026) or
# list(source = "openmeteo", site_id = "abc", issue_date = "2026-01-01").
dataset_partition_dir <- function(store_root, table, parts, create = FALSE) {
  segs <- vapply(names(parts), function(nm) {
    paste0(nm, "=", parts[[nm]])
  }, character(1))
  dir <- file.path(.table_dir(store_root, table), do.call(file.path, as.list(segs)))
  if (create) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

#' Build a dataset path for a store table
#'
#' Constructs the hive-partitioned directory for one partition of `table`
#' under `store_root`, optionally creating it. This is the single path
#' builder used by every `store_write_*()`/`store_read_*()` function so the
#' on-disk layout is defined in exactly one place.
#'
#' @param store_root Root directory of the store (a site's `store_root`).
#' @param table One of `"observations"`, `"forecasts"`, `"forecast_aux"`.
#' @param parts A named list of partition key/value pairs, in nesting order.
#' @param create Logical; create the directory if it does not exist.
#' @return A single-string path.
#' @keywords internal
#' @noRd
dataset_path <- function(store_root, table, parts, create = FALSE) {
  dataset_partition_dir(store_root, table, parts, create = create)
}

# A fresh, collision-resistant part-file name for appending to a partition.
# Kept SHORT (follow-up review, item 1): Windows cannot open paths longer
# than 259 characters, and the hive partition directories already take ~70
# of them, so the old 34-39 character names ("part-<pid>-<n>-<12>.parquet",
# temp files with a ".tmp-" prefix) pushed real stores over the limit.
# Writers hold the store lock, and .write_part() never overwrites an existing
# file, so 10 random characters (36^10) are ample. Temp files start with a
# dot, which arrow datasets ignore. No wall clock is read (house style).
.part_file_name <- function(tmp = FALSE) {
  paste0(
    if (tmp) ".t" else "p-",
    paste(sample(c(letters, 0:9), 10, replace = TRUE), collapse = ""),
    ".parquet"
  )
}

# A part-file path in `dir` that does not exist yet.
.new_part_path <- function(dir, tmp = FALSE) {
  repeat {
    path <- file.path(dir, .part_file_name(tmp = tmp))
    if (!file.exists(path)) {
      return(path)
    }
  }
}

# ---- Windows path-length guard (follow-up review, item 1) --------------------

# The longest path Windows opens without the LongPathsEnabled policy.
.max_windows_path <- function() 259L

# The budget planned store paths must fit in: a margin under the hard limit.
.store_path_budget <- function() {
  getOption("meteoTidy.max_store_path", 250L)
}

# The full long-form spelling of `path`, as arrow and Windows will open it:
# 8.3 short names (C:/Users/KATOOM~1/...) are expanded, so a path that looks
# short enough can still be too long. `path` need not exist; its deepest
# existing ancestor is normalised and the rest appended.
.long_form_path <- function(path) {
  path <- gsub("\\\\", "/", path)
  tail <- character(0)
  head <- path
  while (!dir.exists(head) && !file.exists(head)) {
    parent <- dirname(head)
    if (identical(parent, head)) break
    tail <- c(basename(head), tail)
    head <- parent
  }
  head <- normalizePath(head, winslash = "/", mustWork = FALSE)
  if (length(tail)) do.call(file.path, as.list(c(head, tail))) else head
}

# Abort (class "store_path_too_long") if `path` cannot be opened on Windows.
.check_path_length <- function(path) {
  full <- .long_form_path(path)
  limit <- .max_windows_path()
  if (.Platform$OS.type == "windows" && nchar(full) > limit) {
    abort_meteo(
      c(
        "Store path is too long for Windows ({nchar(full)} characters; the limit is {limit}).",
        "x" = "{.path {full}}",
        "i" = "Use a shorter {.field store_root} (e.g. {.path C:/meteo/store})."
      ),
      class = "store_path_too_long"
    )
  }
  invisible(full)
}

# The longest path a sync of `site_ids` x `sources` can write under
# `store_root`: every partitioned table plus the longest part-file name.
.planned_max_path <- function(store_root, site_ids, sources) {
  root <- .long_form_path(store_root)
  sources <- c(sources, "")
  name <- nchar(.part_file_name(tmp = TRUE)) + 1L
  per_site <- vapply(site_ids, function(sid) {
    max(
      nchar(file.path(root, "observations", paste0("site_id=", sid), "year=2026")),
      nchar(file.path(root, "forecast_aux", paste0("source=", sources),
                      paste0("site_id=", sid), "issue_date=2026-01-01")),
      nchar(file.path(root, "verification_diagnostics", paste0("site_id=", sid))),
      nchar(file.path(root, "obs_transport", paste0("site_id=", sid))),
      nchar(file.path(root, "qc_log", paste0("site_id=", sid)))
    ) + name
  }, integer(1))
  # Calibration coefficient files (<variable>-<source>-vN.parquet) carry
  # their own long names instead of a short part-file name.
  longest_var <- max(nchar(met_variables()$variable))
  calib <- vapply(site_ids, function(sid) {
    nchar(.calib_dir(root, sid)) + 1L + longest_var + 1L + max(nchar(sources)) +
      nchar("-v999.parquet")
  }, integer(1))
  max(per_site, calib)
}

# Refuse to start a sync whose store paths could exceed the Windows limit:
# failing up front beats a run that writes some partitions and not others.
.check_store_paths <- function(store_root, site_ids, sources) {
  planned <- .planned_max_path(store_root, site_ids, sources)
  budget <- .store_path_budget()
  limit <- .max_windows_path()
  if (planned > budget) {
    root <- .long_form_path(store_root)
    abort_meteo(
      c(
        "{.field store_root} is too long: store paths would reach {planned} characters (budget {budget}; Windows cannot open more than {limit}).", # nolint: line_length_linter.
        "x" = "{.path {root}} is {nchar(root)} characters.",
        "i" = "Use a store_root of at most {nchar(root) - (planned - budget)} characters (e.g. {.path C:/meteo/store})."
      ),
      class = "store_path_too_long"
    )
  }
  invisible(planned)
}

# Re-open a just-written part file the way readers will (arrow expands the
# path to its long form) and check its row count, so a write that cannot be
# read back is an error rather than a silent loss.
.verify_part <- function(path, n) {
  got <- tryCatch({
    # Close the handle explicitly: an open file cannot be renamed or removed
    # on Windows.
    f <- arrow::ReadableFile$create(.long_form_path(path))
    on.exit(f$close(), add = TRUE)
    arrow::ParquetFileReader$create(f)$num_rows
  }, error = function(e) conditionMessage(e))
  if (!is.numeric(got) || got != n) {
    detail <- if (is.numeric(got)) sprintf("%d rows read back, %d written", as.integer(got), as.integer(n)) else got
    detail <- gsub("([{}])", "\\1\\1", .one_line(detail))
    abort_meteo(
      c("A store write could not be verified: {.path {path}}.", "x" = detail),
      class = "store_write_unverified"
    )
  }
  invisible(path)
}

# Write `df` as a new Parquet part-file inside `dir` (created if needed):
# to a temp name first (readers ignore dot-files), verified, renamed into
# place and verified again. On any failure nothing is left behind -- an
# unreadable part-file in a partition would break every later read.
.write_part <- function(dir, df) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tmp <- .new_part_path(dir, tmp = TRUE)
  path <- .new_part_path(dir)
  .check_path_length(tmp)
  .check_path_length(path)
  done <- FALSE
  on.exit(if (!done) unlink(c(tmp, path)), add = TRUE)
  arrow::write_parquet(df, tmp)
  .verify_part(tmp, nrow(df))
  if (!isTRUE(suppressWarnings(file.rename(tmp, path)))) {
    abort_meteo("Could not move {.path {tmp}} into place.", class = "store_write_unverified")
  }
  .verify_part(path, nrow(df))
  done <- TRUE
  invisible(path)
}

# Atomically replace the contents of a partition directory with a single
# file containing `df`. Writes a verified temp file in the *same* directory
# (so the rename is on the same filesystem and therefore atomic), renames it
# into place, verifies it again, and only then removes the old part-files.
# Any failure leaves the old files untouched (and removes the temp file).
# Used by the supersede rewrite path (store-obs.R) and store_compact().
.atomic_rewrite_partition <- function(dir, df) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tmp <- .new_part_path(dir, tmp = TRUE)
  final <- .new_part_path(dir)
  .check_path_length(tmp)
  .check_path_length(final)
  old <- list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
  done <- FALSE
  on.exit(if (!done) unlink(c(tmp, if (file.exists(final) && !(final %in% old)) final)), add = TRUE)
  arrow::write_parquet(df, tmp)
  .verify_part(tmp, nrow(df))
  if (!isTRUE(suppressWarnings(file.rename(tmp, final)))) {
    abort_meteo("Could not move {.path {tmp}} into place.", class = "store_write_unverified")
  }
  .verify_part(final, nrow(df))
  done <- TRUE
  # Remove the old part-files only after the new one is safely in place.
  unlink(setdiff(old, final))
  invisible(final)
}

# Open an arrow Dataset for a table if its directory exists and has data,
# else NULL (an empty/nonexistent dataset reads as zero rows).
.open_dataset <- function(store_root, table) {
  dir <- .table_dir(store_root, table)
  if (!dir.exists(dir) || length(list.files(dir, pattern = "\\.parquet$", recursive = TRUE)) == 0) {
    return(NULL)
  }
  arrow::open_dataset(dir, format = "parquet", partitioning = arrow::hive_partition())
}

#' Compact a store's Parquet tables
#'
#' Every sync appends small part-files: one per partition written to the
#' observation and forecast tables, and one per site per run to the
#' `qc_log` and `obs_transport` logs (which are otherwise never rewritten).
#' Left alone, an hourly schedule accumulates tens of thousands of tiny
#' files, which slows every read. `met_compact()` rewrites each partition
#' holding more than one part-file as a single file, atomically (a verified
#' temp file renamed into place; the old files are removed only afterwards).
#'
#' What readers see does not change: current, superseded and `as_of`
#' observation reads, forecast reads, and the deduplicated `qc_log` /
#' `obs_transport` reads return the same rows before and after. Rows that no
#' read can return are dropped: exact duplicate forecast keys (left by
#' pre-lock concurrent writers), and in the two logs every entry superseded
#' by a later write for the same key.
#'
#' The whole compaction holds the store's write lock, so it is safe to run
#' while syncs are scheduled: it waits for a running sync (up to
#' `lock_timeout` seconds) and a sync that starts meanwhile waits for it.
#' Run it on a schedule, e.g. weekly, at a time no sync is due.
#'
#' @param store_root Root directory of the store (`config$store_root`).
#' @param tables Tables to compact; any of `"observations"`, `"forecasts"`,
#'   `"forecast_aux"`, `"qc_log"`, `"obs_transport"` (default: all).
#' @param lock_timeout Seconds to wait for another process's store lock
#'   before aborting with class `meteoTidy_error_store_locked`. Defaults to
#'   `getOption("meteoTidy.lock_timeout", 600)`.
#' @return A tibble with one row per table: `table`, `files_before`,
#'   `files_after`, and `rows_before`/`rows_after` (rows in the partitions it
#'   rewrote), invisibly.
#' @family pipeline
#' @export
#' @examples
#' \dontrun{
#' # Weekly, e.g. Sunday 03:40, between two hourly syncs:
#' met_compact("C:/meteo/store")
#' }
met_compact <- function(store_root, tables = .compact_tables(), lock_timeout = NULL) {
  unknown <- setdiff(tables, .compact_tables())
  if (length(unknown) > 0) {
    abort_meteo(
      "Unknown table{?s} for compaction: {.val {unknown}}.",
      class = "unknown_store_table"
    )
  }
  with_store_lock(store_root, .store_compact_impl(store_root, tables), timeout = lock_timeout)
}

# Internal name kept for existing callers.
store_compact <- function(store_root, tables = .store_tables()) {
  met_compact(store_root, tables = tables)
}

# Every table met_compact() knows: the hive-partitioned tables plus the two
# append-only logs.
.compact_tables <- function() {
  c(.store_tables(), "qc_log", "obs_transport")
}

# Rows a compacted partition keeps (see met_compact()): identical to what
# the table's reader returns, minus rows no read can see.
.compact_rows <- function(table, df) {
  latest_per_key <- function(df, key_cols, stamp) {
    key <- do.call(paste, c(lapply(df[key_cols], function(x) {
      if (inherits(x, "POSIXct")) format(x, "%Y-%m-%dT%H:%M:%OS6", tz = "UTC") else as.character(x)
    }), sep = "\r"))
    ord <- order(key, df[[stamp]], decreasing = c(FALSE, TRUE), method = "radix")
    df <- df[ord, , drop = FALSE]
    df[!duplicated(key[ord]), , drop = FALSE]
  }
  switch(table,
    qc_log = latest_per_key(df, c("site_id", "datetime_utc", "variable", "rule"), "logged_at"),
    obs_transport = latest_per_key(df, c("site_id", "datetime_utc", "variable", "source"), "ingested_at"),
    forecasts = df[!duplicated(df[intersect(c("site_id", "source", "model", "issue_time", "valid_time",
                                               "member", "stat", "variable"), names(df))]), , drop = FALSE],
    forecast_aux = df[!duplicated(df[intersect(c("site_id", "source", "issue_time", "valid_time", "field"),
                                               names(df))]), , drop = FALSE],
    df
  )
}

.store_compact_impl <- function(store_root, tables) {
  out <- lapply(tables, function(table) {
    dir <- .table_dir(store_root, table)
    part_files <- if (dir.exists(dir)) {
      list.files(dir, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
    } else {
      character(0)
    }
    rows_before <- 0L
    rows_after <- 0L
    for (pdir in unique(dirname(part_files))) {
      files <- list.files(pdir, pattern = "\\.parquet$", full.names = TRUE)
      if (length(files) <= 1) next
      combined <- tibble::as_tibble(do.call(rbind, lapply(files, arrow::read_parquet)))
      kept <- .compact_rows(table, combined)
      rows_before <- rows_before + nrow(combined)
      rows_after <- rows_after + nrow(kept)
      .atomic_rewrite_partition(pdir, kept)
    }
    files_after <- if (dir.exists(dir)) {
      length(list.files(dir, pattern = "\\.parquet$", recursive = TRUE))
    } else {
      0L
    }
    tibble::tibble(table = table, files_before = length(part_files), files_after = files_after,
                   rows_before = rows_before, rows_after = rows_after)
  })
  invisible(vctrs::vec_rbind(!!!out))
}
