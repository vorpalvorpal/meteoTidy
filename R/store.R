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
    )
  }, integer(1))
  max(per_site) + name
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

# Write `df` as a new Parquet part-file inside `dir` (created if needed) and
# verify it reads back.
.write_part <- function(dir, df) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- .new_part_path(dir)
  .check_path_length(path)
  arrow::write_parquet(df, path)
  .verify_part(path, nrow(df))
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

#' Compact partitioned Parquet tables in a store
#'
#' Rewrites every partition of every requested table that contains more than
#' one part-file into a single file, atomically (temp file + rename). This
#' does not change which rows are readable -- current, superseded, and
#' `as_of` reads return identical content before and after compaction. Safe
#' to call repeatedly (a no-op on an already-compacted store). Never runs
#' implicitly; intended to be called on a schedule (Plan 16's
#' `met_refit()`).
#'
#' @param store_root Root directory of the store.
#' @param tables Character vector of tables to compact; any of
#'   `"observations"`, `"forecasts"`, `"forecast_aux"`.
#' @return `store_root`, invisibly.
#' @keywords internal
#' @noRd
store_compact <- function(store_root, tables = .store_tables()) {
  with_store_lock(store_root, .store_compact_impl(store_root, tables))
}

.store_compact_impl <- function(store_root, tables) {
  unknown <- setdiff(tables, .store_tables())
  if (length(unknown) > 0) {
    abort_meteo(
      "Unknown table{?s} for compaction: {.val {unknown}}.",
      class = "unknown_store_table"
    )
  }

  for (table in tables) {
    dir <- .table_dir(store_root, table)
    if (!dir.exists(dir)) next
    part_files <- list.files(dir, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
    if (length(part_files) == 0) next
    partition_dirs <- unique(dirname(part_files))
    for (pdir in partition_dirs) {
      files <- list.files(pdir, pattern = "\\.parquet$", full.names = TRUE)
      if (length(files) <= 1) next
      combined <- do.call(rbind, lapply(files, arrow::read_parquet))
      .atomic_rewrite_partition(pdir, combined)
    }
  }

  invisible(store_root)
}
