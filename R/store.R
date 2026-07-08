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
# Uses only a process id + an in-session counter + random suffix -- no wall
# clock read, per house style (`.now()` is the package's sole clock reader).
.part_file_counter <- local({
  i <- 0L
  function() {
    i <<- i + 1L
    i
  }
})

.part_file_name <- function() {
  paste0(
    "part-", Sys.getpid(), "-", .part_file_counter(), "-",
    paste(sample(c(letters, 0:9), 12, replace = TRUE), collapse = ""),
    ".parquet"
  )
}

# Write `df` as a new Parquet part-file inside `dir` (created if needed).
.write_part <- function(dir, df) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, .part_file_name())
  arrow::write_parquet(df, path)
  invisible(path)
}

# Atomically replace the contents of a partition directory with a single
# file containing `df`. Writes to a temp file in the *same* directory (so the
# rename is on the same filesystem and therefore atomic), then removes the
# old part-files and moves the temp file into place. Used by both the
# supersede rewrite path (store-obs.R) and store_compact().
.atomic_rewrite_partition <- function(dir, df) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tmp <- file.path(dir, paste0(".tmp-", .part_file_name()))
  arrow::write_parquet(df, tmp)
  old <- list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
  final <- file.path(dir, .part_file_name())
  file.rename(tmp, final)
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
