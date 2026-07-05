# Helpers for Plan 03 — an isolated, auto-cleaned Parquet store.

# Create a fresh temp `store_root` and return its path. Cleaned up when the
# calling frame exits. Reuse the Plan 01 builders for canonical inputs.
local_store <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  root
}

# Count Parquet part-files under a partitioned table (used by the compaction
# tests to assert file-count invariants).
count_parts <- function(store_root, table = "observations") {
  length(list.files(file.path(store_root, table),
                     pattern = "\\.parquet$", recursive = TRUE))
}

# Every `.rds` under a directory (the calibration store must contain none).
list_rds <- function(dir) {
  list.files(dir, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
}
