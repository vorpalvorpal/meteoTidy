# Plan 03 — optional DuckDB/arrow connection over the store (SCOPING §8,
# §11; experimental). Read-only, and must see exactly the rows
# `store_read_obs()` sees by default (i.e. the same current-vs-superseded
# distinction is available to the caller via the `superseded` column).

#' Connect to the store for ad-hoc SQL/dplyr access
#'
#' Returns a DBI connection (`backend = "duckdb"`) registered with views over
#' the Parquet datasets in `store_root`, or an arrow `Dataset` list
#' (`backend = "arrow"`). Read-only. The `observations` view/table exposes
#' the raw stored schema including `ingested_at`/`superseded`, so callers can
#' reproduce `store_read_obs()`'s current-vs-superseded filtering themselves
#' (`WHERE superseded = FALSE`).
#'
#' @param store_root Root directory of the store.
#' @param backend Either `"arrow"` (default) or `"duckdb"`.
#' @return For `backend = "duckdb"`, a `DBI` connection with `observations`,
#'   `forecasts`, and `forecast_aux` views registered (for whichever
#'   datasets exist on disk). For `backend = "arrow"`, a named list of open
#'   arrow `Dataset` objects.
#' @keywords internal
#' @noRd
store_connect <- function(store_root, backend = c("arrow", "duckdb")) {
  backend <- rlang::arg_match(backend)

  if (backend == "arrow") {
    tables <- .store_tables()
    out <- stats::setNames(lapply(tables, function(t) .open_dataset(store_root, t)), tables)
    return(out[!vapply(out, is.null, logical(1))])
  }

  rlang::check_installed("duckdb", reason = "to use `store_connect(backend = \"duckdb\")`.")
  con <- DBI::dbConnect(duckdb::duckdb())

  for (table in .store_tables()) {
    dir <- .table_dir(store_root, table)
    if (!dir.exists(dir)) next
    files <- list.files(dir, pattern = "\\.parquet$", recursive = TRUE)
    if (length(files) == 0) next
    glob <- file.path(dir, "**", "*.parquet")
    DBI::dbExecute(con, sprintf(
      "CREATE VIEW %s AS SELECT * FROM read_parquet('%s', hive_partitioning = true)",
      table, glob
    ))
  }

  con
}
