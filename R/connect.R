# Plan 14 -- met_connect(): the experimental SQL/dplyr path over the store,
# wrapping Plan 03's `store_connect()` (SCOPING sections 8/11). The tibble
# read API (`R/read-api.R`) is the only STABLE contract; this exposes the
# physical schema directly (raw `superseded`/`ingested_at` bookkeeping
# columns visible, hive-partitioned layout not hidden), so it is documented
# **experimental** and may change without the deprecation process the
# tibble functions get.
#
# Not wired to the `lifecycle` package's `badge()` Rd macro (which needs
# `RdMacros: lifecycle` registration and generated badge assets via
# `usethis::use_lifecycle()`) since no test exercises the rendered
# documentation -- `met_connect_lifecycle()` below is a plain, testable,
# machine-readable marker instead, and the roxygen prose states the same
# thing for a human reader.

#' A machine-readable lifecycle marker for `met_connect()`
#'
#' @return The single string `"experimental"`.
#' @keywords internal
#' @noRd
met_connect_lifecycle <- function() {
  "experimental"
}

#' Connect to a site's store for ad-hoc SQL/dplyr access
#'
#' **Experimental.** Thin wrapper around the internal `store_connect()`
#' (Plan 03) for a single [met_site()]: returns a live connection over the
#' *same* Parquet tree the stable tibble read API ([met_record()],
#' [met_history()], ...) reads, but exposes the raw physical schema
#' (including bookkeeping columns like `superseded`/`ingested_at`) rather
#' than the curated tibble contract. Only the tibble read functions are a
#' stability promise (SCOPING §11) -- this function's exact schema may
#' change without a deprecation cycle.
#'
#' @param site A [met_site()] object.
#' @param backend Either `"duckdb"` (a `DBI` connection with `observations`/
#'   `forecasts`/`forecast_aux` views registered for whichever datasets
#'   exist) or `"arrow"` (a named list of open arrow `Dataset` objects).
#'   Default `"duckdb"`.
#' @return For `backend = "duckdb"`, a `DBI` connection. For `backend =
#'   "arrow"`, a named list of arrow `Dataset` objects.
#' @family read-api
#' @export
#' @examples
#' \dontrun{
#' con <- met_connect(site, backend = "duckdb")
#' dplyr::tbl(con, "observations")
#' }
met_connect <- function(site, backend = c("duckdb", "arrow")) {
  backend <- rlang::arg_match(backend)
  store_connect(site_store_root(site), backend = backend)
}
