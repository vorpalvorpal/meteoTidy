# Plan 03 — forecast + forecast_aux IO.
#
# Partition layout: source=<src>/site_id=<id>/issue_date=<yyyy-mm-dd>, where
# issue_date = as.Date(issue_time) in UTC, derived at write time.
#
# store_write_forecast() deduplicates at ROW level (the full forecast key:
# source, model, issue_time, valid_time, member, stat, variable): re-writing
# an archived issuance is a no-op, and -- unlike an issuance-level
# (source, model, issue_time) key -- a partially archived issuance (e.g. a
# sync that ran while a source was still publishing steps) is completed by a
# later re-fetch instead of being permanently frozen at whatever subset
# happened to arrive first. Forecasts are immutable once issued (SCOPING §9),
# so matching keys are dropped, never superseded.

.forecast_issue_date <- function(issue_time) {
  format(issue_time, "%Y-%m-%d", tz = "UTC")
}

#' Write forecasts to the store
#'
#' Validates `fc` with `new_forecast()` and writes into the
#' `source`/`site_id`/`issue_date`-partitioned forecast dataset.
#' Deduplicates on the full row key `(source, model, issue_time, valid_time,
#' member, stat, variable)`: rows already present in the store are dropped
#' from the incoming batch before writing, so re-archiving an issuance is a
#' no-op while a partially archived issuance can still be completed later.
#'
#' @param store_root Root directory of the store.
#' @param fc A canonical forecast tibble (see `new_forecast()`).
#' @param now Injectable current time; see `.now()` (currently unused beyond
#'   signature symmetry with `store_write_obs()`; forecasts carry no
#'   ingestion bookkeeping since they are immutable once issued).
#' @return Invisibly, `fc`.
#' @keywords internal
#' @noRd
store_write_forecast <- function(store_root, fc, now = .now()) {
  fc <- new_forecast(fc)
  .write_forecast_like(store_root, fc, table = "forecasts",
                       dedup_cols = c("source", "model", "issue_time",
                                      "valid_time", "member", "stat", "variable"))
}

#' Write forecast_aux rows to the store
#'
#' Mirrors `store_write_forecast()` for the non-numeric companion table.
#' Deduplicates on the full row key `(source, issue_time, valid_time,
#' field)`.
#'
#' @inheritParams store_write_forecast
#' @param aux A canonical forecast_aux tibble (see `new_forecast_aux()`).
#' @keywords internal
#' @noRd
store_write_forecast_aux <- function(store_root, aux, now = .now()) {
  aux <- new_forecast_aux(aux)
  .write_forecast_like(store_root, aux, table = "forecast_aux",
                       dedup_cols = c("source", "issue_time", "valid_time", "field"))
}

# Shared write path for forecasts/forecast_aux: partition by source +
# site_id + issue_date, dropping rows whose dedup key already exists in the
# target partition.
.write_forecast_like <- function(store_root, df, table, dedup_cols) {
  if (nrow(df) == 0) {
    return(invisible(df))
  }

  issue_date <- .forecast_issue_date(df$issue_time)
  part_key <- paste(df$source, df$site_id, issue_date, sep = "\r")

  for (pk in unique(part_key)) {
    rows <- df[part_key == pk, , drop = FALSE]
    src <- rows$source[1]
    sid <- rows$site_id[1]
    idate <- issue_date[part_key == pk][1]
    dir <- dataset_path(store_root, table,
                        list(source = src, site_id = sid, issue_date = idate))

    existing <- .read_forecast_partition_or_empty(dir, df)
    if (nrow(existing) > 0) {
      existing_key <- .paste_cols(existing, dedup_cols)
      incoming_key <- .paste_cols(rows, dedup_cols)
      rows <- rows[!(incoming_key %in% existing_key), , drop = FALSE]
    }

    if (nrow(rows) == 0) next
    .write_part(dir, rows)
  }

  invisible(df)
}

.paste_cols <- function(df, cols) {
  do.call(paste, c(as.list(df[cols]), sep = "\r"))
}

.read_forecast_partition_or_empty <- function(dir, template) {
  files <- if (dir.exists(dir)) {
    list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
  } else {
    character(0)
  }
  if (length(files) == 0) {
    return(template[0, , drop = FALSE])
  }
  tibble::as_tibble(do.call(rbind, lapply(files, arrow::read_parquet)))
}

#' Read forecasts from the store
#'
#' Opens the forecast dataset, partition-prunes on `source` and
#' `issue_date`, filters on `site_id`/`valid_time` range, and optionally
#' drops per-member rows.
#'
#' @param store_root Root directory of the store.
#' @param site_id Site identifier to read.
#' @param source Optional character vector to filter `source`.
#' @param issue_from,issue_to Optional UTC POSIXct bounds on `issue_time`.
#' @param valid_from,valid_to Optional UTC POSIXct bounds on `valid_time`.
#' @param members Logical; if `FALSE`, drop rows with non-`NA` `member`
#'   (keeping only deterministic and `stat`-summary rows). Defaults to
#'   `TRUE`: per-member trajectories remain retrievable by default (SCOPING
#'   §4).
#' @return A `new_forecast()`-valid tibble.
#' @keywords internal
#' @noRd
store_read_forecast <- function(store_root, site_id, source = NULL,
                                issue_from = NULL, issue_to = NULL,
                                valid_from = NULL, valid_to = NULL, members = TRUE) {
  out <- .read_forecast_like(store_root, "forecasts", site_id, source,
                             issue_from, issue_to, valid_from, valid_to)
  if (!members && nrow(out) > 0) {
    out <- out[is.na(out$member), , drop = FALSE]
  }
  if (nrow(out) == 0) {
    return(out)
  }
  new_forecast(out)
}

#' Read forecast_aux rows from the store
#'
#' Mirrors `store_read_forecast()` for the non-numeric companion table.
#'
#' @inheritParams store_read_forecast
#' @return A `new_forecast_aux()`-valid tibble.
#' @keywords internal
#' @noRd
store_read_forecast_aux <- function(store_root, site_id, source = NULL,
                                    issue_from = NULL, issue_to = NULL,
                                    valid_from = NULL, valid_to = NULL) {
  out <- .read_forecast_like(store_root, "forecast_aux", site_id, source,
                             issue_from, issue_to, valid_from, valid_to)
  if (nrow(out) == 0) {
    return(out)
  }
  new_forecast_aux(out)
}

.read_forecast_like <- function(store_root, table, site_id, source,
                                issue_from, issue_to, valid_from, valid_to) {
  ds <- .open_dataset(store_root, table)
  empty_cols <- if (table == "forecasts") {
    c("site_id", "source", "model", "issue_time", "valid_time",
      "lead_time", "member", "stat", "variable", "value")
  } else {
    c("site_id", "source", "issue_time", "valid_time", "field", "value_text")
  }

  if (is.null(ds)) {
    return(tibble::as_tibble(stats::setNames(
      lapply(empty_cols, function(x) character(0)), empty_cols
    ))[0, , drop = FALSE])
  }

  tbl <- dplyr::filter(ds, .data$site_id == !!site_id)
  if (!is.null(source)) {
    tbl <- dplyr::filter(tbl, .data$source %in% !!source)
  }
  if (!is.null(issue_from)) {
    tbl <- dplyr::filter(tbl, .data$issue_date >= !!.forecast_issue_date(issue_from))
  }
  if (!is.null(issue_to)) {
    tbl <- dplyr::filter(tbl, .data$issue_date <= !!.forecast_issue_date(issue_to))
  }

  out <- dplyr::collect(tbl)
  out <- tibble::as_tibble(out)
  out$issue_date <- NULL
  out$site_id <- as.character(out$site_id)
  out$source <- as.character(out$source)

  # The dataset filters above prune on the day-granular issue_date partition
  # column; re-apply the caller's exact POSIXct bounds so same-day earlier/
  # later cycles are not leaked into (or out of) the window.
  if (!is.null(issue_from)) {
    out <- out[out$issue_time >= issue_from, , drop = FALSE]
  }
  if (!is.null(issue_to)) {
    out <- out[out$issue_time <= issue_to, , drop = FALSE]
  }

  if (!is.null(valid_from)) {
    out <- out[out$valid_time >= valid_from, , drop = FALSE]
  }
  if (!is.null(valid_to)) {
    out <- out[out$valid_time <= valid_to, , drop = FALSE]
  }

  out
}
