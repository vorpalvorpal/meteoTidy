# Plan 03 — observation IO: write/read with the supersede-not-overwrite
# revision policy and as_of point-in-time reads (SCOPING §9).
#
# On top of Plan 01's canonical obs columns (site_id, datetime_utc, variable,
# value, source, method, qc_flag) the stored schema adds two bookkeeping
# columns:
#   ingested_at  -- UTC POSIXct, when this row was written (`now`)
#   superseded   -- logical, FALSE = current truth, TRUE = kept for audit
#
# Partition layout: site_id=<id>/year=<yyyy>, year derived from
# datetime_utc (UTC) at write time.
#
# Design note on the atomic rewrite: Parquet files are immutable, so
# "marking a row superseded" is implemented by reading the *entire* affected
# year-partition into memory, updating the `superseded` column for the
# matching keys, appending the new incoming rows, and rewriting the whole
# partition as a single new file via `.atomic_rewrite_partition()` (temp
# file + rename within the same directory). Only partitions that contain at
# least one affected key are touched; partitions with no overlap are left
# alone. This keeps the operation atomic per-partition (a reader always sees
# either the fully-old or fully-new partition file, never a half-write) at
# the cost of rewriting a whole year's data on every supersede that touches
# it -- acceptable given yearly partitioning and the plan's stated scope.

.obs_store_col_spec <- function() {
  c(names(.obs_col_spec_names()), "ingested_at", "superseded")
}

.obs_col_spec_names <- function() {
  c(site_id = "site_id", datetime_utc = "datetime_utc", variable = "variable",
    value = "value", source = "source", method = "method", qc_flag = "qc_flag")
}

.obs_key_cols <- function() {
  c("site_id", "datetime_utc", "variable", "source")
}

# Add ingested_at/superseded bookkeeping columns to a canonical obs tibble.
.stamp_obs_bookkeeping <- function(obs, now, superseded = FALSE) {
  obs$ingested_at <- now
  obs$superseded <- superseded
  obs
}

# Derive the `year` partition value (integer) from datetime_utc.
.obs_year <- function(datetime_utc) {
  as.integer(format(datetime_utc, "%Y", tz = "UTC"))
}

#' Write observations to the store
#'
#' Validates `obs` with `new_obs()`, stamps `ingested_at`/`superseded`
#' bookkeeping columns, and writes into the year-partitioned observation
#' dataset under `store_root`.
#'
#' `mode = "append"` is a plain append (used for brand-new windows with no
#' possibility of overlap). `mode = "supersede"` is the revision path
#' (SCOPING §9): for each incoming key `(site_id, datetime_utc, variable,
#' source)` that already has a current row (`superseded == FALSE`) with a
#' *different* `value`/`qc_flag`/`method`, the existing row is marked
#' `superseded = TRUE` and the incoming row is appended as the new current
#' row. If the incoming row is identical to the current row, nothing is
#' written (idempotency: re-running a sync must not duplicate rows or create
#' audit noise).
#'
#' @param store_root Root directory of the store.
#' @param obs A canonical observation tibble (see `new_obs()`).
#' @param now Injectable current time; see `.now()`.
#' @param mode Either `"append"` or `"supersede"`.
#' @return Invisibly, a list `(n_new, n_superseded, n_unchanged)`.
#' @keywords internal
#' @noRd
store_write_obs <- function(store_root, obs, now = .now(), mode = c("append", "supersede")) {
  mode <- rlang::arg_match(mode)
  obs <- new_obs(obs)

  if (nrow(obs) == 0) {
    return(invisible(list(n_new = 0L, n_superseded = 0L, n_unchanged = 0L)))
  }

  if (mode == "append") {
    stamped <- .stamp_obs_bookkeeping(obs, now, superseded = FALSE)
    .write_obs_by_year(store_root, stamped)
    return(invisible(list(n_new = nrow(obs), n_superseded = 0L, n_unchanged = 0L)))
  }

  # mode == "supersede": compare against current rows, partition by year of
  # the incoming rows so we only rewrite affected partitions.
  years <- unique(.obs_year(obs$datetime_utc))
  n_new <- 0L
  n_superseded <- 0L
  n_unchanged <- 0L

  for (yr in years) {
    incoming <- obs[.obs_year(obs$datetime_utc) == yr, , drop = FALSE]
    site_ids <- unique(incoming$site_id)

    for (sid in site_ids) {
      inc <- incoming[incoming$site_id == sid, , drop = FALSE]
      dir <- dataset_path(store_root, "observations", list(site_id = sid, year = yr))
      existing <- .read_partition_or_empty(dir)

      current <- existing[!isTRUE_vec(existing$superseded), , drop = FALSE]
      current_key <- .obs_key_strings(current)
      inc_key <- .obs_key_strings(inc)

      match_idx <- match(inc_key, current_key)
      has_match <- !is.na(match_idx)

      identical_mask <- rep(FALSE, nrow(inc))
      if (any(has_match)) {
        m <- match_idx[has_match]
        identical_mask[has_match] <- current$value[m] == inc$value[has_match] &
          current$qc_flag[m] == inc$qc_flag[has_match] &
          current$method[m] == inc$method[has_match]
      }

      to_write <- inc[!identical_mask, , drop = FALSE]
      n_unchanged <- n_unchanged + sum(identical_mask)

      if (nrow(to_write) == 0) next

      write_match_idx <- match_idx[!identical_mask]
      is_revision <- !is.na(write_match_idx)
      n_superseded <- n_superseded + sum(is_revision)
      n_new <- n_new + sum(!is_revision)

      # Mark the superseded rows in `existing` (not just `current`).
      superseded_keys <- current_key[write_match_idx[is_revision]]
      if (length(superseded_keys) > 0) {
        existing_key <- .obs_key_strings(existing)
        mark <- !isTRUE_vec(existing$superseded) & existing_key %in% superseded_keys
        existing$superseded[mark] <- TRUE
      }

      new_rows <- .stamp_obs_bookkeeping(to_write, now, superseded = FALSE)
      combined <- .rbind_obs_store(existing, new_rows)
      .atomic_rewrite_partition(dir, combined)
    }
  }

  invisible(list(n_new = n_new, n_superseded = n_superseded, n_unchanged = n_unchanged))
}

# `existing$superseded` may be logical(0) or NA-free logical; treat NA as
# FALSE (should not occur, but be defensive).
isTRUE_vec <- function(x) { # nolint: object_name_linter. mirrors base isTRUE() naming, vectorised.
  if (length(x) == 0) return(logical(0))
  !is.na(x) & x
}

.obs_key_strings <- function(df) {
  if (nrow(df) == 0) return(character(0))
  paste(df$site_id, format(df$datetime_utc, "%Y-%m-%dT%H:%M:%OS6", tz = "UTC"),
        df$variable, df$source, sep = "")
}

.rbind_obs_store <- function(a, b) {
  cols <- .obs_store_col_spec()
  a <- a[cols]
  b <- b[cols]
  rbind(a, b)
}

# Read one partition directory's part-files combined into one tibble with
# the store's bookkeeping columns, or an empty-but-typed tibble if the
# partition does not exist.
.read_partition_or_empty <- function(dir) {
  files <- if (dir.exists(dir)) {
    list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
  } else {
    character(0)
  }
  if (length(files) == 0) {
    return(tibble::tibble(
      site_id = character(0),
      datetime_utc = as.POSIXct(character(0), tz = "UTC"),
      variable = character(0),
      value = double(0),
      source = character(0),
      method = character(0),
      qc_flag = character(0),
      ingested_at = as.POSIXct(character(0), tz = "UTC"),
      superseded = logical(0)
    ))
  }
  tibble::as_tibble(do.call(rbind, lapply(files, arrow::read_parquet)))
}

# Plain-append write path: group incoming rows by (site_id, year) and write
# one new part-file per partition.
.write_obs_by_year <- function(store_root, stamped) {
  yr <- .obs_year(stamped$datetime_utc)
  key <- paste(stamped$site_id, yr, sep = "")
  for (k in unique(key)) {
    rows <- stamped[key == k, , drop = FALSE]
    sid <- rows$site_id[1]
    y <- yr[key == k][1]
    dir <- dataset_path(store_root, "observations", list(site_id = sid, year = y))
    .write_part(dir, rows[.obs_store_col_spec()])
  }
  invisible(stamped)
}

#' Read observations from the store
#'
#' Opens the year-partitioned observation dataset under `store_root`,
#' partition-prunes on `year` (derived from `[from, to]`) and filters on
#' `site_id`/`variable`, then applies the revision policy:
#'
#' - By default, returns only current rows (`superseded == FALSE`).
#' - `include_superseded = TRUE` returns both current and superseded rows.
#' - `as_of` reconstructs what the store would have served at that UTC
#'   instant: for each key, the row with the greatest `ingested_at <= as_of`
#'   (SCOPING §3.2/§9 point-in-time reproducibility). `as_of` and
#'   `include_superseded` are mutually exclusive framings; `as_of` wins if
#'   both are supplied.
#'
#' @param store_root Root directory of the store.
#' @param site_id Site identifier to read.
#' @param variables Optional character vector to filter `variable`.
#' @param from,to Optional UTC POSIXct bounds on `datetime_utc` (inclusive).
#' @param include_superseded Logical; include superseded rows.
#' @param as_of Optional UTC POSIXct instant for a point-in-time read.
#' @return A `new_obs()`-valid tibble (bookkeeping columns dropped).
#' @keywords internal
#' @noRd
store_read_obs <- function(store_root, site_id, variables = NULL, from = NULL, to = NULL,
                           include_superseded = FALSE, as_of = NULL) {
  raw <- .scan_obs_partitions(store_root, site_id, from = from, to = to)

  if (nrow(raw) > 0) {
    raw <- raw[raw$site_id == site_id, , drop = FALSE]
    if (!is.null(variables)) {
      raw <- raw[raw$variable %in% variables, , drop = FALSE]
    }
    if (!is.null(from)) {
      raw <- raw[raw$datetime_utc >= from, , drop = FALSE]
    }
    if (!is.null(to)) {
      raw <- raw[raw$datetime_utc <= to, , drop = FALSE]
    }
  }

  if (!is.null(as_of)) {
    raw <- .obs_as_of(raw, as_of)
  } else if (!include_superseded) {
    raw <- raw[!isTRUE_vec(raw$superseded), , drop = FALSE]
  }

  out <- raw[c("site_id", "datetime_utc", "variable", "value", "source", "method", "qc_flag")]
  out <- tibble::as_tibble(out)
  if (nrow(out) == 0) {
    return(out)
  }
  # `new_obs()` enforces key uniqueness, which only holds for the
  # current-truth view (default) or a resolved as_of snapshot: with
  # `include_superseded = TRUE` the same key legitimately appears twice (the
  # old and new value), by design (test-store-revision.R). Validate
  # everything else `new_obs()` checks by running it when the key is unique,
  # and otherwise apply the same column typing without the uniqueness check.
  key <- out[.obs_key_cols()]
  if (include_superseded && is.null(as_of) && anyDuplicated(key) > 0) {
    validate_qc_flag(out$qc_flag)
    validate_method(out$method)
    return(out)
  }
  new_obs(out)
}

# For each key, keep the row with the greatest ingested_at <= as_of. Rows
# never ingested by as_of are dropped entirely.
.obs_as_of <- function(df, as_of) {
  if (nrow(df) == 0) return(df)
  eligible <- df[df$ingested_at <= as_of, , drop = FALSE]
  if (nrow(eligible) == 0) return(eligible[0, , drop = FALSE])
  key <- .obs_key_strings(eligible)
  ord <- order(key, eligible$ingested_at, decreasing = c(FALSE, TRUE), method = "radix")
  eligible <- eligible[ord, , drop = FALSE]
  key <- key[ord]
  keep <- !duplicated(key)
  eligible[keep, , drop = FALSE]
}

# Open the observations dataset, prune to the years overlapping [from, to]
# and to site_id, and collect a plain tibble with bookkeeping columns
# intact. Returns an empty-but-typed tibble if there is no data at all.
.scan_obs_partitions <- function(store_root, site_id, from = NULL, to = NULL) {
  ds <- .open_dataset(store_root, "observations")
  if (is.null(ds)) {
    empty_dir <- dataset_path(store_root, "observations", list(site_id = site_id, year = 1970))
    return(.read_partition_or_empty(empty_dir))
  }

  tbl <- ds
  tbl <- dplyr::filter(tbl, .data$site_id == !!site_id)
  if (!is.null(from)) {
    tbl <- dplyr::filter(tbl, .data$year >= !!.obs_year(from))
  }
  if (!is.null(to)) {
    tbl <- dplyr::filter(tbl, .data$year <= !!.obs_year(to))
  }
  out <- dplyr::collect(tbl)
  out <- tibble::as_tibble(out)
  # Partition columns come back as extra columns (site_id already real,
  # year is partition-derived); drop `year`, keep the rest.
  out$year <- NULL
  # arrow's hive partitioning yields site_id as dictionary/factor-like;
  # coerce back to plain character to match canonical typing.
  out$site_id <- as.character(out$site_id)
  out
}
