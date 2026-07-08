# Plan 03 — calibration store: manifest + coefficient tables persisted as
# Parquet, never `.rds` (SCOPING §7.1, the review's anti-.rds fix).
#
# Layout:
#   <store_root>/calibrations/site_id=<id>/manifest.json
#   <store_root>/calibrations/site_id=<id>/<variable>-<source>-v<ver>.parquet

.calib_dir <- function(store_root, site_id) {
  file.path(store_root, "calibrations", paste0("site_id=", site_id))
}

.calib_manifest_path <- function(store_root, site_id) {
  file.path(.calib_dir(store_root, site_id), "manifest.json")
}

.calib_coeffs_path <- function(store_root, site_id, variable, source, version) {
  file.path(.calib_dir(store_root, site_id),
            paste0(variable, "-", source, "-v", version, ".parquet"))
}

#' The calibration manifest for a site
#'
#' @param store_root Root directory of the store.
#' @param site_id Site identifier.
#' @return A tibble with columns `variable`, `source`, `version`, `tier`,
#'   `fit_date`, `train_start`, `train_end`, `n_pairs`, `lead_bucket`,
#'   `path`. Zero rows (typed) if no calibration has been written yet.
#' @keywords internal
#' @noRd
calib_manifest <- function(store_root, site_id) {
  path <- .calib_manifest_path(store_root, site_id)
  empty <- tibble::tibble(
    variable = character(0), source = character(0), version = integer(0),
    tier = character(0), fit_date = as.POSIXct(character(0), tz = "UTC"),
    train_start = as.POSIXct(character(0), tz = "UTC"),
    train_end = as.POSIXct(character(0), tz = "UTC"),
    n_pairs = integer(0), lead_bucket = character(0), path = character(0)
  )
  if (!file.exists(path)) {
    return(empty)
  }
  rows <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  if (length(rows) == 0 || (is.list(rows) && length(rows$variable) == 0)) {
    return(empty)
  }
  out <- tibble::as_tibble(rows)
  out$fit_date <- as.POSIXct(out$fit_date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OSZ")
  out$train_start <- as.POSIXct(out$train_start, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OSZ")
  out$train_end <- as.POSIXct(out$train_end, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OSZ")
  out$version <- as.integer(out$version)
  out$n_pairs <- as.integer(out$n_pairs)
  out[order(out$variable, out$source, out$version), , drop = FALSE]
}

#' Write a new calibration version
#'
#' Bumps `version` for `(variable, source)` monotonically, writes the
#' coefficient tibble as `<variable>-<source>-v<ver>.parquet` (never
#' `.rds`), and appends a manifest row atomically.
#'
#' @param store_root Root directory of the store.
#' @param site_id Site identifier.
#' @param variable Variable name the calibration corrects.
#' @param source Data source name.
#' @param tier Correction tier name (see `TIER_LEVELS`; not validated here --
#'   Plan 11/12 own tier semantics).
#' @param coeffs A tidy tibble of coefficients; shape is tier-defined.
#' @param meta A list with elements `train_start`, `train_end`, `n_pairs`,
#'   and optionally `lead_bucket`.
#' @param now Injectable current time; see `.now()`.
#' @return Invisibly, the manifest row just written.
#' @keywords internal
#' @noRd
calib_write <- function(store_root, site_id, variable, source, tier, coeffs, meta, now = .now()) {
  existing <- calib_manifest(store_root, site_id)
  prior <- existing[existing$variable == variable & existing$source == source, , drop = FALSE]
  version <- if (nrow(prior) == 0) 1L else max(prior$version) + 1L

  dir.create(.calib_dir(store_root, site_id), recursive = TRUE, showWarnings = FALSE)
  coeffs_path <- .calib_coeffs_path(store_root, site_id, variable, source, version)
  arrow::write_parquet(coeffs, coeffs_path)

  row <- tibble::tibble(
    variable = variable, source = source, version = version, tier = tier,
    fit_date = now,
    train_start = meta$train_start, train_end = meta$train_end,
    n_pairs = as.integer(meta$n_pairs %||% NA_integer_),
    lead_bucket = meta$lead_bucket %||% NA_character_,
    path = basename(coeffs_path)
  )

  combined <- rbind(existing, row)
  manifest_json <- combined
  manifest_json$fit_date <- format(manifest_json$fit_date, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
  manifest_json$train_start <- format(manifest_json$train_start, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
  manifest_json$train_end <- format(manifest_json$train_end, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")

  manifest_path <- .calib_manifest_path(store_root, site_id)
  tmp <- paste0(manifest_path, ".tmp-", Sys.getpid())
  jsonlite::write_json(manifest_json, tmp, auto_unbox = FALSE, pretty = TRUE, na = "null")
  file.rename(tmp, manifest_path)

  invisible(row)
}

#' Read a calibration's coefficients + manifest row
#'
#' @param store_root Root directory of the store.
#' @param site_id Site identifier.
#' @param variable Variable name.
#' @param source Data source name.
#' @param version Either `"current"` (the highest version) or an integer
#'   version number.
#' @return A list with elements `coeffs` (the coefficient tibble, read back
#'   from Parquet) and `manifest` (the single manifest row).
#' @keywords internal
#' @noRd
calib_read <- function(store_root, site_id, variable, source, version = "current") {
  man <- calib_manifest(store_root, site_id)
  rows <- man[man$variable == variable & man$source == source, , drop = FALSE]
  if (nrow(rows) == 0) {
    abort_meteo(
      "No calibration found for variable {.val {variable}} / source {.val {source}} at site {.val {site_id}}.", # nolint: line_length_linter.
      class = "calib_not_found"
    )
  }

  if (identical(version, "current")) {
    row <- rows[which.max(rows$version), , drop = FALSE]
  } else {
    row <- rows[rows$version == as.integer(version), , drop = FALSE]
    if (nrow(row) == 0) {
      abort_meteo(
        "No calibration version {.val {version}} found for variable {.val {variable}} / source {.val {source}}.", # nolint: line_length_linter.
        class = "calib_not_found"
      )
    }
  }

  coeffs_path <- file.path(.calib_dir(store_root, site_id), row$path)
  coeffs <- tibble::as_tibble(arrow::read_parquet(coeffs_path))
  list(coeffs = coeffs, manifest = row)
}
