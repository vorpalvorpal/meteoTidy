#' @include met-table.R
NULL

# Plan 15 -- per-column content hashing (the review fix, SCOPING section 3.2).
# `dplyr_reconstruct()` cannot detect `mutate(temperature_2m = temperature_2m
# + 1)`: the class and provenance survive while the value silently changes.
# To make the "metadata authoritative at validated boundaries" guarantee
# real rather than aspirational, every value column's content is hashed at
# construction time (`new_met_table()`) and re-hashed on demand
# (`met_validate_boundary()`); a mismatch downgrades that column's
# provenance tier to `"unverified"`, visibly, rather than silently trusting
# stale metadata.
#
# `digest::digest()` is used (not `rlang::hash()`) because it is the
# long-established, dependency-light standard for content hashing in R and
# its default algorithm (xxhash) is fast enough to run on every boundary
# validation without a caching layer.

#' Compute the per-column content hash of a wide table
#'
#' @param x A tibble (or `met_table`) with a `time` column plus value
#'   columns.
#' @return A named character vector, one hash per value column
#'   (`met_value_columns(x)`), keyed by variable name.
#' @keywords internal
#' @noRd
met_content_hash <- function(x) {
  value_cols <- met_value_columns(x)
  hashes <- vapply(value_cols, function(v) digest::digest(x[[v]]), character(1))
  names(hashes) <- value_cols
  hashes
}

#' Validate a `met_table` at a consumer boundary (the hash re-check)
#'
#' Recomputes each value column's content hash from its *current* data and
#' compares it against the hash stored at construction time
#' (`attr(x, "content_hash")`). Any column whose hash now differs was
#' mutated in place since the metadata was set (something
#' `dplyr_reconstruct()` cannot see) -- that column's provenance `tier` is
#' downgraded to `"unverified"`, leaving every other column's provenance
#' untouched. This is a per-column, granular downgrade, distinct from the
#' whole-object downgrade-to-plain-tibble path in `R/met-table-dplyr.R`.
#'
#' @param x A `met_table`.
#' @return The `met_table`, with `met_provenance(x)$tier` updated for any
#'   column whose content hash no longer matches.
#' @family met-table
#' @export
#' @examples
#' mt <- new_met_table(
#'   tibble::tibble(time = as.POSIXct("2026-01-01", tz = "UTC"), temperature_2m = 20),
#'   provenance = tibble::tibble(variable = "temperature_2m", tier = "raw",
#'                              train_overlap = 0, source = "openmeteo"),
#'   keys = list(site_id = "test"),
#'   versions = list(schema_version = "1.0.0", calibration_manifest_version = 0L)
#' )
#' met_validate_boundary(mt)
met_validate_boundary <- function(x) {
  stored <- attr(x, "content_hash")
  current <- met_content_hash(x)

  changed <- names(current)[vapply(names(current), function(v) {
    !identical(current[[v]], stored[[v]])
  }, logical(1))]

  if (length(changed) > 0) {
    prov <- met_provenance(x)
    prov$tier[prov$variable %in% changed] <- "unverified"
    attr(x, "provenance") <- prov
  }

  x
}
