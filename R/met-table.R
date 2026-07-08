#' @include conditions.R
NULL

# Plan 15 -- the classed tibble `met_table` (SCOPING section 3.2): an S3
# subclass of `tbl_df`, not an opaque S7 wrapper (the sf/tsibble pattern), so
# dplyr/ggplot2 keep working untouched. Carries per-variable provenance, the
# site/window keys, and schema/calibration-manifest versions as attributes,
# plus a per-column content hash (R/met-table-hash.R) that makes the
# "metadata authoritative at validated boundaries" guarantee enforceable.

.met_table_provenance_cols <- function() {
  c("variable", "tier", "train_overlap", "source")
}

# The value columns of a met_table's underlying tibble: everything except the
# `time` index column.
met_value_columns <- function(x) {
  setdiff(names(x), "time")
}

.validate_met_table_provenance <- function(x, provenance) {
  missing_from_dict <- !.met_table_provenance_cols() %in% names(provenance)
  if (any(missing_from_dict)) {
    abort_meteo(
      c(
        "{.arg provenance} is missing required column{?s}: {.val {.met_table_provenance_cols()[missing_from_dict]}}.", # nolint: line_length_linter.
        "i" = "Required columns: {.val {.met_table_provenance_cols()}}"
      ),
      class = "provenance_incomplete"
    )
  }

  value_cols <- met_value_columns(x)
  missing_vars <- setdiff(value_cols, provenance$variable)
  if (length(missing_vars) > 0) {
    abort_meteo(
      c(
        "{.arg provenance} does not cover every value column of {.arg x}.",
        "x" = "Missing provenance for: {.val {missing_vars}}."
      ),
      class = "provenance_incomplete"
    )
  }
  invisible(TRUE)
}

.validate_met_table_versions <- function(versions) {
  required <- c("schema_version", "calibration_manifest_version")
  missing <- setdiff(required, names(versions))
  if (length(missing) > 0) {
    abort_meteo(
      c(
        "{.arg versions} is missing required element{?s}: {.val {missing}}.",
        "i" = "Required: {.val {required}}"
      ),
      class = "missing_versions"
    )
  }
  invisible(TRUE)
}

#' Construct a `met_table`, the meteoHazard classed-tibble interface
#'
#' `met_table` is an S3 subclass of `tbl_df` (SCOPING section 3.2, the
#' sf/tsibble pattern rather than an opaque S7 wrapper) carrying per-variable
#' provenance, site/window keys, and schema/calibration-manifest versions as
#' attributes, plus a per-column content hash so staleness is detectable
#' (see [met_validate_boundary()]). `dplyr` verbs keep working on it
#' untouched (see `R/met-table-dplyr.R`): column-preserving operations keep
#' the class; operations that invalidate the metadata downgrade visibly to a
#' plain tibble with a warning.
#'
#' @param x The underlying wide tibble (SCOPING section 3.1 columns): a
#'   `time` column plus one column per variable.
#' @param provenance A tibble with columns `variable`, `tier`,
#'   `train_overlap`, `source` -- one row per value column of `x`. Every
#'   value column (`setdiff(names(x), "time")`) must be covered.
#' @param keys A list with at least `site_id`; optionally `from`/`to` (the
#'   window the table was read over).
#' @param versions A list with `schema_version` and
#'   `calibration_manifest_version`.
#' @return A `met_table`.
#' @family met-table
#' @export
#' @examples
#' wide <- tibble::tibble(
#'   time = as.POSIXct("2026-01-01", tz = "UTC"),
#'   temperature_2m = 20
#' )
#' prov <- tibble::tibble(variable = "temperature_2m", tier = "raw",
#'                        train_overlap = 0, source = "openmeteo")
#' new_met_table(wide, provenance = prov, keys = list(site_id = "test"),
#'              versions = list(schema_version = "1.0.0",
#'                              calibration_manifest_version = 0L))
new_met_table <- function(x, provenance, keys, versions) {
  x <- tibble::as_tibble(x)
  .validate_met_table_versions(versions)
  .validate_met_table_provenance(x, provenance)

  class(x) <- c("met_table", class(x))
  attr(x, "provenance") <- tibble::as_tibble(provenance)
  attr(x, "keys") <- keys
  attr(x, "versions") <- versions
  attr(x, "content_hash") <- met_content_hash(x)
  x
}

#' Accessors for a `met_table`'s attached metadata
#'
#' @param x A `met_table`.
#' @return `met_provenance()` returns the per-variable provenance tibble;
#'   `met_keys()` the site/window key list; `met_versions()` the version
#'   list.
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
#' met_provenance(mt)
met_provenance <- function(x) {
  attr(x, "provenance")
}

#' @rdname met_provenance
#' @export
met_keys <- function(x) {
  attr(x, "keys")
}

#' @rdname met_provenance
#' @export
met_versions <- function(x) {
  attr(x, "versions")
}

# A compact, terse per-column provenance line: "variable: tier (source, Nh overlap)".
.met_table_banner_lines <- function(x) {
  prov <- met_provenance(x)
  ord <- match(met_value_columns(x), prov$variable)
  prov <- prov[ord, , drop = FALSE]
  sprintf("%s: %s (%s, %sh overlap)",
          prov$variable, prov$tier, prov$source, prov$train_overlap)
}

#' @export
format.met_table <- function(x, ...) {
  banner <- c(
    cli::col_grey("# met_table provenance:"),
    cli::col_grey(paste0("#   ", .met_table_banner_lines(x)))
  )
  tbl <- NextMethod()
  c(banner, tbl)
}

#' @export
print.met_table <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
