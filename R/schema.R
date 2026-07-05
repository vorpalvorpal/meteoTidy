# Plan 01 — shared schema utilities used by every `new_*()` constructor.

#' Describe one expected column
#'
#' A tiny column-spec type used by `assert_columns()` to check a data frame
#' against an expected shape.
#'
#' @param name Single string, the column name.
#' @param type Single string, one of `"character"`, `"double"`, `"integer"`,
#'   `"POSIXct"`, `"difftime"`, or `"any"` (skip the type check).
#' @param required Logical, default `TRUE`. If `FALSE`, the column may be
#'   absent.
#' @return A list with class `"meteo_col_spec"`.
#' @keywords internal
#' @noRd
col_spec <- function(name, type = "any", required = TRUE) {
  structure(
    list(name = name, type = type, required = required),
    class = "meteo_col_spec"
  )
}

# Does `x` satisfy the type named in a col_spec?
.col_spec_type_ok <- function(x, type) {
  switch(type,
    any = TRUE,
    character = is.character(x),
    double = is.double(x),
    integer = is.integer(x),
    POSIXct = inherits(x, "POSIXct"),
    difftime = inherits(x, "difftime"),
    rlang::abort(paste("Unknown col_spec type:", type)) # nolint: unreachable_code_linter.
  )
}

#' Assert a data frame matches a column spec
#'
#' The one validator every `new_*()` constructor calls: checks that every
#' required column in `spec` is present in `df` and, where a `type` is given,
#' has that type. Produces uniform, class-tagged errors.
#'
#' @param df A data frame.
#' @param spec A list of `col_spec()` entries.
#' @param table_name Single string used in error messages (e.g. `"obs"`).
#' @return `df`, invisibly, if it satisfies `spec`.
#' @keywords internal
#' @noRd
assert_columns <- function(df, spec, table_name) {
  names_needed <- vapply(spec, function(s) s$name, character(1))
  required <- vapply(spec, function(s) s$required, logical(1))
  missing <- setdiff(names_needed[required], names(df))
  if (length(missing) > 0) {
    abort_meteo(
      c(
        "{.val {table_name}} table is missing required column{?s}: {.val {missing}}.",
        "i" = "Required columns: {.val {names_needed[required]}}"
      ),
      class = "schema_missing_column"
    )
  }

  for (s in spec) {
    if (!(s$name %in% names(df))) next
    if (!.col_spec_type_ok(df[[s$name]], s$type)) {
      abort_meteo(
        c(
          "Column {.field {s$name}} of the {.val {table_name}} table has the wrong type.",
          "x" = "Expected {.cls {s$type}}, got {.cls {class(df[[s$name]])}}."
        ),
        class = "schema_bad_type"
      )
    }
  }

  invisible(df)
}
