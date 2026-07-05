# Plan 01 — variable dictionary registry.
#
# The dictionary is the single lookup table QC, fill, and correction dispatch
# on. It is environment-backed so users can extend it at runtime (SCOPING
# §3), seeded from the built-ins in R/dict-builtin.R.

.meteo_dict_env <- new.env(parent = emptyenv())

# (Re)seed the dictionary environment with only the built-ins. Called at
# package load and by `dict_reset()`.
.dict_seed <- function() {
  assign("tbl", .meteo_builtin_variables(), envir = .meteo_dict_env)
  invisible(NULL)
}

#' Reset the variable dictionary to the built-ins
#'
#' Restores the dictionary to exactly the built-in variables, discarding any
#' registrations made with [met_register_variable()]. Intended for test
#' cleanup (see `local_clean_dict()` in `tests/testthat/helper-schema.R`); not
#' something normal package usage should need.
#'
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
dict_reset <- function() {
  .dict_seed()
  invisible(NULL)
}

#' The current variable dictionary
#'
#' Returns the dictionary of known variables — the built-ins plus anything
#' added in this session with [met_register_variable()] — as a tibble.
#'
#' @return A tibble with columns `variable`, `unit`, `min`, `max`,
#'   `statistical_class`, `measurability_class`, `circular_period`,
#'   `description`.
#' @family dictionary
#' @export
#' @examples
#' met_variables()
met_variables <- function() {
  if (!exists("tbl", envir = .meteo_dict_env, inherits = FALSE)) {
    .dict_seed()
  }
  get("tbl", envir = .meteo_dict_env)
}

#' Look up a single dictionary variable
#'
#' @param variable A single variable name.
#' @return A one-row tibble (see [met_variables()] for columns).
#' @family dictionary
#' @export
#' @examples
#' met_variable("temperature_2m")
met_variable <- function(variable) {
  d <- met_variables()
  row <- d[d$variable == variable, , drop = FALSE]
  if (nrow(row) == 0) {
    abort_meteo(
      c(
        "Unknown variable {.val {variable}}.",
        "i" = "Register it first with {.code met_register_variable()}."
      ),
      class = "unknown_variable"
    )
  }
  row
}

#' Register a variable in the dictionary
#'
#' Adds (or, with `overwrite = TRUE`, replaces) a row in the variable
#' dictionary. Attempting to silently redefine a built-in without
#' `overwrite = TRUE` aborts.
#'
#' @param variable Single string, the canonical variable name.
#' @param unit Single string, a `units`-package-parseable unit.
#' @param min,max Single doubles (canonical unit), or `NA` if unbounded on
#'   that side. Must satisfy `min <= max` when both are given.
#' @param statistical_class Single string, one of [STAT_CLASS_LEVELS].
#' @param measurability_class Single string, one of [MEASURABILITY_LEVELS].
#' @param circular_period Single double or `NA` (default). Use `360` for
#'   angular/direction variables.
#' @param description Single string, a one-line description.
#' @param overwrite Logical, default `FALSE`. Must be `TRUE` to redefine an
#'   existing variable (built-in or previously registered).
#' @return The updated dictionary tibble, invisibly.
#' @family dictionary
#' @export
#' @examples
#' met_register_variable(
#'   variable = "leaf_wetness", unit = "1", min = 0, max = 1,
#'   statistical_class = "bounded", measurability_class = "site_measurable",
#'   description = "Fraction of time the leaf surface is wet.",
#'   overwrite = TRUE
#' )
met_register_variable <- function(variable, unit, min = NA_real_, max = NA_real_,
                                  statistical_class, measurability_class,
                                  circular_period = NA_real_, description,
                                  overwrite = FALSE) {
  if (!rlang::is_string(variable)) {
    abort_meteo("{.arg variable} must be a single string.", class = "bad_variable_name")
  }
  if (!isTRUE(units::ud_are_convertible(unit, unit))) {
    abort_meteo("{.arg unit} = {.val {unit}} does not parse as a unit.", class = "bad_units")
  }
  validate_statistical_class(statistical_class)
  validate_measurability_class(measurability_class)
  if (!is.na(min) && !is.na(max) && min > max) {
    abort_meteo(
      "{.arg min} ({.val {min}}) must be <= {.arg max} ({.val {max}}).",
      class = "bad_variable_range"
    )
  }

  d <- met_variables()
  exists_already <- variable %in% d$variable
  if (exists_already && !overwrite) {
    abort_meteo(
      c(
        "Variable {.val {variable}} is already registered.",
        "i" = "Pass {.code overwrite = TRUE} to redefine it."
      ),
      class = "duplicate_variable"
    )
  }

  new_row <- tibble::tibble(
    variable = variable,
    unit = unit,
    min = as.double(min),
    max = as.double(max),
    statistical_class = statistical_class,
    measurability_class = measurability_class,
    circular_period = as.double(circular_period),
    description = description
  )

  d <- if (exists_already) {
    d[d$variable != variable, , drop = FALSE]
  } else {
    d
  }
  d <- vctrs::vec_rbind(d, new_row)
  assign("tbl", d, envir = .meteo_dict_env)
  invisible(d)
}
