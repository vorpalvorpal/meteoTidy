# Plan 01 — canonical unit conversion.
#
# Canonical VALUES are stored as plain doubles in the dictionary's canonical
# unit (see R/schema-obs.R for the rationale). `units`-classed vectors are
# used only transiently, at conversion time, via the `units` package.

#' Canonical unit for a dictionary variable
#'
#' @param variable A single dictionary variable name.
#' @return A single string: the `units`-package unit string for `variable`.
#' @family units
#' @export
#' @examples
#' canonical_unit("wind_speed_10m")
canonical_unit <- function(variable) {
  met_variable(variable)$unit
}

#' Convert a value into a variable's canonical unit
#'
#' Converts `x`, expressed in unit `from`, into the canonical unit for
#' `variable` (looked up in the variable dictionary). If `x` already carries
#' `units` (i.e. is a `units`-classed vector), the carried unit is honoured
#' and `from` is ignored — except that a disagreement between the carried
#' unit and `from` raises a warning (class `"units_conflict"`); it never
#' errors, since the carried unit is trusted over the caller's claim.
#'
#' This is the choke point for the "km/h footgun" (SCOPING §3.1): the
#' canonical wind-speed unit is `m/s`, so a value supplied as `km/h` is always
#' converted, never silently passed through.
#'
#' @param x A numeric or `units`-classed vector.
#' @param from A single string giving the unit `x` is expressed in (ignored,
#'   with a warning on disagreement, when `x` already carries units).
#' @param variable A single dictionary variable name; determines the target
#'   canonical unit.
#' @return A `units`-classed vector in the canonical unit for `variable`.
#' @family units
#' @export
#' @examples
#' to_canonical(10, "km/h", "wind_speed_10m")
to_canonical <- function(x, from, variable) {
  target <- canonical_unit(variable)

  if (inherits(x, "units")) {
    carried <- units::deparse_unit(x)
    if (!isTRUE(units::ud_are_convertible(carried, from)) || carried != from) {
      warn_meteo(
        c(
          "{.arg x} carries units {.val {carried}} which disagrees with {.arg from}.",
          "x" = "{.arg from} was {.val {from}}.",
          "i" = "Trusting the carried unit {.val {carried}}."
        ),
        class = "units_conflict"
      )
    }
    from_unit <- carried
    x_val <- x
  } else {
    from_unit <- from
    x_val <- x
  }

  if (!isTRUE(units::ud_are_convertible(from_unit, target))) {
    abort_meteo(
      c(
        "Unit {.val {from_unit}} is not convertible to the canonical unit for {.val {variable}}.",
        "x" = "Canonical unit is {.val {target}}."
      ),
      class = "bad_units"
    )
  }

  if (inherits(x_val, "units")) {
    units::set_units(x_val, target, mode = "standard")
  } else {
    x_units <- units::set_units(x_val, from_unit, mode = "standard")
    units::set_units(x_units, target, mode = "standard")
  }
}
