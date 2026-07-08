# Plan 11 -- day-0 physical adjustments (SCOPING section 7.1): deterministic,
# always applied, never fitted. These are the ONLY corrections available
# before enough overlap accrues to fit a statistical tier (Plan 12), and they
# remain the floor for any variable/lead the tier gates (R/tier-select.R)
# keep at the physical tier. Both adjustments carry documented physical
# assumptions that are wrong in specific, named regimes -- see each
# function's roxygen.

#' Log-wind-profile height correction (day-0 physical tier)
#'
#' Rescales a wind speed observed at `from_height` to the equivalent speed at
#' `to_height` using the neutral-stability logarithmic wind profile
#' `u(z) = u_ref * ln(z / z0) / ln(z_ref / z0)`. This is the standard
#' surface-layer similarity relation and is the same arithmetic
#' `height_correct()` (`R/fill-treatments.R`, Plan 10) uses for gap-fill --
#' called directly here rather than reimplemented, since it is the identical
#' physics applied for a different purpose (correction provenance vs.
#' gap-fill).
#'
#' **Neutral-stability assumption.** The log law assumes a
#' neutrally-stratified surface layer. It does **not** account for stability
#' departures (stable/unstable regimes), which can be substantial in
#' practice (SCOPING section 7.1's inversion caveat) -- the correction is
#' most reliable for near-neutral conditions (overcast, moderate-to-strong
#' wind) and least reliable on clear, calm nights when a strong surface
#' inversion decouples the near-surface layer from the log profile (the
#' regime SCOPING section 7.3 flags as most consequential).
#'
#' @param value Numeric vector, wind speed(s) at `from_height`.
#' @param from_height,to_height Single doubles (metres), source and target
#'   reference heights.
#' @param z0 Single double (metres), the aerodynamic roughness length.
#'   Required: `NA` aborts `"missing_roughness"`, since height correction is
#'   meaningless without it (the site registry, Plan 02, already enforces a
#'   non-`NA` `z0` on wind instruments -- this is a defensive re-check at the
#'   point of use, for callers that pass `z0` directly rather than through a
#'   `met_site`).
#' @return Numeric vector, the height-corrected wind speed(s) at `to_height`.
#' @keywords internal
#' @noRd
correct_physical_wind <- function(value, from_height, to_height, z0) {
  if (length(z0) != 1 || is.na(z0)) {
    abort_meteo(
      c(
        "Log-wind-profile height correction needs a roughness length {.arg z0}.", # nolint: line_length_linter.
        "i" = "Height correction between {.val {from_height}} m and {.val {to_height}} m is meaningless without {.field z0}." # nolint: line_length_linter.
      ),
      class = "missing_roughness"
    )
  }
  height_correct(value, from_height = from_height, to_height = to_height, z0 = z0)
}

#' Fixed-lapse-rate temperature adjustment (day-0 physical tier)
#'
#' Adjusts a temperature for an elevation difference using a fixed
#' environmental lapse rate: `value - lapse_rate * elevation_delta`, where
#' `elevation_delta` is the target elevation minus the source elevation
#' (metres) and `lapse_rate` is in degC per metre (default `0.0065`, the
#' standard atmosphere's ~6.5 degC/km).
#'
#' **Inversion caveat (documented per SCOPING section 7.1 review note).** A
#' fixed lapse rate is wrong under temperature inversions -- on clear, calm
#' nights the near-surface atmosphere can warm *with* height instead of
#' cooling, and the standard environmental lapse rate over-corrects (or
#' corrects in the wrong direction entirely). This is a day-0 crutch,
#' superseded once a fitted tier (Plan 12) exists. `lapse_rate` is exposed as
#' a parameter (including the degenerate override `lapse_rate = 0`, a valid
#' "disable the adjustment" choice for a site known to sit in a persistent
#' inversion regime) precisely so a caller can override the fixed default;
#' no validation rejects `0` or a negative value.
#'
#' @param value Numeric vector, temperature(s) at the source elevation.
#' @param elevation_delta Single double (metres), target elevation minus
#'   source elevation.
#' @param lapse_rate Single double (degC/m), default `0.0065`. Overridable
#'   (including `0`) per the inversion caveat above.
#' @return Numeric vector, the lapse-adjusted temperature(s).
#' @keywords internal
#' @noRd
correct_physical_lapse <- function(value, elevation_delta, lapse_rate = 0.0065) {
  value - lapse_rate * elevation_delta
}

# Is `variable` a wind-speed-like variable that the height correction
# applies to? Mirrors `.is_wind_variable()` (R/site.R) but narrowed to speed
# (not direction/gusts), since direction has no log-wind-profile meaning and
# gusts are not a mean-wind quantity the log law describes.
.is_wind_speed_variable <- function(variable) {
  grepl("^wind_speed_", variable)
}

.is_temperature_variable <- function(variable) {
  grepl("^temperature_", variable)
}

# The reference height (metres) wind speed variables are corrected to.
# `wind_speed_10m` is the dictionary's 10 m reference variable (SCOPING
# section 3); other wind_speed_* variables would name their own reference
# height, but only the 10 m variable exists in the dictionary today.
.wind_reference_height <- function() {
  10
}

# Height-correct one variable's obs rows for `site`. Looks up the
# instrument that measures `variable` (by name match in `variable`) to get
# its height and roughness_length. If the instrument is already at the
# reference height, the correction is a formal, harmless no-op (log(x)/log(x)
# = 1) -- deliberately not special-cased away, so the arithmetic path (and
# its `z0` requirement) is exercised uniformly regardless of instrument
# height.
.correct_wind_rows <- function(rows, site) {
  instruments <- site_instruments(site)
  variable <- rows$variable[[1]]
  match_idx <- which(vapply(instruments, function(i) variable %in% i@variable, logical(1)))
  if (length(match_idx) == 0) {
    return(rows) # no instrument metadata for this variable: leave values as-is
  }
  instrument <- instruments[[match_idx[[1]]]]
  from_height <- as.numeric(instrument@height)
  z0 <- as.numeric(instrument@roughness_length)
  to_height <- .wind_reference_height()

  rows$value <- correct_physical_wind(rows$value, from_height = from_height,
                                      to_height = to_height, z0 = z0)
  rows
}

# Lapse-adjust one variable's obs rows for `site`. There is no established
# "reference/grid elevation" concept anywhere in the codebase (adapters and
# the dictionary carry no per-source grid elevation), so absent a caller-
# supplied `grid_elevation` the default elevation_delta is 0 -- a documented
# no-op that still stamps tier "physical". When `grid_elevation` is supplied
# (metres), elevation_delta is the site's own elevation (`site@elevation`, a
# units-classed quantity, converted via as.numeric() the same way
# `.correct_wind_rows()` converts instrument height) minus the grid
# elevation, and the lapse rate actually applies.
.correct_temperature_rows <- function(rows, site, # nolint: object_usage_linter.
                                      grid_elevation = NULL) {
  elevation_delta <- if (is.null(grid_elevation)) {
    0
  } else {
    as.numeric(site@elevation) - grid_elevation
  }
  rows$value <- correct_physical_lapse(rows$value, elevation_delta = elevation_delta)
  rows
}

#' Apply day-0 physical adjustments to a canonical obs tibble
#'
#' Dispatches each variable present in `obs` to the appropriate physical
#' adjustment (SCOPING section 7.1): wind-speed variables get the log-wind-
#' profile height correction from their instrument height to a 10 m
#' reference (see `correct_physical_wind()`); temperature variables get the
#' fixed-lapse-rate elevation adjustment (see `correct_physical_lapse()`),
#' keyed on `grid_elevation` when supplied. Variables with no physical
#' adjustment defined simply pass through unchanged. Every output row is
#' stamped `tier = "physical"` regardless -- this is the day-0 tier, always
#' applied, never fitted, and superseded once a fitted calibration exists
#' (Plan 12).
#'
#' @param obs A canonical long obs tibble (possibly multiple variables).
#' @param site A [met_site()] object, used to look up instrument height and
#'   roughness length for the wind correction, and the site's own elevation
#'   for the temperature lapse adjustment.
#' @param ... Reserved for future physical adjustments (pressure reduction);
#'   unused.
#' @param grid_elevation Optional single double (metres), the elevation of
#'   the grid/model cell the temperature values originate from. When
#'   supplied, the lapse adjustment uses `elevation_delta = site elevation -
#'   grid_elevation`; when `NULL` (default), the lapse adjustment stays a
#'   no-op (no grid-elevation source exists yet to supply it) -- this
#'   argument only makes the adjustment possible for a caller that has one.
#' @return `obs` with `value` adjusted where a physical rule applies, and a
#'   new/overwritten `tier` column set to `"physical"` for every row.
#' @keywords internal
#' @noRd
correct_physical <- function(obs, site, ..., grid_elevation = NULL) {
  if (nrow(obs) == 0) {
    obs$tier <- character(0)
    return(obs)
  }

  out <- obs
  for (variable in unique(obs$variable)) {
    idx <- which(obs$variable == variable)
    rows <- obs[idx, , drop = FALSE]
    rows <- if (.is_wind_speed_variable(variable)) {
      .correct_wind_rows(rows, site)
    } else if (.is_temperature_variable(variable)) {
      .correct_temperature_rows(rows, site, grid_elevation = grid_elevation)
    } else {
      rows
    }
    out$value[idx] <- rows$value
  }

  out$tier <- "physical"
  out
}
