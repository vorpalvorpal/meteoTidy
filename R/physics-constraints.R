# Plan 09 -- shared physical-consistency constraints module.
#
# This module is deliberately SHARED between Plan 09 (QC: flags raw
# observations that violate a physical relation) and Plan 12 (correction: the
# consistency pass that CLIPS corrected output to the same relations). It
# operates on a WIDE, single-row-per-timestamp frame (columns are variable
# names, e.g. `temperature_2m`, `dewpoint_2m`) rather than the long canonical
# form, because the relations it checks are cross-variable at a fixed
# (site_id, datetime_utc) -- exactly the shape `widen_obs()` produces.
#
# Kept free of any qc_flag-enum coupling beyond what `mode = "flag"` needs
# (it does not read or write QC_FLAG_LEVELS at all; it uses the strings
# "ok"/"suspect" as descriptive labels only), so Plan 12 can call it in
# "enforce" mode without depending on anything QC-specific.
#
# The relations checked (plans/09-curation-qc.md), in words rather than
# operators so this comment block isn't mistaken for dead code:
#   - dewpoint_2m must not exceed temperature_2m
#   - relative_humidity_2m must not exceed 100
#   - wind_gusts_10m must be at least wind_speed_10m
#   - direct_radiation plus diffuse_radiation must not exceed clear_sky_ceiling

# The constraint table: one row per relation, naming the columns involved and
# how "enforce" mode resolves a violation. `clip` says which side of the
# relation gets adjusted to satisfy it (the other side is trusted).
.physics_constraint_specs <- function() {
  list(
    list(
      id = "dewpoint_le_temperature",
      lhs = "dewpoint_2m", rhs = "temperature_2m", op = "le",
      clip = "lhs"
    ),
    list(
      id = "rh_le_100",
      lhs = "relative_humidity_2m", rhs = NULL, op = "le_const", bound = 100,
      clip = "lhs"
    ),
    list(
      id = "gusts_ge_speed",
      lhs = "wind_gusts_10m", rhs = "wind_speed_10m", op = "ge",
      clip = "lhs"
    ),
    list(
      id = "radiation_le_ceiling",
      lhs = c("direct_radiation", "diffuse_radiation"), rhs = "clear_sky_ceiling",
      op = "sum_le", clip = "lhs_sum"
    )
  )
}

# Does `row` have every column a spec needs (both sides present, non-NA)?
.physics_spec_applicable <- function(row, spec) {
  needed <- c(spec$lhs, spec$rhs)
  all(needed %in% names(row)) && all(!is.na(row[needed]))
}

# Evaluate whether `spec` is violated for `row` (a single-row data frame).
# Returns a logical scalar.
.physics_spec_violated <- function(row, spec) {
  if (!.physics_spec_applicable(row, spec)) {
    return(FALSE)
  }
  switch(spec$op,
    le = row[[spec$lhs]] > row[[spec$rhs]],
    ge = row[[spec$lhs]] < row[[spec$rhs]],
    le_const = row[[spec$lhs]] > spec$bound,
    sum_le = sum(vapply(spec$lhs, function(v) row[[v]], double(1))) > row[[spec$rhs]],
    FALSE
  )
}

#' Check (or enforce) the shared physical-consistency constraints
#'
#' Operates on a single-timestamp WIDE row (columns named after dictionary
#' variables; see `qc_wide_row()` in `tests/testthat/helper-qc.R`). In `mode
#' = "flag"` (used by Plan 09's internal-consistency QC rule), returns `row`
#' with a `violated` logical column plus one `flag_<variable>` column per
#' relation's implicated left-hand-side variable(s), set to `"suspect"` where
#' that specific relation is violated and `"ok"` otherwise; relations whose
#' inputs are not all present (`NA` or missing columns) are treated as not
#' violated. In `mode = "enforce"` (used by Plan 12's consistency pass),
#' clips violating values to satisfy the relation (e.g. `wind_gusts_10m` is
#' raised to at least `wind_speed_10m`) and attaches `attr(out, "n_violations")`,
#' the count of relations that were clipped (0 when nothing needed clipping).
#'
#' @param row A single-row wide tibble/data.frame; columns are variable names
#'   plus `site_id`/`datetime_utc`.
#' @param mode Either `"flag"` or `"enforce"`.
#' @return `row` with either flag columns (`mode = "flag"`) or clipped values
#'   plus an `"n_violations"` attribute (`mode = "enforce"`) added.
#' @keywords internal
#' @noRd
physics_constraints <- function(row, mode = c("flag", "enforce")) {
  mode <- rlang::arg_match(mode)
  specs <- .physics_constraint_specs()

  if (mode == "flag") {
    return(.physics_constraints_flag(row, specs))
  }
  .physics_constraints_enforce(row, specs)
}

.physics_constraints_flag <- function(row, specs) {
  out <- row
  out$violated <- FALSE
  for (spec in specs) {
    bad <- .physics_spec_violated(row, spec)
    out$violated <- out$violated || isTRUE(bad)
    lhs_vars <- spec$lhs
    for (v in lhs_vars) {
      flag_col <- paste0("flag_", v)
      current <- if (flag_col %in% names(out)) out[[flag_col]] else "ok"
      out[[flag_col]] <- if (isTRUE(bad)) "suspect" else current
    }
  }
  out
}

.physics_constraints_enforce <- function(row, specs) {
  out <- row
  n_violations <- 0L
  for (spec in specs) {
    bad <- .physics_spec_violated(out, spec)
    if (!isTRUE(bad)) next
    n_violations <- n_violations + 1L
    out <- .physics_clip(out, spec)
  }
  attr(out, "n_violations") <- n_violations
  out
}

# Clip the violating side of `spec` in `row` to the constraint boundary.
.physics_clip <- function(row, spec) {
  switch(spec$op,
    le = {
      row[[spec$lhs]] <- pmin(row[[spec$lhs]], row[[spec$rhs]])
      row
    },
    ge = {
      row[[spec$lhs]] <- pmax(row[[spec$lhs]], row[[spec$rhs]])
      row
    },
    le_const = {
      row[[spec$lhs]] <- pmin(row[[spec$lhs]], spec$bound)
      row
    },
    sum_le = {
      total <- sum(vapply(spec$lhs, function(v) row[[v]], double(1)))
      ceiling_val <- row[[spec$rhs]]
      scale <- ceiling_val / total
      for (v in spec$lhs) {
        row[[v]] <- row[[v]] * scale
      }
      row
    },
    row
  )
}
