# Plan 18 Part B -- deterministic derivation fill tier (physics before donors).
#
# Some canonical variables are *exactly* computable from other variables
# observed at the SAME site and timestamp -- the thermodynamic RH <-> dewpoint
# <-> temperature triangle being the archetype. When such a target has a gap
# but all of its inputs are co-observed and QC-clean, computing it from the
# physics beats fetching a donor station (no inter-station bias) and beats
# temporal interpolation. This tier formalises that: it reuses the Magnus
# helpers already in R/fill-treatments.R (`.rh_from_dewpoint`,
# `.dewpoint_from_rh`) rather than re-deriving the physics, and runs BEFORE the
# donor/model tiers (see `fill_tier()` in R/fill-tiers.R).
#
# The registry is an extensible list of specs; only the two RH/dewpoint
# directions are wired now. A future entry (noted in plan 18, not implemented
# here) is the direct/diffuse radiation split from global irradiance + solar
# geometry.

# The derivation registry: one spec per derivable target. Each spec carries
# the `target` variable, its `inputs` (other variable names that must be
# co-observed and QC-clean), and a pure `fn(vals)` where `vals` is a named
# list of numeric vectors (one per input, keyed by input variable name,
# aligned to the gap rows being filled) returning the target's numeric value
# in its canonical unit. The Magnus helpers take canonical-unit numerics
# (temperature/dewpoint in degC, RH in %), which is exactly how the store
# holds them, so no unit juggling is needed here.
.derive_registry <- function() {
  list(
    list(
      target = "relative_humidity_2m",
      inputs = c("temperature_2m", "dewpoint_2m"),
      fn = function(vals) .rh_from_dewpoint(vals$temperature_2m, vals$dewpoint_2m)
    ),
    list(
      target = "dewpoint_2m",
      inputs = c("temperature_2m", "relative_humidity_2m"),
      fn = function(vals) .dewpoint_from_rh(vals$temperature_2m, vals$relative_humidity_2m)
    )
  )
}

# A composite (site_id, datetime_utc) row key for matching a target's gap rows
# to its inputs across variables. `as.numeric()` on the POSIXct sidesteps any
# timezone/format ambiguity; the "\r" separator cannot occur in a site_id.
.derive_row_key <- function(df) {
  paste(df$site_id, as.numeric(df$datetime_utc), sep = "\r")
}

#' Fill coupled variables from co-observed inputs (derivation tier)
#'
#' For each derivable target in the derivation registry (`.derive_registry()`)
#' that has gap rows (NA `value`) in `obs`, computes the target exactly from
#' its input variables wherever, at the gap's `(site_id, datetime_utc)`, EVERY
#' input has a QC-clean row (`qc_flag == "ok"`, non-`NA` `value`). Filled rows
#' are stamped `method = "derived"` and `qc_flag = "ok"`, keeping the site's
#' own `source` (this is the site's own physics, not a donor's value). Gap
#' timestamps missing any input are left untouched for the donor/model tiers.
#'
#' `obs` is the FULL multi-variable frame (the derivation needs cross-variable
#' lookup), so this runs at the `fill_tier()` level, not inside the
#' per-variable `.fill_tier_one_variable()`. It is a pure function: it returns
#' a modified copy and has no side effects.
#'
#' @param obs A canonical long obs tibble, one or more variables, one or more
#'   sites, with gaps expressed as NA-`value` rows.
#' @param dict The variable dictionary (see [met_variables()]). Accepted for
#'   signature symmetry with the other fill tiers; the registry drives which
#'   targets are derivable.
#' @return `obs` with every derivable gap filled.
#' @keywords internal
#' @noRd
fill_derive <- function(obs, dict = met_variables()) {
  if (nrow(obs) == 0) {
    return(obs)
  }
  present <- unique(obs$variable)

  for (spec in .derive_registry()) {
    if (!spec$target %in% present) {
      next
    }
    if (!all(spec$inputs %in% present)) {
      next
    }

    gap_rows <- which(obs$variable == spec$target & is.na(obs$value))
    if (length(gap_rows) == 0) {
      next
    }

    gap_key <- .derive_row_key(obs[gap_rows, , drop = FALSE])
    input_vals <- vector("list", length(spec$inputs))
    names(input_vals) <- spec$inputs
    have_all <- rep(TRUE, length(gap_rows))

    for (input in spec$inputs) {
      clean <- obs[obs$variable == input & obs$qc_flag == "ok" & !is.na(obs$value), ,
                   drop = FALSE]
      matched <- match(gap_key, .derive_row_key(clean))
      values <- clean$value[matched]
      input_vals[[input]] <- values
      have_all <- have_all & !is.na(values)
    }

    if (!any(have_all)) {
      next
    }

    fill_rows <- gap_rows[have_all]
    ready_vals <- lapply(input_vals, function(v) v[have_all])
    obs$value[fill_rows] <- spec$fn(ready_vals)
    obs$method[fill_rows] <- "derived"
    obs$qc_flag[fill_rows] <- "ok"
  }

  obs
}
