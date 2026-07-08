# Plan 10 -- per-variable statistical treatment for gap-fill (SCOPING section
# 6). Each treatment converts to a well-behaved statistical space,
# interpolates/transfers there, then converts back:
#
#   RH                 <-> dewpoint (Magnus-Tetens), using the co-located
#                          temperature series
#   circular (direction) <-> unit-vector sin/cos components
#   intermittent (rain) -> occurrence (wet/dry) + amount, handled separately
#   clear_sky_indexed (solar) <-> clear-sky index (observed over modelled ceiling)
#   wind speed          -> log-wind-profile height correction to a common
#                          reference height before any cross-station
#                          transfer
#
# None of these treatments interpolate a raw angle across the 0/360 wrap, nor
# linearly interpolate rainfall across a gap (both would produce physically
# nonsensical output -- a ~180-degree turn instead of a ~20-degree one, or a
# smeared "drizzle ramp" through what may have been a dry spell).

# ---- wind height correction (log-wind profile) -----------------------------

#' Log-wind-profile height correction
#'
#' Rescales a wind speed observed at `from_height` to the equivalent speed at
#' `to_height`, assuming a neutral-stability logarithmic wind profile:
#' `u(z) = u_ref * ln(z / z0) / ln(z_ref / z0)`. This is the standard
#' surface-layer similarity relation for a neutrally-stratified boundary
#' layer; it does **not** account for stability (stable/unstable departures
#' from the log law), which can be substantial in practice (SCOPING section
#' 7.1's inversion caveat) -- treat the corrected value as an approximation,
#' most reliable for near-neutral conditions (overcast, moderate-to-strong
#' wind) and least reliable on clear, calm nights when a strong surface
#' inversion decouples the near-surface layer from the log profile.
#'
#' @param value Numeric vector, wind speed(s) at `from_height`.
#' @param from_height,to_height Single doubles (metres), source and target
#'   reference heights.
#' @param z0 Single double (metres), the aerodynamic roughness length.
#' @return Numeric vector, the height-corrected wind speed(s) at `to_height`.
#' @keywords internal
#' @noRd
height_correct <- function(value, from_height, to_height, z0) {
  value * log(to_height / z0) / log(from_height / z0)
}

# ---- RH <-> dewpoint (Magnus-Tetens) ----------------------------------------

# Magnus-Tetens constants (Alduchov & Eskridge, 1996, "Improved Magnus form
# approximation of saturation vapor pressure", J. Appl. Meteor. 35(4)) --
# a standard, widely-used approximation. Used consistently in both directions
# (RH+T -> dewpoint, dewpoint+T -> RH) so a round trip is numerically stable.
.magnus_b <- function() 17.625
.magnus_c <- function() 243.04

# Dewpoint (degC) from temperature (degC) and relative humidity (%).
.dewpoint_from_rh <- function(temp_c, rh_pct) {
  b <- .magnus_b()
  c <- .magnus_c()
  alpha <- log(pmax(rh_pct, 1e-6) / 100) + (b * temp_c) / (c + temp_c)
  (c * alpha) / (b - alpha)
}

# Relative humidity (%) from temperature (degC) and dewpoint (degC), inverse
# of `.dewpoint_from_rh()`, clamped to the physically valid [0, 100] range
# (interpolation in dewpoint space can occasionally overshoot slightly at the
# boundary due to floating point, so the clamp is defensive, not load-bearing
# in the typical case).
.rh_from_dewpoint <- function(temp_c, dewpoint_c) {
  b <- .magnus_b()
  c <- .magnus_c()
  es_dew <- exp((b * dewpoint_c) / (c + dewpoint_c))
  es_temp <- exp((b * temp_c) / (c + temp_c))
  rh <- 100 * es_dew / es_temp
  pmin(pmax(rh, 0), 100)
}

# Fill a relative_humidity_2m gap by converting to dewpoint space (using the
# co-located temperature series, matched on datetime_utc), interpolating the
# dewpoint, then converting back to RH. Falls back to a direct (bounded)
# interpolation of RH itself, clamped to [0, 100], if no temperature series
# is available at all (rare, defensive path -- not the primary treatment).
.fill_rh_dewpoint <- function(rh_obs, temp_obs) {
  gap_idx <- which(is.na(rh_obs$value))
  if (length(gap_idx) == 0) {
    return(rh_obs)
  }

  if (is.null(temp_obs) || nrow(temp_obs) == 0) {
    rh_obs$value <- .bounded_interpolate(rh_obs$value, lo = 0, hi = 100)
    rh_obs$qc_flag[gap_idx] <- "ok"
    rh_obs$method[gap_idx] <- "imputed"
    return(rh_obs)
  }

  temp_at <- temp_obs$value[match(rh_obs$datetime_utc, temp_obs$datetime_utc)]
  dewpoint <- rep(NA_real_, nrow(rh_obs))
  known <- !is.na(rh_obs$value) & !is.na(temp_at)
  dewpoint[known] <- .dewpoint_from_rh(temp_at[known], rh_obs$value[known])

  dewpoint_filled <- imputeTS::na_interpolation(dewpoint, option = "linear")

  temp_gap <- temp_at[gap_idx]
  has_temp <- !is.na(temp_gap)
  rh_filled <- rh_obs$value
  dewpoint_at_gap <- dewpoint_filled[gap_idx[has_temp]]
  rh_filled[gap_idx[has_temp]] <- .rh_from_dewpoint(temp_gap[has_temp], dewpoint_at_gap)
  # Defensive fallback for any gap row with no matching temperature: hold the
  # interpolated dewpoint's RH equivalent using the nearest available
  # temperature (bounded interpolation of RH itself as a last resort).
  if (any(!has_temp)) {
    rh_bounded <- .bounded_interpolate(rh_obs$value, lo = 0, hi = 100)
    rh_filled[gap_idx[!has_temp]] <- rh_bounded[gap_idx[!has_temp]]
  }

  rh_obs$value <- pmin(pmax(rh_filled, 0), 100)
  rh_obs$qc_flag[gap_idx] <- "ok"
  rh_obs$method[gap_idx] <- "imputed"
  rh_obs
}

# ---- circular (wind direction) ----------------------------------------------

# Fill a circular (direction) gap by converting to unit-vector sin/cos
# components, interpolating each component linearly, then recombining via
# atan2() and wrapping to [0, 360). Never interpolates the raw angle (a
# 350 -> 10 gap would otherwise pass through ~180, a full reversal, instead
# of the true ~20-degree turn through the wrap).
.fill_circular <- function(obs) {
  gap_idx <- which(is.na(obs$value))
  if (length(gap_idx) == 0) {
    return(obs)
  }
  rad <- obs$value * pi / 180
  s <- sin(rad)
  co <- cos(rad)
  s_filled <- imputeTS::na_interpolation(s, option = "linear")
  co_filled <- imputeTS::na_interpolation(co, option = "linear")
  angle <- atan2(s_filled, co_filled) * 180 / pi
  angle <- (angle + 360) %% 360

  obs$value[gap_idx] <- angle[gap_idx]
  obs$qc_flag[gap_idx] <- "ok"
  obs$method[gap_idx] <- "imputed"
  obs
}

# ---- intermittent (rain): occurrence + amount -------------------------------

# Fill a precipitation gap by treating occurrence (rained: value > 0) and
# amount separately, and NEVER linearly interpolating the raw amount series
# (which would smear a genuine dry spell into invented drizzle, or invent a
# dry spell inside what was actually a wet gap).
#
# Policy (documented, simplest-thing-that-satisfies-the-tests per
# plans/README's ambiguity order): a gap flanked by two dry (0) neighbours
# stays entirely dry -- there is no occurrence signal on either side to
# suggest rain fell inside the gap. A gap flanked by two wet neighbours (or
# any occurrence uncertainty) is filled by holding the last known amount
# constant across the gap (a "persistence" rain estimate) rather than a
# smooth ramp between the two amounts -- this avoids inventing a monotone
# drizzle trend the raw linear interpolation would produce, while still
# producing a non-negative, plausible amount. Values are always clamped to
# be non-negative (a physical floor for precipitation).
.fill_rain <- function(obs) {
  gap_idx <- which(is.na(obs$value))
  if (length(gap_idx) == 0) {
    return(obs)
  }
  ord <- order(obs$datetime_utc)
  obs <- obs[ord, , drop = FALSE]
  gap_idx <- which(is.na(obs$value))

  value <- obs$value
  n <- length(value)
  for (idx in gap_idx) {
    # look at the nearest already-resolved (non-NA) neighbour on each side
    left <- idx - 1
    while (left >= 1 && is.na(value[left])) left <- left - 1
    right <- idx + 1
    while (right <= n && is.na(value[right])) right <- right + 1
    left_val <- if (left >= 1) value[left] else NA_real_
    right_val <- if (right <= n) value[right] else NA_real_

    if (!is.na(left_val) && !is.na(right_val) && left_val == 0 && right_val == 0) {
      value[idx] <- 0 # dry gap between dry neighbours: stays dry
    } else if (!is.na(left_val)) {
      value[idx] <- left_val # hold the last known amount (no smeared ramp)
    } else if (!is.na(right_val)) {
      value[idx] <- right_val
    } else {
      value[idx] <- 0
    }
  }

  obs$value <- pmax(value, 0)
  obs$qc_flag[gap_idx] <- "ok"
  obs$method[gap_idx] <- "imputed"
  obs
}

# ---- clear-sky index (solar) ------------------------------------------------

# Fill a clear_sky_indexed (radiation) gap by dividing by the clear-sky
# irradiance ceiling (Plan 09's `clear_sky_irradiance()`) to get a bounded
# clear-sky index, interpolating the index, then multiplying back. Any
# timestamp where clear_sky_irradiance() is 0 (night) is forced to 0
# regardless of the interpolated index, since the index is undefined
# (0/0) at night.
.fill_clear_sky <- function(obs, site) {
  gap_idx <- which(is.na(obs$value))
  if (length(gap_idx) == 0) {
    return(obs)
  }
  if (is.null(site)) {
    abort_meteo(
      c(
        "Filling a {.val clear_sky_indexed} variable needs {.arg site}.",
        "i" = "Pass the {.code met_site} object so clear-sky irradiance can be computed."
      ),
      class = "fill_missing_site"
    )
  }

  csi_ceiling <- clear_sky_irradiance(site, obs$datetime_utc)
  index <- ifelse(csi_ceiling > 0, obs$value / csi_ceiling, NA_real_)
  # Night timestamps (ceiling == 0) have an undefined index; keep them NA so
  # they don't influence the interpolation, and force them to 0 afterwards.
  index_filled <- imputeTS::na_interpolation(index, option = "linear")

  filled_value <- index_filled * csi_ceiling
  filled_value[csi_ceiling == 0] <- 0

  obs$value[gap_idx] <- pmax(filled_value[gap_idx], 0)
  obs$qc_flag[gap_idx] <- "ok"
  obs$method[gap_idx] <- "imputed"
  obs
}

# ---- plain linear/bounded interpolation ------------------------------------

.linear_interpolate <- function(obs) {
  gap_idx <- which(is.na(obs$value))
  if (length(gap_idx) == 0) {
    return(obs)
  }
  obs$value <- imputeTS::na_interpolation(obs$value, option = "linear")
  obs$qc_flag[gap_idx] <- "ok"
  obs$method[gap_idx] <- "imputed"
  obs
}

# Interpolate then clamp into [lo, hi] -- used for the RH-without-temperature
# fallback path.
.bounded_interpolate <- function(value, lo, hi) {
  filled <- imputeTS::na_interpolation(value, option = "linear")
  pmin(pmax(filled, lo), hi)
}
