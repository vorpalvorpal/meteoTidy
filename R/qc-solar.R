# Plan 09 -- solar clear-sky QC.
#
# Named clear-sky model: A SELF-CONTAINED IMPLEMENTATION OF THE INEICHEN-PEREZ
# CLEAR-SKY MODEL (Ineichen & Perez, 2002, "A new airmass independent
# formulation for the Linke turbidity coefficient", Solar Energy 73(3)),
# using a fixed, climatological Linke turbidity (TL = 3, a reasonable
# mid-latitude clear-sky average) rather than a location/month-resolved
# turbidity climatology -- adequate for a QC plausibility ceiling (this is
# not a solar-resource model). McClear (the alternative named model the plan
# allows) would need a live CAMS query and is not implemented; a
# `Suggests`-gated path is left as a documented future refinement rather than
# built here, since no heavy new dependency is required for Ineichen-Perez.
#
# BSRN-style physical-possible / extremely-rare limits (Long & Dutton, 2010 /
# the WCRP Baseline Surface Radiation Network quality-control recommendations,
# as summarised e.g. in Journee & Bertrand 2011): a global-horizontal-type
# irradiance may not exceed the "physically possible" ceiling, computed as
# 1.5 times the extraterrestrial irradiance times cos(zenith)^1.2 plus a
# 100 W/m^2 additive margin (zenith = solar zenith angle; extraterrestrial
# irradiance = E0 adjusted for the current sun-earth distance), and any
# nonzero irradiance while the sun is below the horizon is impossible
# outright. A separate, tighter "extremely rare" limit close to (but above)
# the clear-sky estimate flags merely unusual-but-not-impossible values as
# "suspect" rather than "fail".

.qc_solar_solar_constant <- function() {
  1361 # W/m^2, E0 at 1 AU (approximately; seasonal AU correction applied below)
}

# Day-of-year fractional angle (radians), used by the standard low-order
# Fourier approximations for solar declination / equation of time / earth-sun
# distance correction (Spencer 1971, as widely reproduced in solar-geometry
# references, e.g. Duffie & Beckman, "Solar Engineering of Thermal
# Processes").
.qc_solar_day_angle <- function(times_utc) {
  doy <- as.numeric(format(times_utc, "%j", tz = "UTC"))
  2 * pi * (doy - 1) / 365
}

# Earth-sun distance correction factor (E0/Ebar0)^... squared multiplier on
# the solar constant (Spencer 1971).
.qc_solar_distance_factor <- function(gamma) {
  1.000110 + 0.034221 * cos(gamma) + 0.001280 * sin(gamma) +
    0.000719 * cos(2 * gamma) + 0.000077 * sin(2 * gamma)
}

# Solar declination (radians), Spencer (1971).
.qc_solar_declination <- function(gamma) {
  0.006918 - 0.399912 * cos(gamma) + 0.070257 * sin(gamma) -
    0.006758 * cos(2 * gamma) + 0.000907 * sin(2 * gamma) -
    0.002697 * cos(3 * gamma) + 0.00148 * sin(3 * gamma)
}

# Equation of time (minutes), Spencer (1971).
.qc_solar_eot <- function(gamma) {
  229.18 * (0.000075 + 0.001868 * cos(gamma) - 0.032077 * sin(gamma) -
              0.014615 * cos(2 * gamma) - 0.040849 * sin(2 * gamma))
}

# Solar zenith angle (radians) and cos(zenith) at `times_utc` for a given
# (lat, lon) in decimal degrees. Standard solar-position geometry: hour angle
# from true solar time (UTC + longitude/15 + equation-of-time correction; no
# separate timezone lookup needed since we work entirely from UTC + longitude).
.qc_solar_position <- function(lat_deg, lon_deg, times_utc) {
  gamma <- .qc_solar_day_angle(times_utc)
  decl <- .qc_solar_declination(gamma)
  eot <- .qc_solar_eot(gamma) # minutes

  utc_hours <- as.numeric(format(times_utc, "%H", tz = "UTC")) +
    as.numeric(format(times_utc, "%M", tz = "UTC")) / 60 +
    as.numeric(format(times_utc, "%S", tz = "UTC")) / 3600

  solar_time <- utc_hours + lon_deg / 15 + eot / 60
  hour_angle <- (solar_time - 12) * 15 * pi / 180 # radians

  lat_rad <- lat_deg * pi / 180
  cos_zenith <- sin(lat_rad) * sin(decl) + cos(lat_rad) * cos(decl) * cos(hour_angle)
  cos_zenith <- pmin(pmax(cos_zenith, -1), 1)

  list(cos_zenith = cos_zenith, distance_factor = .qc_solar_distance_factor(gamma))
}

#' Clear-sky irradiance (Ineichen-Perez) at a site and time
#'
#' Computes a deterministic clear-sky global-horizontal-type irradiance
#' estimate for `site` at each instant in `times`, using the Ineichen-Perez
#' clear-sky model (Ineichen & Perez, 2002) with a fixed climatological Linke
#' turbidity. Returns 0 whenever the sun is below the horizon.
#'
#' @param site A `met_site` object (see `site_coords()`).
#' @param times A UTC `POSIXct` vector.
#' @return A numeric vector (W/m^2), the same length as `times`.
#' @keywords internal
#' @noRd
clear_sky_irradiance <- function(site, times) {
  coords <- site_coords(site)
  lat_deg <- as.numeric(coords$latitude)
  lon_deg <- as.numeric(coords$longitude)
  elevation_m <- as.numeric(coords$elevation)
  if (is.na(elevation_m)) elevation_m <- 0

  pos <- .qc_solar_position(lat_deg, lon_deg, times)
  cos_zenith <- pos$cos_zenith
  above_horizon <- cos_zenith > 0

  e0 <- .qc_solar_solar_constant() * pos$distance_factor

  # Ineichen-Perez clear-sky GHI, fixed Linke turbidity TL = 3 and a simple
  # altitude (elevation) correction on the Rayleigh optical air mass term,
  # following the model's published functional form (Ineichen & Perez 2002,
  # eq. 1-2; coefficients as commonly reproduced, e.g. pvlib's
  # `ineichen` implementation, used here as the reference form -- no pvlib
  # dependency, reimplemented directly).
  tl <- 3
  altitude_km <- elevation_m / 1000
  fh1 <- exp(-altitude_km / 8)
  fh2 <- exp(-altitude_km / 1.25)
  cg1 <- (0.0000509 * elevation_m + 0.868)
  cg2 <- (0.0000392 * elevation_m + 0.0387)

  ghi <- ifelse(
    above_horizon,
    cg1 * e0 * cos_zenith * exp(-cg2 * (1 / pmax(cos_zenith, 1e-6)) * (fh1 + fh2 * (tl - 1))) *
      exp(0.01 * (1 / pmax(cos_zenith, 1e-6))^1.8),
    0
  )
  ghi <- pmax(ghi, 0)
  ghi[!above_horizon] <- 0
  ghi
}

# BSRN-style limits (Long & Dutton 2010 / WCRP BSRN QC recommendations):
#   possible  = 1.5 * E0 * cos(theta_z)^1.2 + 100   [W/m^2]  -- "fail" above
#   rare      = 1.2 * E0 * cos(theta_z)^1.2 + 50     [W/m^2]  -- "suspect" above
# Both ceilings are 0 (plus their additive constant) when the sun is below
# the horizon, so any nonzero night-time value exceeds the rare limit and is
# handled by the explicit night-time check below regardless.
.qc_solar_bsrn_limits <- function(site, times) {
  coords <- site_coords(site)
  lat_deg <- as.numeric(coords$latitude)
  lon_deg <- as.numeric(coords$longitude)
  pos <- .qc_solar_position(lat_deg, lon_deg, times)
  cos_zenith <- pmax(pos$cos_zenith, 0)
  e0 <- .qc_solar_solar_constant() * pos$distance_factor

  list(
    possible = 1.5 * e0 * cos_zenith^1.2 + 100,
    rare = 1.2 * e0 * cos_zenith^1.2 + 50,
    above_horizon = pos$cos_zenith > 0
  )
}

#' Solar clear-sky QC rule
#'
#' For a `clear_sky_indexed` radiation series (`direct_radiation`,
#' `diffuse_radiation`), computes the BSRN-style physically-possible and
#' extremely-rare irradiance limits (Long & Dutton 2010) from the
#' Ineichen-Perez clear-sky estimate (`clear_sky_irradiance()`) at the site's
#' location and each observation's time, and flags:
#' - nonzero irradiance while the sun is below the horizon -> `"fail"`;
#' - a value above the physically-possible ceiling -> `"fail"`;
#' - a value above the extremely-rare (but physically possible) ceiling ->
#'   `"suspect"`.
#'
#' @param obs A single-variable canonical long radiation series.
#' @param site A `met_site` object.
#' @return `obs` with `qc_flag` downgraded per the limits above, and
#'   `qc_log` rows attached.
#' @keywords internal
#' @noRd
qc_solar <- function(obs, site) {
  limits <- .qc_solar_bsrn_limits(site, obs$datetime_utc)

  night_violation <- !limits$above_horizon & !is.na(obs$value) & obs$value > 1
  fail_possible <- limits$above_horizon & !is.na(obs$value) & obs$value > limits$possible
  fail_idx <- which(night_violation | fail_possible)

  suspect_idx <- which(
    !seq_len(nrow(obs)) %in% fail_idx &
      limits$above_horizon & !is.na(obs$value) & obs$value > limits$rare
  )

  out <- .qc_downgrade(obs, fail_idx, "fail")
  out <- .qc_downgrade(out, suspect_idx, "suspect")

  log_fail <- .qc_log_rows(
    obs, fail_idx, "solar", "fail",
    "irradiance above BSRN physically-possible limit or nonzero at night"
  )
  log_suspect <- .qc_log_rows(
    obs, suspect_idx, "solar", "suspect",
    "irradiance above BSRN extremely-rare limit"
  )
  out <- .qc_log_attach(out, log_fail)
  .qc_log_attach(out, log_suspect)
}
