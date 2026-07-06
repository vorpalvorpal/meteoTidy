# Plan 12 -- model-only experiments (SCOPING section 7.3). Model-only
# variables (no site truth to verify against) default to raw pass-through,
# tier "raw" (done in Plan 11). This file adds three OPT-IN, EXPERIMENTAL
# extensions, each defaulting off and stamping a provenance marker that the
# value is experimental when used:
#
#  - profile_rescale(): rescale upper-level winds (80/120/180 m) by the
#    ratio implied by the corrected/raw 10 m wind, damped with height,
#    capped, and suppressed under stable stratification.
#  - diagnostic_blh(): an AERMET-style boundary-layer height recomputed from
#    corrected surface variables, served *alongside* the raw model BLH.
#  - radiation_resplit(): where a pyranometer exists, correct global
#    irradiance via the clear-sky index and re-split direct/diffuse
#    preserving the model's split ratio; without a pyranometer, raw model
#    values pass through unchanged (tier "raw").

#' Rescale an upper-level wind using the corrected 10 m wind ratio
#'
#' Multiplies `raw` (an upper-level wind, e.g. at 80/120/180 m) by the ratio
#' `corrected_10m / raw_10m`, **damped with height** (the influence of a
#' near-surface correction on a level far above it should taper off) and
#' **capped** (an extreme implied ratio is not applied unbounded). Under
#' `stable = TRUE` (stable stratification -- typically night + low corrected
#' 10 m wind) the rescale is suppressed entirely and `raw` is returned
#' unchanged, since the log-wind-profile assumption this experiment leans on
#' breaks down most under a strong surface inversion
#' (`R/fill-treatments.R`'s `height_correct()` documents the same caveat).
#'
#' This is opt-in and experimental (SCOPING section 7.3): callers that use
#' it should treat the result as flagged experimental, never as a default
#' correction path.
#'
#' @param raw Numeric vector, the raw upper-level wind speed.
#' @param height Single double (metres), the upper level's height (e.g. 80,
#'   120, 180).
#' @param corrected_10m,raw_10m Numeric vectors, the corrected and raw 10 m
#'   wind speed.
#' @param stable Logical, whether stable stratification applies (suppresses
#'   the rescale). Default `FALSE`.
#' @param reference_height Single double (metres), the height the ratio was
#'   computed at. Default `10`.
#' @param cap Single double, the maximum (and reciprocal minimum) damped
#'   ratio allowed. Default `3`.
#' @return Numeric vector, the rescaled (or, if `stable`/uncapped-degenerate,
#'   unchanged) upper-level wind speed.
#' @keywords internal
#' @noRd
profile_rescale <- function(raw, height, corrected_10m, raw_10m, stable = FALSE,
                            reference_height = 10, cap = 3) {
  if (isTRUE(stable)) {
    return(raw)
  }
  ratio <- corrected_10m / raw_10m
  damping <- reference_height / height
  damped_ratio <- 1 + (ratio - 1) * damping
  damped_ratio <- pmin(pmax(damped_ratio, 1 / cap), cap)
  raw * damped_ratio
}

#' Diagnostic boundary-layer height (EXPERIMENTAL APPROXIMATION, not AERMET)
#'
#' **This is a heuristic approximation, not the AERMET-style scheme SCOPING
#' section 7.3 describes**, and is explicitly marked as such here rather than
#' implying a full scheme: a simple mixing-height proxy scaling with
#' near-surface wind shear production and a thermal stability proxy,
#' recomputed from *corrected* surface temperature and wind and served
#' **alongside** (never replacing) the raw model BLH. Off by default
#' (SCOPING section 7.3's model-only opt-in convention) and not verified
#' against site truth (Plan 13) -- acceptable for v1 as an off-by-default
#' diagnostic, but callers must not treat it as a validated BLH scheme.
#'
#' @param raw_blh Numeric vector, the raw model boundary-layer height.
#' @param corrected_surface A list/tibble with `temperature_2m` and
#'   `wind_speed_10m` elements (corrected surface variables).
#' @return A list with `raw_blh` (unchanged) and `diagnostic_blh` (the
#'   recomputed estimate).
#' @keywords internal
#' @noRd
diagnostic_blh <- function(raw_blh, corrected_surface) {
  temp <- corrected_surface$temperature_2m
  wind <- corrected_surface$wind_speed_10m
  # A simple mechanical-plus-thermal mixing-height proxy: BLH grows with
  # near-surface wind (mechanical turbulence) and with warmer temperatures
  # (a rough stand-in for daytime convective mixing) -- deliberately basic;
  # see roxygen note above.
  diagnostic <- 100 * (1 + 0.1 * wind) * (1 + 0.02 * pmax(temp, 0))
  list(raw_blh = raw_blh, diagnostic_blh = diagnostic)
}

#' Radiation re-split (direct/diffuse) using a clear-sky index correction
#'
#' Where a pyranometer exists (`has_pyranometer = TRUE`): correct global
#' irradiance via the clear-sky index, then re-split into direct/diffuse
#' preserving the model's original split ratio (the simpler of the plan's
#' two accepted choices -- "preserving the model's split ratio (or a
#' decomposition model, e.g. BRL)", `plans/12-correction-fitted-tiers.md`).
#' Without a pyranometer (`has_pyranometer = FALSE`, the default/only tested
#' path): the raw model direct/diffuse pass through unchanged, tier `"raw"`.
#'
#' @param direct,diffuse Numeric vectors, model direct/diffuse irradiance.
#' @param has_pyranometer Logical, whether a site pyranometer's global
#'   irradiance is available to correct against. Default `FALSE`.
#' @param global_corrected Numeric vector, the pyranometer-corrected global
#'   irradiance (only used when `has_pyranometer = TRUE`).
#' @return A list with `direct`, `diffuse`, and `tier` (`"raw"` when no
#'   pyranometer, `"experimental"` otherwise).
#' @keywords internal
#' @noRd
radiation_resplit <- function(direct, diffuse, has_pyranometer = FALSE,
                              global_corrected = NULL) {
  if (!isTRUE(has_pyranometer)) {
    return(list(direct = direct, diffuse = diffuse, tier = "raw"))
  }
  global_raw <- direct + diffuse
  split_ratio <- direct / global_raw
  direct_corrected <- global_corrected * split_ratio
  diffuse_corrected <- global_corrected * (1 - split_ratio)
  list(direct = direct_corrected, diffuse = diffuse_corrected, tier = "experimental")
}

#' Apply the (opt-in) model-only correction experiments to a variable
#'
#' Default (`enable_profile_rescale = FALSE`): `obs` passes through
#' unchanged with tier `"raw"` stamped on every row -- the SCOPING section
#' 7.3 default for model-only variables. When enabled, routes each row
#' through `profile_rescale()` using the height parsed from the variable
#' name (e.g. `wind_speed_80m` -> height `80`) and stamps tier
#' `"experimental"` to mark the value as an opt-in, unverified rescale.
#'
#' @param obs A canonical long obs tibble for one model-only wind variable.
#' @param corrected_10m,raw_10m Numeric vectors (recycled against `obs`),
#'   the corrected and raw 10 m wind speed for the same timestamps.
#' @param enable_profile_rescale Logical, opt-in flag. Default `FALSE`.
#' @param ... Passed on to `profile_rescale()` (e.g. `stable`, `cap`).
#' @return `obs` with `value`/`tier` set per the above.
#' @keywords internal
#' @noRd
model_only_correct <- function(obs, corrected_10m, raw_10m,
                               enable_profile_rescale = FALSE, ...) {
  if (!isTRUE(enable_profile_rescale)) {
    obs$tier <- "raw"
    return(obs)
  }

  height <- as.numeric(sub(".*_([0-9]+)m$", "\\1", obs$variable[[1]]))
  obs$value <- profile_rescale(obs$value, height = height,
                               corrected_10m = corrected_10m, raw_10m = raw_10m, ...)
  obs$tier <- "experimental"
  obs
}
