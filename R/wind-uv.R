# Plan 12 -- wind direction correction as joint u/v components (SCOPING
# section 6). Direction is a circular quantity: quantile-mapping (or any
# other univariate transform of) a raw angle is physically nonsensical near
# the 0/360 wrap (a small bias across north would appear as a ~180-degree
# jump). Correction always goes through the u/v vector decomposition and
# recombines afterwards; `fit_qmap()` is never called on a direction.

#' Decompose a wind direction/speed into u/v components
#'
#' Standard meteorological "from" convention: `dir` is the compass direction
#' the wind is blowing *from* (0/360 = north, 90 = east), so the velocity
#' vector points in the opposite sense: `u = -speed * sin(dir)`,
#' `v = -speed * cos(dir)`.
#'
#' @param speed Numeric vector, wind speed.
#' @param dir Numeric vector, wind direction in compass degrees (`from`
#'   convention).
#' @return A list with `u` and `v` numeric vectors.
#' @keywords internal
#' @noRd
dir_to_uv <- function(speed, dir) {
  rad <- dir * pi / 180
  list(u = -speed * sin(rad), v = -speed * cos(rad))
}

#' Recover a wind direction from u/v components
#'
#' Inverse of `dir_to_uv()`: `(atan2(-u, -v) * 180/pi) %% 360`.
#'
#' @param u,v Numeric vectors, the velocity components from `dir_to_uv()`.
#' @return Numeric vector, wind direction in compass degrees, `[0, 360)`.
#' @keywords internal
#' @noRd
uv_to_dir <- function(u, v) {
  (atan2(-u, -v) * 180 / pi) %% 360
}

# Rotate a u/v vector by `angle_deg` degrees (standard 2D rotation matrix).
.uv_rotate <- function(u, v, angle_deg) {
  a <- angle_deg * pi / 180
  list(u = u * cos(a) - v * sin(a), v = u * sin(a) + v * cos(a))
}

#' Correct a wind direction using a joint u/v bias
#'
#' Converts `dir`/`speed` to u/v, recovers the bias **angle** implied by
#' `bias_uv` (never touching the raw forecast angle directly), rotates the
#' forecast's u/v vector by that angle so the bias is subtracted from the
#' resulting direction (`.uv_rotate(u, v, a)`'s convention yields a rotated
#' direction of `dir - a`, so passing the bias angle itself removes it), and
#' converts back to a direction. Operating as a rotation (rather than a raw
#' vector subtraction) is what makes this correction well-behaved across the
#' 0/360 wrap: a uniform bias near north is removed as a uniform rotation,
#' never smeared toward the opposite (south) side of the compass. Direction
#' is never quantile-mapped as a raw angle (SCOPING section 6) -- this
#' function does not call `fit_qmap()` anywhere in its path.
#'
#' @param dir Numeric vector, forecast wind direction (compass degrees).
#' @param speed Numeric vector, forecast wind speed (recycled against `dir`).
#' @param bias_uv A list/tibble with `u`/`v` elements (e.g. from
#'   `dir_to_uv()`) representing the fitted directional bias as a vector.
#' @return Numeric vector, the corrected wind direction (compass degrees,
#'   `[0, 360)`).
#' @keywords internal
#' @noRd
correct_wind_direction <- function(dir, speed, bias_uv) {
  bias_mag <- sqrt(bias_uv$u^2 + bias_uv$v^2)
  bias_dir <- ifelse(bias_mag > 1e-9, uv_to_dir(bias_uv$u, bias_uv$v), 0)

  fc_uv <- dir_to_uv(speed, dir)
  rotated <- .uv_rotate(fc_uv$u, fc_uv$v, bias_dir)
  uv_to_dir(rotated$u, rotated$v)
}
