# Plan 10 -- SILO daily -> hourly disaggregation: the donor ladder's LAST
# rung (SCOPING sections 6 and 13). Genuinely hard in general (recovering
# sub-daily structure from a daily total has no unique answer), which is
# exactly why it sits last: every higher rung (BOM, GHCNh, ERA5) gives real
# sub-daily structure directly, so this is only reached when none of them
# have any coverage at all.
#
# The approach never invents sub-daily structure beyond a supplied `shape` --
# a diurnal curve (0..1-ish, or any monotonic-enough profile; it need not
# integrate/sum to anything in particular) taken from the best available
# sub-daily reference for the window (a donor station's own diurnal profile,
# else a model series, else a site climatological diurnal cycle -- selecting
# which of those is out of this function's scope; `disaggregate_silo()` only
# does the scaling once a `shape` is supplied). The shape is rescaled, in the
# variable's own treatment space, so the 24 disaggregated hours EXACTLY
# reproduce the daily value: sum for rain, min/max for temperature (given as
# two rows, `temperature_2m_min`/`temperature_2m_max`), mean otherwise.

.silo_disagg_hours <- function(day_date) {
  seq(as.POSIXct(paste(day_date, "00:00:00"), tz = "UTC"), by = "hour", length.out = 24)
}

# Scale `shape` (a length-24 numeric vector) so its 24 values SUM exactly to
# `total`. An all-zero (or all-NA) shape degrades to all-zero hours rather
# than NaN/Inf (the all-dry-day edge case) -- there is no diurnal signal to
# distribute a zero total across anyway.
.disagg_scale_sum <- function(shape, total) {
  shape_sum <- sum(shape, na.rm = TRUE)
  if (!is.finite(shape_sum) || shape_sum == 0 || total == 0) {
    return(rep(0, length(shape)))
  }
  shape / shape_sum * total
}

# Affine-rescale `shape` onto [lo, hi] exactly: shape's own min maps to `lo`,
# its own max maps to `hi`. Degenerate (`shape` constant) collapses to a flat
# line at the midpoint of [lo, hi] (no information to place a diurnal swing).
.disagg_scale_range <- function(shape, lo, hi) {
  shape_min <- min(shape, na.rm = TRUE)
  shape_max <- max(shape, na.rm = TRUE)
  if (!is.finite(shape_min) || !is.finite(shape_max) || shape_max == shape_min) {
    return(rep((lo + hi) / 2, length(shape)))
  }
  lo + (shape - shape_min) / (shape_max - shape_min) * (hi - lo)
}

#' Disaggregate a SILO daily value to 24 hourly values
#'
#' Scales a length-24 `shape` (a diurnal curve from the best available
#' sub-daily reference -- a donor station, a model series, or a site
#' climatological cycle; supplied by the caller, never invented here) in the
#' variable's own treatment space so the 24 disaggregated hours **exactly
#' reproduce** the daily value(s) in `daily`:
#'
#' - `precipitation`: `shape` is rescaled so the 24 hourly values sum exactly
#'   to the daily total. An all-zero day disaggregates to all-zero hours (no
#'   invented drizzle), handled explicitly rather than via a `0/0` division.
#' - temperature (given as two rows, `variable` = `"temperature_2m_min"` and
#'   `"temperature_2m_max"`): `shape` is affine-rescaled onto `[min, max]`
#'   so the 24 hourly values' own min/max reproduce the daily min/max
#'   exactly. Output `variable` becomes the plain `"temperature_2m"`.
#' - anything else: `shape` is rescaled around its own mean so the 24 hourly
#'   values average to the daily value.
#'
#' @param daily A one-(or, for temperature, two-)row long tibble: a single
#'   day, single site, for one variable (or the min/max pair).
#' @param shape A length-24 numeric vector, the diurnal shape to scale (not
#'   required to sum/integrate to any particular value itself).
#' @return A 24-row canonical-shaped long tibble, `method =
#'   "disaggregated"`, with a `shape_source` column recording the shape's
#'   provenance (a placeholder string, since the actual shape-selection
#'   policy is the caller's concern, not this function's).
#' @keywords internal
#' @noRd
disaggregate_silo <- function(daily, shape) {
  stopifnot(length(shape) == 24)
  day_date <- as.Date(daily$datetime_utc[1])
  hours <- .silo_disagg_hours(day_date)
  site_id <- daily$site_id[1]
  source <- daily$source[1]

  is_temp_pair <- all(c("temperature_2m_min", "temperature_2m_max") %in% daily$variable)

  if (is_temp_pair) {
    lo <- daily$value[daily$variable == "temperature_2m_min"][1]
    hi <- daily$value[daily$variable == "temperature_2m_max"][1]
    values <- .disagg_scale_range(shape, lo, hi)
    variable <- "temperature_2m"
  } else if (identical(daily$variable[1], "precipitation")) {
    values <- .disagg_scale_sum(shape, daily$value[1])
    variable <- "precipitation"
  } else {
    target_mean <- daily$value[1]
    shape_mean <- mean(shape, na.rm = TRUE)
    values <- if (is.finite(shape_mean) && shape_mean != 0) {
      shape / shape_mean * target_mean
    } else {
      rep(target_mean, length(shape))
    }
    variable <- daily$variable[1]
  }

  out <- tibble::tibble(
    site_id = site_id,
    datetime_utc = hours,
    variable = variable,
    value = values,
    source = source,
    method = "disaggregated",
    qc_flag = "ok",
    shape_source = "sub-daily reference (donor/model/climatology, caller-supplied)"
  )
  attr(out, "shape_source") <- out$shape_source[1]
  out
}
