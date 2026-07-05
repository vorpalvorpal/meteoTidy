# Plan 01 — built-in variable dictionary.
#
# `min`/`max` are the *starting* physically plausible ranges cited against
# WMO / BSRN guidance where noted; refine with a cited authority as later
# plans need tighter bounds. `circular_period = 360` is set for every
# wind_direction_* variable (direction is corrected as joint u/v components,
# never quantile-mapped as a raw angle — SCOPING §6; enforced in Plan 12).
# Every other variable has `circular_period = NA`.
#
# The hub-height wind directions and the two layered soil-moisture variables
# were added when the §3.1 contract was re-verified against meteoHazard's
# sources, 2026-07-05 — `odour_hazard()` requires the soil layers and
# `pressure_msl`; `ventilation_state()` optionally consumes the directions.

.meteo_builtin_variables <- function() {
  variable <- c(
    "temperature_2m", "relative_humidity_2m", "dewpoint_2m",
    "surface_pressure", "pressure_msl", "precipitation", "cloud_cover",
    "direct_radiation", "diffuse_radiation",
    "wind_speed_10m", "wind_direction_10m", "wind_gusts_10m",
    "wind_speed_80m", "wind_direction_80m",
    "wind_speed_120m", "wind_direction_120m",
    "wind_speed_180m", "wind_direction_180m",
    "boundary_layer_height",
    "soil_moisture_0_to_1cm", "soil_moisture_1_to_3cm",
    "cape", "uv_index"
  )

  tibble::tibble(
    variable = variable,
    unit = c(
      "degC", "%", "degC",
      "hPa", "hPa", "mm", "%",
      "W/m2", "W/m2",
      "m/s", "degree", "m/s",
      "m/s", "degree",
      "m/s", "degree",
      "m/s", "degree",
      "m",
      "m3/m3", "m3/m3",
      "J/kg", "1"
    ),
    min = c(
      -50, 0, -60,
      700, 870, 0, 0,
      0, 0,
      0, 0, 0,
      0, 0,
      0, 0,
      0, 0,
      0,
      0, 0,
      0, 0
    ),
    max = c(
      60, 100, 40,
      1100, 1085, 500, 100,
      1400, 1000,
      120, 360, 150,
      150, 360,
      150, 360,
      150, 360,
      5000,
      1, 1,
      8000, 20
    ),
    statistical_class = c(
      "linear", "bounded", "linear",
      "linear", "linear", "intermittent", "bounded",
      "clear_sky_indexed", "clear_sky_indexed",
      "linear", "circular", "linear",
      "linear", "circular",
      "linear", "circular",
      "linear", "circular",
      "linear",
      "bounded", "bounded",
      "linear", "bounded"
    ),
    measurability_class = c(
      "site_measurable", "site_measurable", "derived_measurable",
      "site_measurable", "derived_measurable", "site_measurable", "donor_observable",
      "derived_measurable", "derived_measurable",
      "site_measurable", "site_measurable", "site_measurable",
      "model_only", "model_only",
      "model_only", "model_only",
      "model_only", "model_only",
      "model_only",
      "model_only", "model_only",
      "model_only", "model_only"
    ),
    circular_period = ifelse(grepl("^wind_direction_", variable), 360, NA_real_),
    description = c(
      "Air temperature at 2 m above ground.",
      "Relative humidity at 2 m above ground.",
      "Dew point temperature at 2 m above ground.",
      "Station-level (surface) air pressure.",
      "Mean-sea-level air pressure.",
      "Precipitation accumulated over the reporting interval.",
      "Total cloud cover fraction.",
      "Direct (beam) shortwave radiation at the surface.",
      "Diffuse shortwave radiation at the surface.",
      "Wind speed at 10 m above ground.",
      "Wind direction at 10 m above ground (meteorological, from-direction).",
      "Maximum wind gust speed at 10 m above ground.",
      "Wind speed at 80 m above ground.",
      "Wind direction at 80 m above ground (meteorological, from-direction).",
      "Wind speed at 120 m above ground.",
      "Wind direction at 120 m above ground (meteorological, from-direction).",
      "Wind speed at 180 m above ground.",
      "Wind direction at 180 m above ground (meteorological, from-direction).",
      "Planetary boundary layer height.",
      "Volumetric soil moisture, 0-1 cm depth.",
      "Volumetric soil moisture, 1-3 cm depth.",
      "Convective available potential energy.",
      "UV index."
    )
  )
}
