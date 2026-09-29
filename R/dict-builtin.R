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
  vctrs::vec_rbind(.meteo_core_variables(), .meteo_forecast_product_variables(),
                   .meteo_category_variables())
}

.meteo_core_variables <- function() {
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

# Variables added for the production forecast archive (2026-09, problems 4 and
# 8 of the production review). Names follow Open-Meteo's conventions (hourly
# names for hourly quantities, `_max`/`_min`/`_sum` suffixes for DAILY
# aggregates), so BOM's edited forecast maps onto the dictionary instead of
# being mislabelled (the old précis parser stored daily Tmax as
# temperature_2m).
#
# Quantile forecasts ("X % chance of at least A mm") are not separate
# variables: they are rows of the base variable with `stat = "p<100-X>"`.
.meteo_forecast_product_variables <- function() {
  variable <- c(
    "shortwave_radiation", "apparent_temperature", "precipitation_probability",
    "temperature_2m_max", "temperature_2m_min", "precipitation_sum",
    "precipitation_probability_max", "uv_index_max"
  )
  tibble::tibble(
    variable = variable,
    unit = c("W/m2", "degC", "%", "degC", "degC", "mm", "%", "1"),
    min = c(0, -70, 0, -50, -50, 0, 0, 0),
    max = c(1400, 70, 100, 60, 60, 1000, 100, 25),
    statistical_class = c(
      "clear_sky_indexed", "linear", "bounded", "linear", "linear",
      "intermittent", "bounded", "bounded"
    ),
    measurability_class = c(
      "site_measurable", "derived_measurable", "model_only",
      "derived_measurable", "derived_measurable", "derived_measurable",
      "model_only", "model_only"
    ),
    circular_period = NA_real_,
    description = c(
      "Global horizontal (direct + diffuse) shortwave radiation at the surface.",
      "Apparent ('feels like') temperature at 2 m.",
      "Probability of precipitation (>= 0.2 mm) during the hour.",
      "Daily maximum air temperature at 2 m (local calendar day).",
      "Daily minimum air temperature at 2 m (local calendar day).",
      "Daily precipitation total (local calendar day).",
      "Daily probability of precipitation (>= 0.2 mm) (local calendar day).",
      "Daily maximum UV index (local calendar day)."
    )
  )
}

# Codes the report email needs (follow-up review, item 9). They are
# categories, not quantities, so they get statistical_class "categorical"
# (never averaged, interpolated or climatology-checked) and a dimensionless
# unit; min/max are the code ranges, which the range QC rule still applies.
.meteo_category_variables <- function() {
  tibble::tibble(
    variable = c("weather_code", "is_day"),
    unit = c("1", "1"),
    min = c(0, 0),
    max = c(99, 1),
    statistical_class = c("categorical", "categorical"),
    measurability_class = c("model_only", "model_only"),
    circular_period = NA_real_,
    description = c(
      "WMO weather interpretation code (WW, 0-99) for the hour; a category, not a quantity.",
      "1 if the hour is in daylight at the site, else 0."
    )
  )
}
