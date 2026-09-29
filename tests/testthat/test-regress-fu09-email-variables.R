# Follow-up review, item 9: archive what the report email will need.
#
# The email shows sky/weather icons (weather_code, is_day), UV, soil
# moisture, mixing height (boundary_layer_height), instability (cape), cloud,
# wind at 10 m and 80 m with gusts and direction, radiation and pressure;
# the probabilistic panel needs a second ensemble; the fire panel needs
# BOM's AFDRS category. Before this change weather_code/is_day were not in
# the dictionary, uv_index was dropped from every model (Open-Meteo reports
# its unit as "", which was read as "unknown"), the ensemble was ECMWF only,
# and fire_danger_category was not archived. Replays forecasts recorded
# 2026-09-29 (GFS, ICON, ICON-EPS; BOM daily for Blaxland).

fu09_email_vars <- c(
  "weather_code", "is_day", "uv_index", "soil_moisture_0_to_1cm", "soil_moisture_1_to_3cm",
  "boundary_layer_height", "cape", "cloud_cover", "wind_direction_10m", "wind_gusts_10m",
  "wind_speed_80m", "direct_radiation", "diffuse_radiation", "shortwave_radiation",
  "surface_pressure"
)

fu09_window <- function() list(from = prod_now() - 86400, to = prod_now())

fu09_fetch <- function(adapter, variables, capture = new.env(), warn = FALSE) {
  f <- function() {
    with_routed_http(openmeteo_routes(), {
      fetch_forecast(adapter, make_prod_site("kat"), variables, fu09_window(), now = prod_now())
    }, capture = capture)
  }
  if (warn) f() else suppressWarnings(f())
}

describe("item 9: variables for the email", {
  it("has weather_code and is_day in the dictionary as categories", {
    dict <- met_variables()
    expect_true(all(c("weather_code", "is_day") %in% dict$variable))
    expect_true("categorical" %in% STAT_CLASS_LEVELS)
    cls <- dict$statistical_class[match(c("weather_code", "is_day"), dict$variable)]
    expect_equal(cls, c("categorical", "categorical"))
    # A category is never averaged or interpolated.
    expect_equal(.aggregate_value(c(1, 3, 61), "categorical"), 61)
    obs <- new_obs(tibble::tibble(
      site_id = "kat", datetime_utc = as.POSIXct("2026-09-29", tz = "UTC") + (0:2) * 3600,
      variable = "weather_code", value = c(3, NA, 61), source = "openmeteo",
      method = "model_fill", qc_flag = "ok"
    ))
    expect_true(is.na(fill_micro(obs)$value[2]))
  })

  it("requests every email variable by default", {
    adapter <- source_openmeteo("forecast")
    expect_true(all(fu09_email_vars %in% adapter@provides))
  })

  it("archives weather_code, is_day, uv_index and the rest from GFS", {
    fc <- fu09_fetch(source_openmeteo("forecast", models = "gfs_global"), fu09_email_vars)
    got <- unique(fc$variable)
    expect_true(all(c("weather_code", "is_day", "uv_index", "boundary_layer_height",
                      "shortwave_radiation", "wind_gusts_10m") %in% got))
    wc <- fc$value[fc$variable == "weather_code"]
    expect_true(all(wc %in% 0:99))
    expect_setequal(unique(fc$value[fc$variable == "is_day"]), c(0, 1))
    expect_gt(max(fc$value[fc$variable == "uv_index"]), 0)
  })

  it("does not request what a model cannot serve", {
    cap <- new.env()
    expect_no_warning(fu09_fetch(source_openmeteo("forecast", models = "icon_global"),
                                 fu09_email_vars, capture = cap, warn = TRUE))
    url <- grep("/v1/forecast", cap$urls, value = TRUE)
    expect_no_match(url, "boundary_layer_height")
    expect_match(url, "soil_moisture_0_to_1cm")
  })

  it("adds the ICON ensemble alongside ECMWF IFS", {
    cap <- new.env()
    fc <- fu09_fetch(source_openmeteo("ensemble"), c("temperature_2m", "wind_gusts_10m"),
                     capture = cap)
    expect_setequal(unique(fc$model), c("ecmwf_ifs025", "icon_seamless"))
    icon <- fc[fc$model == "icon_seamless", ]
    meta <- jsonlite::read_json(fixture_path("openmeteo/meta-dwd_icon_eps.json"))
    expect_equal(unique(icon$issue_time),
                 as.POSIXct(meta$last_run_initialisation_time, origin = "1970-01-01", tz = "UTC"))
    expect_equal(length(unique(icon$member)), 40L)
    # ICON-EPS has no gusts on Open-Meteo: not requested, not archived.
    expect_no_match(grep("models=icon_seamless", cap$urls, value = TRUE), "wind_gusts_10m")
    expect_false("wind_gusts_10m" %in% icon$variable)
  })

  it("archives BOM's fire-danger category", {
    body <- jsonlite::read_json(fixture_path("bom/webapi-daily-r65050.json"), simplifyVector = FALSE)
    aux <- bom_parse_webapi_daily_aux(body, "blax", geohash = "r65050")
    fdc <- aux[aux$field == "fire_danger_category", ]
    expect_gte(nrow(fdc), 4)
    expect_true(all(fdc$value_text %in% c("No Rating", "Moderate", "High", "Extreme", "Catastrophic")))
  })
})

describe("item 9: reading the archived text fields", {
  it("met_forecast_aux() returns the fire-danger category a sync archived", {
    root <- withr::local_tempdir()
    site <- make_prod_site("blax", store_root = root, sources = list(
      bom_forecast = list(adapter = "bom_forecast", allow_web_api = TRUE)
    ))
    routes <- list(
      "r65050/forecasts/daily" = "bom/webapi-daily-r65050.json",
      "r65050/forecasts/hourly" = "bom/webapi-hourly-r65050.json"
    )
    cfg <- list(store_root = root, obs_sources = character(0), forecast_sources = "bom_forecast")
    suppressMessages(with_routed_http(routes, met_sync_live(site, now = prod_now(), config = cfg)))
    fdc <- met_forecast_aux(site, source = "bom_forecast", field = "fire_danger_category")
    expect_gte(nrow(fdc), 4)
    expect_setequal(unique(fdc$field), "fire_danger_category")
    expect_true("Moderate" %in% fdc$value_text)
    expect_equal(nrow(met_forecast_aux(site, source = "openmeteo")), 0)
  })
})
