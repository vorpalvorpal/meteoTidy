# Follow-up review, item 10: met_wide() needs a statistic and per-variable
# model precedence.
#
# With named Open-Meteo models (item 6) no single model serves every
# variable (ECMWF IFS has no boundary-layer height or soil moisture), and
# met_wide() served one model or aborted. The email also needs upper
# percentiles (e.g. the p95 gust or temperature of the ensemble, BOM's 10 %
# chance rain amount), but met_wide() only ever served the member mean (or
# BOM's median). Now `model` is a precedence list -- each variable comes
# from the first listed model that has it, never a blend -- and `stat`
# ("mean", "median" or "pNN") picks the statistic: a percentile across
# ensemble members, or the matching published quantile (BOM's p90 rain).
# Built from recorded responses (GFS, ICON, ECMWF IFS, both ensembles, BOM)
# through the real parsers and store.

fu10_vars <- c("temperature_2m", "wind_speed_10m", "boundary_layer_height",
               "soil_moisture_0_to_1cm", "precipitation")

fu10_archive <- function(root) {
  site <- make_prod_site("kat", store_root = root, sources = list(
    openmeteo = list(adapter = "openmeteo", product = "forecast",
                     provides = c("temperature_2m", "wind_speed_10m", "boundary_layer_height",
                                  "soil_moisture_0_to_1cm")),
    om_ens = list(adapter = "openmeteo", product = "ensemble", provides = "temperature_2m"),
    bom_forecast = list(adapter = "bom_forecast", allow_web_api = TRUE)
  ))
  routes <- c(openmeteo_routes(), list(
    "r64bhq/forecasts/daily" = "bom/webapi-daily-r64bhq.json",
    "r64bhq/forecasts/hourly" = "bom/webapi-hourly-r64bhq.json"
  ))
  suppressWarnings(with_routed_http(routes, {
    archive_forecasts(root, site, c("openmeteo", "om_ens", "bom_forecast"), now = prod_now())
  }))
  site
}

fu10_window <- function() {
  from <- as.POSIXct("2026-09-29 07:00:00", tz = "UTC")
  list(from = from, to = from + 71 * 3600)
}

stored_series <- function(root, source, model, variable, times, stat = NA) {
  fc <- store_read_forecast(root, "kat", source = source)
  fc <- fc[fc$model == model & fc$variable == variable & is.na(fc$member) &
             (if (is.na(stat)) is.na(fc$stat) else fc$stat %in% stat), ]
  fc <- fc[fc$issue_time == max(fc$issue_time), ]
  fc$value[match(times, fc$valid_time)]
}

describe("item 10: per-variable model precedence", {
  it("serves each variable from the first model that has it", {
    root <- withr::local_tempdir()
    site <- fu10_archive(root)
    wide <- met_wide(site, fu10_window(), variables = fu10_vars[1:4], now = prod_now())
    prov <- met_provenance(wide)
    got <- stats::setNames(prov$model, prov$variable)
    expect_equal(got[["temperature_2m"]], "ecmwf_ifs025")
    expect_equal(got[["boundary_layer_height"]], "gfs_global")     # ECMWF has none
    expect_equal(got[["soil_moisture_0_to_1cm"]], "icon_global")   # neither ECMWF nor GFS
    expect_equal(wide$boundary_layer_height,
                 stored_series(root, "openmeteo", "gfs_global", "boundary_layer_height", wide$time))
    expect_equal(wide$temperature_2m,
                 stored_series(root, "openmeteo", "ecmwf_ifs025", "temperature_2m", wide$time))
  })

  it("follows an explicit precedence, and rejects one with no archived model", {
    root <- withr::local_tempdir()
    site <- fu10_archive(root)
    wide <- met_wide(site, fu10_window(), variables = "temperature_2m",
                     model = c("icon_global", "ecmwf_ifs025"), now = prod_now())
    expect_equal(met_provenance(wide)$model, "icon_global")
    expect_equal(wide$temperature_2m,
                 stored_series(root, "openmeteo", "icon_global", "temperature_2m", wide$time))
    expect_error(met_wide(site, fu10_window(), model = c("ukmo_global", "jma_gsm"), now = prod_now()),
                 class = "meteoTidy_error_wide_source_unavailable")
  })
})

describe("item 10: stat", {
  it("serves an ensemble percentile across members", {
    root <- withr::local_tempdir()
    site <- fu10_archive(root)
    mean_w <- met_wide(site, fu10_window(), variables = "temperature_2m", source = "om_ens",
                       now = prod_now())
    p95_w <- met_wide(site, fu10_window(), variables = "temperature_2m", source = "om_ens",
                      stat = "p95", now = prod_now())
    expect_equal(met_provenance(p95_w)$stat, "p95")
    expect_equal(met_provenance(p95_w)$model, "ecmwf_ifs025")
    fc <- store_read_forecast(root, "kat", source = "om_ens")
    fc <- fc[fc$model == "ecmwf_ifs025" & fc$variable == "temperature_2m", ]
    t1 <- p95_w$time[10]
    expect_equal(p95_w$temperature_2m[10],
                 unname(stats::quantile(fc$value[fc$valid_time == t1], 0.95)))
    expect_true(all(p95_w$temperature_2m >= mean_w$temperature_2m, na.rm = TRUE))
    expect_true(any(p95_w$temperature_2m > mean_w$temperature_2m, na.rm = TRUE))
  })

  it("serves BOM's published rain percentile", {
    root <- withr::local_tempdir()
    site <- fu10_archive(root)
    p90 <- met_wide(site, fu10_window(), variables = "precipitation", source = "bom_forecast",
                    model = "hourly", stat = "p90", now = prod_now())
    expect_equal(p90$precipitation,
                 stored_series(root, "bom_forecast", "hourly", "precipitation", p90$time, "p90"))
    expect_equal(met_provenance(p90)$stat, "p90")
    med <- met_wide(site, fu10_window(), variables = "precipitation", source = "bom_forecast",
                    model = "hourly", now = prod_now())
    expect_equal(met_provenance(med)$stat, "p50")
    expect_true(all(p90$precipitation >= med$precipitation, na.rm = TRUE))
  })

  it("rejects a statistic it cannot serve", {
    root <- withr::local_tempdir()
    site <- fu10_archive(root)
    expect_error(met_wide(site, fu10_window(), stat = "p150", now = prod_now()),
                 class = "meteoTidy_error_bad_wide_stat")
  })
})
