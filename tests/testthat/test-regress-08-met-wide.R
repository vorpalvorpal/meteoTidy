# Regression (production review, problem 8): met_wide() pooled every source
# and model in the archive into one mean (Open-Meteo best_match + ECMWF
# ensemble + BOM, all averaged), and its provenance named only the first
# source. It also has to satisfy meteoHazard: gusts >= mean wind after any
# aggregation, and shortwave_radiation (litter_risk(use_wetness_state = TRUE)).
#
# The archive is built from recorded responses (Open-Meteo best_match and
# ECMWF IFS ensemble, BOM hourly/daily for Katoomba) through the real parsers.

wide_archive <- function(root) {
  site <- make_prod_site("kat", store_root = root, sources = list(
    openmeteo = list(adapter = "openmeteo", product = "forecast", models = "best_match",
                     provides = c("temperature_2m", "wind_speed_10m", "wind_gusts_10m")),
    om_ens = list(adapter = "openmeteo", product = "ensemble", provides = "temperature_2m"),
    bom_forecast = list(adapter = "bom_forecast", allow_web_api = TRUE)
  ))
  routes <- c(openmeteo_routes(), list(
    "r64bhq/forecasts/daily" = "bom/webapi-daily-r64bhq.json",
    "r64bhq/forecasts/hourly" = "bom/webapi-hourly-r64bhq.json"
  ))
  with_routed_http(routes, {
    archive_forecasts(root, site, c("openmeteo", "om_ens", "bom_forecast"), now = prod_now())
  })
  site
}

next72 <- function() {
  from <- as.POSIXct("2026-09-29 07:00:00", tz = "UTC")
  list(from = from, to = from + 71 * 3600)
}

describe("problem 8: met_wide() serves one source/model, never a pooled mean", {
  it("defaults to Open-Meteo best_match and returns its values unpooled", {
    root <- withr::local_tempdir()
    site <- wide_archive(root)
    wide <- met_wide(site, next72(), now = prod_now())
    expect_equal(nrow(wide), 72L)
    expect_equal(as.numeric(diff(wide$time), units = "hours"), rep(1, 71))

    fc <- store_read_forecast(root, "kat", source = "openmeteo")
    om_t <- fc$value[fc$variable == "temperature_2m"][match(wide$time, fc$valid_time[fc$variable == "temperature_2m"])]
    expect_equal(wide$temperature_2m, om_t)
    prov <- met_provenance(wide)
    expect_true(all(prov$source[!is.na(prov$source)] == "openmeteo"))
  })

  it("serves BOM's hourly forecast when asked, with p50 rain as precipitation", {
    root <- withr::local_tempdir()
    site <- wide_archive(root)
    wide <- met_wide(site, next72(), source = "bom_forecast", model = "hourly", now = prod_now())
    fc <- store_read_forecast(root, "kat", source = "bom_forecast")
    h <- fc[fc$model == "hourly" & fc$variable == "temperature_2m", ]
    expect_equal(wide$temperature_2m, h$value[match(wide$time, h$valid_time)])
    p <- fc[fc$model == "hourly" & fc$variable == "precipitation" & fc$stat == "p50", ]
    expect_equal(wide$precipitation, p$value[match(wide$time, p$valid_time)])
    expect_true(all(met_provenance(wide)$source %in% c("bom_forecast", NA)))
  })

  it("reports the ensemble as its own source (member mean), not pooled with best_match", {
    root <- withr::local_tempdir()
    site <- wide_archive(root)
    wide <- met_wide(site, next72(), source = "om_ens", now = prod_now())
    det <- met_wide(site, next72(), now = prod_now())
    expect_false(isTRUE(all.equal(wide$temperature_2m, det$temperature_2m)))
  })

  it("rejects a source/model that is not in the archive", {
    root <- withr::local_tempdir()
    site <- wide_archive(root)
    expect_error(met_wide(site, next72(), source = "nope", now = prod_now()),
                 class = "meteoTidy_error_wide_source_unavailable")
  })
})

describe("problem 8: meteoHazard contract", {
  it("keeps gusts >= mean wind in every row", {
    root <- withr::local_tempdir()
    site <- wide_archive(root)
    for (spec in list(list(source = NULL, model = NULL),
                      list(source = "bom_forecast", model = "hourly"))) {
      wide <- met_wide(site, next72(), source = spec$source, model = spec$model, now = prod_now())
      ok <- !is.na(wide$wind_gusts_10m) & !is.na(wide$wind_speed_10m)
      expect_true(any(ok))
      expect_true(all(wide$wind_gusts_10m[ok] >= wide$wind_speed_10m[ok]))
    }
  })

  it("enforces gusts >= wind even when a model's gust is below its mean wind", {
    root <- withr::local_tempdir()
    site <- make_prod_site("kat", store_root = root)
    issue <- as.POSIXct("2026-09-29 06:00:00", tz = "UTC")
    valid <- issue + 3600 * 1:3
    mk <- function(v, x) tibble::tibble(
      site_id = "kat", source = "openmeteo", model = "best_match", issue_time = issue,
      valid_time = valid, lead_time = as.difftime(1:3, units = "hours"),
      member = NA_integer_, stat = NA_character_, variable = v, value = x
    )
    store_write_forecast(root, rbind(mk("wind_speed_10m", c(5, 6, 7)),
                                     mk("wind_gusts_10m", c(4, 9, 6.5))))
    wide <- met_wide(site, list(from = min(valid), to = max(valid)), now = prod_now())
    expect_equal(wide$wind_gusts_10m, c(5, 9, 7))
  })

  it("includes shortwave_radiation, derived as direct + diffuse when not archived", {
    root <- withr::local_tempdir()
    site <- make_prod_site("kat", store_root = root)
    issue <- as.POSIXct("2026-09-29 00:00:00", tz = "UTC")
    valid <- issue + 3600 * 1:2
    mk <- function(v, x) tibble::tibble(
      site_id = "kat", source = "openmeteo", model = "best_match", issue_time = issue,
      valid_time = valid, lead_time = as.difftime(1:2, units = "hours"),
      member = NA_integer_, stat = NA_character_, variable = v, value = x
    )
    store_write_forecast(root, rbind(mk("direct_radiation", c(300, 400)),
                                     mk("diffuse_radiation", c(100, 120))))
    wide <- met_wide(site, list(from = min(valid), to = max(valid)), now = prod_now())
    expect_true("shortwave_radiation" %in% names(wide))
    expect_equal(wide$shortwave_radiation, c(400, 520))
  })
})
