# Regression (production review, problem 4): the BOM forecast adapter
#  - fetched a placeholder URL (https://reg.bom.gov.au/fwo/precis.xml, 404);
#  - parsed the FIRST <area> of the product, not the site's;
#  - stored only daily Tmax, mislabelled as hourly temperature_2m;
#  - ignored allow_web_api (so the hourly forecast was never archived).
# Replays responses recorded 2026-09-29: the NSW précis product IDN11060
# (trimmed to the Blue Mountains areas) and the web API daily/hourly
# forecasts for Katoomba (r64bhq) and Blaxland (r65050).

bom_routes <- function() {
  list(
    "reg\\.bom\\.gov\\.au/fwo/IDN11060\\.xml" = "bom/precis-IDN11060-trimmed.xml",
    "api\\.weather\\.bom\\.gov\\.au/v1/locations/r64bhq/forecasts/daily" = "bom/webapi-daily-r64bhq.json",
    "api\\.weather\\.bom\\.gov\\.au/v1/locations/r64bhq/forecasts/hourly" = "bom/webapi-hourly-r64bhq.json",
    "api\\.weather\\.bom\\.gov\\.au/v1/locations/r65050/forecasts/daily" = "bom/webapi-daily-r65050.json",
    "api\\.weather\\.bom\\.gov\\.au/v1/locations/r65050/forecasts/hourly" = "bom/webapi-hourly-r65050.json"
  )
}

bom_fc <- function(site, allow_web_api = TRUE, routes = bom_routes(), capture = new.env()) {
  adapter <- source_bom_forecast(store_root = site_store_root(site), allow_web_api = allow_web_api)
  with_routed_http(routes, {
    fetch_forecast(adapter, site, adapter@provides,
                   list(from = prod_now() - 3 * 86400, to = prod_now()), now = prod_now())
  }, capture = capture)
}

describe("problem 4: BOM daily forecast from the official précis product", {
  it("fetches the state's précis product and reads the site's own area", {
    site <- make_prod_site("kat")
    site <- site_set_resolved(site, c("bom", "aac"), "NSW_PT072")
    cap <- new.env()
    fc <- bom_fc(site, allow_web_api = FALSE, capture = cap)
    expect_true(any(grepl("IDN11060.xml", cap$urls, fixed = TRUE)))
    daily <- fc[fc$model == "daily", ]
    # Katoomba's 30 Sep forecast in the recorded product: min 8, max 22.
    d <- daily[daily$valid_time == as.POSIXct("2026-09-29 14:00:00", tz = "UTC"), ]
    expect_equal(d$value[d$variable == "temperature_2m_min"], 8)
    expect_equal(d$value[d$variable == "temperature_2m_max"], 22)
    expect_equal(fc$issue_time[1], as.POSIXct("2026-09-29 06:00:00", tz = "UTC"))
    expect_false("temperature_2m" %in% daily$variable) # no more mislabelling
    expect_gte(length(unique(daily$valid_time)), 7)
  })

  it("maps the précis rain range to p50/p75 precipitation_sum", {
    site <- site_set_resolved(make_prod_site("kat"), c("bom", "aac"), "NSW_PT072")
    fc <- bom_fc(site, allow_web_api = FALSE)
    # 2 Oct (local): "0 to 8 mm", 80 %
    d <- fc[fc$valid_time == as.POSIXct("2026-10-01 14:00:00", tz = "UTC"), ]
    expect_equal(d$value[d$variable == "precipitation_sum" & d$stat == "p50"], 0)
    expect_equal(d$value[d$variable == "precipitation_sum" & d$stat == "p75"], 8)
    expect_equal(d$value[d$variable == "precipitation_probability_max"], 80)
  })

  it("serves different values for different sites' areas", {
    kat <- bom_fc(site_set_resolved(make_prod_site("kat"), c("bom", "aac"), "NSW_PT072"),
                  allow_web_api = FALSE)
    spr <- bom_fc(site_set_resolved(make_prod_site("blax"), c("bom", "aac"), "NSW_PT129"),
                  allow_web_api = FALSE)
    tmax <- function(fc) fc$value[fc$variable == "temperature_2m_max"]
    expect_false(identical(tmax(kat), tmax(spr)))
  })
})

describe("problem 4: BOM web API (allow_web_api = TRUE) daily + hourly", {
  it("archives daily and hourly forecasts for the site's geohash", {
    site <- make_prod_site("kat")
    cap <- new.env()
    fc <- bom_fc(site, capture = cap)
    expect_true(any(grepl("/r64bhq/forecasts/daily", cap$urls)))
    expect_true(any(grepl("/r64bhq/forecasts/hourly", cap$urls)))
    expect_setequal(unique(fc$model), c("daily", "hourly"))

    hourly <- fc[fc$model == "hourly", ]
    expect_true(all(c("temperature_2m", "relative_humidity_2m", "wind_speed_10m",
                      "wind_gusts_10m", "wind_direction_10m", "uv_index",
                      "precipitation_probability") %in% hourly$variable))
    expect_setequal(unique(hourly$stat[hourly$variable == "precipitation"]),
                    c("p90", "p75", "p50"))
    expect_equal(unique(hourly$issue_time), as.POSIXct("2026-09-29 05:23:37", tz = "UTC"))
    expect_gte(length(unique(hourly$valid_time)), 72)

    # km/h converted to m/s (compare against the recorded first hour).
    raw <- jsonlite::read_json(fixture_path("bom/webapi-hourly-r64bhq.json"))$data[[1]]
    first <- hourly[hourly$valid_time == min(hourly$valid_time), ]
    ws <- first$value[first$variable == "wind_speed_10m"]
    expect_equal(ws, raw$wind$speed_kilometre / 3.6, tolerance = 1e-9)
    expect_true(all(first$value[first$variable == "wind_gusts_10m"] >= ws))

    daily <- fc[fc$model == "daily", ]
    expect_gte(length(unique(daily$valid_time)), 7)
    expect_true(all(c("temperature_2m_max", "precipitation_probability_max",
                      "uv_index_max") %in% daily$variable))
    expect_setequal(unique(daily$stat[daily$variable == "precipitation_sum"]),
                    c("p25", "p50", "p75"))
  })

  it("carries précis text, extended text and fire danger as forecast_aux", {
    fc <- bom_fc(make_prod_site("kat"))
    aux <- attr(fc, "aux")
    expect_true(all(c("precis", "forecast", "fire_danger") %in% aux$field))
  })

  it("does not fetch the hourly forecast when the web API is off", {
    site <- site_set_resolved(make_prod_site("kat"), c("bom", "aac"), "NSW_PT072")
    cap <- new.env()
    fc <- bom_fc(site, allow_web_api = FALSE, capture = cap)
    expect_false(any(grepl("api.weather.bom.gov.au", cap$urls, fixed = TRUE)))
    expect_equal(unique(fc$model), "daily")
  })

  it("serves each site's own location (Blaxland differs from Katoomba)", {
    kat <- bom_fc(make_prod_site("kat"))
    blax <- bom_fc(make_prod_site("blax"))
    t_kat <- kat$value[kat$model == "hourly" & kat$variable == "temperature_2m"]
    t_blax <- blax$value[blax$model == "hourly" & blax$variable == "temperature_2m"]
    expect_false(identical(t_kat, t_blax))
  })

  it("truncates a 7-character geohash to the 6 the API accepts", {
    site <- site_set_resolved(make_prod_site("kat"), c("bom", "geohash"), "r64bhqg")
    cap <- new.env()
    bom_fc(site, capture = cap)
    expect_true(all(grepl("/r64bhq/", grep("api.weather", cap$urls, value = TRUE))))
  })

  it("archives through archive_forecasts(), forecast_aux included, idempotently", {
    root <- withr::local_tempdir()
    site <- make_prod_site("kat", store_root = root, sources = list(
      bom_forecast = list(adapter = "bom_forecast", allow_web_api = TRUE, store_root = root)
    ))
    with_routed_http(bom_routes(), {
      archive_forecasts(root, site, "bom_forecast", now = prod_now())
      n1 <- nrow(store_read_forecast(root, "kat"))
      archive_forecasts(root, site, "bom_forecast", now = prod_now() + 3600.5)
    })
    expect_equal(nrow(store_read_forecast(root, "kat")), n1)
    aux <- store_read_forecast_aux(root, "kat")
    expect_true("fire_danger" %in% aux$field)
  })
})
