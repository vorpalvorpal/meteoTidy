# Regression (production review, problem 5): BOM observations were broken.
#  - the product-feed URL had no station in it (404);
#  - the web-API parser expected data[] rows, but the API returns ONE object
#    with km/h wind and compass directions;
#  - a 7-character geohash gives HTTP 400.
# Replays the rolling 72-h JSON recorded 2026-09-29 for Mount Boyce (94743,
# near Katoomba) and Penrith (94763, near Blaxland), and the web-API current
# observation for both geohashes.

obs_routes <- function() {
  list(
    "reg\\.bom\\.gov\\.au/fwo/IDN60901/IDN60901\\.94743\\.json" = "bom/obs72h-IDN60901-94743.json",
    "reg\\.bom\\.gov\\.au/fwo/IDN60901/IDN60901\\.94763\\.json" = "bom/obs72h-IDN60901-94763.json",
    "api\\.weather\\.bom\\.gov\\.au/v1/locations/r64bhq/observations" = "bom/webapi-obs-r64bhq.json",
    "api\\.weather\\.bom\\.gov\\.au/v1/locations/r65050/observations" = "bom/webapi-obs-r65050.json",
    "reg\\.bom\\.gov\\.au" = 404L,
    "api\\.weather\\.bom\\.gov\\.au" = 400L
  )
}

obs_win <- function() list(from = prod_now() - 6 * 3600, to = prod_now())

describe("problem 5: BOM 72-h station observations", {
  it("fetches the configured station's JSON and parses every field", {
    site <- site_set_resolved(make_prod_site("kat"), c("bom", "wmo"), "94743")
    adapter <- source_bom_obs(store_root = site_store_root(site))
    cap <- new.env()
    out <- with_routed_http(obs_routes(), {
      fetch(adapter, site, adapter@provides, obs_win(), now = prod_now())
    }, capture = cap)
    expect_true(any(grepl("IDN60901/IDN60901.94743.json", cap$urls, fixed = TRUE)))
    expect_true(all(out$transport == "ftp_feeds"))
    expect_true(all(c("temperature_2m", "wind_speed_10m", "wind_gusts_10m",
                      "wind_direction_10m", "relative_humidity_2m") %in% out$variable))
    raw <- jsonlite::read_json(fixture_path("bom/obs72h-IDN60901-94743.json"))$observations$data[[1]]
    t0 <- as.POSIXct(raw$aifstime_utc, format = "%Y%m%d%H%M%S", tz = "UTC")
    row <- out[out$datetime_utc == t0, ]
    expect_equal(row$value[row$variable == "temperature_2m"], raw$air_temp)
    expect_equal(row$value[row$variable == "wind_gusts_10m"], raw$gust_kmh / 3.6, tolerance = 1e-9)
  })

  it("serves different stations for different sites", {
    kat <- site_set_resolved(make_prod_site("kat"), c("bom", "wmo"), "94743")
    blax <- site_set_resolved(make_prod_site("blax"), c("bom", "wmo"), "94763")
    get <- function(site) {
      with_routed_http(obs_routes(), {
        fetch(source_bom_obs(store_root = site_store_root(site)), site, "temperature_2m",
              obs_win(), now = prod_now())
      })
    }
    expect_false(identical(get(kat)$value, get(blax)$value))
  })
})

describe("problem 5: BOM web-API observation fallback", {
  it("parses the single-object response with km/h wind and a compass direction", {
    site <- make_prod_site("kat") # no WMO configured: product feed rung is skipped
    adapter <- source_bom_obs(store_root = site_store_root(site), allow_web_api = TRUE)
    out <- with_routed_http(obs_routes(), {
      fetch(adapter, site, adapter@provides, obs_win(), now = prod_now())
    })
    raw <- jsonlite::read_json(fixture_path("bom/webapi-obs-r64bhq.json"))
    expect_true(all(out$transport == "web_api"))
    expect_equal(unique(out$datetime_utc),
                 as.POSIXct(sub("Z$", "", raw$metadata$observation_time),
                            format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
    expect_equal(out$value[out$variable == "temperature_2m"], raw$data$temp)
    expect_equal(out$value[out$variable == "wind_speed_10m"],
                 raw$data$wind$speed_kilometre / 3.6, tolerance = 1e-9)
    expect_equal(out$value[out$variable == "wind_direction_10m"],
                 compass2angle(raw$data$wind$direction))
    expect_equal(out$value[out$variable == "wind_gusts_10m"],
                 raw$data$gust$speed_kilometre / 3.6, tolerance = 1e-9)
  })

  it("requests the 6-character geohash even when 7 are configured", {
    site <- site_set_resolved(make_prod_site("kat"), c("bom", "geohash"), "r64bhqg")
    adapter <- source_bom_obs(store_root = site_store_root(site), allow_web_api = TRUE)
    cap <- new.env()
    out <- with_routed_http(obs_routes(), {
      fetch(adapter, site, "temperature_2m", obs_win(), now = prod_now())
    }, capture = cap)
    expect_gt(nrow(out), 0)
    expect_true(any(grepl("/r64bhq/observations", cap$urls, fixed = TRUE)))
  })

  it("names every rung's failure when all transports fail", {
    site <- make_prod_site("kat")
    adapter <- source_bom_obs(store_root = site_store_root(site), allow_web_api = FALSE)
    expect_error(
      with_routed_http(obs_routes(), {
        fetch(adapter, site, "temperature_2m", obs_win(), now = prod_now())
      }),
      "WMO",
      class = "meteoTidy_error_bom_all_transports_failed"
    )
  })
})
