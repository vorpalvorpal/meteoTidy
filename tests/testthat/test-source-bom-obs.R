# Plan 07 — source_bom_obs(): 72-h JSON parse + transport provenance + fallback.

describe("72-h obs JSON → canonical obs", {
  it("parses the rolling 72-h station JSON with transport == ftp_feeds", {
    site <- make_test_site()
    site <- site_set_resolved(site, c("bom", "product"), "IDN60901")
    adapter <- source_bom_obs(store_root = local_store())
    win <- list(from = as.POSIXct("2025-12-31", tz = "UTC"),
                to = as.POSIXct("2026-01-02", tz = "UTC"))
    json <- read_bom_json("obs-72h-sample.json")
    # FTP mirror serves the JSON; parse converts km/h wind to m/s
    testthat::local_mocked_bindings(
      .ftp_get = function(url, ...) jsonlite::toJSON(json, auto_unbox = TRUE)
    )
    out <- fetch(adapter, site, c("temperature_2m", "wind_speed_10m"), win)
    expect_canonical_obs(out)
    expect_true(all(out$transport == "ftp_feeds"))
    expect_true(all(out$method == "measured"))
    wind <- out$value[out$variable == "wind_speed_10m"]
    expect_true(all(wind < 5))            # 10-12 km/h → ~3 m/s, never > 5
  })
})

describe("web-API fallback (opt-in) when FTP is gone", {
  it("serves obs from the web API and records the fallback transport", {
    site <- make_test_site()
    site <- site_set_resolved(site, c("bom", "geohash"), "r3gx2f")
    adapter <- source_bom_obs(store_root = local_store(), allow_web_api = TRUE)
    win <- list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                to = as.POSIXct("2026-01-02", tz = "UTC"))
    # FTP rung is gone; the web-API rung returns the webapi fixture
    testthat::local_mocked_bindings(
      .ftp_get = function(url, ...) abort_meteo("gone", class = "http_gone")
    )
    body <- read_bom_json("webapi-obs-sample.json")
    with_mocked_http(body, {
      out <- fetch(adapter, site, "temperature_2m", win)
    })
    expect_canonical_obs(out)
    expect_true(all(out$transport == "web_api"))
  })
})
