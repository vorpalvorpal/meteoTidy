# Plan 07 — source_bom_forecast(): précis parse, model == NA, forecast_aux,
# geohash resolution. Mocked FTP/HTTP seams; no live calls.

describe("précis XML → canonical forecast", {
  it("parses the précis to canonical forecast rows with model == NA", {
    site <- make_test_site()
    site <- site_set_resolved(site, c("bom", "geohash"), "r3gx2f")
    adapter <- source_bom_forecast(store_root = local_store())
    win <- list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                to = as.POSIXct("2026-01-08", tz = "UTC"))
    xml <- as.character(read_bom_xml("ftp-precis-sample.xml"))
    fake_ftp <- function(url, ...) xml
    testthat::local_mocked_bindings(.ftp_get = fake_ftp)
    out <- fetch_forecast(adapter, site, "temperature_2m", win)
    expect_canonical_forecast(out)
    # the edited BOM product has no model name
    expect_true(all(is.na(out$model)))
  })

  it("populates forecast_aux with précis text and fire-danger/UV categories verbatim", {
    site <- make_test_site()
    site <- site_set_resolved(site, c("bom", "geohash"), "r3gx2f")
    adapter <- source_bom_forecast(store_root = local_store())
    win <- list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                to = as.POSIXct("2026-01-08", tz = "UTC"))
    xml <- as.character(read_bom_xml("ftp-precis-sample.xml"))
    testthat::local_mocked_bindings(.ftp_get = function(url, ...) xml)
    aux <- fetch_forecast_aux(adapter, site, win)
    expect_canonical_forecast_aux(aux)
    expect_true(any(aux$field == "precis" & aux$value_text == "Partly cloudy."))
    expect_true(any(aux$field == "fire_danger" & aux$value_text == "High"))
    expect_true(any(grepl("Extreme", aux$value_text[aux$field == "uv_alert"])))
  })
})

describe("geohash resolution and the disabled-web-api guard", {
  it("caches the geohash from a web-API search when opt-in is on", {
    site <- make_test_site()
    adapter <- source_bom_forecast(store_root = local_store(),
                                   allow_web_api = TRUE)
    body <- read_bom_json("webapi-geohash-sample.json")
    with_mocked_http(body, {
      resolved <- resolve_station(adapter, site)
    })
    expect_equal(site_resolved(resolved, c("bom", "geohash")), "r3gx2f")
  })

  it("aborts bom_geohash_unavailable with web API off and no cached geohash", {
    site <- make_test_site()               # no cached geohash
    adapter <- source_bom_forecast(store_root = local_store(),
                                   allow_web_api = FALSE)
    expect_error(
      resolve_station(adapter, site),
      class = "meteoTidy_error_bom_geohash_unavailable"
    )
  })
})
