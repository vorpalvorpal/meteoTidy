# Follow-up review, item 3: the BOM daily forecast preferred the town précis.
#
# For Blaxland the précis area is Springwood (NSW_PT129): a different place,
# with no 25 % rain amount (p75/p25 split), no fire danger, and an evening
# issue drops today's maximum. BOM's web API serves a daily forecast for the
# site's own 6-character geohash, so it must be the first choice; the précis
# is only a fallback, and its rows are labelled as the précis area's.
# Replays the NSW précis product and Blaxland's web-API forecasts recorded
# on 2026-09-29.

fu03_routes <- function(webapi_down = FALSE) {
  c(
    if (webapi_down) list("api\\.weather\\.bom\\.gov\\.au" = 500L),
    list(
      "reg\\.bom\\.gov\\.au/fwo/IDN11060\\.xml" = "bom/precis-IDN11060-trimmed.xml",
      "api\\.weather\\.bom\\.gov\\.au/v1/locations/r65050/forecasts/daily" = "bom/webapi-daily-r65050.json",
      "api\\.weather\\.bom\\.gov\\.au/v1/locations/r65050/forecasts/hourly" = "bom/webapi-hourly-r65050.json"
    )
  )
}

fu03_site <- function() {
  site_set_resolved(make_prod_site("blax"), c("bom", "aac"), "NSW_PT129")
}

fu03_fetch <- function(site, webapi_down = FALSE, capture = new.env()) {
  adapter <- source_bom_forecast(store_root = site_store_root(site), allow_web_api = TRUE)
  with_routed_http(fu03_routes(webapi_down), {
    fc <- suppressWarnings(fetch_forecast(adapter, site, adapter@provides,
                                          list(from = prod_now() - 86400, to = prod_now()),
                                          now = prod_now()))
  }, capture = capture)
  fc
}

describe("item 3: BOM daily forecast source order", {
  it("uses the site's geohash web-API daily forecast, not the town précis", {
    cap <- new.env()
    fc <- fu03_fetch(fu03_site(), capture = cap)
    expect_true(any(grepl("r65050/forecasts/daily", cap$urls)))
    expect_false(any(grepl("IDN11060", cap$urls)))

    daily <- fc[fc$model == "daily", ]
    expect_equal(unique(daily$issue_time), as.POSIXct("2026-09-29 06:00:13", tz = "UTC"))
    # What only the web API has: the 75 %-chance amount (stat p25) ...
    expect_true(any(daily$variable == "precipitation_sum" & daily$stat %in% "p25"))
    # ... and today's maximum (23 degC for 29 Sep; the précis dropped it).
    today <- as.POSIXct("2026-09-28 14:00:00", tz = "UTC")
    expect_equal(daily$value[daily$variable == "temperature_2m_max" & daily$valid_time == today], 23)
    expect_gte(length(unique(daily$valid_time)), 7)

    aux <- attr(fc, "aux")
    loc <- unique(aux$value_text[aux$field == "location"])
    expect_match(loc, "r65050")
    expect_false(any(grepl("Springwood", aux$value_text)))
  })

  it("falls back to the précis, labelled with the précis area, when the web API fails", {
    cap <- new.env()
    fc <- fu03_fetch(fu03_site(), webapi_down = TRUE, capture = cap)
    expect_true(any(grepl("IDN11060", cap$urls)))
    expect_false("daily" %in% fc$model)
    pre <- fc[fc$model == "daily_precis", ]
    expect_gt(nrow(pre), 0)
    expect_equal(unique(pre$issue_time), as.POSIXct("2026-09-29 06:00:00", tz = "UTC"))
    aux <- attr(fc, "aux")
    expect_match(unique(aux$value_text[aux$field == "location"]), "Springwood.*NSW_PT129")
  })

  it("still uses the précis when the web API is not allowed", {
    site <- fu03_site()
    adapter <- source_bom_forecast(store_root = site_store_root(site), allow_web_api = FALSE)
    fc <- with_routed_http(fu03_routes(), fetch_forecast(
      adapter, site, adapter@provides, list(from = prod_now() - 86400, to = prod_now()),
      now = prod_now()
    ))
    expect_true(all(fc$model == "daily_precis"))
  })
})
