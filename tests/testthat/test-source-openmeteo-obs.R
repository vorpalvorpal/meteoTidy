# Plan 05 — Open-Meteo observation products (Historical Weather / ERA5).
# All mocked through the Plan 04 `.http_get()` seam; no live calls.

describe("source_openmeteo(product = 'historical') parsing", {
  it("maps an ERA5 hourly block to canonical obs marked model_fill", {
    out <- om_fetch("historical", read_om_fixture("historical-era5.json"))
    expect_canonical_obs(out)
    # ERA5 is reanalysis, not a site measurement — it must be honestly flagged.
    era5 <- out[out$variable == "temperature_2m", ]
    expect_true(all(era5$method == "model_fill"))
    expect_true(all(out$qc_flag == "ok"))
    expect_true(all(out$source == "openmeteo"))
  })

  it("converts a km/h wind field to canonical m/s (the §3.1 footgun)", {
    # The fixture serves wind_speed_10m in km/h; both the explicit unit request
    # AND the belt-and-braces to_canonical() check must yield m/s.
    out <- om_fetch("historical", read_om_fixture("historical-era5.json"),
                    variables = c("temperature_2m", "wind_speed_10m"))
    wind <- out$value[out$variable == "wind_speed_10m"]
    expect_equal(wind[1], 10 / 3.6, tolerance = 1e-6)
    expect_true(all(wind < 5))            # 8-12 km/h is ~2-3 m/s, never > 5
  })

  it("stamps UTC times parsed from the naive Open-Meteo timestamps", {
    out <- om_fetch("historical", read_om_fixture("historical-era5.json"))
    expect_identical(attr(out$datetime_utc, "tzone"), "UTC")
    expect_equal(out$datetime_utc[1], as.POSIXct("2020-01-01 00:00", tz = "UTC"))
  })
})

describe("dictionary-name parity (guards a silent rename break)", {
  it("requests Open-Meteo variables under their dictionary names", {
    # §3.1 is Open-Meteo-named, so the request parameter names must equal the
    # dictionary variable names verbatim; a future dictionary rename that broke
    # this would silently request the wrong field.
    cap <- new.env()
    om_fetch("historical", read_om_fixture("historical-era5.json"),
             variables = c("temperature_2m", "wind_speed_10m"), capture = cap)
    expect_match(cap$query$hourly %||% cap$url, "temperature_2m")
    expect_match(cap$query$hourly %||% cap$url, "wind_speed_10m")
    # and the names it asked for are genuine dictionary variables
    expect_no_error(met_variable("temperature_2m"))
    expect_no_error(met_variable("wind_speed_10m"))
  })

  it("always requests canonical units explicitly (wind_speed_unit=ms)", {
    # Must use the current spelling `wind_speed_unit`, not the legacy
    # `windspeed_unit` a rename could silently drop (leaving km/h default).
    cap <- new.env()
    om_fetch("historical", read_om_fixture("historical-era5.json"),
             variables = "wind_speed_10m", capture = cap)
    q <- paste(unlist(cap$query), cap$url, collapse = "&")
    expect_match(q, "wind_speed_unit")
    expect_match(q, "ms")
    expect_false(grepl("windspeed_unit", q))   # legacy spelling must be absent
  })
})
