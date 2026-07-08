# keyless requests (free host, non-commercial) / emits the non-commercial notice exactly once (snapshot)

    Code
      adapter <- source_openmeteo(product = "forecast")
      with_mocked_http(read_om_fixture("forecast.json"), {
        invisible(fetch_forecast(adapter, make_test_site(), "temperature_2m", list(
          from = as.POSIXct("2026-01-01", tz = "UTC"), to = as.POSIXct("2026-01-02",
            tz = "UTC")), now = om_now()))
      })
    Message
      Using the Open-Meteo free tier.
      i Free-tier data is licensed for non-commercial use only (< 10,000 calls/day).

