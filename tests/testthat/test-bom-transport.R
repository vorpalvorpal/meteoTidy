# Plan 07 — the BOM transport ladder (ordered rungs + failover + provenance).
# All rung fetch_fns are fakes; no live calls.

describe("ladder_fetch() rung ordering", {
  it("stops at the first successful rung and never calls later rungs", {
    counter <- new.env()
    ladder <- list(
      fake_transport("ftp_feeds", "obs_72h", bom_rows(), counter),
      fake_transport("web_api",   "obs_72h", bom_rows(), counter),
      fake_transport("gateway",   "obs_72h", bom_rows(), counter)
    )
    breaker <- breaker_read(local_store())
    out <- ladder_fetch(ladder, bom_request(), breaker,
                        now = as.POSIXct("2026-01-01", tz = "UTC"))
    expect_equal(counter$ftp_feeds, 1L)
    expect_equal(counter$web_api %||% 0L, 0L)   # never reached
    expect_equal(counter$gateway %||% 0L, 0L)
    # provenance records which transport served the rows
    expect_true(all(out$transport == "ftp_feeds"))
  })

  it("falls through http_gone to the next rung and stamps the fallback transport", {
    counter <- new.env()
    ladder <- list(
      fake_transport("ftp_feeds", "obs_72h", "gone", counter),
      fake_transport("web_api",   "obs_72h", bom_rows(), counter)
    )
    breaker <- breaker_read(local_store())
    out <- ladder_fetch(ladder, bom_request(), breaker,
                        now = as.POSIXct("2026-01-01", tz = "UTC"))
    expect_equal(counter$ftp_feeds, 1L)
    expect_equal(counter$web_api, 1L)
    expect_true(all(out$transport == "web_api"))   # the fallback rung
  })

  it("aborts bom_all_transports_failed when every applicable rung fails", {
    counter <- new.env()
    ladder <- list(
      fake_transport("ftp_feeds", "obs_72h", "gone", counter),
      fake_transport("web_api",   "obs_72h", "gone", counter)
    )
    breaker <- breaker_read(local_store())
    expect_error(
      ladder_fetch(ladder, bom_request(), breaker,
                   now = as.POSIXct("2026-01-01", tz = "UTC")),
      class = "meteoTidy_error_bom_all_transports_failed"
    )
  })
})

describe("opt-in web/gateway rungs", {
  it("makes an hourly-forecast request only web/gateway can serve abort with guidance", {
    # allow_web_api = FALSE removes rungs 2/3; only they serve hourly forecasts.
    counter <- new.env()
    ladder <- list(
      fake_transport("ftp_feeds", "precis_daily", bom_rows(), counter)
      # no web_api / gateway rung available
    )
    breaker <- breaker_read(local_store())
    req <- bom_request(product = "forecast_hourly")
    expect_error(
      ladder_fetch(ladder, req, breaker, now = as.POSIXct("2026-01-01", tz = "UTC")),
      class = "meteoTidy_error_bom_all_transports_failed"
    )
  })
})

describe("source substitution rung (quality fallback, not compliance)", {
  it("serves a sub-daily request from Open-Meteo flagged as a different source", {
    counter <- new.env()
    om_rows <- new_obs(make_obs(n = 2, source = "openmeteo", method = "model_fill"))
    ladder <- list(
      fake_transport("ftp_feeds", "precis_daily", "gone", counter),
      fake_transport("substitute", c("obs_72h", "forecast_hourly"), om_rows, counter)
    )
    breaker <- breaker_read(local_store())
    out <- ladder_fetch(ladder, bom_request(product = "obs_72h"), breaker,
                        now = as.POSIXct("2026-01-01", tz = "UTC"))
    expect_true(all(out$transport == "substitute"))
    expect_true(all(out$source == "openmeteo"))    # a DIFFERENT source, flagged
  })
})
