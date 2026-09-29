# Follow-up review, item 11: lead_time read-back still failed intermittently.
#
# A live sync on a fresh store, then met_forecast_archive(kat, source =
# "bom_forecast"), aborted "lead_time inconsistent": 8.51361 h expected,
# 8.51333 h stored -- exactly one second short. lead_time was held as a
# difftime in HOURS; arrow writes it as duration[s] by multiplying by 3600
# and truncating, so e.g. 30649 s = 8.513611... h came back as 30648 s.
# Times with a fractional second (a wall-clock issue time) made it worse.
# Real parquet round-trips of the recorded BOM hourly forecast.

fu11_bom_hourly <- function(site_id = "kat", geohash = "r64bhq") {
  body <- jsonlite::read_json(fixture_path("bom", sprintf("webapi-hourly-%s.json", geohash)),
                              simplifyVector = FALSE)
  bom_parse_webapi_hourly(body, site_id)
}

expect_leads_exact <- function(fc) {
  expected <- as.numeric(difftime(fc$valid_time, fc$issue_time, units = "secs"))
  expect_equal(as.numeric(fc$lead_time, units = "secs"), expected)
}

describe("item 11: lead_time survives a parquet round trip", {
  it("reads back every lead of a recorded BOM hourly issuance exactly", {
    root <- withr::local_tempdir()
    fc <- new_forecast(fu11_bom_hourly())
    store_write_forecast(root, fc)
    back <- store_read_forecast(root, "kat", source = "bom_forecast")
    expect_equal(nrow(back), nrow(fc))
    expect_leads_exact(back)
  })

  it("reads back leads whose hour fraction does not survive duration[s] (e.g. 30649 s)", {
    root <- withr::local_tempdir()
    fc <- fu11_bom_hourly()
    # The live failure: a lead of 8.51361 h (30649 s) came back 8.51333 h.
    # Re-issue the recorded forecast 60 times, one second apart, ending with
    # the first step 30649 s after issue, so every seconds offset is covered.
    first <- min(fc$valid_time)
    shifted <- lapply(0:59, function(k) {
      x <- fc
      x$issue_time <- first - 30649 - k
      x$lead_time <- difftime(x$valid_time, x$issue_time, units = "hours")
      x
    })
    fc <- new_forecast(vctrs::vec_rbind(!!!shifted))
    store_write_forecast(root, fc)
    back <- met_forecast_archive(make_prod_site("kat", store_root = root), source = "bom_forecast")
    expect_equal(nrow(back), nrow(fc))
    expect_leads_exact(back)
  })

  it("stores whole-second times even when the issue time has a fraction", {
    root <- withr::local_tempdir()
    fc <- fu11_bom_hourly()
    # A wall-clock-stamped issuance, e.g. 05:23:37.482913.
    fc$issue_time <- fc$issue_time + 0.482913
    fc$lead_time <- difftime(fc$valid_time, fc$issue_time, units = "hours")
    store_write_forecast(root, new_forecast(fc))
    raw <- arrow::read_parquet(list.files(file.path(root, "forecasts"), "\\.parquet$",
                                          recursive = TRUE, full.names = TRUE))
    expect_equal(as.numeric(raw$issue_time) %% 1, rep(0, nrow(raw)))
    back <- store_read_forecast(root, "kat")
    expect_leads_exact(back)
    # Re-archiving the same issuance is still a no-op.
    store_write_forecast(root, new_forecast(fc))
    expect_equal(nrow(store_read_forecast(root, "kat")), nrow(fc))
  })

  it("met_forecast_archive() reads a synced BOM archive for each source", {
    root <- withr::local_tempdir()
    site <- make_prod_site("kat", store_root = root, sources = list(
      bom_forecast = list(adapter = "bom_forecast", allow_web_api = TRUE)
    ))
    routes <- list(
      "r64bhq/forecasts/daily" = "bom/webapi-daily-r64bhq.json",
      "r64bhq/forecasts/hourly" = "bom/webapi-hourly-r64bhq.json"
    )
    cfg <- list(store_root = root, obs_sources = character(0), forecast_sources = "bom_forecast")
    status <- with_routed_http(routes, met_sync_live(site, now = prod_now(), config = cfg))
    expect_equal(status$status, "ok")
    arch <- met_forecast_archive(site, source = "bom_forecast")
    expect_gt(nrow(arch), 0)
    expect_leads_exact(arch)
  })

  it("repairs a one-second truncation left in stores written before the fix", {
    root <- withr::local_tempdir()
    fc <- new_forecast(fu11_bom_hourly())
    old <- fc
    old$lead_time <- as.difftime(as.numeric(old$lead_time, units = "secs") - 1, units = "secs")
    dir <- dataset_path(root, "forecasts", list(source = "bom_forecast", site_id = "kat",
                                                issue_date = .forecast_issue_date(fc$issue_time[1])))
    dir.create(dir, recursive = TRUE)
    arrow::write_parquet(old, file.path(dir, "part-legacy.parquet"))
    back <- store_read_forecast(root, "kat")
    expect_leads_exact(back)
  })
})
