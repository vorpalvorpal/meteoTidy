# Plan 16 — archive_forecasts(): archive-on-every-sync with dedup.

describe("dedup on (source, model, issue_time)", {
  it("archives new issuances and is a no-op on re-running the same issuances", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    calls <- mock_acquisition(forecast = new_forecast(make_forecast(n = 3)))
    now <- as.POSIXct("2026-01-01", tz = "UTC")

    archive_forecasts(root, site, sources = "openmeteo", now = now)
    n1 <- nrow(store_read_forecast(root, "test"))
    archive_forecasts(root, site, sources = "openmeteo", now = now)
    n2 <- nrow(store_read_forecast(root, "test"))

    expect_equal(n1, 3)
    expect_equal(n2, n1)          # identical issuance dedups → no growth
  })
})

describe("gap semantics per source", {
  it("notes a missed BOM issuance as non-backfillable", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    mock_acquisition()
    summary <- archive_forecasts(root, site, sources = "bom_forecast",
                                 now = as.POSIXct("2026-01-01", tz = "UTC"),
                                 missed = TRUE)
    expect_true(any(grepl("cannot be backfilled|non-backfillable",
                          summary$note, ignore.case = TRUE)))
  })

  it("flags a missed Open-Meteo issuance for Previous/Single-Runs self-heal", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    mock_acquisition()
    summary <- archive_forecasts(root, site, sources = "openmeteo",
                                 now = as.POSIXct("2026-01-01", tz = "UTC"),
                                 missed = TRUE)
    expect_true(any(grepl("self-heal|previous", summary$note, ignore.case = TRUE)))
  })
})
