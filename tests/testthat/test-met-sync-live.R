# Plan 16 — met_sync_live() (hourly, best-effort). Everything mocked; frozen clock.
#
# Plan 17 item 1c: correction is no longer applied inside met_sync_live() (it
# is applied at SERVE time instead, R/correct-forecast.R), so `correct_apply`
# is not mocked/asserted here anymore -- see test-plan17-serve-correction.R's
# "met_sync_live no longer computes-and-discards corrections" spec for the
# positive assertion that it is never called.

describe("live window is QC'd, filled, and forecasts archived", {
  it("runs the live pipeline and advances the live watermark", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    now <- as.POSIXct("2026-01-01 12:00", tz = "UTC")
    local_frozen_clock(now)
    mock_acquisition()
    ran <- new.env()
    ran$qc <- 0L
    ran$fill <- 0L
    ran$arch <- 0L
    testthat::local_mocked_bindings(
      qc_run = function(...) {
        ran$qc <- ran$qc + 1L
        invisible()
      },
      fill_run = function(...) {
        ran$fill <- ran$fill + 1L
        invisible()
      },
      archive_forecasts = function(...) {
        ran$arch <- ran$arch + 1L
        tibble::tibble(note = "ok")
      }
    )
    status <- met_sync_live(site, now = now, config = pipeline_config(root))
    expect_equal(status$status, "ok")
    expect_true(ran$qc > 0 && ran$fill > 0 && ran$arch > 0)
    wm <- store_get_watermark(root, "test", "observations", "live")
    expect_false(is.na(wm))
  })

  it("never uses GHCNh for the live head (its ~1-week lag)", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    now <- as.POSIXct("2026-01-01 12:00", tz = "UTC")
    calls <- mock_acquisition()
    testthat::local_mocked_bindings(
      qc_run = function(...) invisible(), fill_run = function(...) invisible(),
      archive_forecasts = function(...) tibble::tibble(note = "ok")
    )
    met_sync_live(site,
      now = now,
      config = pipeline_config(root, obs_sources = c("site_aws", "bom_obs"))
    )
    expect_false("ghcnh" %in% calls$obs) # GHCNh excluded from the live head
  })
})

describe("graceful degradation on a dead channel", {
  it("marks the site degraded (not an error) and isolates other sites", {
    root <- local_store()
    s1 <- make_test_site(site_id = "site_1", store_root = root)
    s2 <- make_test_site(site_id = "site_2", store_root = root)
    now <- as.POSIXct("2026-01-01 12:00", tz = "UTC")
    # site_aws is dead; the verb must degrade, not crash
    mock_acquisition(fail_sources = "site_aws")
    testthat::local_mocked_bindings(
      qc_run = function(...) invisible(), fill_run = function(...) invisible(),
      archive_forecasts = function(...) tibble::tibble(note = "ok")
    )
    status <- met_sync_live(met_sites(list(s1, s2)),
      now = now,
      config = pipeline_config(root)
    )
    expect_equal(nrow(status), 2)
    expect_true(all(status$status %in% c("ok", "degraded")))
    expect_true(any(status$status == "degraded"))
  })
})
