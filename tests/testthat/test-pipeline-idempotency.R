# Plan 16 — the cross-cutting idempotency + multi-site isolation guarantees.

# Common mocks so each verb sequences real store writes but fake science.
local_pipeline_mocks <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    qc_run = function(...) invisible(),
    fill_run = function(...) invisible(),
    # nolint next: object_usage_linter. sibling helper
    correct_apply = function(...) make_obs(n = 1),
    # nolint next: object_usage_linter. sibling helper
    correct_refit = function(...) invisible(skill_verdict(promote = FALSE)),
    verify_run = function(...) invisible(),
    build_history_hourly = function(...) make_obs(n = 1),
    build_history_daily = function(...) make_obs(n = 1),
    .env = env
  )
}

describe("each verb is idempotent over the same inputs and clock", {
  it("met_sync_daily twice yields identical store contents and watermarks", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    now <- as.POSIXct("2026-01-02", tz = "UTC")
    mock_acquisition()
    local_pipeline_mocks()

    met_sync_daily(site, now = now, config = pipeline_config(root))
    rows1 <- nrow(store_read_forecast(root, "test"))
    wm1 <- store_get_watermark(root, "test", "forecasts", "openmeteo")

    met_sync_daily(site, now = now, config = pipeline_config(root))
    rows2 <- nrow(store_read_forecast(root, "test"))
    wm2 <- store_get_watermark(root, "test", "forecasts", "openmeteo")

    expect_equal(rows2, rows1) # no duplicate rows
    expect_equal(wm2, wm1) # watermark not double-advanced
  })

  it("met_sync_live twice yields identical observation contents", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    now <- as.POSIXct("2026-01-01 12:00", tz = "UTC")
    mock_acquisition()
    local_pipeline_mocks()
    testthat::local_mocked_bindings(archive_forecasts = function(...) {
      tibble::tibble(note = "ok")
    })

    met_sync_live(site, now = now, config = pipeline_config(root))
    n1 <- nrow(store_read_obs(root, "test", include_superseded = TRUE))
    met_sync_live(site, now = now, config = pipeline_config(root))
    n2 <- nrow(store_read_obs(root, "test", include_superseded = TRUE))
    expect_equal(n2, n1)
  })
})

describe("multi-site processing with per-site isolation", {
  it("processes both sites; one failing leaves the other complete", {
    root <- local_store()
    s1 <- make_test_site(site_id = "site_1", store_root = root)
    s2 <- make_test_site(site_id = "site_2", store_root = root)
    now <- as.POSIXct("2026-01-02", tz = "UTC")
    local_pipeline_mocks()
    testthat::local_mocked_bindings(archive_forecasts = function(...) {
      tibble::tibble(note = "ok")
    })
    # site_1's obs source is dead; site_2 is healthy
    calls <- new.env()
    testthat::local_mocked_bindings(
      .acquire_obs = function(source, site, window, now = NULL, ...) {
        if (site_id(site) == "site_1") {
          abort_meteo("dead", class = "http_gone")
        }
        new_obs(make_obs(n = 2, site_id = "site_2"))
      },
      .acquire_forecast = function(...) new_forecast(make_forecast(n = 2))
    )
    status <- met_sync_daily(met_sites(list(s1, s2)),
      now = now,
      config = pipeline_config(root)
    )
    expect_equal(nrow(status), 2)
    expect_setequal(status$site_id, c("site_1", "site_2"))
    expect_true(any(status$status == "degraded"))
    expect_true(any(status$status == "ok"))
  })
})

describe("for_each_site() error isolation", {
  it("isolates a failing site by default and collects per-site status", {
    sites <- make_test_sites(2)
    status <- for_each_site(sites, function(site) {
      if (site@site_id == "site_1") stop("boom")
      "done"
    }, on_error = "isolate")
    expect_equal(nrow(status), 2)
    expect_true(any(status$status == "error"))
    expect_true(any(status$status == "ok"))
  })

  it("keeps per-site results aligned when a mid-list site fails", {
    # Regression: appending to the result list with `[[i]] <- NULL` was a
    # no-op that shortened the list -- with 3 sites and a mid-list failure,
    # tibble() then errored on the size mismatch (and with 2 sites the lone
    # surviving result was silently recycled onto the failed site's row).
    sites <- make_test_sites(3)
    status <- for_each_site(sites, function(site) {
      if (site@site_id == "site_2") stop("boom")
      paste0("done_", site@site_id)
    }, on_error = "isolate")
    expect_equal(nrow(status), 3)
    expect_equal(status$status, c("ok", "error", "ok"))
    expect_null(status$result[[2]])
    expect_equal(status$result[[1]], "done_site_1")
    expect_equal(status$result[[3]], "done_site_3")
  })

  it("re-raises under on_error = 'stop'", {
    sites <- make_test_sites(2)
    expect_error(
      for_each_site(sites, function(site) stop("boom"), on_error = "stop")
    )
  })
})
