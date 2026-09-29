# Regression (found preparing the live acceptance run): on a fresh store the
# obs watermark is absent, store_effective_fetch_window() returns
# from = NULL ("full history"), and met_sync_daily() handed that straight to
# the adapters -- SILO (as.Date(NULL)) and eagle.io (no startTime) cannot
# fetch an open-ended window, so the first daily run failed every obs source.
# The daily sync now bounds a first run by the source's refetch window;
# full-history backfill is met_backfill()'s job.

describe("met_sync_daily() on a fresh store", {
  it("fetches each obs source over a bounded window (now - refetch, now)", {
    root <- withr::local_tempdir()
    site <- make_prod_site("kat", store_root = root)
    seen <- new.env()
    testthat::local_mocked_bindings(
      .acquire_obs = function(source, site, window, now = .now(), variables = NULL) {
        seen[[source]] <- window
        new_obs(make_obs(n = 0))
      },
      archive_forecasts = function(...) .empty_source_status()
    )
    cfg <- list(store_root = root, obs_sources = c("silo", "eagleio"), forecast_sources = character(0),
                refetch_windows = list(silo = as.difftime(30, units = "days")))
    met_sync_daily(site, now = prod_now(), config = cfg)
    expect_equal(seen$silo$from, prod_now() - as.difftime(30, units = "days"))
    expect_equal(seen$eagleio$from, prod_now() - as.difftime(7, units = "days"))
    expect_equal(seen$silo$to, prod_now())
  })
})
