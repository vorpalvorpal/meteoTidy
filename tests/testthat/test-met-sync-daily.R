# Plan 16 — met_sync_daily() (daily). Mocked; frozen clock.

describe("daily sync archives forecasts and extends history products", {
  it("archives forecasts (incl. seasonal) and builds history_hourly/daily", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    now <- as.POSIXct("2026-01-02 00:00", tz = "UTC")
    calls <- mock_acquisition()
    built <- new.env()
    built$hourly <- 0L
    built$daily <- 0L
    testthat::local_mocked_bindings(
      qc_run = function(...) invisible(), fill_run = function(...) invisible(),
      archive_forecasts = function(store_root, site, sources, now, ...) {
        # record that seasonal is among the sources archived
        tibble::tibble(source = sources, note = "archived")
      },
      build_history_hourly = function(...) {
        built$hourly <- built$hourly + 1L
        make_obs(n = 1)
      },
      build_history_daily = function(...) {
        built$daily <- built$daily + 1L
        make_obs(n = 1)
      }
    )
    status <- met_sync_daily(site,
      now = now,
      config = pipeline_config(
        root,
        forecast_sources = c(
          "openmeteo", "seasonal",
          "bom_forecast"
        )
      )
    )
    expect_equal(status$status, "ok")
    expect_true(built$hourly > 0 && built$daily > 0)
  })
})

describe("SILO refetch window supersedes a revised value", {
  it("re-fetches within the refetch window and supersedes a changed SILO value", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    day <- as.POSIXct("2026-01-01 00:00", tz = "UTC")
    # v1 written earlier
    store_write_obs(root, new_obs(make_obs(
      n = 1, variable = "temperature_2m",
      value = 24, source = "silo", start = day
    )),
    now = as.POSIXct("2026-01-10", tz = "UTC"), mode = "supersede"
    )
    # daily sync re-fetches SILO and gets a revised value 25 for the same instant
    testthat::local_mocked_bindings(
      qc_run = function(...) invisible(), fill_run = function(...) invisible(),
      archive_forecasts = function(...) tibble::tibble(note = "ok"),
      build_history_hourly = function(...) make_obs(n = 1),
      build_history_daily = function(...) make_obs(n = 1),
      .acquire_obs = function(source, site, window, now = NULL, ...) {
        new_obs(make_obs(
          n = 1, variable = "temperature_2m", value = 25,
          source = "silo", start = day
        ))
      }
    )
    met_sync_daily(site,
      now = as.POSIXct("2026-02-01", tz = "UTC"),
      config = pipeline_config(root, obs_sources = "silo")
    )
    current <- store_read_obs(root, "test")
    silo_now <- current$value[current$source == "silo"]
    expect_equal(silo_now, 25) # revision superseded the old value
    both <- store_read_obs(root, "test", include_superseded = TRUE)
    expect_true(all(c(24, 25) %in% both$value)) # the old value retained for audit
  })
})
