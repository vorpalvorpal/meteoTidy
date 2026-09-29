# Follow-up review, item 2: fail_on needs a middle setting.
#
# "any" failed every hourly run while Blaxland's eagle.io logger was merely
# offline ("stale"); "all" missed a dead feed as long as anything else worked.
# fail_on = "failed" fails on any source "failed" or site "error", and
# ignores "stale". Replayed with recorded responses (Blaxland's eagle.io
# node really stopped reporting on 2026-09-24).

fu02_routes <- function(openmeteo_down = FALSE) {
  c(
    if (openmeteo_down) list("open-meteo\\.com" = 404L),
    list(
      "api\\.weather\\.bom\\.gov\\.au/v1/locations/r65050/forecasts/daily" = "bom/webapi-daily-r65050.json",
      "api\\.weather\\.bom\\.gov\\.au/v1/locations/r65050/forecasts/hourly" = "bom/webapi-hourly-r65050.json",
      "nodes/647e9492dac9bf46aa1ffd0f/historic" = "eagleio/historic-blax-precipitation-offline.json",
      "nodes/647e9492dac9bf46aa1ffd0f" = "eagleio/node-blax-precipitation.json"
    ),
    openmeteo_routes()
  )
}

fu02_site <- function(root) {
  make_prod_site("blax", store_root = root, sources = list(
    eagleio = list(adapter = "eagleio", api_key_env = "FU02_EAGLE_KEY",
                   nodes = list(precipitation = "647e9492dac9bf46aa1ffd0f")),
    openmeteo = list(adapter = "openmeteo", product = "forecast",
                     models = "ecmwf_ifs025", provides = c("temperature_2m", "wind_speed_10m")),
    bom_forecast = list(adapter = "bom_forecast", allow_web_api = TRUE, products = "daily")
  ))
}

fu02_cfg <- function(root) {
  list(store_root = root, obs_sources = "eagleio",
       forecast_sources = c("openmeteo", "bom_forecast"))
}

fu02_sync <- function(root, fail_on, openmeteo_down = FALSE) {
  withr::local_envvar(FU02_EAGLE_KEY = "test-key")
  suppressMessages(with_routed_http(fu02_routes(openmeteo_down), {
    met_sync_live(fu02_site(root), now = prod_now(), config = fu02_cfg(root), fail_on = fail_on)
  }))
}

describe("item 2: fail_on = 'failed'", {
  it("does not fail a run whose only problem is a stale station", {
    root <- withr::local_tempdir()
    status <- fu02_sync(root, "none")
    src <- status$sources[[1]]
    expect_equal(src$status[src$source == "eagleio"], "stale")
    expect_true(all(src$status[src$source != "eagleio"] == "ok"))
    # "any" (the old strict setting) fails on this; "failed" must not.
    expect_error(fu02_sync(withr::local_tempdir(), "any"), class = "meteoTidy_error_sync_failed")
    expect_no_error(fu02_sync(withr::local_tempdir(), "failed"))
  })

  it("fails a run in which any source failed, even when others worked", {
    root <- withr::local_tempdir()
    # "all" misses this dead feed; "failed" catches it.
    expect_no_error(fu02_sync(root, "all", openmeteo_down = TRUE))
    err <- tryCatch(fu02_sync(withr::local_tempdir(), "failed", openmeteo_down = TRUE),
                    error = identity)
    expect_s3_class(err, "meteoTidy_error_sync_failed")
    expect_match(conditionMessage(err), "openmeteo")
    expect_false(grepl("eagleio", conditionMessage(err)))
  })

  it("does not fail a site whose only source is a stale station", {
    # Found by the live acceptance (check i): with eagle.io as the site's
    # only source, nothing was acquired, the site rolled up to "failed" and
    # fail_on = "failed" aborted although no source had failed.
    root <- withr::local_tempdir()
    cfg <- list(store_root = root, obs_sources = "eagleio", forecast_sources = character(0))
    withr::local_envvar(FU02_EAGLE_KEY = "test-key")
    run <- function(fail_on) {
      suppressMessages(with_routed_http(fu02_routes(), {
        met_sync_live(fu02_site(root), now = prod_now(), config = cfg, fail_on = fail_on)
      }))
    }
    status <- run("none")
    expect_equal(status$sources[[1]]$status, "stale")
    expect_no_error(run("failed"))
    expect_error(run("any"), class = "meteoTidy_error_sync_failed")
  })

  it("fails on a site error", {
    status <- tibble::tibble(site_id = "blax", status = "error", message = "boom",
                             sources = list(.empty_source_status()))
    expect_error(.apply_fail_on("met_sync_live", status, "failed"),
                 class = "meteoTidy_error_sync_failed")
  })

  it("is accepted by both sync verbs", {
    expect_true("failed" %in% eval(formals(met_sync_live)$fail_on))
    expect_true("failed" %in% eval(formals(met_sync_daily)$fail_on))
  })
})
