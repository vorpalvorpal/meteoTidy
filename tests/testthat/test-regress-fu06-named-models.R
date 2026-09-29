# Follow-up review, item 6: best_match issue times were guesses.
#
# The Forecast API's default "best_match" blends several models, so it has no
# run time; the adapter stamped it with the clock floored to 6 h and archived
# it as if that were an issuance. Verification then scored a blend against a
# made-up issue time. By default the adapter now archives NAMED models, each
# with its real run initialisation time from Open-Meteo's metadata
# (ECMWF IFS 18 UTC 28 Sep, GFS 00 UTC and ICON 06 UTC 29 Sep in the
# recordings below). best_match is archived only when configured, and then
# flagged in forecast_aux as having no verifiable run time.

fu06_fetch <- function(adapter, variables = "temperature_2m", capture = new.env()) {
  with_routed_http(openmeteo_routes(), {
    suppressWarnings(fetch_forecast(adapter, make_prod_site("kat"), variables,
                                    list(from = prod_now() - 3 * 86400, to = prod_now()),
                                    now = prod_now()))
  }, capture = capture)
}

fu06_init <- function(name) {
  meta <- jsonlite::read_json(fixture_path("openmeteo", sprintf("meta-%s.json", name)))
  as.POSIXct(meta$last_run_initialisation_time, origin = "1970-01-01", tz = "UTC")
}

describe("item 6: named Open-Meteo models with real run times", {
  it("defaults to ECMWF IFS, GFS and ICON, never best_match", {
    cap <- new.env()
    fc <- fu06_fetch(source_openmeteo("forecast"), capture = cap)
    expect_setequal(unique(fc$model), c("ecmwf_ifs025", "gfs_global", "icon_global"))
    data_urls <- grep("/v1/forecast", cap$urls, value = TRUE)
    expect_length(data_urls, 3)
    expect_true(all(grepl("models=", data_urls)))
  })

  it("stamps each model with its own run initialisation time", {
    cap <- new.env()
    fc <- fu06_fetch(source_openmeteo("forecast"), capture = cap)
    issue <- function(m) unique(fc$issue_time[fc$model == m])
    expect_equal(issue("ecmwf_ifs025"), fu06_init("ecmwf_ifs025"))
    expect_equal(issue("gfs_global"), fu06_init("ncep_gfs025"))
    expect_equal(issue("icon_global"), fu06_init("dwd_icon"))
    expect_true(any(grepl("/data/ncep_gfs025/static/meta.json", cap$urls)))
    expect_true(any(grepl("/data/dwd_icon/static/meta.json", cap$urls)))
  })

  it("flags a configured best_match as having no verifiable run time", {
    fc <- fu06_fetch(source_openmeteo("forecast", models = "best_match"))
    expect_equal(unique(fc$model), "best_match")
    aux <- attr(fc, "aux")
    expect_false(is.null(aux))
    flag <- aux[aux$field == "issue_time_basis:best_match", ]
    expect_equal(nrow(flag), 1)
    expect_match(flag$value_text, "not verifiable")
    expect_equal(flag$issue_time, unique(fc$issue_time))
  })

  it("keeps the other models when one model's run time is unknown", {
    routes <- openmeteo_routes()
    routes[["/data/ncep_gfs025/static/meta.json"]] <- 503
    expect_warning(
      fc <- with_routed_http(routes, {
        fetch_forecast(source_openmeteo("forecast"), make_prod_site("kat"), "temperature_2m",
                       list(from = prod_now() - 3 * 86400, to = prod_now()), now = prod_now())
      }),
      class = "meteoTidy_warning_openmeteo_model_failed"
    )
    expect_setequal(unique(fc$model), c("ecmwf_ifs025", "icon_global"))
  })

  it("archives the three runs separately through a sync", {
    root <- withr::local_tempdir()
    site <- make_prod_site("kat", store_root = root, sources = list(
      openmeteo = list(adapter = "openmeteo", product = "forecast", provides = "temperature_2m")
    ))
    cfg <- list(store_root = root, obs_sources = character(0), forecast_sources = "openmeteo")
    st <- suppressMessages(with_routed_http(openmeteo_routes(),
                                            met_sync_live(site, now = prod_now(), config = cfg)))
    expect_equal(st$status, "ok")
    arch <- met_forecast_archive(site, source = "openmeteo")
    runs <- unique(arch[c("model", "issue_time")])
    expect_equal(nrow(runs), 3)
    expect_false("best_match" %in% arch$model)
  })
})
