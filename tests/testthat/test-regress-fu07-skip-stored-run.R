# Follow-up review, item 7: the ensemble was re-downloaded every hour.
#
# ECMWF IFS runs every 6 h (ensemble every 12 h on Open-Meteo), but each
# hourly sync downloaded the full 51-member, 15-day ensemble again only for
# the store to drop every row as a duplicate: ~5 MB per call and the bulk of
# the free-tier call budget. The adapter now reads the run time from
# Open-Meteo's metadata first and skips the data request when that run is
# already archived in full. Replays the recorded ECMWF IFS ensemble.

fu07_site <- function(root) {
  make_prod_site("kat", store_root = root, sources = list(
    om_ens = list(adapter = "openmeteo", product = "ensemble", models = "ecmwf_ifs025",
                  provides = "temperature_2m")
  ))
}

fu07_cfg <- function(root) {
  list(store_root = root, obs_sources = character(0), forecast_sources = "om_ens")
}

fu07_sync <- function(root, now, routes = openmeteo_routes(), capture = new.env()) {
  suppressMessages(with_routed_http(routes, {
    met_sync_live(fu07_site(root), now = now, config = fu07_cfg(root))
  }, capture = capture))
}

ensemble_calls <- function(cap) sum(grepl("/v1/ensemble", cap$urls))

describe("item 7: skip the download of a run already stored", {
  it("makes no ensemble data request when the run is already archived", {
    root <- withr::local_tempdir()
    cap1 <- new.env()
    st1 <- fu07_sync(root, prod_now(), capture = cap1)
    expect_equal(st1$status, "ok")
    expect_equal(ensemble_calls(cap1), 1)
    n1 <- nrow(store_read_forecast(root, "kat"))
    expect_gt(n1, 0)

    cap2 <- new.env()
    st2 <- fu07_sync(root, prod_now() + 3600, capture = cap2)
    expect_equal(st2$status, "ok")
    expect_equal(ensemble_calls(cap2), 0)
    expect_true(any(grepl("ecmwf_ifs025_ensemble/static/meta.json", cap2$urls)))
    expect_match(st2$sources[[1]]$message, "already archived")
    expect_equal(nrow(store_read_forecast(root, "kat")), n1)
  })

  it("downloads again when the stored copy of the run is incomplete", {
    root <- withr::local_tempdir()
    fu07_sync(root, prod_now())
    full <- store_read_forecast(root, "kat")
    # Keep only the first day, as if the earlier fetch caught a partial run.
    unlink(file.path(root, "forecasts"), recursive = TRUE)
    store_write_forecast(root, full[full$valid_time < min(full$valid_time) + 86400, ])

    cap <- new.env()
    fu07_sync(root, prod_now() + 3600, capture = cap)
    expect_equal(ensemble_calls(cap), 1)
    expect_equal(nrow(store_read_forecast(root, "kat")), nrow(full))
  })

  it("downloads a new run as soon as the metadata announces it", {
    root <- withr::local_tempdir()
    fu07_sync(root, prod_now())
    meta <- jsonlite::read_json(fixture_path("openmeteo/meta-ecmwf_ifs025_ensemble.json"))
    meta$last_run_initialisation_time <- meta$last_run_initialisation_time + 12 * 3600
    routes <- openmeteo_routes()
    routes[["ensemble-api.*/data/ecmwf_ifs025_ensemble/static/meta.json"]] <- function(url) meta
    cap <- new.env()
    fu07_sync(root, prod_now() + 3600, routes = routes, capture = cap)
    expect_equal(ensemble_calls(cap), 1)
    runs <- unique(store_read_forecast(root, "kat")$issue_time)
    expect_length(runs, 2)
  })
})
