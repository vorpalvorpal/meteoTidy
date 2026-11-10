# Archiving the whole worst-case ensemble pool (ifs01 plan, part A).
#
# Open-Meteo froze dwd_icon_eps/meta.json on 2026-10-06, so ICON stopped
# archiving silently; UKMO never archived because its metadata name fell back
# to a 404. GEM's wind gusts are all null, and GEM's data runs a day past the
# 15 days requested, so its stored run was never recognised as complete.

describe("ensemble metadata dataset names", {
  meta_name <- function(model) meteoTidy:::.openmeteo_meta_name("ensemble", model)

  it("reads ICON's run time from the native-grid ensemble dataset", {
    expect_equal(meta_name("icon_seamless"), "dwd_icon_global_eps_native")
  })

  it("reads UKMO's run time from its own name, not a _ensemble suffix", {
    expect_equal(meta_name("ukmo_global_ensemble_20km"), "ukmo_global_ensemble_20km")
  })

  it("names the ECMWF 0.1 degree ensemble explicitly", {
    expect_equal(meta_name("ecmwf_ifs01"), "ecmwf_ifs01_ensemble")
  })

  it("maps GFS and GEM to their GEFS / GEPS datasets", {
    expect_equal(meta_name("gfs025"), "ncep_gefs025")
    expect_equal(meta_name("gem_global"), "cmc_gem_geps")
  })

  it("leaves the existing mappings alone", {
    expect_equal(meta_name("ecmwf_ifs025"), "ecmwf_ifs025_ensemble")
    expect_equal(meta_name("ecmwf_aifs025"), "ecmwf_aifs025_ensemble")
    expect_equal(meta_name("gfs05"), "ncep_gefs05")
    expect_equal(meta_name("bom_access_global_ensemble"), "bom_access_global_ensemble")
  })

  it("builds the metadata URL from the new name", {
    url <- meteoTidy:::.openmeteo_meta_url("ensemble", "icon_seamless", api_key = NULL)
    expect_match(url, "/data/dwd_icon_global_eps_native/static/meta.json", fixed = TRUE)
  })
})

describe("per-model ensemble variable requests", {
  unsupported <- function(model) meteoTidy:::.openmeteo_model_unsupported("ensemble", model)

  it("does not request wind gusts from GEM, which returns them all null", {
    expect_true("wind_gusts_10m" %in% unsupported("gem_global"))
  })

  it("leaves the other models' lists unchanged", {
    expect_equal(unsupported("icon_seamless"), "wind_gusts_10m")
    for (m in c("ecmwf_ifs025", "ecmwf_ifs01", "gfs025", "ukmo_global_ensemble_20km")) {
      expect_equal(unsupported(m), character(0), info = m)
    }
  })

  it("leaves wind gusts in GEM's request URL out, and keeps the other variables", {
    cap <- new.env()
    adapter <- source_openmeteo(product = "ensemble", models = "gem_global")
    fake <- function(url, headers = list(), query = list(), retry = 3, now = NULL, ...) {
      if (grepl("/static/meta[.]json$", url)) {
        return(list(last_run_initialisation_time = as.numeric(om_now()),
                    data_end_time = as.numeric(om_now()) + 15 * 86400))
      }
      cap$url <- url
      read_om_fixture("ensemble.json")
    }
    testthat::local_mocked_bindings(.http_get = fake)
    window <- list(from = om_now(), to = om_now() + 86400)
    fetch_forecast(adapter, make_test_site(), c("temperature_2m", "wind_gusts_10m"),
                   window, now = om_now())
    expect_match(cap$url, "temperature_2m")
    expect_no_match(cap$url, "wind_gusts_10m")
  })
})

describe("a run longer than the requested window counts as stored", {
  site_for <- function(root) {
    make_prod_site("kat", store_root = root, sources = list(
      om_ens = list(adapter = "openmeteo", product = "ensemble", models = "ecmwf_ifs025",
                    provides = "temperature_2m")
    ))
  }
  sync <- function(root, now, routes, capture = new.env()) {
    cfg <- list(store_root = root, obs_sources = character(0), forecast_sources = "om_ens")
    suppressMessages(with_routed_http(routes, {
      met_sync_live(site_for(root), now = now, config = cfg)
    }, capture = capture))
  }
  # The recorded body ends 2026-10-13 23:00 (15 days from the 29th). Metadata
  # whose data_end_time lies a day beyond that is a model like GEM, whose data
  # runs 16 days when we only request 15.
  routes_with_data_end <- function(end_utc) {
    meta <- jsonlite::read_json(fixture_path("openmeteo/meta-ecmwf_ifs025_ensemble.json"))
    meta$data_end_time <- as.numeric(as.POSIXct(end_utc, tz = "UTC"))
    routes <- openmeteo_routes()
    routes <- c(list("ensemble-api.*/data/ecmwf_ifs025_ensemble/static/meta.json" =
                       function(url) meta), routes)
    routes
  }
  calls <- function(cap) sum(grepl("/v1/ensemble", cap$urls))

  it("makes no data request once the archive reaches the end of the requested window", {
    root <- withr::local_tempdir()
    routes <- routes_with_data_end("2026-10-14 23:00:00")
    cap1 <- new.env()
    sync(root, prod_now(), routes, cap1)
    expect_equal(calls(cap1), 1)
    expect_gt(nrow(store_read_forecast(root, "kat")), 0)

    cap2 <- new.env()
    sync(root, prod_now() + 3600, routes, cap2)
    expect_equal(calls(cap2), 0)
  })

  it("still downloads when the archive stops short of the requested window", {
    root <- withr::local_tempdir()
    routes <- routes_with_data_end("2026-10-14 23:00:00")
    sync(root, prod_now(), routes)
    full <- store_read_forecast(root, "kat")
    unlink(file.path(root, "forecasts"), recursive = TRUE)
    store_write_forecast(root, full[full$valid_time < min(full$valid_time) + 5 * 86400, ])

    cap <- new.env()
    sync(root, prod_now() + 3600, routes, cap)
    expect_equal(calls(cap), 1)
  })
})

describe("members that stop short of the horizon", {
  # GFS / UKMO members do not all reach the full horizon: Open-Meteo pads the
  # tail with null. The parser's convention (.openmeteo_tidy_rows) is to drop
  # null cells rather than store NA, so an all-NA day yields no rows at all.
  body_with_null_tail <- function(from_index) {
    body <- read_om_fixture("ensemble.json")
    n <- length(body$hourly$time)
    cols <- grep("_member", names(body$hourly), value = TRUE)
    for (col in cols) {
      body$hourly[[col]][from_index:n] <- list(NULL)
    }
    body
  }

  it("does not error and stores no NA cells for days that are entirely null", {
    full <- om_fetch("ensemble", read_om_fixture("ensemble.json"))
    n <- length(read_om_fixture("ensemble.json")$hourly$time)
    out <- om_fetch("ensemble", body_with_null_tail(n))
    expect_false(anyNA(out$value))
    expect_lt(nrow(out), nrow(full))
    expect_gt(nrow(out), 0)
    expect_true(max(out$valid_time) < max(full$valid_time))
  })

  it("returns an empty, well-formed forecast when every member is null", {
    out <- om_fetch("ensemble", body_with_null_tail(1))
    expect_equal(nrow(out), 0)
    expect_true(all(c("valid_time", "member", "value") %in% names(out)))
  })

  it("keeps the members that do reach a day when others are null there", {
    body <- read_om_fixture("ensemble.json")
    n <- length(body$hourly$time)
    cols <- grep("_member", names(body$hourly), value = TRUE)
    body$hourly[[cols[1]]][n] <- list(NULL)
    out <- om_fetch("ensemble", body)
    last <- out[out$valid_time == max(out$valid_time), ]
    expect_false(anyNA(out$value))
    expect_equal(length(unique(last$member)), length(cols) - 1)
  })
})
