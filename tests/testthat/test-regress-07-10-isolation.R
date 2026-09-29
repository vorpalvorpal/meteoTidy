# Regressions (production review):
#  7. one failing source took the site down: archive_forecasts() was not
#     wrapped and ran FIRST in met_sync_daily(), so a dead forecast source
#     skipped every obs source, QC/fill and history for that site;
# 10. silent failure: met_sync_*() never threw and logged nothing, so an
#     Rscript run exited 0 even when every site failed.

iso_routes <- function(openmeteo_down = TRUE) {
  c(
    if (openmeteo_down) list("open-meteo\\.com" = 404L) else openmeteo_routes(),
    list(
      "api\\.weather\\.bom\\.gov\\.au/v1/locations/r64bhq/forecasts/daily" = "bom/webapi-daily-r64bhq.json",
      "api\\.weather\\.bom\\.gov\\.au/v1/locations/r64bhq/forecasts/hourly" = "bom/webapi-hourly-r64bhq.json",
      "reg\\.bom\\.gov\\.au/fwo/IDN60901/IDN60901\\.94743\\.json" = "bom/obs72h-IDN60901-94743.json"
    )
  )
}

iso_site <- function(root) {
  site <- make_prod_site("kat", store_root = root, sources = list(
    openmeteo = list(adapter = "openmeteo", product = "forecast",
                     provides = c("temperature_2m", "wind_speed_10m")),
    bom_forecast = list(adapter = "bom_forecast", allow_web_api = TRUE),
    bom_obs = list(adapter = "bom_obs")
  ))
  site_set_resolved(site, c("bom", "wmo"), "94743")
}

iso_config <- function(root) {
  list(store_root = root, obs_sources = "bom_obs",
       forecast_sources = c("openmeteo", "bom_forecast"))
}

describe("problem 7: a failing source does not take the site down", {
  it("met_sync_daily() still archives the other forecast source and collects obs", {
    root <- withr::local_tempdir()
    status <- suppressMessages(with_routed_http(iso_routes(), {
      met_sync_daily(iso_site(root), now = prod_now(), config = iso_config(root))
    }))
    expect_equal(status$status, "degraded")
    expect_match(status$message, "openmeteo")

    fc <- store_read_forecast(root, "kat")
    expect_true("bom_forecast" %in% fc$source)
    expect_false("openmeteo" %in% fc$source)
    obs <- store_read_obs(root, "kat")
    expect_true("bom_obs" %in% obs$source)
  })

  it("reports a per-source status table", {
    root <- withr::local_tempdir()
    status <- suppressMessages(with_routed_http(iso_routes(), {
      met_sync_live(iso_site(root), now = prod_now(), config = iso_config(root))
    }))
    src <- status$sources[[1]]
    expect_setequal(src$source, c("bom_obs", "openmeteo", "bom_forecast"))
    expect_equal(src$status[src$source == "openmeteo"], "failed")
    expect_equal(src$status[src$source == "bom_forecast"], "ok")
    expect_gt(src$n[src$source == "bom_forecast"], 0)
    expect_match(src$message[src$source == "openmeteo"], "404")
  })

  it("is ok with every source up", {
    root <- withr::local_tempdir()
    status <- suppressMessages(with_routed_http(iso_routes(openmeteo_down = FALSE), {
      met_sync_live(iso_site(root), now = prod_now(), config = iso_config(root))
    }))
    expect_equal(status$status, "ok")
    expect_true(all(status$sources[[1]]$status == "ok"))
  })
})

describe("problem 10: schedulers can detect failure", {
  it("logs one line per site naming each source's outcome on stderr", {
    withr::local_options(meteoTidy.sync_log = TRUE)
    root <- withr::local_tempdir()
    expect_message(
      with_routed_http(iso_routes(), {
        met_sync_live(iso_site(root), now = prod_now(), config = iso_config(root))
      }),
      "met_sync_live kat: degraded.*openmeteo FAILED.*bom_forecast ok"
    )
  })

  it("fail_on = 'any' aborts with class sync_failed when a source failed", {
    root <- withr::local_tempdir()
    expect_error(
      suppressMessages(with_routed_http(iso_routes(), {
        met_sync_live(iso_site(root), now = prod_now(), config = iso_config(root),
                      fail_on = "any")
      })),
      class = "meteoTidy_error_sync_failed"
    )
    # ...after doing all the work it could.
    expect_true("bom_forecast" %in% store_read_forecast(root, "kat")$source)
  })

  it("fail_on = 'all' only aborts when every site failed outright", {
    root <- withr::local_tempdir()
    expect_no_error(suppressMessages(with_routed_http(iso_routes(), {
      met_sync_live(iso_site(root), now = prod_now(), config = iso_config(root),
                    fail_on = "all")
    })))
    dead <- list(store_root = root, obs_sources = character(0), forecast_sources = "openmeteo")
    expect_error(
      suppressMessages(with_routed_http(iso_routes(), {
        met_sync_live(iso_site(root), now = prod_now(), config = dead, fail_on = "all")
      })),
      class = "meteoTidy_error_sync_failed"
    )
  })

  it("defaults to fail_on = 'none' (returns status, never throws)", {
    root <- withr::local_tempdir()
    expect_no_error(suppressMessages(with_routed_http(iso_routes(), {
      met_sync_daily(iso_site(root), now = prod_now(), config = iso_config(root))
    })))
  })

  it("gives an Rscript run a non-zero exit status", {
    skip_on_cran()
    root <- normalizePath(withr::local_tempdir(), winslash = "/")
    pkg <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = FALSE)
    load <- if (file.exists(file.path(pkg, "DESCRIPTION"))) {
      skip_if_not_installed("pkgload")
      sprintf("suppressMessages(pkgload::load_all('%s', quiet = TRUE, helpers = FALSE))", pkg)
    } else {
      "suppressMessages(library(meteoTidy))"
    }
    script <- file.path(root, "sync.R")
    writeLines(c(
      load,
      "site <- met_site(site_id = 'kat', latitude = units::set_units(-33.7, 'degree'),",
      "  longitude = units::set_units(150.32, 'degree'), elevation = units::set_units(1000, 'm'),",
      "  timezone = 'Australia/Sydney', instruments = list(),",
      "  sources = list(openmeteo = list(adapter = 'openmeteo', product = 'forecast')),",
      sprintf("  store_root = '%s')", root),
      sprintf("cfg <- list(store_root = '%s', obs_sources = character(0), forecast_sources = 'openmeteo')", root),
      "met_sync_live(site, config = cfg, fail_on = 'any')"
    ), script)
    rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
    # METEOTIDY_NO_NET makes the only source fail without touching the network.
    out <- withr::with_envvar(c(METEOTIDY_NO_NET = "1"), system2(
      rscript, c("--vanilla", shQuote(script)), stdout = TRUE, stderr = TRUE
    ))
    expect_false(is.null(attr(out, "status")))
    expect_true(attr(out, "status") != 0)
    expect_true(any(grepl("openmeteo FAILED", out)))
  })
})
