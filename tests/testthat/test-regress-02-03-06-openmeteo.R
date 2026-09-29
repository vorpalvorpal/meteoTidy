# Regressions (production review, problems 2, 3 and 6) against recorded
# Open-Meteo responses (Katoomba, 2026-09-29):
#   2. no forecast horizon: the archive's issue window (now-3d..now) was sent
#      as start_date/end_date, so an "issuance" was past data;
#   3. issue_time = now, so every hourly sync re-archived the same run;
#   6. ensemble unusable: no `models` -> HTTP 400; unit "undefined" aborted
#      to_canonical(); YAML could not narrow `provides`; HTTP 429 not backed
#      off long enough.

issue_window_for <- function(now) {
  list(from = now - as.difftime(3, units = "days"), to = now)
}

describe("problem 2: forecast horizon", {
  it("requests the full forecast horizon, not the issue window as dates", {
    site <- make_prod_site("kat")
    cap <- new.env()
    fc <- with_routed_http(openmeteo_routes(), {
      fetch_forecast(source_openmeteo("forecast"), site, "temperature_2m",
                     issue_window_for(prod_now()), now = prod_now())
    }, capture = cap)
    data_url <- grep("/v1/forecast", cap$urls, value = TRUE)
    expect_match(data_url, "forecast_days=16")
    expect_no_match(data_url, "start_date|end_date")

    lead_days <- as.numeric(max(fc$valid_time) - min(fc$issue_time), units = "days")
    expect_gt(lead_days, 14)
    expect_true(all(fc$valid_time >= fc$issue_time))
  })

  it("requests the 15-day ensemble horizon", {
    site <- make_prod_site("kat")
    cap <- new.env()
    fc <- suppressWarnings(with_routed_http(openmeteo_routes(), {
      fetch_forecast(source_openmeteo("ensemble"), site, "temperature_2m",
                     issue_window_for(prod_now()), now = prod_now())
    }, capture = cap))
    expect_match(grep("/v1/ensemble", cap$urls, value = TRUE), "forecast_days=15")
    expect_gt(as.numeric(max(fc$valid_time) - fc$issue_time[1], units = "days"), 7)
  })
})

describe("problem 3: issue_time is the model run, so re-syncs dedup", {
  it("uses the model's run initialisation time from Open-Meteo metadata", {
    site <- make_prod_site("kat")
    meta <- jsonlite::read_json(fixture_path("openmeteo/meta-ecmwf_ifs025.json"))
    fc <- suppressWarnings(with_routed_http(openmeteo_routes(), {
      fetch_forecast(source_openmeteo("forecast", models = "ecmwf_ifs025"), site,
                     "temperature_2m", issue_window_for(prod_now()), now = prod_now())
    }))
    expect_equal(unique(fc$issue_time),
                 as.POSIXct(meta$last_run_initialisation_time, origin = "1970-01-01", tz = "UTC"))
    expect_equal(unique(fc$model), "ecmwf_ifs025")
  })

  it("floors a configured best_match (no single run) to the 6-hourly cycle", {
    site <- make_prod_site("kat")
    fc <- with_routed_http(openmeteo_routes(), {
      fetch_forecast(source_openmeteo("forecast", models = "best_match"), site, "temperature_2m",
                     issue_window_for(prod_now()), now = prod_now())
    })
    expect_equal(unique(fc$issue_time), as.POSIXct("2026-09-29 06:00:00", tz = "UTC"))
    expect_equal(unique(fc$model), "best_match")
  })

  it("archives an hourly re-sync of the same run as no new rows", {
    root <- withr::local_tempdir()
    site <- make_prod_site("kat", store_root = root, sources = list(
      openmeteo = list(adapter = "openmeteo", product = "forecast",
                       provides = c("temperature_2m", "wind_speed_10m"))
    ))
    with_routed_http(openmeteo_routes(), {
      archive_forecasts(root, site, "openmeteo", now = prod_now())
      n1 <- nrow(store_read_forecast(root, "kat"))
      archive_forecasts(root, site, "openmeteo", now = prod_now() + 3600.37)
      n2 <- nrow(store_read_forecast(root, "kat"))
    })
    expect_gt(n1, 0)
    expect_equal(n2, n1)
  })
})

describe("problem 6: ensemble", {
  it("defaults to named models: ECMWF IFS 0.25 and ICON-EPS (the API rejects requests without models)", {
    site <- make_prod_site("kat")
    cap <- new.env()
    suppressWarnings(with_routed_http(openmeteo_routes(), {
      fetch_forecast(source_openmeteo("ensemble"), site, "temperature_2m",
                     issue_window_for(prod_now()), now = prod_now())
    }, capture = cap))
    ens <- grep("/v1/ensemble", cap$urls, value = TRUE)
    expect_setequal(sub(".*models=([a-z0-9_]+).*", "\\1", ens), c("ecmwf_ifs025", "icon_seamless"))
  })

  it("skips a variable with unit 'undefined' with a warning, keeping the rest", {
    site <- make_prod_site("kat")
    adapter <- source_openmeteo("ensemble", models = "ecmwf_ifs025",
                                provides = c("temperature_2m", "wind_speed_10m"))
    # boundary_layer_height is not in the ensemble defaults; the recorded
    # body carries it with unit "undefined" -- request it explicitly.
    adapter@provides <- c("temperature_2m", "boundary_layer_height")
    expect_warning(
      fc <- with_routed_http(openmeteo_routes(), {
        fetch_forecast(adapter, site, c("temperature_2m", "boundary_layer_height"),
                       issue_window_for(prod_now()), now = prod_now())
      }),
      class = "meteoTidy_warning_openmeteo_unknown_unit"
    )
    expect_setequal(unique(fc$variable), "temperature_2m")
    # 50 perturbed members + the control run (member 0)
    expect_equal(length(unique(fc$member)), 51L)
    expect_true(0L %in% fc$member)
    meta <- jsonlite::read_json(fixture_path("openmeteo/meta-ecmwf_ifs025_ensemble.json"))
    expect_equal(unique(fc$issue_time),
                 as.POSIXct(meta$last_run_initialisation_time, origin = "1970-01-01", tz = "UTC"))
  })

  it("honours `provides` from site YAML", {
    yml <- withr::local_tempfile(fileext = ".yml")
    root <- withr::local_tempdir()
    writeLines(c(
      "sites:",
      "  - site_id: kat",
      "    latitude: -33.70",
      "    longitude: 150.32",
      "    elevation: 1000",
      "    timezone: Australia/Sydney",
      paste0("    store_root: ", normalizePath(root, winslash = "/")),
      "    sources:",
      "      om_ens:",
      "        adapter: openmeteo",
      "        product: ensemble",
      "        models: [ecmwf_ifs025]",
      "        provides: [temperature_2m, precipitation]"
    ), yml)
    site <- read_sites_yaml(yml)@sites[[1]]
    adapter <- adapters_for_site(site)$om_ens
    expect_setequal(adapter@provides, c("temperature_2m", "precipitation"))

    cap <- new.env()
    with_routed_http(openmeteo_routes(), {
      .acquire_forecast("om_ens", site, issue_window_for(prod_now()), now = prod_now())
    }, capture = cap)
    data_url <- grep("/v1/ensemble", cap$urls, value = TRUE)
    expect_match(data_url, "hourly=temperature_2m%2Cprecipitation")
  })

  it("rejects a `provides` entry the adapter cannot serve", {
    expect_error(source_openmeteo("forecast", provides = "not_a_variable"),
                 class = "meteoTidy_error_bad_provides")
  })

  it("backs off on HTTP 429, honouring Retry-After, then succeeds", {
    withr::local_envvar(METEOTIDY_NO_NET = "0")
    withr::local_options(meteoTidy.http_backoff_base = 2, meteoTidy.http_backoff_max = 60)
    body <- readBin(fixture_path("openmeteo/meta-ecmwf_ifs025.json"), "raw",
                    file.size(fixture_path("openmeteo/meta-ecmwf_ifs025.json")))
    responses <- list(
      httr2::response(status_code = 429, headers = list(`Retry-After` = "7"),
                      body = charToRaw("{\"error\":true,\"reason\":\"Too many requests\"}")),
      httr2::response(status_code = 429,
                      body = charToRaw("{\"error\":true,\"reason\":\"Too many requests\"}")),
      httr2::response(status_code = 200,
                      headers = list(`Content-Type` = "application/json"), body = body)
    )
    i <- 0
    slept <- numeric(0)
    local_mocked_bindings(
      req_perform = function(req, ...) {
        i <<- i + 1
        responses[[i]]
      },
      .package = "httr2"
    )
    local_mocked_bindings(.http_sleep = function(seconds) slept <<- c(slept, seconds))
    out <- .http_get("https://api.open-meteo.com/data/ecmwf_ifs025/static/meta.json")
    expect_equal(out$last_run_initialisation_time, 1790618400)
    expect_equal(slept, c(7, 4))
  })
})

describe("problem 3 (found in the live acceptance run): unknown run time", {
  it("fails the fetch instead of stamping a named model with the clock's 6 h floor", {
    # Live 2026-09-29 07:21 UTC: one ensemble metadata request failed, the
    # fallback stamped ECMWF's 18 UTC run as 06 UTC, and the next hour the
    # same data was archived again under its true run time.
    site <- make_prod_site("kat")
    adapter <- source_openmeteo(product = "ensemble", models = "ecmwf_ifs025", provides = "temperature_2m")
    routes <- openmeteo_routes()
    routes[["ensemble-api.*/data/ecmwf_ifs025_ensemble/static/meta.json"]] <- 503
    err <- tryCatch(
      with_routed_http(routes, {
        fetch_forecast(adapter, site, "temperature_2m", issue_window_for(prod_now()), now = prod_now())
      }),
      error = identity
    )
    expect_s3_class(err, "meteoTidy_error_openmeteo_run_unknown")
    expect_match(conditionMessage(err), "ecmwf_ifs025")
  })
})
