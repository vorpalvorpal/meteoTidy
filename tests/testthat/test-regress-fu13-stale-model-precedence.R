# Post-review blocker: met_wide() served a stale forecast after the upgrade.
#
# best_match heads the default model precedence, and production stores
# already hold openmeteo/best_match runs from before item 6. The upgraded
# sync fetches named models only, so the last pre-upgrade best_match run
# (a 16-day horizon) kept winning every variable for up to 16 days although
# newer ecmwf_ifs025/gfs_global/icon_global runs were archived. A model
# whose latest run is well behind the newest run in the precedence must not
# win over the fresh models. Built from recorded responses through the real
# parsers and store; the old best_match run is the stored ECMWF run relabelled
# and issued two days earlier, as a pre-upgrade run covering the window.

fu13_archive <- function(root) {
  site <- make_prod_site("kat", store_root = root, sources = list(
    openmeteo = list(adapter = "openmeteo", product = "forecast",
                     provides = c("temperature_2m", "wind_speed_10m", "boundary_layer_height",
                                  "soil_moisture_0_to_1cm"))
  ))
  suppressMessages(suppressWarnings(with_routed_http(openmeteo_routes(), {
    archive_forecasts(root, site, "openmeteo", now = prod_now())
  })))
  site
}

fu13_add_old_best_match <- function(root, age_hours = 48, bump = 100) {
  fc <- store_read_forecast(root, "kat", source = "openmeteo")
  old <- fc[fc$model == "ecmwf_ifs025" & fc$issue_time == max(fc$issue_time[fc$model == "ecmwf_ifs025"]), ]
  old$model <- "best_match"
  old$issue_time <- old$issue_time - age_hours * 3600
  old$lead_time <- old$valid_time - old$issue_time
  old$value <- old$value + bump
  store_write_forecast(root, old, now = prod_now() - age_hours * 3600)
  invisible(old)
}

fu13_window <- function() {
  from <- as.POSIXct("2026-09-29 07:00:00", tz = "UTC")
  list(from = from, to = from + 71 * 3600)
}

describe("stale models in met_wide()'s precedence", {
  it("does not serve a stale pre-upgrade best_match run over newer named models", {
    root <- withr::local_tempdir()
    site <- fu13_archive(root)
    fu13_add_old_best_match(root)
    wide <- met_wide(site, fu13_window(), variables = c("temperature_2m", "wind_speed_10m",
                                                        "boundary_layer_height"),
                     source = "openmeteo", now = prod_now())
    served <- stats::setNames(met_provenance(wide)$model, met_provenance(wide)$variable)
    expect_false(any(served == "best_match", na.rm = TRUE))
    expect_equal(served[["temperature_2m"]], "ecmwf_ifs025")
    expect_equal(served[["boundary_layer_height"]], "gfs_global")
  })

  it("still serves best_match when it is current", {
    root <- withr::local_tempdir()
    site <- fu13_archive(root)
    fu13_add_old_best_match(root, age_hours = 0)
    wide <- met_wide(site, fu13_window(), variables = "temperature_2m",
                     source = "openmeteo", now = prod_now())
    expect_equal(met_provenance(wide)$model, "best_match")
  })

  it("demotes a stale model in an explicit precedence too, but serves it when named alone", {
    root <- withr::local_tempdir()
    site <- fu13_archive(root)
    fu13_add_old_best_match(root)
    wide <- met_wide(site, fu13_window(), variables = "temperature_2m",
                     model = c("best_match", "ecmwf_ifs025"), now = prod_now())
    expect_equal(met_provenance(wide)$model, "ecmwf_ifs025")
    alone <- met_wide(site, fu13_window(), variables = "temperature_2m",
                      model = "best_match", now = prod_now())
    expect_equal(met_provenance(alone)$model, "best_match")
  })

  it("lets a stale model fill a variable no current model has", {
    root <- withr::local_tempdir()
    site <- fu13_archive(root)
    old <- fu13_add_old_best_match(root)
    # Only the stale run has this variable (relabel one of its series).
    extra <- old[old$variable == "temperature_2m", ]
    extra$variable <- "dewpoint_2m"
    store_write_forecast(root, extra, now = prod_now())
    fc <- store_read_forecast(root, "kat", source = "openmeteo")
    skip_if(any(fc$variable == "dewpoint_2m" & fc$model != "best_match"))
    wide <- met_wide(site, fu13_window(), variables = c("temperature_2m", "dewpoint_2m"),
                     now = prod_now())
    served <- stats::setNames(met_provenance(wide)$model, met_provenance(wide)$variable)
    expect_equal(served[["temperature_2m"]], "ecmwf_ifs025")
    expect_equal(served[["dewpoint_2m"]], "best_match")
  })
})
