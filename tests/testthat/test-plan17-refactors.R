# Plan 17 — BDD specs for the three behaviour-preserving refactors: hoisting
# qc_run/fill_run out of the live per-source loop (item 11), vectorising the
# aggregators (item 12), and resetting the met_table downgrade flag defensively
# (item 13).

describe("item 11: met_sync_live QCs and fills once, not once per obs source", {
  it("invokes qc_run a single time per site regardless of obs-source count", {

    root <- local_store()
    site <- make_test_site(store_root = root)
    now <- as.POSIXct("2026-01-01 12:00", tz = "UTC")
    counts <- new.env()
    counts$qc <- 0L
    counts$fill <- 0L

    mock_acquisition()
    testthat::local_mocked_bindings(
      qc_run = function(...) {
        counts$qc <- counts$qc + 1L
        invisible()
      },
      fill_run = function(...) {
        counts$fill <- counts$fill + 1L
        invisible()
      },
      archive_forecasts = function(...) tibble::tibble(note = "ok")
    )

    config <- list(store_root = root,
                   obs_sources = c("site_aws", "silo"),
                   forecast_sources = "openmeteo")
    met_sync_live(site, now = now, config = config)

    expect_equal(counts$qc, 1L)
    expect_equal(counts$fill, 1L)
  })
})

describe("item 12: vectorised aggregation is output-equivalent", {
  it("aggregates each (site, variable, bucket) to the same value as before", {
    # Two sites, one hour, six 10-minute temperature samples each.
    mk <- function(sid, base) {
      times <- as.POSIXct("2026-01-01 00:00", tz = "UTC") + (0:5) * 600
      tibble::tibble(
        site_id = sid, datetime_utc = times, variable = "temperature_2m",
        value = base + (0:5), source = "aws", method = "measured", qc_flag = "ok"
      )
    }
    obs <- rbind(mk("a", 10), mk("b", 20))
    hourly <- aggregate_hourly(obs, dict = met_variables())

    expect_setequal(hourly$site_id, c("a", "b"))
    expect_equal(hourly$value[hourly$site_id == "a"], mean(10:15))
    expect_equal(hourly$value[hourly$site_id == "b"], mean(20:25))
  })
})

describe("item 13: a failed bind_rows does not poison the next met_table combine", {
  it("keeps the class and stays silent on a clean combine after a leaked flag", {
    mk <- function() {
      wide <- tibble::tibble(time = as.POSIXct("2026-01-01", tz = "UTC"),
                             temperature_2m = 20)
      prov <- tibble::tibble(variable = "temperature_2m", tier = "raw",
                             train_overlap = 0, source = "openmeteo")
      new_met_table(wide, provenance = prov, keys = list(site_id = "test"),
                    versions = list(schema_version = "1.0.0",
                                    calibration_manifest_version = 0L))
    }

    # Simulate a stale flag left TRUE by an aborted prior combine.
    withr::defer(assign("downgrade_pending", FALSE, envir = .met_table_state))
    .met_table_state$downgrade_pending <- TRUE

    out <- expect_no_warning(dplyr::bind_rows(mk(), mk()))
    expect_s3_class(out, "met_table")
  })
})
