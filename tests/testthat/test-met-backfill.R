# Plan 16 — met_backfill() (ad-hoc day-0 bootstrap + donor-coverage audit).

describe("day-0 bootstrap", {
  it("pulls history, ingests an AWS export, makes initial fits", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    now <- as.POSIXct("2026-01-01", tz = "UTC")
    ran <- new.env(); ran$history <- 0L; ran$ingest <- 0L; ran$fit <- 0L
    testthat::local_mocked_bindings(
      .acquire_obs = function(source, site, window, now = NULL, ...) {
        ran$history <- ran$history + 1L; new_obs(make_obs(n = 5, source = source))
      },
      ingest_aws_export = function(...) { ran$ingest <- ran$ingest + 1L
                                          new_obs(make_obs(n = 5, source = "site_aws")) },
      correct_refit = function(...) { ran$fit <- ran$fit + 1L; invisible() },
      station_coverage = function(...) tibble::tibble(variable = "temperature_2m",
                                                      completeness = 0.9)
    )
    summary <- met_backfill(site, now = now,
                            config = pipeline_config(
                              root, obs_sources = c("silo", "openmeteo")),
                            aws_export = test_path("_fixtures/file/logger-a.csv"))
    expect_true(ran$history > 0 && ran$ingest > 0 && ran$fit > 0)
    expect_s3_class(summary, "tbl_df")
  })
})

describe("donor-coverage audit (SCOPING §13)", {
  it("flags a variable with no nearby GHCNh coverage", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    testthat::local_mocked_bindings(
      .acquire_obs = function(...) new_obs(make_obs(n = 3)),
      ingest_aws_export = function(...) new_obs(make_obs(n = 3)),
      correct_refit = function(...) invisible(),
      station_coverage = function(...) tibble::tibble(
        variable = c("temperature_2m", "direct_radiation"),
        completeness = c(0.95, 0.0))          # no radiation donor nearby
    )
    summary <- met_backfill(site, now = as.POSIXct("2026-01-01", tz = "UTC"),
                            config = pipeline_config(root, obs_sources = "ghcnh"))
    audit <- summary$coverage[[1]] %||% summary
    expect_true(any(audit$completeness == 0))  # the gap is surfaced to the operator
  })
})

describe("forecast-gap backfillability", {
  it("reports BOM gaps non-backfillable while Open-Meteo gaps self-heal", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    testthat::local_mocked_bindings(
      .acquire_obs = function(...) new_obs(make_obs(n = 3)),
      .acquire_forecast = function(source, ...) new_forecast(make_forecast(n = 3)),
      ingest_aws_export = function(...) new_obs(make_obs(n = 3)),
      correct_refit = function(...) invisible(),
      station_coverage = function(...) tibble::tibble(variable = "temperature_2m",
                                                      completeness = 1)
    )
    summary <- met_backfill(site, now = as.POSIXct("2026-01-01", tz = "UTC"),
                            config = pipeline_config(
                              root, forecast_sources = c("openmeteo", "bom_forecast")))
    notes <- paste(unlist(summary), collapse = " ")
    expect_match(notes, "bom", ignore.case = TRUE)
    expect_match(notes, "backfill|self-heal", ignore.case = TRUE)
  })
})
