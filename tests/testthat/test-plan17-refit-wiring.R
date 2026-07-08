# Plan 17 — BDD specs for fitting forecast-source calibrations in met_refit
# (item 3) and comparing a candidate refit against the fitted incumbent (item 6).

# Seed the store with an archived forecast + matching QC-clean observations from
# a forecast_obs_pairs() frame, so correct_refit() has real overlap to fit on.
seed_pairs_into_store <- function(root, pairs, fc_source = "openmeteo") {
  archive <- tibble::tibble(
    site_id = "test", source = fc_source, model = "ecmwf_ifs025",
    issue_time = pairs$issue_time, valid_time = pairs$valid_time,
    lead_time = pairs$lead_time, member = NA_integer_, stat = NA_character_,
    variable = pairs$variable, value = pairs$forecast
  )
  store_write_forecast(root, new_forecast(archive))

  obs <- tibble::tibble(
    site_id = "test", datetime_utc = pairs$valid_time, variable = pairs$variable,
    value = pairs$observation, source = "site_aws", method = "measured",
    qc_flag = "ok"
  )
  store_write_obs(root, new_obs(obs))
  invisible(NULL)
}

describe("item 3: met_refit fits calibrations for forecast sources", {
  it("writes a calibration for an archived forecast source", {

    root <- local_store()
    site <- make_test_site(store_root = root)
    pairs <- forecast_obs_pairs(n = 400, bias_fun = function(doy, lead) -8)  # strong, learnable
    seed_pairs_into_store(root, pairs, fc_source = "openmeteo")

    config <- list(store_root = root, obs_sources = "site_aws",
                   forecast_sources = "openmeteo")
    met_refit(site, now = as.POSIXct("2026-06-01", tz = "UTC"), config = config)

    man <- calib_manifest(root, "test")
    row <- man[man$variable == "temperature_2m" & man$source == "openmeteo", ]
    expect_gt(nrow(row), 0)
    expect_true(row$tier[[1]] %in% c("mean_bias", "qmap", "emos"))
  })
})

describe("item 6: promotion must beat the fitted incumbent, not merely raw", {
  it("does not advance the version when the candidate cannot beat the incumbent", {

    root <- local_store()
    site <- make_test_site(store_root = root)
    pairs <- forecast_obs_pairs(n = 400, bias_fun = function(doy, lead) -8)
    seed_pairs_into_store(root, pairs, fc_source = "openmeteo")

    # An incumbent qmap that already removes the bias this data carries.
    incumbent <- fit_qmap(pairs)
    calib_write(root, "test", "temperature_2m", "openmeteo", "qmap", incumbent,
                meta = list(train_start = min(pairs$issue_time),
                            train_end = max(pairs$issue_time), n_pairs = nrow(pairs)))
    v0 <- max(calib_manifest(root, "test")$version)

    config <- list(store_root = root, obs_sources = "site_aws",
                   forecast_sources = "openmeteo")
    met_refit(site, now = as.POSIXct("2026-06-01", tz = "UTC"), config = config)

    # A re-fit of the same tier on the same data is no better out-of-sample, so
    # the skill gate keeps the incumbent and the version does not advance.
    v1 <- max(calib_manifest(root, "test")$version)
    expect_equal(v1, v0)
  })
})
