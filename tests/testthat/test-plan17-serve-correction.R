# Plan 17 — BDD specs for serve-time FORECAST correction (item 1) and the
# mean-bias valid-time keying fix (item 7).
#
# Every it() starts with skip(); delete that one line when you implement the
# named item and make the block pass without weakening the assertion.

describe("item 1: met_wide(kind='forecast') applies the current calibration", {
  it("serves corrected values and truthful provenance tiers", {
    skip("plan 17 item 1: correct_forecast() + met_wide honesty — un-skip when implementing")

    root <- local_store()
    site <- make_test_site(store_root = root)

    # A qmap calibration that adds ~+5 (obs = forecast + 5 in the training pairs).
    pairs <- forecast_obs_pairs(n = 400, bias_fun = function(doy, lead) -5)
    coeffs <- fit_qmap(pairs)
    calib_write(root, "test", "temperature_2m", "openmeteo", "qmap", coeffs,
                meta = list(train_start = min(pairs$issue_time),
                            train_end = max(pairs$issue_time),
                            n_pairs = nrow(pairs)))

    issue <- as.POSIXct("2026-01-01 00:00", tz = "UTC")
    fc <- rbind(
      make_forecast(n = 1, variable = "temperature_2m", source = "openmeteo",
                    value = 10, issue_time = issue),
      make_forecast(n = 1, variable = "wind_speed_80m", source = "openmeteo",
                    value = 8, issue_time = issue),      # model-only, stays raw
      make_forecast(n = 1, variable = "surface_pressure", source = "openmeteo",
                    value = 1000, issue_time = issue)     # no calibration, stays physical
    )
    store_write_forecast(root, new_forecast(fc))
    valid <- fc$valid_time[fc$variable == "temperature_2m"][1]

    out <- met_wide(
      site,
      window = list(from = valid - 3600, to = valid + 3600),
      kind = "forecast",
      variables = c("temperature_2m", "wind_speed_80m", "surface_pressure"),
      now = issue
    )

    # corrected, not the raw archive value of 10
    expect_equal(as.numeric(out$temperature_2m), 15, tolerance = 0.75)

    prov <- met_provenance(out)
    tier_of <- function(v) prov$tier[prov$variable == v]
    expect_equal(tier_of("temperature_2m"), "qmap")     # applied, not merely claimed
    expect_equal(tier_of("wind_speed_80m"), "raw")      # model-only
    expect_equal(tier_of("surface_pressure"), "physical") # no calibration on file
  })

  it("leaves values untouched when no calibration exists (tier physical)", {
    skip("plan 17 item 1: correct_forecast() no-calib path — un-skip when implementing")

    root <- local_store()
    site <- make_test_site(store_root = root)
    issue <- as.POSIXct("2026-01-01 00:00", tz = "UTC")
    fc <- make_forecast(n = 1, variable = "temperature_2m", source = "openmeteo",
                        value = 12, issue_time = issue)
    store_write_forecast(root, new_forecast(fc))
    valid <- fc$valid_time[1]

    out <- met_wide(site, window = list(from = valid - 3600, to = valid + 3600),
                    kind = "forecast", variables = "temperature_2m", now = issue)
    expect_equal(as.numeric(out$temperature_2m), 12)
    expect_equal(met_provenance(out)$tier, "physical")
  })
})

describe("item 1c: met_sync_live no longer computes-and-discards corrections", {
  it("does not call correct_apply inside the live sync", {
    skip("plan 17 item 1c: remove dead correct_apply calls — un-skip when implementing")

    root <- local_store()
    site <- make_test_site(store_root = root)
    now <- as.POSIXct("2026-01-01 12:00", tz = "UTC")
    mock_acquisition()
    testthat::local_mocked_bindings(
      qc_run = function(...) invisible(),
      fill_run = function(...) invisible(),
      archive_forecasts = function(...) tibble::tibble(note = "ok"),
      correct_apply = function(...) stop("correct_apply must not be called by met_sync_live")
    )
    expect_no_error(
      met_sync_live(site, now = now, config = pipeline_config(root))
    )
  })
})

describe("item 7: mean-bias harmonics track the value's own (valid) time", {
  it("recovers and removes a bias that is a function of valid hour-of-day", {
    skip("plan 17 item 7: key harmonics on valid_time — un-skip when implementing")

    withr::local_seed(1)
    d <- 0:239
    issue <- as.POSIXct("2025-03-01 00:00", tz = "UTC") + d * 86400  # issue hour const
    lead_h <- d %% 24                                                # valid hour cycles 0..23
    valid <- issue + lead_h * 3600
    valid_hod <- as.integer(format(valid, "%H", tz = "UTC"))
    truth <- 15 + stats::rnorm(length(d), 0, 0.3)
    bias <- 3 * sin(2 * pi * valid_hod / 24)
    forecast <- truth - bias      # forecast biased by a diurnal term keyed on VALID hour

    pairs <- tibble::tibble(
      issue_time = issue, valid_time = valid,
      lead_time = as.difftime(as.numeric(lead_h), units = "hours"),
      variable = "temperature_2m", forecast = forecast, observation = truth
    )

    coeffs <- fit_mean_bias(pairs)
    newdata <- tibble::tibble(issue_time = issue, valid_time = valid, forecast = forecast)
    corrected <- apply_mean_bias(coeffs, newdata)$value

    # Keyed on valid time, the diurnal bias is captured and removed; keyed on the
    # (constant) issue hour it cannot be represented, so this stays biased.
    expect_lt(mean(abs(truth - corrected)), 0.5)
  })
})
