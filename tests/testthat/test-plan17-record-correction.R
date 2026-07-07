# Plan 17 — BDD specs for site-correcting the SILO leg of history_daily (item 2).

describe("item 2: build_history_daily site-corrects the SILO leg", {
  it("applies the (variable, silo) calibration to SILO-served days", {
    skip("plan 17 item 2: SILO daily QM in history_daily — un-skip when implementing")

    root <- local_store()
    site <- make_test_site(store_root = root)

    # A qmap calibration on (temperature_2m, silo) that adds ~+5.
    pairs <- forecast_obs_pairs(n = 400, bias_fun = function(doy, lead) -5)
    coeffs <- fit_qmap(pairs)
    calib_write(root, "test", "temperature_2m", "silo", "qmap", coeffs,
                meta = list(train_start = min(pairs$issue_time),
                            train_end = max(pairs$issue_time), n_pairs = nrow(pairs)))

    day <- as.POSIXct("2026-02-10 09:00", tz = "Australia/Sydney")
    attr(day, "tzone") <- "UTC"
    silo <- new_obs(make_obs(n = 1, variable = "temperature_2m", value = 20,
                             source = "silo", method = "model_fill", start = day))
    store_write_obs(root, silo)

    hd <- build_history_daily(root, site,
                              window = list(from = day - 2 * 86400, to = day + 86400))
    expect_equal(hd$value[1], 25, tolerance = 0.75)   # corrected, not raw 20
    expect_true("tier" %in% names(hd))
    expect_equal(hd$tier[1], "qmap")
  })

  it("keeps a QC-clean AWS-served day as raw measured truth (tier raw)", {
    skip("plan 17 item 2: AWS leg is raw truth — un-skip when implementing")

    root <- local_store()
    site <- make_test_site(store_root = root)
    day <- as.POSIXct("2026-02-10 00:00", tz = "Australia/Sydney")
    attr(day, "tzone") <- "UTC"
    store_write_obs(root, rbind(
      new_obs(make_obs(n = 1, variable = "temperature_2m", value = 20,
                       source = "silo", method = "model_fill", start = day)),
      new_obs(make_obs(n = 1, variable = "temperature_2m", value = 30,
                       source = "site_aws", method = "aggregated", start = day))
    ))
    hd <- build_history_daily(root, site,
                              window = list(from = day - 2 * 86400, to = day + 86400))
    expect_equal(hd$value[1], 30)         # AWS wins where clean
    expect_equal(hd$tier[1], "raw")       # measured truth carries no model correction
  })

  it("leaves the SILO leg unchanged when no calibration exists", {
    skip("plan 17 item 2: no-calib SILO stays raw — un-skip when implementing")

    root <- local_store()
    site <- make_test_site(store_root = root)
    day <- as.POSIXct("2026-02-10 09:00", tz = "Australia/Sydney")
    attr(day, "tzone") <- "UTC"
    store_write_obs(root, new_obs(make_obs(n = 1, variable = "temperature_2m",
                                           value = 20, source = "silo",
                                           method = "model_fill", start = day)))
    hd <- build_history_daily(root, site,
                              window = list(from = day - 2 * 86400, to = day + 86400))
    expect_equal(hd$value[1], 20)
  })
})
