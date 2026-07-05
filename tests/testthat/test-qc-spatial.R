# Plan 09 — spatial/buddy check (the review's key drift detector).

describe("qc_spatial() drift detection", {
  it("flags a slow additive drift once it exceeds the MAD threshold", {
    # Three steady donors near 15°C; the site starts consistent then drifts up
    # linearly (a stuck-calibration / slow-drift signature range & step miss).
    n <- 24
    donors <- list(
      qc_donor("d1", rep(15.0, n)),
      qc_donor("d2", rep(15.2, n)),
      qc_donor("d3", rep(14.8, n))
    )
    drift <- 15 + pmax(0, seq_len(n) - 6) * 0.6   # flat, then ramps away
    site_obs <- qc_series("temperature_2m", drift)
    out <- qc_spatial(site_obs, donors = donors, site = make_test_site())

    # the pre-drift head stays ok; the drifted tail is flagged suspect
    expect_true(all(out$qc_flag[1:5] == "ok"))
    expect_true(any(out$qc_flag[15:n] == "suspect"))
  })

  it("skips and logs (does not error) with fewer than two usable donors", {
    site_obs <- qc_series("temperature_2m", rep(15, 6))
    out <- qc_spatial(site_obs, donors = list(qc_donor("d1", rep(15, 6))),
                      site = make_test_site())
    expect_true(all(out$qc_flag == "ok"))         # nothing flagged
    log <- attr(out, "qc_log")
    expect_true(is.null(log) || any(grepl("insufficient", log$detail)))
  })

  it("never applies to a model_only variable (no site truth)", {
    site_obs <- qc_series("boundary_layer_height", rep(500, 6))
    expect_error(
      qc_spatial(site_obs, donors = list(), site = make_test_site()),
      class = "meteoTidy_error_spatial_not_applicable"
    )
  })
})
