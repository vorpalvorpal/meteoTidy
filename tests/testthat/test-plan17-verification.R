# Plan 17 — BDD specs for the enriched verification report (item 5) and the
# verified per-lead shrink weight it feeds (item 1b).

# Seed several years of daily SILO obs (for the climatology baseline) plus a run
# of recent daily forecasts overlapping the most recent obs (for scoring).
seed_verification_store <- function(root, fc_source = "openmeteo") {
  obs_days <- seq(as.POSIXct("2023-01-01", tz = "UTC"),
                  as.POSIXct("2026-01-31", tz = "UTC"), by = "day")
  doy <- as.integer(format(obs_days, "%j"))
  obs <- tibble::tibble(
    site_id = "test", datetime_utc = obs_days, variable = "temperature_2m",
    value = 15 + 10 * sin(2 * pi * doy / 365.25), source = "silo",
    method = "model_fill", qc_flag = "ok"
  )
  store_write_obs(root, new_obs(obs))

  fc_days <- seq(as.POSIXct("2025-07-01", tz = "UTC"),
                 as.POSIXct("2026-01-31", tz = "UTC"), by = "day")
  fdoy <- as.integer(format(fc_days, "%j"))
  archive <- tibble::tibble(
    site_id = "test", source = fc_source, model = "ecmwf_ifs025",
    issue_time = fc_days - 24 * 3600, valid_time = fc_days,
    lead_time = as.difftime(24, units = "hours"),
    member = NA_integer_, stat = NA_character_, variable = "temperature_2m",
    value = 15 + 10 * sin(2 * pi * fdoy / 365.25) + 4   # a +4 bias raw beats neither cleanly
  )
  store_write_forecast(root, new_forecast(archive))
  invisible(NULL)
}

describe("item 5: verify_run scores against baselines, not just before/after", {
  it("emits raw, persistence, and climatology report rows", {
    skip("plan 17 item 5: baselines in verify_run — un-skip when implementing")

    root <- local_store()
    site <- make_test_site(store_root = root)
    seed_verification_store(root)

    verify_run(root, site, sources = "openmeteo",
               now = as.POSIXct("2026-02-01", tz = "UTC"))
    report <- read_verification_report(root, "test")

    expect_true(all(c("raw", "persistence", "climatology") %in% report$tier))
    # baselines are scored out-of-sample like everything else
    expect_true(all(report$n_pairs[report$tier == "climatology"] > 0))
  })

  it("writes ensemble calibration diagnostics for a member-carrying source", {
    skip("plan 17 item 5: rank/spread/Brier diagnostics — un-skip when implementing")

    root <- local_store()
    site <- make_test_site(store_root = root)

    valids <- seq(as.POSIXct("2025-11-01", tz = "UTC"),
                  by = "day", length.out = 60)
    members <- 1:5
    rows <- do.call(rbind, lapply(members, function(m) {
      tibble::tibble(
        site_id = "test", source = "openmeteo", model = "ecmwf_ifs025",
        issue_time = valids - 24 * 3600, valid_time = valids,
        lead_time = as.difftime(24, units = "hours"),
        member = as.integer(m), stat = NA_character_, variable = "temperature_2m",
        value = 15 + m * 0.2 + stats::rnorm(length(valids), 0, 1)
      )
    }))
    store_write_forecast(root, new_forecast(rows))
    store_write_obs(root, new_obs(tibble::tibble(
      site_id = "test", datetime_utc = valids, variable = "temperature_2m",
      value = 15, source = "silo", method = "model_fill", qc_flag = "ok"
    )))

    verify_run(root, site, sources = "openmeteo",
               now = as.POSIXct("2026-01-01", tz = "UTC"))
    diag <- read_verification_diagnostics(root, "test")
    expect_gt(nrow(diag), 0)
    expect_true(is.finite(diag$spread_error_ratio[[1]]))
  })
})

describe("item 1b: serve_shrink_weight from verified per-lead skill", {
  it("falls back to a tier-based weight before any report exists", {
    skip("plan 17 item 1b: serve_shrink_weight fallback — un-skip when implementing")

    root <- local_store()
    # No verification report yet: a fitted tier is trusted (weight 1); a raw /
    # physical tier is fully shrunk toward climatology (weight 0).
    expect_equal(serve_shrink_weight(root, "test", "openmeteo", "temperature_2m",
                                     "d5", tier = "qmap"), 1)
    expect_equal(serve_shrink_weight(root, "test", "openmeteo", "temperature_2m",
                                     "d5", tier = "physical"), 0)
  })
})
