# Plan 06 — source_ghcnh() over worldmet::import_ghcn_hourly. Mocked; no live.

describe("source_ghcnh() fetch → canonical hourly obs", {
  it("maps a worldmet frame to canonical measured obs", {
    site <- make_test_site()
    site <- site_set_resolved(site, c("ghcnh", "station_id"), "ASN00072150")
    adapter <- source_ghcnh()
    win <- list(from = as.POSIXct("2026-06-01", tz = "UTC"),
                to = as.POSIXct("2026-06-02", tz = "UTC"))
    with_mocked_ghcnh(make_ghcnh_frame(), {
      out <- fetch(adapter, site, "temperature_2m", win)
    })
    expect_canonical_obs(out)
    expect_true(all(out$method == "measured"))
    expect_true(all(out$source == "ghcnh"))
  })
})

describe("source_ghcnh() resolve_station()", {
  it("records the nearest station id and its distance_km", {
    site <- make_test_site()
    adapter <- source_ghcnh()
    cat <- make_station_catalogue()
    resolved <- with_mocked_ghcnh(cat, resolve_station(adapter, site))
    expect_equal(site_resolved(resolved, c("ghcnh", "station_id")), "near")
    d <- site_resolved(resolved, c("ghcnh", "distance_km"))
    expect_true(is.numeric(d) && d >= 0)
  })

  it("returns n distinct stations ordered by distance for n = 3", {
    site <- make_test_site()
    adapter <- source_ghcnh()
    cat <- make_station_catalogue()
    resolved <- with_mocked_ghcnh(cat, resolve_station(adapter, site, n = 3))
    ids <- site_resolved(resolved, c("ghcnh", "station_ids"))
    expect_length(unique(ids), 3)
    expect_equal(ids, c("near", "mid", "far"))    # ascending distance
  })
})

describe("source_ghcnh() cadence metadata", {
  it("marks the ~1-week lag so the pipeline won't use it for the live head", {
    # SCOPING §5.1/§13: GHCNh is updated daily but publishes no real-time
    # latency figure — best-effort backfill only. Plan 16 reads this.
    adapter <- source_ghcnh()
    expect_false(adapter@cadence$live)            # not a live-head source
    expect_true(adapter@cadence$lag_days >= 1)
  })
})

describe("station_coverage()", {
  it("returns per-variable completeness for a fixture window", {
    site <- make_test_site()
    site <- site_set_resolved(site, c("ghcnh", "station_id"), "ASN00072150")
    adapter <- source_ghcnh()
    win <- list(from = as.POSIXct("2026-06-01 00:00", tz = "UTC"),
                to = as.POSIXct("2026-06-01 05:00", tz = "UTC"))
    # 3 of 6 expected hours present → 50% completeness
    with_mocked_ghcnh(make_ghcnh_frame(n = 3), {
      cov <- station_coverage(adapter, site, win)
    })
    expect_true(all(c("variable", "completeness") %in% names(cov)))
    expect_true(all(cov$completeness >= 0 & cov$completeness <= 1))
    temp_cov <- cov$completeness[cov$variable == "temperature_2m"]
    expect_equal(temp_cov, 0.5, tolerance = 1e-9)
  })
})
