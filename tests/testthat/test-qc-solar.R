# Plan 09 — solar clear-sky QC (Ineichen–Perez built-in + BSRN-style limits).

# Build a radiation series at explicit local-referenced UTC instants so we can
# place a value at solar noon and one at local midnight.
solar_obs <- function(times_utc, value, variable = "direct_radiation") {
  tibble::tibble(
    site_id = "test",
    datetime_utc = as.POSIXct(times_utc, tz = "UTC"),
    variable = variable,
    value = as.double(value),
    source = "test_src",
    method = "measured",
    qc_flag = "ok"
  )
}

describe("qc_solar() physical limits", {
  it("fails a value above the physically-possible clear-sky limit", {
    # Australia/Sydney solar noon ~ 02:00 UTC in January. 3000 W/m² is impossible.
    obs <- solar_obs("2026-01-01 02:00:00", 3000)
    out <- qc_solar(obs, site = make_test_site())
    expect_equal(out$qc_flag, "fail")
  })

  it("fails nonzero radiation at night", {
    # Local midnight ~ 13:00 UTC; the sun is well below the horizon.
    obs <- solar_obs("2026-01-01 13:00:00", 200)
    out <- qc_solar(obs, site = make_test_site())
    expect_equal(out$qc_flag, "fail")
  })

  it("leaves a normal clear-day value ok", {
    obs <- solar_obs("2026-01-01 02:00:00", 600)  # plausible near solar noon
    out <- qc_solar(obs, site = make_test_site())
    expect_equal(out$qc_flag, "ok")
  })
})

describe("clear-sky model determinism", {
  it("computes the same clear-sky irradiance for a fixed site/time (snapshot)", {
    times <- c("2026-01-01 00:00:00", "2026-01-01 02:00:00",
               "2026-01-01 04:00:00", "2026-01-01 13:00:00")
    ghi <- clear_sky_irradiance(make_test_site(),
                                as.POSIXct(times, tz = "UTC"))
    expect_snapshot(round(as.numeric(ghi), 1))
  })
})
