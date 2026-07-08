# Plan 12 — post-correction physical-consistency pass (clip + count), sharing
# Plan 09's physics-constraints module in 'enforce' mode.

describe("consistency_pass() clips and counts violations", {
  it("clips gusts<wind, dewpoint>temp, RH>100, direct+diffuse>ceiling", {
    wide <- tibble::tibble(
      site_id = "test",
      datetime_utc = as.POSIXct("2026-01-01", tz = "UTC"),
      temperature_2m = 20, dewpoint_2m = 25, # dewpoint exceeds temp
      relative_humidity_2m = 130, # RH exceeds 100
      wind_speed_10m = 8, wind_gusts_10m = 5, # gust below wind
      direct_radiation = 1200, diffuse_radiation = 400,
      clear_sky_ceiling = 1000 # sum exceeds ceiling
    )
    out <- consistency_pass(wide)
    expect_lte(out$result$dewpoint_2m, out$result$temperature_2m)
    expect_lte(out$result$relative_humidity_2m, 100)
    expect_gte(out$result$wind_gusts_10m, out$result$wind_speed_10m)
    expect_lte(out$result$direct_radiation + out$result$diffuse_radiation,
               out$result$clear_sky_ceiling)
    expect_gt(out$n_violations, 0)                    # violations counted
  })

  it("leaves a clean set unchanged with a zero violation count", {
    wide <- tibble::tibble(
      site_id = "test",
      datetime_utc = as.POSIXct("2026-01-01", tz = "UTC"),
      temperature_2m = 20, dewpoint_2m = 12,
      relative_humidity_2m = 60,
      wind_speed_10m = 5, wind_gusts_10m = 9
    )
    out <- consistency_pass(wide)
    expect_equal(out$n_violations, 0)
    expect_equal(out$result$wind_gusts_10m, 9)
  })
})

describe("shared module parity with Plan 09", {
  it("uses the same physics-constraints relations in flag and enforce modes", {
    row <- qc_wide_row(wind_speed_10m = 8, wind_gusts_10m = 5)
    flagged <- physics_constraints(row, mode = "flag")
    enforced <- physics_constraints(row, mode = "enforce")
    # flag mode marks the violation; enforce mode fixes the SAME relation
    expect_true(isTRUE(flagged$violated) || any(grepl("suspect", unlist(flagged))))
    expect_gte(enforced$wind_gusts_10m, enforced$wind_speed_10m)
  })
})
