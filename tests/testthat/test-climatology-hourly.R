# DOY x hour-of-day climatology: the shrink target (and the verification
# climatology baseline) must resolve the diurnal cycle for sub-daily forecasts,
# not flatten every hour toward the daily mean.

# An hourly series with a pure diurnal cycle (no seasonal term), peaking near
# local midday: temp(hour) = 15 + 6*sin(2*pi*(hour - 6)/24).
diurnal_hourly_obs <- function(source = "site_aws",
                               from = as.POSIXct("2023-01-01 00:00", tz = "UTC"),
                               to = as.POSIXct("2025-12-31 23:00", tz = "UTC")) {
  times <- seq(from, to, by = "hour")
  hour <- as.integer(format(times, "%H", tz = "UTC"))
  tibble::tibble(
    site_id = "test", datetime_utc = times, variable = "temperature_2m",
    value = 15 + 6 * sin(2 * pi * (hour - 6) / 24),
    source = source, method = "measured", qc_flag = "ok"
  )
}

describe("baseline_climatology() hour-of-day conditioning", {
  it("resolves the diurnal cycle when hour_window is set", {
    hist <- diurnal_hourly_obs()
    at15 <- as.POSIXct("2024-06-15 15:00", tz = "UTC")
    at03 <- as.POSIXct("2024-06-15 03:00", tz = "UTC")

    c15 <- baseline_climatology(hist, at15, "temperature_2m", hour_window = 1L)$mean
    c03 <- baseline_climatology(hist, at03, "temperature_2m", hour_window = 1L)$mean

    # afternoon warmer than pre-dawn, tracking the diurnal cycle
    expect_gt(c15, 18)
    expect_lt(c03, 12)
    expect_gt(c15 - c03, 6)
  })

  it("stays DOY-only (flat across hours) when hour_window is NULL", {
    hist <- diurnal_hourly_obs()
    at15 <- as.POSIXct("2024-06-15 15:00", tz = "UTC")
    at03 <- as.POSIXct("2024-06-15 03:00", tz = "UTC")

    c15 <- baseline_climatology(hist, at15, "temperature_2m")$mean
    c03 <- baseline_climatology(hist, at03, "temperature_2m")$mean

    # both collapse to the daily mean (~15); the diurnal sine averages out
    expect_equal(c15, 15, tolerance = 0.5)
    expect_equal(c03, 15, tolerance = 0.5)
  })
})

describe(".climatology_series() hourly-aware shrink target", {
  it("tracks the diurnal cycle when hourly history exists", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    store_write_obs(root, new_obs(diurnal_hourly_obs()))

    times <- as.POSIXct(c("2024-06-15 15:00", "2024-06-15 03:00"), tz = "UTC")
    clim <- .climatology_series(root, site, times,
                                rep("temperature_2m", 2), fallback = c(0, 0))

    expect_gt(clim[[1]], 18)   # 15:00 afternoon
    expect_lt(clim[[2]], 12)   # 03:00 pre-dawn
  })

  it("falls back to the daily-DOY mean when no hourly history exists", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    # SILO daily-only history: one value per day, stamped 9am -- no diurnal
    # structure to resolve, so the hourly cell is empty and both hours collapse
    # to the same daily-DOY climatology (the pre-change behaviour, preserved).
    days <- seq(as.POSIXct("2023-06-01 09:00", tz = "UTC"),
                as.POSIXct("2025-07-01 09:00", tz = "UTC"), by = "day")
    store_write_obs(root, new_obs(tibble::tibble(
      site_id = "test", datetime_utc = days, variable = "temperature_2m",
      value = 20, source = "silo", method = "model_fill", qc_flag = "ok"
    )))

    times <- as.POSIXct(c("2024-06-15 15:00", "2024-06-15 03:00"), tz = "UTC")
    clim <- .climatology_series(root, site, times,
                                rep("temperature_2m", 2), fallback = c(-99, -99))

    expect_equal(clim[[1]], 20, tolerance = 0.5)   # daily-DOY mean, not the fallback
    expect_equal(clim[[2]], clim[[1]])             # flat across hours (no diurnal data)
  })
})
