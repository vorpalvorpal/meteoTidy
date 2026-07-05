# Plan 10 — native→hourly and hourly→local-day daily aggregation.

describe("aggregate_hourly() per-class rules", {
  it("sums rain, means temperature, and vector-means direction", {
    rain <- native_10min("precipitation", rep(0.5, 6))            # 6×0.5 = 3.0 mm
    temp <- native_10min("temperature_2m", seq(15, 20, length.out = 6))
    dir  <- native_10min("wind_direction_10m", c(350, 10, 350, 10, 355, 5))
    out <- aggregate_hourly(rbind(rain, temp, dir), dict = met_variables())

    r <- out$value[out$variable == "precipitation"]
    t <- out$value[out$variable == "temperature_2m"]
    d <- out$value[out$variable == "wind_direction_10m"]
    expect_equal(r, 3.0, tolerance = 1e-9)                        # summed
    expect_equal(t, mean(seq(15, 20, length.out = 6)), tolerance = 1e-9)
    expect_true(d < 20 || d > 340)                               # vector mean ~0°
    expect_true(all(out$method == "aggregated"))
  })

  it("leaves an hour below the completeness threshold missing", {
    partial <- native_10min("temperature_2m", c(15, 16))          # only 2 of 6
    out <- aggregate_hourly(partial, dict = met_variables())
    expect_true(nrow(out) == 0 ||
                  all(out$qc_flag == "missing" | is.na(out$value)))
  })
})

describe("aggregate_daily() on the local-day boundary (DST-aware)", {
  # SCOPING §3: daily boundary is local clock time in the site IANA zone; DST
  # transition days are 23-/25-hour days by construction. Site tz is
  # Australia/Sydney: DST starts 1st Sun Oct (→23h day), ends 1st Sun Apr (→25h).
  hourly_span <- function(from_utc, to_utc, variable, value_fun) {
    times <- seq(as.POSIXct(from_utc, tz = "UTC"),
                 as.POSIXct(to_utc, tz = "UTC"), by = "hour")
    tibble::tibble(
      site_id = "test", datetime_utc = times, variable = variable,
      value = value_fun(seq_along(times)), source = "test_src",
      method = "aggregated", qc_flag = "ok"
    )
  }

  it("builds a 25-hour rain-day across the April DST end", {
    site <- make_test_site()   # Australia/Sydney
    # cover the local rain-day 2026-04-05 09:00 → 2026-04-06 09:00 (25 h)
    hourly <- hourly_span("2026-04-04 20:00", "2026-04-06 00:00",
                          "precipitation", function(i) rep(1, length(i)))
    daily <- aggregate_daily(hourly, site)
    day <- daily[format(daily$datetime_utc, "%Y-%m-%d") %in%
                   c("2026-04-04", "2026-04-05"), ]
    # the transition rain-day accumulates 25 contributing hours (1 mm each)
    expect_true(any(daily$value == 25))
  })

  it("builds a 23-hour day across the October DST start", {
    site <- make_test_site()
    hourly <- hourly_span("2026-10-03 20:00", "2026-10-05 00:00",
                          "precipitation", function(i) rep(1, length(i)))
    daily <- aggregate_daily(hourly, site)
    expect_true(any(daily$value == 23))
  })
})
