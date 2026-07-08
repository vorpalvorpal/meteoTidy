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
    site <- make_test_site()   # the test site observes Sydney DST
    # DST ends 03:00 AEDT on 2026-04-05, so the 25-hour rain-day is the
    # window from 2026-04-04 09:00 AEDT to 2026-04-05 09:00 AEST -- i.e.
    # 2026-04-03 22:00 UTC to 2026-04-04 23:00 UTC -- assigned to Apr 5
    # (the day of observation).
    hourly <- hourly_span("2026-04-03 20:00", "2026-04-05 00:00",
                          "precipitation", function(i) rep(1, length(i)))
    daily <- aggregate_daily(hourly, site)
    # the transition rain-day accumulates 25 contributing hours (1 mm each)
    expect_true(any(daily$value == 25))
  })

  it("builds a 23-hour day across the October DST start", {
    site <- make_test_site()
    # DST starts 02:00 AEST on 2026-10-04, so the 23-hour rain-day is the
    # window [2026-10-03 09:00 AEST, 2026-10-04 09:00 AEDT), assigned Oct 4.
    hourly <- hourly_span("2026-10-02 20:00", "2026-10-04 00:00",
                          "precipitation", function(i) rep(1, length(i)))
    daily <- aggregate_daily(hourly, site)
    expect_true(any(daily$value == 23))
  })

  it("assigns the rain day to the DAY OF OBSERVATION, stamped at 9am local", {
    # SILO's documented convention (plans/10, SCOPING §3): rainfall in the
    # 24 h ending 9am local on day D is assigned to day D -- the window END,
    # not its start -- and the SILO ingest stamps day D at 9am local
    # (.silo_day_to_utc_instant()), so the AWS leg must use the identical
    # label + instant for history_daily's compositing to pair the same
    # physical window.
    site <- make_test_site()   # Australia/Sydney (AEDT in January, UTC+11)
    # 08:00 local on Jan 15 = 21:00 UTC Jan 14 -> rain day Jan 15
    before_9am <- tibble::tibble(
      site_id = "test",
      datetime_utc = as.POSIXct("2026-01-14 21:00", tz = "UTC"),
      variable = "precipitation", value = 4, source = "site_aws",
      method = "aggregated", qc_flag = "ok"
    )
    # 10:00 local on Jan 15 = 23:00 UTC Jan 14 -> rain day Jan 16
    after_9am <- before_9am
    after_9am$datetime_utc <- as.POSIXct("2026-01-14 23:00", tz = "UTC")
    after_9am$value <- 7

    daily <- aggregate_daily(rbind(before_9am, after_9am), site)
    # relabel to UTC (same tzone as daily$datetime_utc) so the comparison does
    # not trip base::Ops.POSIXt's "inconsistent tzone" warning -- these are the
    # same absolute instants either way.
    to_utc <- function(x) {
      attr(x, "tzone") <- "UTC"
      x
    }
    expected_15 <- to_utc(as.POSIXct("2026-01-15 09:00", tz = "Australia/Sydney"))
    expected_16 <- to_utc(as.POSIXct("2026-01-16 09:00", tz = "Australia/Sydney"))
    expect_equal(daily$value[daily$datetime_utc == expected_15], 4)
    expect_equal(daily$value[daily$datetime_utc == expected_16], 7)
  })
})
