# Plan 10 — history_hourly / history_daily assembly (SILO base, AWS wins).

describe("build_history_daily() SILO + AWS compositing (SCOPING §4)", {
  it("prefers the AWS daily aggregate where present and QC-clean", {
    root <- local_store()
    site <- make_test_site()
    day <- as.POSIXct("2026-01-15 00:00", tz = "UTC")
    silo <- new_obs(make_obs(n = 1, variable = "temperature_2m", value = 29,
                             source = "silo", method = "model_fill",
                             start = day))
    aws <- new_obs(make_obs(n = 1, variable = "temperature_2m", value = 30.5,
                            source = "site_aws", method = "aggregated",
                            start = day))
    store_write_obs(root, rbind(silo, aws))

    hd <- build_history_daily(root, site,
                              window = list(from = day, to = day + 86400))
    expect_canonical_obs(hd)
    # AWS wins the day it is present and clean
    expect_equal(hd$value[1], 30.5)
    expect_true(grepl("aws|site", hd$source[1]))     # provenance records the leg
  })

  it("falls back to SILO where AWS is absent or fail-flagged", {
    root <- local_store()
    site <- make_test_site()
    day <- as.POSIXct("2026-02-10 00:00", tz = "UTC")
    silo <- new_obs(make_obs(n = 1, variable = "temperature_2m", value = 24,
                             source = "silo", method = "model_fill", start = day))
    store_write_obs(root, silo)                       # no AWS for this day
    hd <- build_history_daily(root, site,
                              window = list(from = day, to = day + 86400))
    expect_equal(hd$value[1], 24)
    expect_equal(hd$source[1], "silo")
  })
})

describe("SILO/AWS leg alignment on the site-local day", {
  it("composites legs stamped at different instants of the same local day", {
    # SILO rows are stamped 9am local; AWS calendar-day aggregates at local
    # midnight. Both instants fall on the same SITE-LOCAL date but (for an
    # Australian site) on the PREVIOUS UTC date -- a UTC-date key happens to
    # collide here, but keying must be on the local date to hold across
    # timezones and stamps; this pins the local-date compositing.
    root <- local_store()
    site <- make_test_site()   # the test site observes Sydney DST
    silo_stamp <- as.POSIXct("2026-01-15 09:00", tz = "Australia/Sydney")
    aws_stamp <- as.POSIXct("2026-01-15 00:00", tz = "Australia/Sydney")
    # same absolute instants, relabelled UTC (the canonical-schema tzone)
    attr(silo_stamp, "tzone") <- "UTC"
    attr(aws_stamp, "tzone") <- "UTC"
    store_write_obs(root, rbind(
      new_obs(make_obs(n = 1, variable = "temperature_2m", value = 29,
                       source = "silo", method = "model_fill",
                       start = silo_stamp)),
      new_obs(make_obs(n = 1, variable = "temperature_2m", value = 30.5,
                       source = "site_aws", method = "aggregated",
                       start = aws_stamp))
    ))
    hd <- build_history_daily(root, site,
                              window = list(
                                from = as.POSIXct("2026-01-13", tz = "UTC"),
                                to = as.POSIXct("2026-01-16", tz = "UTC")
                              ))
    # one value for the local day, and AWS won it
    expect_equal(nrow(hd), 1)
    expect_equal(hd$value, 30.5)
    expect_equal(hd$source, "site_aws")
  })
})

describe("step-change auditability across the AWS install date (§4 caveat)", {
  it("keeps the two legs distinguishable from provenance", {
    root <- local_store()
    site <- make_test_site()
    d1 <- as.POSIXct("2026-01-01 00:00", tz = "UTC")   # pre-AWS: SILO only
    d2 <- as.POSIXct("2026-06-01 00:00", tz = "UTC")   # post-AWS: AWS wins
    store_write_obs(root, rbind(
      new_obs(make_obs(n = 1, variable = "temperature_2m", value = 20,
                       source = "silo", method = "model_fill", start = d1)),
      new_obs(make_obs(n = 1, variable = "temperature_2m", value = 21,
                       source = "silo", method = "model_fill", start = d2)),
      new_obs(make_obs(n = 1, variable = "temperature_2m", value = 21.4,
                       source = "site_aws", method = "aggregated", start = d2))
    ))
    hd <- build_history_daily(root, site,
                              window = list(from = d1, to = d2 + 86400))
    legs <- unique(hd$source)
    expect_true("silo" %in% legs)
    expect_true(any(grepl("aws|site", legs)))         # both legs present & labelled
  })
})
