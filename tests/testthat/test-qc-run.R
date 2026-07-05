# Plan 09 — qc_run() orchestration: idempotency, incrementality, supersede.

describe("qc_run() idempotency", {
  it("yields identical flags and no duplicate qc_log rows on a second run", {
    root <- local_store()
    site <- make_test_site()
    obs <- new_obs(qc_series("temperature_2m", c(15, 15.2, 40, 15.4)))  # one spike
    store_write_obs(root, obs, now = as.POSIXct("2026-01-02", tz = "UTC"))

    qc_run(root, site, now = as.POSIXct("2026-01-02", tz = "UTC"))
    flags1 <- store_read_obs(root, "test")$qc_flag
    log1 <- qc_log_read(root, "test")

    qc_run(root, site, now = as.POSIXct("2026-01-02", tz = "UTC"))
    flags2 <- store_read_obs(root, "test")$qc_flag
    log2 <- qc_log_read(root, "test")

    expect_equal(flags2, flags1)
    expect_equal(nrow(log2), nrow(log1))   # dedup on (site, time, variable, rule)
  })
})

describe("qc_run() incrementality", {
  it("advances the watermark and does not re-flag finalised earlier rows", {
    root <- local_store()
    site <- make_test_site()
    jan <- new_obs(qc_series("temperature_2m", rep(15, 5),
                             start = as.POSIXct("2026-01-01 00:00", tz = "UTC")))
    store_write_obs(root, jan)
    qc_run(root, site, now = as.POSIXct("2026-01-01 06:00", tz = "UTC"))
    wm1 <- store_get_watermark(root, "test", "observations", "qc")

    feb <- new_obs(qc_series("temperature_2m", rep(16, 5),
                             start = as.POSIXct("2026-02-01 00:00", tz = "UTC")))
    store_write_obs(root, feb)
    qc_run(root, site, now = as.POSIXct("2026-02-01 06:00", tz = "UTC"))
    wm2 <- store_get_watermark(root, "test", "observations", "qc")

    expect_true(wm2 > wm1)                 # watermark advanced past the look-back
  })
})

describe("qc_run() persists flag changes via supersede", {
  it("keeps the pre-QC flag retrievable with include_superseded = TRUE", {
    root <- local_store()
    site <- make_test_site()
    # A temperature spike within the dictionary range (so new_obs accepts it,
    # all rows flagged ok) that the step rule downgrades to suspect. The flag
    # transition ok→suspect must supersede, not overwrite, the original.
    obs <- new_obs(qc_series("temperature_2m", c(15, 15.2, 40, 15.4)))
    store_write_obs(root, obs, now = as.POSIXct("2026-01-02", tz = "UTC"))
    qc_run(root, site, now = as.POSIXct("2026-01-02", tz = "UTC"))

    current <- store_read_obs(root, "test")
    audited <- store_read_obs(root, "test", include_superseded = TRUE)
    expect_true(any(current$qc_flag == "suspect"))    # QC downgraded the spike
    expect_true(nrow(audited) > nrow(current))         # the pre-QC row retained
    expect_true("ok" %in% audited$qc_flag)             # original flag still on file
  })
})
