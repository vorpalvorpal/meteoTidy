# Plan 07 — circuit-breaker state (persists across runs; strikes; cooldown).

describe("breaker strike / trip / reset", {
  it("trips a rung after three consecutive persistent failures and skips it", {
    root <- local_store()
    counter <- new.env()
    ladder <- list(
      fake_transport("ftp_feeds", "obs_72h", "gone", counter),
      fake_transport("web_api",   "obs_72h", bom_rows(), counter)
    )
    now <- as.POSIXct("2026-01-01", tz = "UTC")
    # three failing fetches accrue three strikes on ftp_feeds
    for (i in 1:3) {
      br <- breaker_read(root)
      out <- ladder_fetch(ladder, bom_request(), br, now = now + i * 3600)
      breaker_write(root, attr(out, "breaker") %||% br)
    }
    tripped <- breaker_read(root)
    expect_true(breaker_tripped(tripped, "ftp_feeds", threshold = 3))

    # on the next fetch the tripped rung is skipped entirely
    before <- counter$ftp_feeds
    br <- breaker_read(root)
    ladder_fetch(ladder, bom_request(), br, now = now + 5 * 3600)
    expect_equal(counter$ftp_feeds, before)        # not called again
  })

  it("resets the strike count to zero on a success", {
    root <- local_store()
    br <- breaker_read(root)
    br <- breaker_strike(br, "ftp_feeds", now = as.POSIXct("2026-01-01", tz = "UTC"))
    br <- breaker_strike(br, "ftp_feeds", now = as.POSIXct("2026-01-01", tz = "UTC"))
    expect_gt(breaker_strikes(br, "ftp_feeds"), 0)
    br <- breaker_reset(br, "ftp_feeds")
    expect_equal(breaker_strikes(br, "ftp_feeds"), 0)
    expect_false(breaker_tripped(br, "ftp_feeds", threshold = 3))
  })
})

describe("cross-run persistence (the SCOPING §5.1 requirement)", {
  it("survives a write / re-read boundary still tripped", {
    root <- local_store()
    now <- as.POSIXct("2026-01-01", tz = "UTC")
    br <- breaker_read(root)
    for (i in 1:3) br <- breaker_strike(br, "gateway", now = now)
    breaker_write(root, br)

    # simulate a new process run: fresh read from disk
    reread <- breaker_read(root)
    expect_true(breaker_tripped(reread, "gateway", threshold = 3))
  })
})

describe("transient vs persistent failures", {
  it("does not accrue a persistent strike on a transient failure", {
    root <- local_store()
    counter <- new.env()
    ladder <- list(
      fake_transport("ftp_feeds", "obs_72h", "transient", counter),
      fake_transport("web_api",   "obs_72h", bom_rows(), counter)
    )
    br <- breaker_read(root)
    out <- ladder_fetch(ladder, bom_request(), br,
                        now = as.POSIXct("2026-01-01", tz = "UTC"))
    after <- attr(out, "breaker") %||% br
    expect_equal(breaker_strikes(after, "ftp_feeds"), 0)  # transient ≠ strike
  })
})
