# Plan 03 — observation IO.

describe("store_write_obs() / store_read_obs()", {
  it("round-trips written rows back as a canonical table", {
    root <- local_store()
    obs <- new_obs(make_obs(n = 5))
    store_write_obs(root, obs, now = as.POSIXct("2026-01-01", tz = "UTC"))
    got <- store_read_obs(root, site_id = "test")
    expect_canonical_obs(got)
    expect_equal(nrow(got), 5)
  })

  it("prunes partitions to the requested year window", {
    root <- local_store()
    y2024 <- new_obs(make_obs(n = 2, start = as.POSIXct("2024-06-01", tz = "UTC")))
    y2026 <- new_obs(make_obs(n = 3, start = as.POSIXct("2026-06-01", tz = "UTC")))
    store_write_obs(root, y2024)
    store_write_obs(root, y2026)
    got <- store_read_obs(root, site_id = "test",
                          from = as.POSIXct("2026-01-01", tz = "UTC"),
                          to = as.POSIXct("2026-12-31", tz = "UTC"))
    expect_equal(nrow(got), 3)
    expect_true(all(format(got$datetime_utc, "%Y") == "2026"))
  })

  it("is idempotent in supersede mode when nothing changed", {
    root <- local_store()
    obs <- new_obs(make_obs(n = 4))
    store_write_obs(root, obs, mode = "supersede")
    summary <- store_write_obs(root, obs, mode = "supersede")
    expect_equal(nrow(store_read_obs(root, "test")), 4)
    expect_equal(summary$n_unchanged, 4)
    expect_equal(summary$n_new, 0)
  })
})
