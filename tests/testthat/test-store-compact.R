# Plan 03 — partition compaction (row-preserving, idempotent).

describe("store_compact()", {
  it("collapses many part-files per partition into one without changing rows", {
    root <- local_store()
    base <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
    # several small appends in the same year -> multiple part files
    for (i in 0:3) {
      store_write_obs(root, new_obs(make_obs(n = 1, start = base + i * 3600)))
    }
    expect_gt(count_parts(root, "observations"), 1)

    before <- store_read_obs(root, "test", include_superseded = TRUE)
    store_compact(root, tables = "observations")

    expect_equal(count_parts(root, "observations"), 1)
    after <- store_read_obs(root, "test", include_superseded = TRUE)
    expect_equal(nrow(after), nrow(before))
    expect_setequal(after$value, before$value)
  })

  it("is a no-op on an already-compacted store", {
    root <- local_store()
    store_write_obs(root, new_obs(make_obs(n = 3)))
    store_compact(root, tables = "observations")
    n1 <- count_parts(root, "observations")
    store_compact(root, tables = "observations")
    expect_equal(count_parts(root, "observations"), n1)
  })
})
