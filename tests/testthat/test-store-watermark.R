# Plan 03 — watermarks + effective fetch window.

describe("store_get_watermark() / store_set_watermark()", {
  it("returns the set instant, and NA when unset", {
    root <- local_store()
    expect_true(is.na(store_get_watermark(root, "test", "observations", "silo")))
    t <- as.POSIXct("2026-03-01 12:00:00", tz = "UTC")
    store_set_watermark(root, "test", "observations", "silo", t)
    expect_equal(store_get_watermark(root, "test", "observations", "silo"), t)
  })
})

describe("store_effective_fetch_window()", {
  it("opens the window at watermark - refetch for the revision re-fetch", {
    root <- local_store()
    wm <- as.POSIXct("2026-03-01 00:00:00", tz = "UTC")
    store_set_watermark(root, "test", "observations", "silo", wm)
    win <- store_effective_fetch_window(root, "test", "observations", "silo",
                                        refetch = as.difftime(30, units = "days"),
                                        now = as.POSIXct("2026-04-01", tz = "UTC"))
    expect_equal(win$from, wm - as.difftime(30, units = "days"))
    expect_equal(win$to, as.POSIXct("2026-04-01", tz = "UTC"))
  })

  it("returns from = NULL (full history) when there is no watermark", {
    root <- local_store()
    win <- store_effective_fetch_window(root, "test", "observations", "silo",
                                        refetch = as.difftime(30, units = "days"),
                                        now = as.POSIXct("2026-04-01", tz = "UTC"))
    expect_null(win$from)
  })
})
