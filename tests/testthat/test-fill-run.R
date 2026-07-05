# Plan 10 — fill_run() orchestration: idempotency + supersede-on-better-donor.

describe("fill_run() idempotency", {
  it("produces identical filled values and no duplicate rows on a second run", {
    root <- local_store()
    site <- make_test_site()
    obs <- new_obs(series_with_gap("temperature_2m", c(15, NA, NA, 18),
                                   gap_at = 2:3))
    store_write_obs(root, obs, now = as.POSIXct("2026-01-02", tz = "UTC"))

    fill_run(root, site, now = as.POSIXct("2026-01-02", tz = "UTC"))
    v1 <- store_read_obs(root, "test")$value
    n1 <- nrow(store_read_obs(root, "test"))

    fill_run(root, site, now = as.POSIXct("2026-01-02", tz = "UTC"))
    v2 <- store_read_obs(root, "test")$value
    n2 <- nrow(store_read_obs(root, "test"))

    expect_equal(v2, v1)
    expect_equal(n2, n1)          # no duplicate fills
  })
})

describe("a better donor supersedes an earlier fill", {
  it("keeps the earlier fill retrievable via include_superseded = TRUE", {
    root <- local_store()
    site <- make_test_site()
    obs <- new_obs(series_with_gap("temperature_2m", c(15, NA, 18), gap_at = 2))
    store_write_obs(root, obs, now = as.POSIXct("2026-01-02", tz = "UTC"))

    # first fill: only a distant/model donor available
    fill_run(root, site, now = as.POSIXct("2026-01-02", tz = "UTC"),
             donors = list())
    first <- store_read_obs(root, "test")
    filled_time <- first$datetime_utc[first$method != "measured"][1]

    # later: a good local donor arrives and supersedes the earlier fill
    good <- make_obs(n = 3, variable = "temperature_2m",
                     value = c(15, 16.5, 18), source = "bom_obs")
    fill_run(root, site, now = as.POSIXct("2026-01-05", tz = "UTC"),
             donors = list(bom = good))

    audited <- store_read_obs(root, "test", include_superseded = TRUE)
    at_gap <- audited[audited$datetime_utc == filled_time, ]
    expect_gte(nrow(at_gap), 2)   # both the earlier and the better fill retained
  })
})
