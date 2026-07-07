# Plan 17 — BDD specs for persisting BOM serving transport (item 8) and honouring
# as_of in met_history (item 9).

describe("item 8: the serving transport is recorded in provenance", {
  it("round-trips an obs_transport row keyed like the observation", {
    skip("plan 17 item 8: obs_transport companion table — un-skip when implementing")

    root <- local_store()
    df <- tibble::tibble(
      site_id = "test",
      datetime_utc = as.POSIXct("2026-01-01 00:00", tz = "UTC"),
      variable = "temperature_2m", source = "bom_obs", transport = "ftp_feeds"
    )
    obs_transport_write(root, df, now = as.POSIXct("2026-01-01 01:00", tz = "UTC"))

    got <- obs_transport_read(root, "test",
                              from = as.POSIXct("2026-01-01", tz = "UTC"),
                              to = as.POSIXct("2026-01-02", tz = "UTC"))
    expect_equal(got$transport[[1]], "ftp_feeds")
    expect_equal(got$source[[1]], "bom_obs")
  })
})

describe("item 9: met_history honours a point-in-time as_of read", {
  it("returns the pre-revision value when as_of predates the revision", {
    skip("plan 17 item 9: thread as_of through history builders — un-skip when implementing")

    root <- local_store()
    site <- make_test_site(store_root = root)
    when <- as.POSIXct("2026-01-01 00:00", tz = "UTC")
    key_obs <- function(v) {
      new_obs(make_obs(n = 1, variable = "temperature_2m", value = v,
                       source = "site_aws", method = "measured", start = when))
    }

    t1 <- as.POSIXct("2026-01-02 00:00", tz = "UTC")  # first ingest
    t2 <- as.POSIXct("2026-01-10 00:00", tz = "UTC")  # revision ingest
    store_write_obs(root, key_obs(20), now = t1, mode = "append")
    store_write_obs(root, key_obs(25), now = t2, mode = "supersede")

    win <- list(from = when - 3600, to = when + 3600)
    old <- met_history(site, resolution = "hourly",
                       from = win$from, to = win$to,
                       as_of = as.POSIXct("2026-01-05", tz = "UTC"))
    new <- met_history(site, resolution = "hourly", from = win$from, to = win$to)

    expect_equal(old$value[old$variable == "temperature_2m"], 20)
    expect_equal(new$value[new$variable == "temperature_2m"], 25)
  })
})
