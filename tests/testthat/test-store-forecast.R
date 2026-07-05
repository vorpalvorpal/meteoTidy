# Plan 03 — forecast IO.

describe("store_write_forecast() / store_read_forecast()", {
  it("stores deterministic, ensemble-member, and summary rows in one dataset", {
    root <- local_store()
    fc <- rbind(
      make_forecast(n = 2, member = NA_integer_, stat = NA_character_),
      make_forecast(n = 2, member = 1L),
      make_forecast(n = 2, member = 2L),
      make_forecast(n = 2, stat = "mean")
    )
    store_write_forecast(root, new_forecast(fc))
    got <- store_read_forecast(root, site_id = "test")
    expect_canonical_forecast(got)
    expect_equal(nrow(got), nrow(fc))
  })

  it("dedups on (source, model, issue_time) so re-archiving is a no-op", {
    root <- local_store()
    fc <- new_forecast(make_forecast(n = 3))
    store_write_forecast(root, fc)
    store_write_forecast(root, fc)
    expect_equal(nrow(store_read_forecast(root, "test")), 3)
  })

  it("keeps members retrievable by default and drops them on members = FALSE", {
    root <- local_store()
    fc <- rbind(
      make_forecast(n = 2, member = 1L),
      make_forecast(n = 2, member = 2L),
      make_forecast(n = 2, stat = "p50")
    )
    store_write_forecast(root, new_forecast(fc))
    with_members <- store_read_forecast(root, "test", members = TRUE)
    without <- store_read_forecast(root, "test", members = FALSE)
    expect_true(any(!is.na(with_members$member)))
    expect_true(all(is.na(without$member)))
  })

  it("prunes partitions on source and issue_date", {
    root <- local_store()
    jan <- make_forecast(n = 1, issue_time = as.POSIXct("2026-01-01", tz = "UTC"))
    feb <- make_forecast(n = 1, issue_time = as.POSIXct("2026-02-01", tz = "UTC"))
    store_write_forecast(root, new_forecast(rbind(jan, feb)))
    got <- store_read_forecast(root, "test",
                               issue_from = as.POSIXct("2026-02-01", tz = "UTC"),
                               issue_to = as.POSIXct("2026-02-28", tz = "UTC"))
    expect_equal(nrow(got), 1)
  })
})

describe("store_write_forecast_aux() / store_read_forecast_aux()", {
  it("round-trips the non-numeric companion table (précis text, categories)", {
    root <- local_store()
    aux <- new_forecast_aux(make_forecast_aux(n = 2))
    store_write_forecast_aux(root, aux)
    got <- store_read_forecast_aux(root, site_id = "test")
    expect_canonical_forecast_aux(got)
    expect_equal(nrow(got), 2)
  })

  it("dedups on (source, issue_time) so re-archiving aux is a no-op", {
    root <- local_store()
    aux <- new_forecast_aux(make_forecast_aux(n = 2))
    store_write_forecast_aux(root, aux)
    store_write_forecast_aux(root, aux)
    expect_equal(nrow(store_read_forecast_aux(root, "test")), 2)
  })
})
