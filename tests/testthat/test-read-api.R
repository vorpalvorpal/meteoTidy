# Plan 14 — the stable read surface (tibble-returning, versioned contract).

describe("each read function returns a canonical tibble", {
  it("met_record() returns canonical obs and validates its output", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    store_write_obs(root, new_obs(make_obs(n = 5)))
    out <- met_record(site)
    expect_canonical_obs(out)
  })

  it("met_history() returns canonical rows at the requested resolution", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    # seed the curated product reader the public function wraps
    testthat::local_mocked_bindings(
      store_read_history = function(store_root, site_id, resolution, ...) {
        make_obs(n = 4, site_id = site_id)
      }
    )
    out <- met_history(site, resolution = "daily")
    expect_canonical_obs(out)
  })

  it("met_forecast_archive() returns canonical forecast rows", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    store_write_forecast(root, new_forecast(make_forecast(n = 3)))
    out <- met_forecast_archive(site)
    expect_canonical_forecast(out)
  })
})

describe("multi-site input row-binds with correct site_ids", {
  it("row-binds met_record across a met_sites", {
    root <- local_store()
    s1 <- make_test_site(site_id = "site_1", store_root = root)
    s2 <- make_test_site(site_id = "site_2", store_root = root)
    store_write_obs(root, new_obs(make_obs(n = 2, site_id = "site_1")))
    store_write_obs(root, new_obs(make_obs(n = 2, site_id = "site_2")))
    out <- met_record(met_sites(list(s1, s2)))
    expect_setequal(unique(out$site_id), c("site_1", "site_2"))
  })
})

describe("as_of reproduces a historical view (ties to Plan 03 revision)", {
  it("returns the value the store would have served at as_of", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    kt <- as.POSIXct("2026-01-01 00:00", tz = "UTC")
    store_write_obs(root, new_obs(make_obs(n = 1, start = kt, value = 20)),
                    now = as.POSIXct("2026-01-02", tz = "UTC"), mode = "supersede")
    store_write_obs(root, new_obs(make_obs(n = 1, start = kt, value = 22)),
                    now = as.POSIXct("2026-01-05", tz = "UTC"), mode = "supersede")
    out <- met_record(site, as_of = as.POSIXct("2026-01-03", tz = "UTC"))
    expect_equal(out$value, 20)
  })
})

describe("member retrievability", {
  it("includes members by default and excludes them on members = FALSE", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    fc <- rbind(make_forecast(n = 2, member = 1L),
                make_forecast(n = 2, member = 2L),
                make_forecast(n = 2, stat = "mean"))
    store_write_forecast(root, new_forecast(fc))
    with_m <- met_forecast_archive(site, members = TRUE)
    without <- met_forecast_archive(site, members = FALSE)
    expect_true(any(!is.na(with_m$member)))
    expect_true(all(is.na(without$member)))
  })
})

describe("the stable signatures are snapshot-guarded", {
  it("snapshots formals() so a breaking contract change is caught in review", {
    expect_snapshot({
      names(formals(met_history))
      names(formals(met_record))
      names(formals(met_forecast_archive))
      names(formals(met_verification))
    })
  })
})
