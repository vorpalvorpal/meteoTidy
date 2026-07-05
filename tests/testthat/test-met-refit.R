# Plan 16 — met_refit() (monthly): skill-gated manifest bump + compaction.

describe("skill-gated manifest bump (end-to-end Plans 11/13)", {
  it("bumps the calibration manifest only when the skill gate passes", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    now <- as.POSIXct("2026-02-01", tz = "UTC")

    # PASS verdict → a new calibration version is written
    testthat::local_mocked_bindings(
      correct_refit = function(store_root, site, source, variables, now, ...) {
        v <- skill_verdict(promote = TRUE)
        if (all(v$promote)) {
          calib_write(store_root, "test", "temperature_2m", source, "qmap",
                      tibble::tibble(source_q = 0.5, target_q = 0.5),
                      list(train_start = now - 86400 * 30, train_end = now,
                           n_pairs = 500), now = now)
        }
        invisible(v)
      },
      verify_run = function(...) invisible(),
      store_compact = function(...) invisible()
    )
    met_refit(site, now = now, config = pipeline_config(root))
    man_pass <- calib_manifest(root, "test")
    expect_equal(nrow(man_pass), 1)

    # FAIL verdict → no new version
    testthat::local_mocked_bindings(
      correct_refit = function(store_root, site, source, variables, now, ...) {
        invisible(skill_verdict(promote = FALSE))   # gate fails → no calib_write
      },
      verify_run = function(...) invisible(),
      store_compact = function(...) invisible()
    )
    met_refit(site, now = as.POSIXct("2026-03-01", tz = "UTC"),
              config = pipeline_config(root))
    man_fail <- calib_manifest(root, "test")
    expect_equal(nrow(man_fail), nrow(man_pass))     # unchanged
  })
})

describe("monthly compaction", {
  it("reduces partition file count without changing readable rows", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    base <- as.POSIXct("2026-01-01 00:00", tz = "UTC")
    for (i in 0:3) {
      store_write_obs(root, new_obs(make_obs(n = 1, start = base + i * 3600)))
    }
    expect_gt(count_parts(root, "observations"), 1)
    before <- store_read_obs(root, "test", include_superseded = TRUE)

    testthat::local_mocked_bindings(
      correct_refit = function(...) invisible(skill_verdict(promote = FALSE)),
      verify_run = function(...) invisible()
    )
    met_refit(site, now = as.POSIXct("2026-02-01", tz = "UTC"),
              config = pipeline_config(root))

    expect_equal(count_parts(root, "observations"), 1)
    after <- store_read_obs(root, "test", include_superseded = TRUE)
    expect_equal(nrow(after), nrow(before))
  })
})
