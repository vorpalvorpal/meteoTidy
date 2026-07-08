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
    # Plan 17 item 3: met_refit() iterates every configured source (obs UNION
    # forecast), not just obs_sources -- pipeline_config()'s default has 3
    # (site_aws, openmeteo, bom_forecast), so the always-promoting mock
    # writes one calibration per source.
    expect_equal(nrow(man_pass), 3)

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

  # NOTE (audit): the test above mocks out `correct_refit()` — the very
  # function under test — so it exercises only the orchestration shell, not
  # the real fit -> verify -> gate -> write path. Today `correct_refit()` is a
  # no-op (`invisible(NULL)`), so nothing is fitted or persisted end-to-end;
  # the mock's own `calib_write()` is what bumps the manifest here. The test
  # below is the *real* acceptance criterion and follows Plan 16's stated
  # design (mock the leaf skill verdict, not `correct_refit`).
  it("fits+writes a calibration through the REAL correct_refit() on a passing gate", {
    # ACCEPTANCE for the correction-wiring gap (IMPLEMENTER_PROMPT.md:
    # "Wire the correction/verification pipeline"). correct_refit() now
    # assembles training pairs, fits a Plan 12 tier, gates on the Plan 13
    # skill verdict, and calib_write()s on promotion.
    root <- local_store()
    site <- make_test_site(store_root = root)
    now  <- as.POSIXct("2026-02-01", tz = "UTC")

    # Overlapping archived-forecast + curated-obs pairs for temperature_2m
    # (source "site_aws", pipeline_config()'s default obs source): 400 daily
    # issuances, each 24h ahead, with a constant +2 forecast bias so a
    # fitted correction has an obvious, out-of-sample-verifiable skill edge
    # over the raw incumbent.
    n <- 400
    issue <- as.POSIXct("2025-01-01", tz = "UTC") + (seq_len(n) - 1L) * 86400
    valid <- issue + 24 * 3600
    doy <- as.integer(format(issue, "%j"))
    truth <- 15 + 10 * sin(2 * pi * doy / 365.25)

    fc <- tibble::tibble(
      site_id = "test", source = "site_aws", model = "test_model",
      issue_time = issue, valid_time = valid,
      lead_time = as.difftime(rep(24, n), units = "hours"),
      member = NA_integer_, stat = NA_character_,
      variable = "temperature_2m", value = truth + 2
    )
    store_write_forecast(root, new_forecast(fc))

    obs <- tibble::tibble(
      site_id = "test", datetime_utc = valid, variable = "temperature_2m",
      value = truth, source = "test_src", method = "measured", qc_flag = "ok"
    )
    store_write_obs(root, new_obs(obs))

    # Mock ONLY the leaf skill verdict (not correct_refit) to promote --
    # everything else (pair assembly, tier_select(), the Plan 12 fit,
    # rolling-origin scoring) runs for real.
    testthat::local_mocked_bindings(
      skill_verdict_compute = function(scores, bootstrap) {
        tibble::tibble(variable = scores$variable, lead_bucket = scores$lead_bucket,
                       promote = TRUE, shrink_weight = 1, consistency_violation_rate = 0)
      },
      verify_run = function(...) invisible(),
      store_compact = function(...) invisible()
    )

    met_refit(site, now = now, config = pipeline_config(root))

    # A real fit on a promoted gate must persist exactly one calibration.
    expect_equal(nrow(calib_manifest(root, "test")), 1)
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
