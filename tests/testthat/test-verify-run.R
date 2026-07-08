# Plan 13 — verify_run() rolling-origin correctness (no in-sample leakage) and
# the stored, readable report.

describe("rolling-origin correctness (the central review fix)", {
  it("reports an overfit calibration's true poor out-of-sample skill", {
    withr::local_seed(41)
    # A calibration that memorises its training window scores ~perfectly
    # in-sample but has no genuine skill. Rolling-origin evaluation must expose
    # the poor out-of-sample skill, never the inflated in-sample number.
    #
    # forecast_obs_pairs(bias_fun = function(doy, lead) 0) makes `forecast`
    # IDENTICAL to `observation` for every row (both derive from the same
    # `truth` draw, and a constant zero bias adds nothing) -- there would be
    # no residual at all to "memorise", so a memo-based overfit fit is
    # perfect both in- and out-of-sample by construction, and the test's
    # in-sample-vs-OOS contrast could never show anything (0 > 0 * 10 is
    # false regardless of implementation). Injecting independent
    # per-row forecast noise here (not touching the shared
    # forecast_obs_pairs() helper other tests rely on) gives the memo
    # mechanism an actual, row-specific residual to memorise: exactly
    # recoverable in-sample, but unknowable -- hence unremoved -- for any
    # issue_time outside the training window.
    pairs <- forecast_obs_pairs(n = 400, bias_fun = function(doy, lead) 0)
    pairs$forecast <- pairs$forecast + rnorm(nrow(pairs), 0, 2)

    overfit_fit <- function(train) {
      # "memorise": store the exact training residuals keyed by issue_time
      list(memo = stats::setNames(train$observation - train$forecast,
                                  as.character(train$issue_time)))
    }
    overfit_apply <- function(fit, newdata) {
      adj <- fit$memo[as.character(newdata$issue_time)]
      adj[is.na(adj)] <- 0                       # no memory out-of-sample → no skill
      newdata$forecast + adj
    }

    ro <- rolling_origin_score(pairs, fit_fn = overfit_fit,
                               apply_fn = overfit_apply,
                               step = "30 days", buffer = "1 day")
    in_sample <- score_deterministic(overfit_apply(overfit_fit(pairs), pairs),
                                     pairs$observation)$rmse
    expect_lt(in_sample, 0.01)                    # inflated in-sample
    expect_gt(ro$rmse, in_sample * 10)            # honest, much worse OOS
  })
})

describe("report persistence", {
  it("writes a well-formed report readable back via the store", {
    root <- local_store()
    site <- make_test_site()
    # a tiny archive + obs so verify_run has pairs to score (mocked internals)
    testthat::local_mocked_bindings(
      assemble_verification_pairs = function(...) forecast_obs_pairs(n = 60)
    )
    verify_run(root, site, sources = "openmeteo",
               now = as.POSIXct("2026-01-01", tz = "UTC"))
    report <- read_verification_report(root, "test")
    expect_s3_class(report, "tbl_df")
    expect_true(all(c("variable", "lead_bucket", "tier") %in% names(report)))
  })

  it("replaces the stored report on re-run instead of accumulating rows", {
    # Regression: verify_run() used to append a new part-file per run while
    # read_verification_report() rbinds every part-file, so each met_refit()
    # duplicated the whole report.
    root <- local_store()
    site <- make_test_site()
    testthat::local_mocked_bindings(
      assemble_verification_pairs = function(...) forecast_obs_pairs(n = 60)
    )
    verify_run(root, site, sources = "openmeteo",
               now = as.POSIXct("2026-01-01", tz = "UTC"))
    once <- read_verification_report(root, "test")
    verify_run(root, site, sources = "openmeteo",
               now = as.POSIXct("2026-02-01", tz = "UTC"))
    twice <- read_verification_report(root, "test")
    expect_equal(nrow(twice), nrow(once))
  })
})
