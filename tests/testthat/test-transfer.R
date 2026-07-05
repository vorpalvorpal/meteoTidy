# Plan 10 — the shared transfer engine (used by both fill and Plan 12).

describe("fit_transfer() / apply_transfer()", {
  it("mean_bias recovers and removes a known constant offset", {
    p <- transfer_pair(offset = 2)
    tr <- fit_transfer(p$source, p$target, method = "mean_bias")
    corrected <- apply_transfer(tr, p$source)
    # source was target + 2; correcting source should recover target within noise
    expect_equal(mean(corrected$value), mean(p$target$value), tolerance = 0.1)
  })

  it("qmap maps a known distributional shift onto the target quantiles", {
    withr::local_seed(42)
    target <- make_obs(n = 500, variable = "temperature_2m",
                       value = rnorm(500, 15, 3), source = "site")
    # source is scaled+shifted: a distributional (not just mean) change
    source <- make_obs(n = 500, variable = "temperature_2m",
                       value = rnorm(500, 20, 6), source = "donor")
    tr <- fit_transfer(source, target, method = "qmap")
    corrected <- apply_transfer(tr, source)
    expect_equal(quantile(corrected$value, 0.5),
                 quantile(target$value, 0.5), tolerance = 0.6,
                 ignore_attr = TRUE)
    expect_equal(sd(corrected$value), sd(target$value), tolerance = 0.6)
  })
})

describe("the skill-decay-free invariant (Plan 12 relies on this)", {
  it("apply_transfer has no lead / weight / shrinkage argument", {
    # The engine assumes two REALISED series; forecast skill decay is added by
    # Plan 12 as a wrapper, never baked into the primitive.
    args <- names(formals(apply_transfer))
    expect_false(any(c("lead", "lead_time", "weight", "shrink", "shrinkage") %in%
                       args))
  })

  it("applies identically regardless of position in the series (no time weighting)", {
    p <- transfer_pair(offset = 3)
    tr <- fit_transfer(p$source, p$target, method = "mean_bias")
    early <- apply_transfer(tr, p$source[1:10, ])
    late  <- apply_transfer(tr, p$source[191:200, ])
    # the same correction constant is applied at both ends (no decay toward late)
    off_early <- mean(early$value - p$source$value[1:10])
    off_late  <- mean(late$value - p$source$value[191:200])
    expect_equal(off_early, off_late, tolerance = 1e-9)
  })
})
