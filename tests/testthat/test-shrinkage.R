# Plan 12 — lead-dependent shrinkage toward climatology (forecast path only).
# The review's core statistical fix: variance-preserving QM keeps full variance
# at long lead when the skilful move is to shrink toward climatology.

describe("shrink_to_climatology() blending", {
  it("returns the corrected value at weight 1 and climatology at weight 0", {
    corrected <- c(20, 22, 18)
    clim <- c(15, 15, 15)
    expect_equal(shrink_to_climatology(corrected, clim, weight = 1), corrected)
    expect_equal(shrink_to_climatology(corrected, clim, weight = 0), clim)
  })

  it("blends linearly at intermediate weights", {
    out <- shrink_to_climatology(c(20), c(10), weight = 0.25)
    expect_equal(out, 0.25 * 20 + 0.75 * 10)
  })
})

describe("the forecast/record distinction (Plan 10 contract)", {
  it("never shrinks a realised record; pulls a low-skill forecast to climatology", {
    corrected <- c(25)
    clim <- c(15)
    # target = 'record' — realised series, no skill decay → no shrinkage applied
    rec <- apply_correction_shrinkage(corrected, clim, weight = 0,
                                      target = "record")
    expect_equal(rec, corrected)
    # target = 'forecast' at low skill (weight ~0) → pulled to climatology
    fc <- apply_correction_shrinkage(corrected, clim, weight = 0,
                                     target = "forecast")
    expect_equal(fc, clim)
  })

  it("reduces long-lead error vs unshrunk QM on a skill-decaying series", {
    withr::local_seed(7)
    n <- 500
    truth <- rnorm(n, 15, 5)
    clim <- rep(15, n)
    # a long-lead 'corrected' forecast that is essentially noise (no skill)
    corrected <- rnorm(n, 15, 5)
    w <- 0.1                                       # verified low skill → small w
    shrunk <- shrink_to_climatology(corrected, clim, weight = w)
    err_unshrunk <- mean((corrected - truth)^2)
    err_shrunk <- mean((shrunk - truth)^2)
    expect_lt(err_shrunk, err_unshrunk)            # shrinking beats full-variance
  })
})
