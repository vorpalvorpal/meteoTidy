# Plan 13 — moving-block bootstrap on autocorrelated score-difference series.

describe("block bootstrap widens CIs vs a naive iid bootstrap", {
  it("is wider on an autocorrelated series (autocorrelation handling matters)", {
    withr::local_seed(31)
    # AR(1) score-difference series (strong positive autocorrelation)
    n <- 500; phi <- 0.8
    e <- rnorm(n)
    x <- numeric(n); for (i in 2:n) x[i] <- phi * x[i - 1] + e[i]

    block_ci <- block_bootstrap_ci(x, block_len = 25, R = 500, seed = 1)
    iid_ci   <- block_bootstrap_ci(x, block_len = 1,  R = 500, seed = 1)
    block_w <- diff(block_ci$ci)
    iid_w   <- diff(iid_ci$ci)
    expect_gt(block_w, iid_w)
  })
})

describe("significance calls", {
  it("judges a tiny-but-noisy improvement not significant", {
    withr::local_seed(32)
    diff <- rnorm(400, mean = 0.01, sd = 1)      # negligible mean, large noise
    res <- block_bootstrap_ci(diff, block_len = 20, R = 500, seed = 2)
    expect_false(res$significant)                # CI straddles 0
  })

  it("judges a large consistent improvement significant", {
    withr::local_seed(33)
    diff <- rnorm(400, mean = 0.8, sd = 0.5)     # clear, consistent gain
    res <- block_bootstrap_ci(diff, block_len = 20, R = 500, seed = 3)
    expect_true(res$significant)
    expect_gt(res$ci[1], 0)                       # whole CI above zero
  })
})
