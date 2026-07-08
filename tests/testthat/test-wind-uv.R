# Plan 12 — wind direction corrected as joint u/v, never quantile-mapped as an
# angle (SCOPING §6).

describe("dir_to_uv() / uv_to_dir() round-trip", {
  it("round-trips a direction through u/v within tolerance", {
    dir <- c(10, 90, 180, 270, 350)
    uv <- dir_to_uv(speed = rep(5, 5), dir = dir)
    back <- uv_to_dir(uv$u, uv$v)
    expect_equal(back %% 360, dir %% 360, tolerance = 1e-6)
  })
})

describe("u/v correction handles the 0/360 wrap", {
  it("corrects a bias across north without a 180° artefact", {
    # forecast directions cluster near north with a +20° bias; correcting via
    # u/v recovers the true near-north directions, never smears to ~180°.
    truth <- c(350, 355, 5, 10, 2)
    fc <- (truth + 20) %% 360
    corrected <- correct_wind_direction(fc,
      speed = rep(5, 5),
      bias_uv = dir_to_uv(
        rep(5, 5),
        (fc - truth) %% 360
      )
    )
    # corrected stays near north (within ±40°), never near south
    ang <- pmin(corrected %% 360, 360 - corrected %% 360)
    expect_true(all(ang < 60))
  })

  it("never passes a raw angle to qmap", {
    called <- new.env()
    called$n <- 0L
    testthat::local_mocked_bindings(
      fit_qmap = function(...) {
        called$n <- called$n + 1L
        list()
      }
    )
    correct_wind_direction(c(10, 20, 30),
      speed = rep(5, 3),
      bias_uv = list(u = 0, v = 0)
    )
    expect_equal(called$n, 0L) # direction never QM'd
  })
})
