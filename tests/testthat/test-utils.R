# Plan 00 — clock seam.

describe(".now()", {
  it("returns a UTC POSIXct", {
    t <- .now()
    expect_s3_class(t, "POSIXct")
    expect_identical(attr(t, "tzone"), "UTC")
  })

  it("honours a requested timezone attribute", {
    t <- .now(tz = "Australia/Sydney")
    expect_identical(attr(t, "tzone"), "Australia/Sydney")
  })

  it("is the single seam a frozen clock overrides for downstream plans", {
    frozen <- as.POSIXct("2026-07-05 00:00:00", tz = "UTC")
    local_frozen_clock(frozen)
    expect_identical(.now(), frozen)
  })
})

test_that(".now() is the only reader of the wall clock in R/", {
  # Guards the house-style rule: no Sys.time()/Sys.Date() in package logic.
  r_dir <- test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir))
  hits <- unlist(lapply(list.files(r_dir, "\\.R$", full.names = TRUE), function(f) {
    grep("Sys\\.(time|Date)\\(", readLines(f, warn = FALSE), value = TRUE)
  }))
  # The only permitted occurrence is inside the body of `.now()` itself.
  expect_true(length(hits) <= 1)
})
