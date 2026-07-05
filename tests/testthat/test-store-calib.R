# Plan 03 — calibration store (persisted as data, never .rds).

describe("calib_write() / calib_read()", {
  it("bumps version on each write and returns the current one", {
    root <- local_store()
    coeffs1 <- tibble::tibble(term = "intercept", estimate = 1.0)
    coeffs2 <- tibble::tibble(term = "intercept", estimate = 2.0)
    meta <- list(train_start = as.POSIXct("2025-01-01", tz = "UTC"),
                 train_end = as.POSIXct("2025-12-31", tz = "UTC"), n_pairs = 8760)

    calib_write(root, "test", "temperature_2m", "openmeteo", "mean_bias",
                coeffs1, meta, now = as.POSIXct("2026-01-01", tz = "UTC"))
    calib_write(root, "test", "temperature_2m", "openmeteo", "mean_bias",
                coeffs2, meta, now = as.POSIXct("2026-02-01", tz = "UTC"))

    current <- calib_read(root, "test", "temperature_2m", "openmeteo",
                          version = "current")
    expect_equal(current$coeffs$estimate, 2.0)

    man <- calib_manifest(root, "test")
    expect_equal(nrow(man), 2)
    expect_true(all(diff(man$version) >= 1))
  })

  it("round-trips coefficients through Parquet and writes no .rds", {
    root <- local_store()
    coeffs <- tibble::tibble(source_q = c(0.1, 0.5, 0.9),
                             target_q = c(0.2, 0.55, 0.88))
    meta <- list(train_start = as.POSIXct("2025-01-01", tz = "UTC"),
                 train_end = as.POSIXct("2025-12-31", tz = "UTC"), n_pairs = 4000)
    calib_write(root, "test", "temperature_2m", "openmeteo", "qmap",
                coeffs, meta, now = as.POSIXct("2026-01-01", tz = "UTC"))

    back <- calib_read(root, "test", "temperature_2m", "openmeteo")$coeffs
    expect_equal(back$target_q, coeffs$target_q, tolerance = 1e-12)
    expect_length(list_rds(file.path(root, "calibrations")), 0)
  })
})
