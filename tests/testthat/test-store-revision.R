# Plan 03 — observation revision policy (supersede-not-overwrite + as_of).
# Directly tests the review fix: point-in-time reproducibility for audit.

describe("supersede semantics", {
  it("keeps only the new value current but retains the old for audit", {
    root <- local_store()
    key_time <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")

    v1 <- new_obs(make_obs(n = 1, start = key_time, value = 20))
    store_write_obs(root, v1, now = as.POSIXct("2026-01-02", tz = "UTC"),
                    mode = "supersede")

    v2 <- new_obs(make_obs(n = 1, start = key_time, value = 22))
    store_write_obs(root, v2, now = as.POSIXct("2026-01-05", tz = "UTC"),
                    mode = "supersede")

    current <- store_read_obs(root, "test")
    expect_equal(nrow(current), 1)
    expect_equal(current$value, 22)

    both <- store_read_obs(root, "test", include_superseded = TRUE)
    expect_setequal(both$value, c(20, 22))
  })

  it("reconstructs the point-in-time value via as_of", {
    root <- local_store()
    key_time <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
    store_write_obs(root, new_obs(make_obs(n = 1, start = key_time, value = 20)),
                    now = as.POSIXct("2026-01-02", tz = "UTC"), mode = "supersede")
    store_write_obs(root, new_obs(make_obs(n = 1, start = key_time, value = 22)),
                    now = as.POSIXct("2026-01-05", tz = "UTC"), mode = "supersede")

    as_of_mid <- store_read_obs(root, "test",
                                as_of = as.POSIXct("2026-01-03", tz = "UTC"))
    expect_equal(as_of_mid$value, 20)  # the value the store would have served then
  })

  it("does not create a superseded row for an unchanged re-write", {
    root <- local_store()
    obs <- new_obs(make_obs(n = 1, value = 20))
    store_write_obs(root, obs, mode = "supersede")
    store_write_obs(root, obs, mode = "supersede")
    expect_equal(nrow(store_read_obs(root, "test", include_superseded = TRUE)), 1)
  })
})
