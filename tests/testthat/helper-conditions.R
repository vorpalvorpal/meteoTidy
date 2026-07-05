# Helpers for Plan 00 — condition system + injectable clock.

# Freeze the package clock. Any code under test that reads time through `.now()`
# (never `Sys.time()`) sees exactly `t`. Restored when the calling frame exits.
local_frozen_clock <- function(t = as.POSIXct("2026-07-05 00:00:00", tz = "UTC"),
                               env = parent.frame()) {
  frozen <- function(tz = "UTC") {
    x <- t
    attr(x, "tzone") <- tz
    x
  }
  testthat::local_mocked_bindings(.now = frozen, .env = env)
  invisible(t)
}

# Catch a condition and hand back the object, so tests can inspect its class
# vector and reported call.
catch_meteo <- function(expr) {
  rlang::catch_cnd(expr)
}
