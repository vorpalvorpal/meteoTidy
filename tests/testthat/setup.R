# Global test setup (Plan 00).
#
# Turn any accidental live HTTP request into a hard error for the whole test
# run: Plan 04's `.http_get()` seam aborts `"network_disabled"` when this is set.
# Restored automatically at the end of the session via testthat's teardown env.
withr::local_envvar(
  METEOTIDY_NO_NET = "1",
  .local_envir = testthat::teardown_env()
)

# Keep snapshots stable across contributor machines: fix the locale-sensitive
# bits that `cli` and time formatting depend on. (Individual tests still set
# their own timezone with `withr::local_timezone()` where it is load-bearing.)
withr::local_envvar(
  TZ = "UTC",
  .local_envir = testthat::teardown_env()
)
