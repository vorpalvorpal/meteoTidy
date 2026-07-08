#' The current instant
#'
#' Returns the current time as a `POSIXct`. This is the **only** place in the
#' package that reads the wall clock. All other code that needs "now" calls
#' `.now()` — or, better, takes `now = .now()` as an injectable argument — so
#' tests can freeze it with `local_frozen_clock()`
#' (`tests/testthat/helper-conditions.R`).
#'
#' @param tz Timezone to attach to the returned value. Defaults to `"UTC"`,
#'   the canonical storage timezone for the package (see the house style in
#'   `plans/README.md`).
#'
#' @return A `POSIXct` scalar with `tzone` set to `tz`.
#' @keywords internal
#' @noRd
.now <- function(tz = "UTC") {
  now <- Sys.time()
  attr(now, "tzone") <- tz
  now
}
