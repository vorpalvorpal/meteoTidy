# Plan 07 — the BOM transport ladder: ordered rungs, circuit-breaker
# integration, provenance stamping. See plans/07-acquisition-bom.md
# (SCOPING §5.1).
#
# A transport ("rung") is a plain list: `list(id, kind, fetch_fn,
# applies_to)`. `applies_to` is a character vector of product names the rung
# can serve (e.g. "obs_72h", "precis_daily", "forecast_hourly"). `fetch_fn`
# is `function(request, now = NULL)`, returning a tibble of canonical rows on
# success, or aborting class "http_gone" (persistent) / "http_client_error"
# (transient) on failure.
#
# The ladder itself is just an ordered `list()` of rungs; `ladder_fetch()`
# walks it. Breaker state is passed in and (possibly) returned via an
# attribute, never touched on disk here -- `ladder_fetch()` is a pure
# function of its arguments plus the fetch_fns' own side effects.

#' Walk a BOM transport ladder for a request
#'
#' Filters `ladder` to the rungs that (a) can serve `request$product` and
#' (b) are not currently tripped in `breaker`, then calls each eligible
#' rung's `fetch_fn` in order until one succeeds.
#'
#' On success, the winning rung's id is stamped onto a new `transport` column
#' on every returned row, and the (possibly breaker-updated) breaker object
#' is attached as `attr(result, "breaker")` so the caller can persist any
#' strikes accrued during the call (via `breaker_write()`) -- `ladder_fetch()`
#' itself never touches disk.
#'
#' Failure handling per rung:
#' - `"http_gone"` (persistent, e.g. 404/410/DNS failure): records a strike
#'   against that rung in the breaker, then moves to the next eligible rung.
#' - `"http_client_error"` (transient, e.g. exhausted retries/429/5xx): no
#'   strike recorded; moves to the next eligible rung.
#' - Any other error class: treated like a persistent failure (a strike is
#'   recorded). The frozen test suite does not exercise this branch; erring
#'   on the side of tripping the breaker for genuinely unexpected failures
#'   is the safer default (a mis-behaving rung gets sidelined rather than
#'   retried indefinitely).
#'
#' If the eligible-rung list is empty to begin with (nothing in `ladder` can
#' serve `request$product`, or every such rung is currently tripped), or
#' every eligible rung fails, aborts class `"bom_all_transports_failed"`.
#'
#' @param ladder A list of transport rungs (see Details above).
#' @param request A list `list(product, variables, window)` (see
#'   `bom_request()` in `tests/testthat/helper-bom.R`).
#' @param breaker A `bom_breaker` list (see `breaker_read()`).
#' @param now Injectable clock, passed through to each rung's `fetch_fn` and
#'   used to timestamp any strikes recorded.
#'
#' @return A tibble of canonical rows with a `transport` column stamped, and
#'   a `"breaker"` attribute carrying the updated breaker state.
#' @keywords internal
#' @noRd
ladder_fetch <- function(ladder, request, breaker, now = .now()) {
  eligible <- Filter(function(rung) {
    request$product %in% rung$applies_to && !breaker_tripped(breaker, rung$id)
  }, ladder)

  for (rung in eligible) {
    result <- tryCatch(
      list(ok = TRUE, value = rung$fetch_fn(request, now)),
      meteoTidy_error_http_gone = function(cnd) list(ok = FALSE, persistent = TRUE),
      meteoTidy_error_http_client_error = function(cnd) list(ok = FALSE, persistent = FALSE),
      meteoTidy_error = function(cnd) list(ok = FALSE, persistent = TRUE)
    )

    if (isTRUE(result$ok)) {
      out <- result$value
      out$transport <- rung$id
      attr(out, "breaker") <- breaker
      return(out)
    }

    if (isTRUE(result$persistent)) {
      breaker <- breaker_strike(breaker, rung$id, now = now)
    }
  }

  abort_meteo(
    c(
      "All BOM transports failed for product {.val {request$product}}.",
      "i" = "Every rung that can serve this product is either tripped or failed just now."
    ),
    class = "bom_all_transports_failed"
  )
}
