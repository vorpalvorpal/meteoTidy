# Plan 04 — the HTTP seam.
#
# `.http_get()` is the *only* function in the package that performs a live
# HTTP request. Every adapter goes through it so that (a) `httptest2` and the
# no-network test guard have exactly one seam to intercept, and (b) retry /
# error classification is implemented once (SCOPING §5.1 circuit-breaker
# spirit, per-request half; the multi-rung transport ladder is Plan 07).
#
# Status classification:
#   - 404, 410            -> "http_gone", never retried (persistent failure;
#                             callers use this to trip to the next rung).
#   - 429, 5xx             -> transient; retried with backoff up to `retry`
#                             attempts, then "http_client_error" if still
#                             failing (see note below on the terminal class).
#   - other 4xx (e.g. 401) -> "http_client_error", never retried.
#   - 2xx                  -> success; body parsed and returned.
#
# Terminal class after retries are exhausted: the plan text does not pin an
# exact class for "still failing after N attempts", offering
# "http_client_error" or a transient-specific class as reasonable choices. We
# use "http_client_error" for both "other 4xx" and "retries exhausted",
# documented here as the deliberate choice: both cases mean "the request
# cannot currently be satisfied and is not the permanent-gone case", and reusing
# one class keeps the taxonomy small. A future plan is free to split this into
# a dedicated "http_retries_exhausted" class if callers need to distinguish
# the two.

# Status codes that never benefit from a retry: the resource is gone.
.http_gone_codes <- c(404L, 410L)

# Status codes considered transient: worth retrying with backoff.
.http_transient_codes <- c(429L)

.is_transient_status <- function(status) {
  status %in% .http_transient_codes || status >= 500L
}

# Exponential backoff with a small fixed base; not injectable via `now`
# because it only affects wall-clock sleep duration, not any comparison the
# package makes against `now`. Kept short so tests that do exercise real
# retries (none currently do; the req_perform mock tests short-circuit before
# a second attempt where relevant) stay fast.
.http_backoff <- function(attempt) {
  Sys.sleep(min(0.05 * 2^(attempt - 1), 1))
}

#' Perform a GET request through the package's single HTTP seam
#'
#' The only function in `meteoTidy` that performs a live HTTP request. Built
#' on `httr2`; every adapter (`source_rest()` and, indirectly via
#' `adapters_for_site()`, sources built by later plans) calls this instead of
#' using `httr2` directly, so mocking (`httptest2`) and the no-network test
#' guard have one seam.
#'
#' @param url Single string, the request URL.
#' @param headers Named list of extra request headers.
#' @param query Named list of query parameters to append to `url`.
#' @param retry Integer, maximum number of attempts for transient failures
#'   (`429`/`5xx`). Persistent failures (`404`/`410`) and other client errors
#'   (e.g. `401`) are never retried.
#' @param now Injectable clock; unused directly (no wall-clock comparison is
#'   made here) but accepted for interface consistency with the rest of the
#'   package's `now = .now()` seam and so callers/tests can pass a frozen
#'   value uniformly.
#'
#' @return The parsed response body (a list, via `httr2::resp_body_json()`).
#' @keywords internal
#' @noRd
.http_get <- function(url, headers = list(), query = list(), retry = 3, now = .now()) {
  if (identical(Sys.getenv("METEOTIDY_NO_NET"), "1")) {
    abort_meteo(
      c(
        "Network access is disabled ({.envvar METEOTIDY_NO_NET} = {.val 1}).",
        "i" = "This guard exists so tests never make a live HTTP request."
      ),
      class = "network_disabled"
    )
  }

  req <- httr2::request(url)
  if (length(headers) > 0) {
    req <- do.call(httr2::req_headers, c(list(req), headers))
  }
  if (length(query) > 0) {
    req <- do.call(httr2::req_url_query, c(list(req), query))
  }
  req <- httr2::req_error(req, is_error = function(resp) FALSE)

  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)

    if (status < 300L) {
      return(.http_parse_body(resp))
    }

    if (status %in% .http_gone_codes) {
      abort_meteo(
        c(
          "Request to {.url {url}} failed permanently (HTTP {status}).",
          "i" = "Not retried: this status is treated as persistent."
        ),
        class = "http_gone"
      )
    }

    if (.is_transient_status(status) && attempt < retry) {
      .http_backoff(attempt)
      next
    }

    abort_meteo(
      c(
        "Request to {.url {url}} failed (HTTP {status}).",
        "i" = if (.is_transient_status(status)) {
          "Retried {attempt} time{?s} without success."
        } else {
          "Not retried: this status is not classified as transient."
        }
      ),
      class = "http_client_error"
    )
  }
}

# Extract the parsed body from a successful response. JSON is the only body
# type this plan's adapters need (source_rest); guard non-JSON bodies with an
# informative error rather than letting httr2's own error surface directly.
.http_parse_body <- function(resp) {
  content_type <- httr2::resp_content_type(resp)
  if (!is.null(content_type) && !grepl("json", content_type, fixed = TRUE)) {
    tryCatch(
      return(httr2::resp_body_json(resp)),
      error = function(e) {
        abort_meteo(
          c(
            "Response body could not be parsed as JSON.",
            "x" = "Content-Type was {.val {content_type}}."
          ),
          class = "http_client_error"
        )
      }
    )
  }
  httr2::resp_body_json(resp)
}
