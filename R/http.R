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
#   - no response in time  -> transient (a timeout, or a dropped/refused
#                             connection); retried like a 5xx, then
#                             "http_client_error".
#
# Every attempt has a timeout (follow-up review, item 12): option
# meteoTidy.http_timeout, default 60 s. Without one, a server that accepted
# the connection and never answered held a scheduled sync -- and the store
# lock -- indefinitely.
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

# Backoff before retrying a transient failure (problem 6: the free Open-Meteo
# tier answers large requests with HTTP 429, and the old 50 ms-to-1 s backoff
# gave up long before its rate window reset). Honours a server `Retry-After`
# (seconds) when present, else waits base * 2^(attempt - 1) seconds; both are
# capped. `base`/`cap` come from options so tests (and impatient callers) can
# shrink them: meteoTidy.http_backoff_base (default 2 s) and
# meteoTidy.http_backoff_max (default 60 s).
.http_backoff <- function(attempt, resp = NULL) {
  base <- getOption("meteoTidy.http_backoff_base", 2)
  cap <- getOption("meteoTidy.http_backoff_max", 60)
  wait <- base * 2^(attempt - 1)
  retry_after <- if (!is.null(resp)) httr2::resp_header(resp, "retry-after") else NULL
  if (!is.null(retry_after)) {
    ra <- suppressWarnings(as.numeric(retry_after))
    if (!is.na(ra)) wait <- max(wait, ra)
  }
  .http_sleep(min(wait, cap))
}

# The one place the HTTP seam sleeps (mockable in tests).
.http_sleep <- function(seconds) {
  Sys.sleep(seconds)
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
#' @param timeout Seconds each attempt may take before it is abandoned and
#'   retried as a transient failure. Defaults to
#'   `getOption("meteoTidy.http_timeout", 60)`.
#' @param now Injectable clock; unused directly (no wall-clock comparison is
#'   made here) but accepted for interface consistency with the rest of the
#'   package's `now = .now()` seam and so callers/tests can pass a frozen
#'   value uniformly.
#' @param parse How to return a successful body: `"json"` (default, parsed via
#'   `httr2::resp_body_json()`, as every Plan 04-06 adapter needs), `"lines"`
#'   (a character vector split on newlines, for text sidecars that are not
#'   themselves valid JSON -- e.g. Plan 08's JSON-*lines* `.index` files), or
#'   `"raw"` (the raw response bytes, for binary bodies -- e.g. Plan 08's
#'   `.grib2` range downloads).
#'
#' @return The response body, shaped per `parse`.
#' @keywords internal
#' @noRd
.http_get <- function(url, headers = list(), query = list(), retry = 5, now = .now(),
                      parse = c("json", "lines", "raw"),
                      timeout = getOption("meteoTidy.http_timeout", 60)) {
  parse <- match.arg(parse)
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
  req <- httr2::req_timeout(req, timeout)

  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    resp <- tryCatch(httr2::req_perform(req), httr2_failure = function(cnd) cnd)
    if (inherits(resp, "httr2_failure")) {
      if (attempt < retry) {
        .http_backoff(attempt)
        next
      }
      reason <- conditionMessage(resp$parent %||% resp) # nolint: object_usage_linter. used via cli glue
      abort_meteo(
        c(
          "Request to {.url {url}} got no response.",
          "x" = "{reason}",
          "i" = "Tried {attempt} time{?s} (timeout {timeout} s each)."
        ),
        class = "http_client_error",
        parent = resp
      )
    }
    status <- httr2::resp_status(resp)

    if (status < 300L) {
      return(switch(parse,
        json = .http_parse_body(resp),
        lines = strsplit(httr2::resp_body_string(resp), "\r?\n")[[1]],
        raw = httr2::resp_body_raw(resp)
      ))
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
      .http_backoff(attempt, resp)
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

# Plan 07 — the FTP/FTP-mirror seam.
#
# `.ftp_get()` mirrors `.http_get()`'s role but for BOM's anonymous-FTP /
# HTTP-mirror product feeds (SCOPING §5.1 ladder rung 1): it is the ONLY
# function in the package that fetches from `ftp.bom.gov.au`/
# `reg.bom.gov.au`. Every frozen test mocks this function directly (binding
# a fixture-returning fake in its place via testthat's edition-3 mocking
# API), so the real body below is not exercised by tests; it is a
# best-effort implementation for live use, built on `curl` (a transitive
# dependency via httr2, guarded here with `rlang::check_installed()` since
# it is not a hard `Imports` entry).

#' Fetch a URL through the package's FTP/FTP-mirror seam
#'
#' The only function in `meteoTidy` that performs a live FTP (or
#' FTP-mirrored-over-HTTP) request, mirroring `.http_get()`'s role for
#' BOM's anonymous-FTP product feeds and `reg.bom.gov.au` HTTP mirrors
#' (SCOPING §5.1 ladder rung 1). Every adapter that reads from this rung
#' calls this seam so tests have one place to mock
#' (`testthat::local_mocked_bindings(.ftp_get = ...)`).
#'
#' @param url Single string, the request URL (`ftp://` or `https://`).
#' @param ... Reserved for future options (timeouts, credentials); unused.
#'
#' @return The raw response body as a single string.
#' @keywords internal
#' @noRd
.ftp_get <- function(url, ...) {
  if (identical(Sys.getenv("METEOTIDY_NO_NET"), "1")) {
    abort_meteo(
      c(
        "Network access is disabled ({.envvar METEOTIDY_NO_NET} = {.val 1}).",
        "i" = "This guard exists so tests never make a live HTTP/FTP request."
      ),
      class = "network_disabled"
    )
  }

  rlang::check_installed("curl", reason = "to fetch BOM FTP/mirror product feeds.")

  handle <- curl::new_handle(useragent = "Mozilla/5.0 (compatible; meteoTidy R package)")
  resp <- curl::curl_fetch_memory(url, handle = handle)
  if (resp$status_code >= 400L) {
    class <- if (resp$status_code %in% .http_gone_codes) "http_gone" else "http_client_error"
    abort_meteo(
      "Request to {.url {url}} failed (status {resp$status_code}).",
      class = class
    )
  }
  rawToChar(resp$content)
}
