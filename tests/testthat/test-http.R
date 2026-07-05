# Plan 04 — the HTTP seam: no-net guard, retry, error classification.

describe(".http_get() no-network guard", {
  it("aborts network_disabled when METEOTIDY_NO_NET=1 (proves no test hits the net)", {
    local_no_net()
    expect_error(.http_get("https://example.com/data"),
                 class = "meteoTidy_error_network_disabled")
  })
})

describe(".http_get() retry + classification", {
  it("retries a transient 500 then succeeds on 200", {
    withr::local_envvar(METEOTIDY_NO_NET = "0")
    httptest2::with_mock_dir(test_path("_fixtures/http/retry-500-200"), {
      # the recorded sequence is 500 then 200; assert eventual success
      out <- .http_get("https://example.com/data", retry = 3)
      expect_false(is.null(out))
    })
  })

  it("does NOT retry a persistent 404 and aborts http_gone", {
    withr::local_envvar(METEOTIDY_NO_NET = "0")
    n_requests <- 0L
    fake_perform <- function(req) {
      n_requests <<- n_requests + 1L
      httr2::response(status_code = 404, url = req$url)
    }
    testthat::local_mocked_bindings(req_perform = fake_perform, .package = "httr2")
    expect_error(.http_get("https://example.com/gone", retry = 3),
                 class = "meteoTidy_error_http_gone")
    expect_equal(n_requests, 1L)  # a 404 is terminal, not retried
  })

  it("classifies a 401 as http_client_error", {
    withr::local_envvar(METEOTIDY_NO_NET = "0")
    fake_perform <- function(req) httr2::response(status_code = 401, url = req$url)
    testthat::local_mocked_bindings(req_perform = fake_perform, .package = "httr2")
    expect_error(.http_get("https://example.com/secret"),
                 class = "meteoTidy_error_http_client_error")
  })
})
