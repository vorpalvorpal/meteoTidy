# Follow-up review, item 12: .http_get() had no timeout.
#
# A server that accepts the connection and then never answers held an hourly
# sync (and the store lock) forever. .http_get() now sets a per-request
# timeout (option meteoTidy.http_timeout, default 60 s), treats a timeout
# as transient -- retried with the usual backoff -- and then fails with the
# same "http_client_error" class as exhausted 5xx retries, so the BOM
# transport ladder moves on to its next rung.
# A real second process listens on a loopback port and never replies.

fu12_silent_server <- function(env = parent.frame()) {
  skip_on_cran()
  skip_if_not_installed("callr")
  port_file <- withr::local_tempfile(.local_envir = env)
  srv <- callr::r_bg(function(port_file) {
    for (port in sample(20000:40000, 50)) {
      s <- tryCatch(serverSocket(port), error = function(e) NULL)
      if (!is.null(s)) break
    }
    writeLines(as.character(port), port_file)
    conns <- list()
    repeat {
      conns <- c(conns, list(tryCatch(socketAccept(s, blocking = TRUE, open = "r+b"), error = function(e) NULL)))
    }
  }, list(port_file))
  withr::defer(srv$kill(), envir = env)
  # Without a client timeout the request would hang for good; end the
  # server after 20 s so a regression fails the test instead of hanging it.
  watchdog <- callr::r_bg(function(pid) {
    Sys.sleep(20)
    tools::pskill(pid)
  }, list(srv$get_pid()))
  withr::defer(watchdog$kill(), envir = env)
  for (i in 1:100) {
    if (file.exists(port_file) && length(readLines(port_file, warn = FALSE)) == 1) break
    Sys.sleep(0.1)
  }
  port <- readLines(port_file, warn = FALSE)
  expect_length(port, 1)
  sprintf("http://127.0.0.1:%s/v1/locations/r64bhq/forecasts/hourly", port)
}

describe("item 12: HTTP requests time out", {
  it("gives up on a server that never answers, after retrying", {
    url <- fu12_silent_server()
    withr::local_envvar(METEOTIDY_NO_NET = "")
    withr::local_options(meteoTidy.http_timeout = 1, meteoTidy.http_backoff_base = 0.1)
    waits <- numeric(0)
    local_mocked_bindings(.http_sleep = function(seconds) waits <<- c(waits, seconds))
    started <- Sys.time()
    err <- expect_error(.http_get(url, retry = 3), class = "meteoTidy_error_http_client_error")
    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    expect_match(conditionMessage(err), "timed out", ignore.case = TRUE)
    expect_length(waits, 2) # three attempts, backoff between them
    expect_lt(elapsed, 20)
  })

  it("defaults to a 60 s timeout", {
    captured <- NULL
    local_mocked_bindings(
      req_perform = function(req, ...) {
        captured <<- req
        httr2::response(status_code = 200, headers = list(`Content-Type` = "application/json"),
                        body = charToRaw("{}"))
      },
      .package = "httr2"
    )
    withr::local_envvar(METEOTIDY_NO_NET = "")
    .http_get("https://api.weather.bom.gov.au/v1/locations/r64bhq/forecasts/hourly")
    expect_equal(captured$options$timeout_ms, 60000)
  })
})
