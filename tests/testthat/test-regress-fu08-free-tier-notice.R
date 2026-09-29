# Follow-up review, item 8: the free-tier notice on every call, and the
# commercial key only half wired.
#
# Every Open-Meteo fetch printed the two-line non-commercial notice, so an
# hourly sync of two sites logged it four or more times an hour and the
# production script had to muffle the message class. It is now shown once
# per R session (option meteoTidy.openmeteo_free_tier_notice = "once",
# "always" or "never"). With `api_key_env` set, EVERY request -- the
# metadata lookups that give each run's time included -- must go to the
# customer- host with the key; the metadata request used the free host.

fu08_fetch <- function(adapter, capture = new.env()) {
  with_routed_http(c(list("customer-.*/data/ecmwf_ifs025/static/meta\\.json" =
                            "openmeteo/meta-ecmwf_ifs025.json",
                          "customer-api\\.open-meteo\\.com/v1/forecast" =
                            "openmeteo/forecast-ecmwf-ifs025-undefined.json"),
                     openmeteo_routes()), {
    suppressWarnings(fetch_forecast(adapter, make_prod_site("kat"), "temperature_2m",
                                    list(from = prod_now() - 86400, to = prod_now()),
                                    now = prod_now()))
  }, capture = capture)
}

count_notices <- function(expr) {
  n <- 0L
  withCallingHandlers(expr, meteoTidy_message_openmeteo_free_tier = function(m) {
    n <<- n + 1L
    invokeRestart("muffleMessage")
  })
  n
}

describe("item 8: free-tier notice and commercial key", {
  it("shows the free-tier notice once per session by default", {
    .openmeteo_reset_free_tier_notice()
    withr::defer(.openmeteo_reset_free_tier_notice())
    adapter <- source_openmeteo("forecast", models = "ecmwf_ifs025")
    n <- count_notices({
      fu08_fetch(adapter)
      fu08_fetch(adapter)
      fu08_fetch(source_openmeteo("ensemble", models = "ecmwf_ifs025", provides = "temperature_2m"))
    })
    expect_equal(n, 1L)
  })

  it("can show it every time, or never", {
    .openmeteo_reset_free_tier_notice()
    withr::defer(.openmeteo_reset_free_tier_notice())
    adapter <- source_openmeteo("forecast", models = "ecmwf_ifs025")
    withr::with_options(list(meteoTidy.openmeteo_free_tier_notice = "always"), {
      expect_equal(count_notices({
        fu08_fetch(adapter)
        fu08_fetch(adapter)
      }), 2L)
    })
    .openmeteo_reset_free_tier_notice()
    withr::with_options(list(meteoTidy.openmeteo_free_tier_notice = "never"), {
      expect_equal(count_notices(fu08_fetch(adapter)), 0L)
    })
  })

  it("sends every request, metadata included, to the customer host with the key", {
    withr::local_envvar(FU08_OM_KEY = "k-9f2e1d")
    cap <- new.env()
    n <- count_notices(
      fc <- fu08_fetch(source_openmeteo("forecast", models = "ecmwf_ifs025",
                                        api_key_env = "FU08_OM_KEY"), capture = cap)
    )
    expect_equal(n, 0L)
    expect_length(cap$urls, 2)
    expect_true(all(grepl("^https://customer-", cap$urls)))
    expect_true(all(grepl("apikey=k-9f2e1d", cap$urls, fixed = TRUE)))
    expect_gt(nrow(fc), 0)
    expect_false(any(vapply(fc, function(col) any(grepl("k-9f2e1d", as.character(col))), logical(1))))
  })

  it("never puts the key in an error message (and so the sync log)", {
    withr::local_envvar(METEOTIDY_NO_NET = "")
    local_mocked_bindings(
      req_perform = function(req, ...) httr2::response(status_code = 401),
      .package = "httr2"
    )
    url <- .openmeteo_meta_url("ensemble", "ecmwf_ifs025", api_key = "k-9f2e1d")
    expect_match(url, "^https://customer-ensemble-api\\.open-meteo\\.com/.*apikey=k-9f2e1d")
    err <- tryCatch(.http_get(url), error = identity)
    expect_s3_class(err, "meteoTidy_error_http_client_error")
    expect_no_match(conditionMessage(err), "k-9f2e1d", fixed = TRUE)
    expect_match(conditionMessage(err), "apikey=<redacted>", fixed = TRUE)
  })

  it("falls back to the free host when the key variable is empty", {
    withr::local_envvar(FU08_OM_KEY = "")
    cap <- new.env()
    fu08_fetch(source_openmeteo("forecast", models = "ecmwf_ifs025", api_key_env = "FU08_OM_KEY"),
               capture = cap)
    expect_false(any(grepl("customer-|apikey", cap$urls)))
  })
})
