# Regression (production review, problem 12): SILO errors were opaque.
#  * SILO's firewall sometimes answers with an HTML "Request Rejected" page
#    (weatherOz re-raises the raw HTML as the error message); the same
#    request through weatherOz::get_data_drill() works again minutes later,
#    so it is transient, not a bad request.
#  * A DataDrill response has no station_code/station_name columns, so the
#    reshape crashed with a base-R error ("arguments imply differing number
#    of rows") instead of returning data -- the live failure on 2026-09-29.
#  * A PatchedPoint adapter with no resolved station sent `station = NA`.
#
# Fixtures were recorded live 2026-09-29 (DataDrill Katoomba, 10 days) and
# from the trial run's rejected response.

silo_fixture <- function(name) fixture_path(file.path("silo", name))

datadrill_frame <- function() {
  utils::read.csv(silo_fixture("datadrill-kat-10d.csv"), stringsAsFactors = FALSE)
}

with_silo_call <- function(fn, expr) {
  testthat::local_mocked_bindings(.weatheroz_call = fn, .env = parent.frame())
  force(expr)
}

silo_window <- function() {
  list(from = as.POSIXct("2026-09-19", tz = "UTC"), to = as.POSIXct("2026-09-28", tz = "UTC"))
}

describe("problem 12: SILO errors are classed and clear", {
  withr::local_envvar(SILO_API_KEY = "someone@example.org")
  site <- make_prod_site("kat")

  it("returns observations from a real DataDrill response (no station columns)", {
    a <- source_silo(api_key_env = "SILO_API_KEY", dataset = "data_drill")
    out <- with_silo_call(function(dataset, args) datadrill_frame(), {
      fetch(a, site, a@provides, silo_window(), now = prod_now())
    })
    expect_gt(nrow(out), 0)
    expect_setequal(unique(out$variable), a@provides)
    rain <- out[out$variable == "precipitation", ]
    expect_equal(nrow(rain), 10L)
    expect_equal(sort(rain$value), sort(datadrill_frame()$rainfall))
  })

  it("turns an HTML 'Request Rejected' page into a transient silo_rejected error", {
    page <- paste(readLines(silo_fixture("request-rejected.html")), collapse = "\n")
    a <- source_silo(api_key_env = "SILO_API_KEY", dataset = "data_drill")
    err <- with_silo_call(function(dataset, args) stop(page, call. = FALSE), {
      tryCatch(fetch(a, site, a@provides, silo_window(), now = prod_now()), error = identity)
    })
    expect_s3_class(err, "meteoTidy_error_silo_rejected")
    msg <- conditionMessage(err)
    expect_false(grepl("<html", msg, fixed = TRUE))
    expect_match(msg, "rejected", ignore.case = TRUE)
    expect_match(msg, "15623724224078599280")
    expect_true(isTRUE(err$transient))
  })

  it("turns SILO's 'Sorry, ... invalid values' text into silo_bad_request", {
    txt <- paste(readLines(silo_fixture("sorry-invalid-values.txt")), collapse = "\n")
    a <- source_silo(api_key_env = "SILO_API_KEY", dataset = "data_drill")
    err <- with_silo_call(function(dataset, args) stop(txt, call. = FALSE), {
      tryCatch(fetch(a, site, a@provides, silo_window(), now = prod_now()), error = identity)
    })
    expect_s3_class(err, "meteoTidy_error_silo_bad_request")
    expect_lt(nchar(conditionMessage(err)), 400)
  })

  it("never puts the API key in an error message", {
    a <- source_silo(api_key_env = "SILO_API_KEY", dataset = "data_drill")
    err <- with_silo_call(function(dataset, args) stop("HTTP (500) - username=someone@example.org", call. = FALSE), {
      tryCatch(fetch(a, site, a@provides, silo_window(), now = prod_now()), error = identity)
    })
    expect_s3_class(err, "meteoTidy_error_silo_failed")
    expect_false(grepl("someone@example.org", conditionMessage(err), fixed = TRUE))
  })

  it("refuses a PatchedPoint fetch with no resolved station before calling SILO", {
    called <- FALSE
    a <- source_silo(api_key_env = "SILO_API_KEY", dataset = "patched_point")
    expect_error(
      with_silo_call(function(dataset, args) { called <<- TRUE; datadrill_frame() }, {
        fetch(a, site, a@provides, silo_window(), now = prod_now())
      }),
      class = "meteoTidy_error_unresolved_station"
    )
    expect_false(called)
  })

  it("reports a missing API key as secret_unresolved", {
    withr::local_envvar(SILO_API_KEY = NA)
    a <- source_silo(api_key_env = "SILO_API_KEY", dataset = "data_drill")
    expect_error(
      with_silo_call(function(dataset, args) datadrill_frame(), {
        fetch(a, site, a@provides, silo_window(), now = prod_now())
      }),
      class = "meteoTidy_error_secret_unresolved"
    )
  })
})

describe("problem 12 (found live): SILO radiation", {
  withr::local_envvar(SILO_API_KEY = "someone@example.org")
  it("stores SILO's daily global radiation (MJ/m2) as mean shortwave_radiation in W/m2", {
    site <- make_prod_site("kat")
    a <- source_silo(api_key_env = "SILO_API_KEY", dataset = "data_drill")
    out <- with_silo_call(function(dataset, args) datadrill_frame(), {
      fetch(a, site, "shortwave_radiation", silo_window(), now = prod_now())
    })
    expect_false("direct_radiation" %in% out$variable)
    expect_equal(sort(out$value), sort(datadrill_frame()$radiation * 1e6 / 86400))
  })
})
