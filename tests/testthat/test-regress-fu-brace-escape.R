# Found while fixing item 6: two error paths "escaped" braces in upstream
# text with gsub("([{}])", "\1\1", ...). In R source "\1" is the control
# character U+0001, not a back-reference, so each brace became two control
# characters and the message lost the text it was meant to quote.

describe("brace escaping in upstream error text", {
  it("keeps SILO's own braces in the error message", {
    body <- "Sorry, your request contains invalid values: {\"start\": \"2026-13-01\"}"
    err <- tryCatch(.silo_abort(body, api_key = "", parent = NULL), error = identity)
    expect_s3_class(err, "meteoTidy_error_silo_bad_request")
    expect_match(conditionMessage(err), "{\"start\"", fixed = TRUE)
    expect_false(grepl("\001", conditionMessage(err), fixed = TRUE))
  })

  it("keeps braces from a failed Open-Meteo metadata request", {
    routes <- openmeteo_routes()
    routes[["/data/ecmwf_ifs025/static/meta.json"]] <- function(url) {
      abort_meteo("HTTP 400 {{\"reason\": \"bad\"}}", class = "http_client_error")
    }
    err <- tryCatch(
      with_routed_http(routes, .openmeteo_issue_time("forecast", "ecmwf_ifs025", NULL, prod_now())),
      error = identity
    )
    expect_s3_class(err, "meteoTidy_error_openmeteo_run_unknown")
    expect_match(conditionMessage(err), "{\"reason\"", fixed = TRUE)
  })
})
