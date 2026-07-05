# Plan 04 — source_rest() (all mocked, no live calls).

describe("apply_mapping() on a REST body", {
  it("produces canonical rows with UTC time and units converted (km/h -> m/s)", {
    body <- read_json_fixture("_fixtures/rest/hourly-kmh.json")
    site <- make_test_site()
    out <- apply_mapping(body, make_rest_mapping(), site,
                         source_id = "site_aws",
                         now = as.POSIXct("2026-01-01", tz = "UTC"))
    expect_canonical_obs(out)
    # the fixture serves wind in km/h; the canonical output must be m/s
    wind <- out$value[out$variable == "wind_speed_10m"]
    expect_equal(wind[1], 10 / 3.6, tolerance = 1e-6)
  })
})

describe("source_rest() request plumbing", {
  it("interpolates {site}/{from}/{to} into the endpoint template", {
    cap <- new.env()
    site <- make_test_site(site_id = "piggery")
    adapter <- source_rest("site_aws",
                           "https://aws.example/api?site={site}&from={from}&to={to}",
                           make_rest_mapping())
    win <- list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                to = as.POSIXct("2026-01-02", tz = "UTC"))
    body <- read_json_fixture("_fixtures/rest/hourly-kmh.json")
    with_mocked_http(body, {
      fetch(adapter, site, "temperature_2m", win)
    }, capture = cap)
    expect_match(cap$url, "site=piggery")
    expect_match(cap$url, "from=2026-01-01")
  })

  it("reads the auth token from the named env var and never leaks it", {
    withr::local_envvar(PIGGERY_AWS_TOKEN = "s3cr3t-value")
    adapter <- source_rest("site_aws", "https://aws.example/{site}",
                           make_rest_mapping(), auth = "header",
                           token_env = "PIGGERY_AWS_TOKEN")
    printed <- paste(utils::capture.output(print(adapter)), collapse = "\n")
    expect_false(grepl("s3cr3t-value", printed))
    formatted <- paste(format(adapter), collapse = "\n")
    expect_false(grepl("s3cr3t-value", formatted))
  })

  it("aborts unsupported_response on a paginated-looking body", {
    site <- make_test_site()
    paginated <- list(next_cursor = "abc", hourly = list())
    adapter <- source_rest("site_aws", "https://x/{site}", make_rest_mapping())
    win <- list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                to = as.POSIXct("2026-01-02", tz = "UTC"))
    with_mocked_http(paginated, {
      expect_error(fetch(adapter, site, "temperature_2m", win),
                   class = "meteoTidy_error_unsupported_response")
    })
  })

  it("returns a 0-row canonical table (not NULL) for an empty response", {
    site <- make_test_site()
    empty <- list(hourly = list(time = list(), temperature_2m = list()))
    adapter <- source_rest("site_aws", "https://x/{site}", make_rest_mapping())
    win <- list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                to = as.POSIXct("2026-01-02", tz = "UTC"))
    with_mocked_http(empty, {
      out <- fetch(adapter, site, "temperature_2m", win)
    })
    expect_s3_class(out, "tbl_df")
    expect_equal(nrow(out), 0)
  })
})
