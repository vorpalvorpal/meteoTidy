# Plan 05 — Open-Meteo licensing guardrails (SCOPING §10). Mocked; no live calls.
# The free tier serves every product — NO product aborts for lack of a key. The
# key, when present, must never leak into print/format/provenance.

describe("keyless requests (free host, non-commercial)", {
  it("serves a historical/ensemble request with no key and no key gate", {
    withr::local_envvar(OPEN_METEO_KEY = NA)      # ensure unset
    expect_no_error(
      om_fetch("historical", read_om_fixture("historical-era5.json"))
    )
    expect_no_error(
      om_fetch("ensemble", read_om_fixture("ensemble.json"))
    )
  })

  it("targets the FREE host when no key env var is configured", {
    cap <- new.env()
    om_fetch("forecast", read_om_fixture("forecast.json"), capture = cap)
    expect_false(grepl("customer-", cap$url))
    expect_match(cap$url, "open-meteo\\.com")
  })

  it("emits the non-commercial notice exactly once (snapshot)", {
    # The free tier is licensed for non-commercial use only; the adapter warns
    # once via inform_meteo(). Snapshot the message text.
    expect_snapshot({
      adapter <- source_openmeteo(product = "forecast")
      with_mocked_http(read_om_fixture("forecast.json"), {
        invisible(fetch_forecast(
          adapter, make_test_site(), "temperature_2m",
          list(from = as.POSIXct("2026-01-01", tz = "UTC"),
               to = as.POSIXct("2026-01-02", tz = "UTC")),
          now = om_now()
        ))
      })
    })
  })
})

describe("keyed requests (commercial host, secret hygiene)", {
  it("targets the customer host and sends the key when the env var is set", {
    withr::local_envvar(OPEN_METEO_KEY = "s3cr3t-key")
    cap <- new.env()
    om_fetch("historical", read_om_fixture("historical-era5.json"),
             api_key_env = "OPEN_METEO_KEY", capture = cap)
    expect_match(cap$url, "customer-")
    q <- paste(unlist(cap$query), cap$url, collapse = "&")
    expect_match(q, "s3cr3t-key")            # the key IS sent on the wire
  })

  it("never leaks the key through print() or format()", {
    withr::local_envvar(OPEN_METEO_KEY = "s3cr3t-key")
    adapter <- source_openmeteo(product = "historical",
                                api_key_env = "OPEN_METEO_KEY")
    printed <- paste(utils::capture.output(print(adapter)), collapse = "\n")
    formatted <- paste(format(adapter), collapse = "\n")
    expect_false(grepl("s3cr3t-key", printed))
    expect_false(grepl("s3cr3t-key", formatted))
  })

  it("never writes the key into returned provenance columns", {
    withr::local_envvar(OPEN_METEO_KEY = "s3cr3t-key")
    out <- om_fetch("historical", read_om_fixture("historical-era5.json"),
                    api_key_env = "OPEN_METEO_KEY")
    leaked <- vapply(out, function(col) any(grepl("s3cr3t-key", as.character(col))),
                     logical(1))
    expect_false(any(leaked))
  })
})

describe("met_attribution()", {
  it("exposes the CC-BY credit string for dashboards/reports", {
    adapter <- source_openmeteo(product = "forecast")
    att <- met_attribution(adapter)
    expect_type(att, "character")
    expect_match(att, "Open-Meteo", ignore.case = TRUE)
  })
})
