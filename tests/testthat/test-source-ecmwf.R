# Plan 08 — source_ecmwf(): index parse, message selection, end-to-end fetch,
# and the terra-absent degradation path. No live downloads (range requests are
# mocked through the .http_get() seam).

describe("ecmwf_index_parse()", {
  it("parses the JSON-lines index into messages with byte offsets/lengths", {
    idx <- ecmwf_index_parse(ecmwf_index_lines())
    expect_true(all(c("param", "step", "member", "_offset", "_length") %in%
                      names(idx)))
    expect_equal(nrow(idx), 6)
    expect_type(idx$`_offset`, "double")
    # the control message for 2t/step24 sits at offset 0
    first <- idx[idx$param == "2t" & idx$step == "24" & idx$member == 0, ]
    expect_equal(first$`_offset`, 0)
  })

  it("selects only the requested variables × leads × members", {
    idx <- ecmwf_index_parse(ecmwf_index_lines())
    sel <- ecmwf_select_messages(idx, params = "2t", steps = 24,
                                 members = c(0, 1))
    expect_setequal(unique(sel$param), "2t")
    expect_setequal(unique(sel$step), "24")
    expect_setequal(sort(unique(sel$member)), c(0, 1))
    # the step-48 and 10u/10v messages are excluded
    expect_false(any(sel$step == "48"))
    expect_false(any(sel$param %in% c("10u", "10v")))
  })
})

describe("fetch_forecast() end-to-end (terra present)", {
  it("yields canonical forecast rows with integer member and units converted", {
    skip_unless_grib_ready()
    site <- make_test_site()
    adapter <- source_ecmwf(stream = "eefo", resolution = "0p25")
    win <- list(from = as.POSIXct("2026-01-01 00:00", tz = "UTC"),
                to = as.POSIXct("2026-01-01 06:00", tz = "UTC"))
    # mock the range-download seam to hand back the committed fixture bytes
    testthat::local_mocked_bindings(
      .http_get = function(url, headers = list(), ...) {
        if (grepl("index", url)) return(ecmwf_index_lines())
        readBin(ecmwf_grib_path(), "raw", file.info(ecmwf_grib_path())$size)
      }
    )
    out <- fetch_forecast(adapter, site, "temperature_2m", win,
                          now = as.POSIXct("2026-01-01", tz = "UTC"))
    expect_canonical_forecast(out)
    expect_type(out$member, "integer")
    expect_true(all(out$model == "ifs_eefo"))
    # GRIB temperature is Kelvin; canonical is degC → plausibly < 60
    expect_true(all(out$value < 60))
    # valid_time = issue_time + step
    expect_true(all(out$valid_time > out$issue_time))
  })
})

describe("graceful degradation when terra is absent", {
  it("aborts terra_required pointing at the Open-Meteo seasonal splice", {
    site <- make_test_site()
    adapter <- source_ecmwf()
    win <- list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                to = as.POSIXct("2026-01-02", tz = "UTC"))
    # simulate terra absence via the guard seam
    testthat::local_mocked_bindings(.have_terra = function() FALSE)
    err <- expect_error(
      fetch_forecast(adapter, site, "temperature_2m", win,
                     now = as.POSIXct("2026-01-01", tz = "UTC")),
      class = "meteoTidy_error_terra_required"
    )
    expect_match(conditionMessage(err), "seasonal", ignore.case = TRUE)
  })
})

describe("met_attribution()", {
  it("returns the required CC-BY credit for ECMWF Open Data", {
    att <- met_attribution(source_ecmwf())
    expect_match(att, "ECMWF", ignore.case = TRUE)
  })
})
