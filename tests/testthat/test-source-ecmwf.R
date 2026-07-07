# Plan 08 — source_ecmwf(): index parse, message selection, end-to-end fetch,
# and the terra-absent degradation path. No live downloads (range requests are
# mocked through the .http_get() seam).
#
# The committed `small.index`/`small.grib2` fixtures are a genuine excerpt of
# a real ECMWF Open Data `enfo` file: 3 messages, all `param = "2t"`,
# `step = "24"`, members 1-3 (see helper-ecmwf.R for provenance). The real
# `enfo` feed ships no separate control/member-0 (verified live, 2026-07-06).

describe("ecmwf_index_parse()", {
  it("parses the JSON-lines index into messages with byte offsets/lengths", {
    idx <- ecmwf_index_parse(ecmwf_index_lines())
    expect_true(all(c("param", "step", "member", "_offset", "_length") %in%
                      names(idx)))
    expect_equal(nrow(idx), 3)
    expect_setequal(idx$member, c(1, 2, 3))
    expect_true(all(idx$param == "2t"))
    expect_true(all(idx$step == "24"))
    first <- idx[idx$member == 1, ]
    expect_equal(first$`_offset`, 0)
  })

  it("selects only the requested variables × leads × members", {
    idx <- ecmwf_index_parse(ecmwf_index_lines())
    sel <- ecmwf_select_messages(idx, params = "2t", steps = 24, members = c(1, 2))
    expect_setequal(unique(sel$param), "2t")
    expect_setequal(unique(sel$step), "24")
    expect_setequal(sort(unique(sel$member)), c(1, 2))
    expect_false(3 %in% sel$member)
  })
})

describe("fetch_forecast() end-to-end", {
  it("yields canonical rows, or aborts grib_ccsds_unsupported if this build can't decode CCSDS", {
    skip_unless_grib_ready()
    site <- make_test_site()
    adapter <- source_ecmwf()
    win <- list(from = as.POSIXct("2026-01-01 00:00", tz = "UTC"),
                to = as.POSIXct("2026-01-02 00:00", tz = "UTC"))
    # mock the range-download seam to hand back the committed fixture bytes
    testthat::local_mocked_bindings(
      .http_get = function(url, headers = list(), parse = "json", ...) {
        if (grepl("index", url)) return(ecmwf_index_lines())
        readBin(ecmwf_grib_path(), "raw", file.info(ecmwf_grib_path())$size)
      }
    )
    result <- tryCatch(
      fetch_forecast(adapter, site, "temperature_2m", win,
                     now = as.POSIXct("2026-01-01", tz = "UTC")),
      meteoTidy_error_grib_ccsds_unsupported = function(e) e
    )
    if (inherits(result, "condition")) {
      expect_match(conditionMessage(result), "seasonal", ignore.case = TRUE)
    } else {
      expect_canonical_forecast(result)
      expect_type(result$member, "integer")
      expect_setequal(result$member, c(1L, 2L, 3L))
      expect_true(all(result$model == "ifs_enfo"))
      # GDAL already decodes 2t into Celsius (see R/grib-read.R); canonical is
      # degC → plausibly a small surface temperature, not still Kelvin-scale.
      expect_true(all(abs(result$value) < 60))
      # valid_time should be issue_time plus the forecast step
      expect_true(all(result$valid_time > result$issue_time))
    }
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

describe(".ecmwf_resolve_issue_times()", {
  it("rounds down to the real 00/06/12/18Z issue schedule", {
    expect_equal(
      meteoTidy:::.ecmwf_round_down_to_cycle(as.POSIXct("2026-07-06 05:30", tz = "UTC")),
      as.POSIXct("2026-07-06 00:00:00", tz = "UTC")
    )
    expect_equal(
      meteoTidy:::.ecmwf_round_down_to_cycle(as.POSIXct("2026-07-06 23:59", tz = "UTC")),
      as.POSIXct("2026-07-06 18:00:00", tz = "UTC")
    )
  })

  it("returns every eligible cycle spanning issue_window, capped at now", {
    win <- list(from = as.POSIXct("2026-07-06 01:00", tz = "UTC"),
                to = as.POSIXct("2026-07-06 20:00", tz = "UTC"))
    all_cycles <- meteoTidy:::.ecmwf_resolve_issue_times(
      win, now = as.POSIXct("2026-07-06 20:00", tz = "UTC")
    )
    expect_equal(all_cycles, as.POSIXct(
      c("2026-07-06 00:00:00", "2026-07-06 06:00:00",
        "2026-07-06 12:00:00", "2026-07-06 18:00:00"), tz = "UTC"
    ))

    capped <- meteoTidy:::.ecmwf_resolve_issue_times(
      win, now = as.POSIXct("2026-07-06 07:00", tz = "UTC")
    )
    expect_equal(capped, as.POSIXct(
      c("2026-07-06 00:00:00", "2026-07-06 06:00:00"), tz = "UTC"
    ))
  })
})

describe("met_attribution()", {
  it("returns the required CC-BY credit for ECMWF Open Data", {
    att <- met_attribution(source_ecmwf())
    expect_match(att, "ECMWF", ignore.case = TRUE)
  })
})
