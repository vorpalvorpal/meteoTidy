# Plan 15 — met_ingest(): the dual-accept boundary validator + single-tier rule.

describe("dual-accept boundary", {
  it("validates a classed input once and trusts its provenance thereafter", {
    mt <- make_met_table()
    out <- met_ingest(mt)
    expect_s3_class(out, "met_table")
    # provenance trusted (not forced to unverified) for an untouched classed input
    expect_false(any(met_provenance(out)$tier == "unverified"))
  })

  it("validates a plain tibble on entry and marks its provenance unverified", {
    plain <- make_wide_tbl()
    out <- met_ingest(plain)
    # accepted after schema validation, but provenance is unverified
    prov <- met_provenance(out)
    expect_true(all(prov$tier == "unverified"))
  })

  it("rejects a plain tibble that violates the §3.1 schema", {
    bad <- tibble::tibble(not_a_time = 1, temperature_2m = 20)
    expect_error(met_ingest(bad),
                 class = "meteoTidy_error_schema_violation")
  })
})

describe("met_assert_single_tier() — the single-provenance-class rule", {
  it("warns when a derived index mixes a corrected 10 m and a raw 80 m wind", {
    prov <- tibble::tibble(
      variable = c("wind_speed_10m", "wind_speed_80m"),
      tier = c("qmap", "raw"),                  # mixed provenance classes
      train_overlap = c(24, 0), source = "openmeteo"
    )
    mt <- make_met_table(
      x = tibble::tibble(time = as.POSIXct("2026-01-01", tz = "UTC"),
                         wind_speed_10m = 5, wind_speed_80m = 9),
      provenance = prov)
    expect_warning(
      met_assert_single_tier(mt, c("wind_speed_10m", "wind_speed_80m")),
      class = "meteoTidy_warning_mixed_tier"
    )
  })

  it("is silent when all inputs to the derived index share a tier", {
    prov <- tibble::tibble(
      variable = c("wind_speed_10m", "wind_speed_80m"),
      tier = c("qmap", "qmap"), train_overlap = 24, source = "openmeteo")
    mt <- make_met_table(
      x = tibble::tibble(time = as.POSIXct("2026-01-01", tz = "UTC"),
                         wind_speed_10m = 5, wind_speed_80m = 9),
      provenance = prov)
    expect_no_warning(
      met_assert_single_tier(mt, c("wind_speed_10m", "wind_speed_80m"))
    )
  })
})
