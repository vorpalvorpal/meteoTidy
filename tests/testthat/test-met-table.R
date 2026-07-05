# Plan 15 — new_met_table() construction, accessors, print, validator.

describe("new_met_table()", {
  it("builds a valid classed tibble that is still a tbl_df", {
    mt <- make_met_table()
    expect_s3_class(mt, "met_table")
    expect_s3_class(mt, "tbl_df")
    expect_equal(met_provenance(mt)$variable,
                 c("temperature_2m", "wind_speed_10m"))
    expect_equal(met_keys(mt)$site_id, "test")
    expect_equal(met_versions(mt)$schema_version, "1.0.0")
  })

  it("preserves class and provenance through class-preserving dplyr verbs", {
    mt <- make_met_table()
    f <- dplyr::filter(mt, temperature_2m > 15)
    a <- dplyr::arrange(mt, dplyr::desc(time))
    s <- dplyr::select(mt, time, temperature_2m, wind_speed_10m)
    for (x in list(f, a, s)) {
      expect_s3_class(x, "met_table")
      expect_equal(nrow(met_provenance(x)), 2)
    }
  })
})

describe("print() shows the provenance banner", {
  it("renders the compact per-column tier banner (snapshot)", {
    expect_snapshot(print(make_met_table()))
  })
})

describe("validator", {
  it("rejects a table whose provenance misses a value column", {
    expect_error(
      new_met_table(make_wide_tbl(),
                    provenance = make_provenance(variables = "temperature_2m"),
                    keys = list(site_id = "test"),
                    versions = list(schema_version = "1.0.0",
                                    calibration_manifest_version = 1L)),
      class = "meteoTidy_error_provenance_incomplete"
    )
  })

  it("rejects a table missing version metadata", {
    expect_error(
      new_met_table(make_wide_tbl(), provenance = make_provenance(),
                    keys = list(site_id = "test"), versions = list()),
      class = "meteoTidy_error_missing_versions"
    )
  })
})
