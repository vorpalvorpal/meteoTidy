# Plan 15 — dplyr reconstruction + visible downgrade on invalidating ops.

describe("metadata-preserving operations keep the class", {
  it("a mutate adding a derived column (value cols untouched) stays a met_table", {
    mt <- make_met_table()
    out <- dplyr::mutate(mt, temp_f = temperature_2m * 9 / 5 + 32)
    expect_s3_class(out, "met_table")
    expect_equal(nrow(met_provenance(out)), 2)   # provenance intact
  })
})

describe("invalidating operations downgrade visibly (not silently)", {
  it("dropping a value column downgrades to a plain tibble with a warning", {
    mt <- make_met_table()
    expect_warning(
      out <- dplyr::select(mt, time, temperature_2m),  # drops wind_speed_10m
      class = "meteoTidy_warning_met_table_downgraded"
    )
    expect_false(inherits(out, "met_table"))
    expect_s3_class(out, "tbl_df")
  })

  it("an incompatible bind_rows (mixed provenance) downgrades with a warning", {
    a <- make_met_table(provenance = make_provenance(tier = "qmap"))
    b <- make_met_table(provenance = make_provenance(tier = "raw"))
    expect_warning(
      out <- dplyr::bind_rows(a, b),
      class = "meteoTidy_warning_met_table_downgraded"
    )
    expect_false(inherits(out, "met_table"))
  })
})
