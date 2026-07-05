# Plan 15 — per-column content hashing (the review fix): dplyr_reconstruct can't
# see in-place value mutation, so the boundary re-hash makes it detectable.

describe("met_validate_boundary()", {
  it("marks a value column unverified after an in-place mutation", {
    mt <- make_met_table()
    # dplyr_reconstruct keeps the class + provenance through this mutate...
    mutated <- dplyr::mutate(mt, temperature_2m = temperature_2m + 1)
    # ...but the boundary re-hash detects the silent value change.
    checked <- met_validate_boundary(mutated)
    prov <- met_provenance(checked)
    temp_tier <- prov$tier[prov$variable == "temperature_2m"]
    wind_tier <- prov$tier[prov$variable == "wind_speed_10m"]
    expect_equal(temp_tier, "unverified")        # downgraded
    expect_false(wind_tier == "unverified")      # untouched column intact
  })

  it("passes an untouched table with provenance intact", {
    mt <- make_met_table()
    checked <- met_validate_boundary(mt)
    expect_false(any(met_provenance(checked)$tier == "unverified"))
  })
})
