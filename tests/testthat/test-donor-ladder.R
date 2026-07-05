# Plan 10 — donor selection order + dedup by physical station identity.

describe("rank_donors() ordering", {
  it("ranks BOM → GHCNh → ERA5 → SILO when all are present", {
    site <- make_test_site()
    ranked <- rank_donors(site, variable = "temperature_2m",
                          window = list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                                        to = as.POSIXct("2026-02-01", tz = "UTC")),
                          available = donor_catalogue())
    expect_equal(vapply(ranked, function(d) d$source, character(1)),
                 c("bom_obs", "ghcnh", "openmeteo", "silo"))
  })
})

describe("dedup by station identity (the review fix)", {
  it("collapses one station served as both BOM and GHCNh to a single donor", {
    site <- make_test_site()
    # bom and ghcnh share identity '072150' — they are the same physical station
    ranked <- rank_donors(site, variable = "temperature_2m",
                          window = list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                                        to = as.POSIXct("2026-02-01", tz = "UTC")),
                          available = donor_catalogue())
    identities <- vapply(ranked, function(d) d$identity, character(1))
    expect_false(any(duplicated(identities)))   # '072150' appears once
    # the higher-priority transport (BOM) is the one kept
    kept <- ranked[[which(identities == "072150")]]
    expect_equal(kept$source, "bom_obs")
  })

  it("still fills after dedup (the collapsed donor remains usable)", {
    site <- make_test_site()
    ranked <- rank_donors(site, variable = "temperature_2m",
                          window = list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                                        to = as.POSIXct("2026-02-01", tz = "UTC")),
                          available = donor_catalogue())
    expect_gt(length(ranked), 0)
  })
})
