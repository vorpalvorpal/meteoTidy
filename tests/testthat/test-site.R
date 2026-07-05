# Plan 02 — met_site S7 class + validator + resolved-ID cache.

describe("met_site validator", {
  it("constructs a minimal valid site", {
    expect_no_error(make_test_site())
  })

  it("rejects out-of-range coordinates, bad timezone, and bad site_id", {
    expect_error(
      met_site(site_id = "s", latitude = units::set_units(-999, "degree"),
               longitude = units::set_units(0, "degree"),
               elevation = units::set_units(10, "m"),
               timezone = "Australia/Sydney", instruments = list(),
               sources = list(), store_root = tempfile()),
      class = "meteoTidy_error_bad_coordinates"
    )
    # longitude is range-checked too (−180..180), not only latitude
    expect_error(
      met_site(site_id = "s", latitude = units::set_units(0, "degree"),
               longitude = units::set_units(999, "degree"),
               elevation = units::set_units(10, "m"),
               timezone = "Australia/Sydney", instruments = list(),
               sources = list(), store_root = tempfile()),
      class = "meteoTidy_error_bad_coordinates"
    )
    expect_error(
      met_site(site_id = "s", latitude = units::set_units(0, "degree"),
               longitude = units::set_units(0, "degree"),
               elevation = units::set_units(10, "m"),
               timezone = "Mars/Olympus", instruments = list(),
               sources = list(), store_root = tempfile()),
      class = "meteoTidy_error_bad_timezone"
    )
    expect_error(
      met_site(site_id = "has spaces!", latitude = units::set_units(0, "degree"),
               longitude = units::set_units(0, "degree"),
               elevation = units::set_units(10, "m"),
               timezone = "Australia/Sydney", instruments = list(),
               sources = list(), store_root = tempfile()),
      class = "meteoTidy_error_bad_site_id"
    )
    # an empty site_id is rejected too (site_id is the join key everywhere)
    expect_error(
      met_site(site_id = "", latitude = units::set_units(0, "degree"),
               longitude = units::set_units(0, "degree"),
               elevation = units::set_units(10, "m"),
               timezone = "Australia/Sydney", instruments = list(),
               sources = list(), store_root = tempfile()),
      class = "meteoTidy_error_bad_site_id"
    )
  })

  it("requires roughness on a wind instrument (the review fix)", {
    no_z0 <- met_instrument(
      name = "anemometer", variable = "wind_speed_10m",
      height = units::set_units(10, "m")
    )
    expect_error(
      met_site(site_id = "s", latitude = units::set_units(0, "degree"),
               longitude = units::set_units(0, "degree"),
               elevation = units::set_units(10, "m"),
               timezone = "Australia/Sydney", instruments = list(no_z0),
               sources = list(), store_root = tempfile()),
      class = "meteoTidy_error_missing_roughness"
    )
    # supplying z0 fixes it
    expect_no_error(make_test_site(with_wind = TRUE))
  })

  it("rejects an instrument measuring an unknown dictionary variable", {
    bad <- met_instrument(name = "x", variable = "unicorn_flux",
                          height = units::set_units(2, "m"))
    expect_error(
      met_site(site_id = "s", latitude = units::set_units(0, "degree"),
               longitude = units::set_units(0, "degree"),
               elevation = units::set_units(10, "m"),
               timezone = "Australia/Sydney", instruments = list(bad),
               sources = list(), store_root = tempfile()),
      class = "meteoTidy_error_unknown_variable"
    )
  })
})

describe("site_set_resolved()", {
  it("returns a new site and does not mutate the original (functional)", {
    site <- make_test_site()
    updated <- site_set_resolved(site, c("ghcnh", "station_id"), "ASN00072150")
    expect_equal(site_resolved(updated, c("ghcnh", "station_id")), "ASN00072150")
    # the original is untouched
    expect_true(is.na(site_resolved(site, c("ghcnh", "station_id"))))
  })
})

describe("met_sites()", {
  it("rejects duplicate site_ids and supports lookup by id", {
    a <- make_test_site("dup")
    b <- make_test_site("dup")
    expect_error(met_sites(list(a, b)),
                 class = "meteoTidy_error_duplicate_site_id")
    sites <- make_test_sites(2)
    expect_s3_class(sites[["site_1"]], "meteoTidy::met_site")
    expect_setequal(site_ids(sites), c("site_1", "site_2"))
  })
})
