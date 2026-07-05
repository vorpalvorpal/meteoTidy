# Plan 02 — site YAML (de)serialisation.

describe("read_sites_yaml()", {
  it("attaches metre units to height slots on read", {
    sites <- read_sites_yaml(test_path("_fixtures/sites/one-site.yaml"))
    site <- as_met_sites(sites)[[1]]
    h <- site_instruments(site)[[1]]@height
    expect_equal(as.character(units::deparse_unit(h)), "m")
    expect_equal(as.numeric(h), 10)
  })

  it("loads a multi-site file into a met_sites of unique ids", {
    sites <- read_sites_yaml(test_path("_fixtures/sites/multi-site.yaml"))
    expect_length(site_ids(sites), 2)
    expect_false(any(duplicated(site_ids(sites))))
  })

  it("aborts on an inline secret value", {
    expect_error(
      read_sites_yaml(test_path("_fixtures/sites/bad-inline-secret.yaml")),
      class = "meteoTidy_error_inline_secret"
    )
  })

  it("aborts on an unknown top-level key (typos fail loud)", {
    expect_error(
      read_sites_yaml(test_path("_fixtures/sites/bad-unknown-key.yaml")),
      class = "meteoTidy_error_unknown_config_key"
    )
  })
})

describe("write_sites_yaml()", {
  it("round-trips read -> write -> read to an equivalent met_sites", {
    tmp <- withr::local_tempfile(fileext = ".yaml")
    orig <- read_sites_yaml(test_path("_fixtures/sites/one-site.yaml"))
    write_sites_yaml(orig, tmp)
    again <- read_sites_yaml(tmp)
    expect_equal(site_ids(again), site_ids(orig))
  })

  it("excludes the resolved cache by default and includes it when asked", {
    tmp <- withr::local_tempfile(fileext = ".yaml")
    site <- make_test_site()
    site <- site_set_resolved(site, c("ghcnh", "station_id"), "ASN00072150")
    sites <- met_sites(list(site))

    write_sites_yaml(sites, tmp)
    expect_false(any(grepl("ASN00072150", readLines(tmp))))

    write_sites_yaml(sites, tmp, include_resolved = TRUE)
    expect_true(any(grepl("ASN00072150", readLines(tmp))))
  })
})
