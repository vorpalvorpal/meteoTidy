# Regression (production review, problem 1): the forecast archive could not be
# read back. `lead_time` was written to Parquet as duration[s], which truncates
# sub-second fractions, while new_forecast() on the read path demanded
# |valid - issue - lead| <= 1e-6 h. A real sync clock has fractional seconds,
# so every archived row failed `lead_inconsistent` on read.

describe("forecast lead_time survives a real Parquet round-trip", {
  it("reads back rows whose issue_time carries fractional seconds", {
    root <- local_store()
    # A realistic wall-clock issue time (Sys.time() has sub-second precision).
    issue <- as.POSIXct("2026-09-29 06:05:13.456789", tz = "UTC")
    valid <- as.POSIXct("2026-09-29 07:00:00", tz = "UTC") + 3600 * 0:47
    fc <- tibble::tibble(
      site_id = "kat", source = "openmeteo", model = "best_match",
      issue_time = issue, valid_time = valid,
      lead_time = as.difftime(as.numeric(difftime(valid, issue, units = "hours")),
                              units = "hours"),
      member = NA_integer_, stat = NA_character_,
      variable = "temperature_2m", value = seq_along(valid) / 2
    )

    store_write_forecast(root, fc)
    back <- store_read_forecast(root, "kat")

    expect_equal(nrow(back), 48L)
    expect_true(all(abs(as.numeric(back$lead_time, units = "secs") -
                        as.numeric(difftime(back$valid_time, back$issue_time, units = "secs"))) < 1))
  })

  it("stores lead_time as whole seconds consistently", {
    issue <- as.POSIXct("2026-09-29 06:05:13.9", tz = "UTC")
    valid <- issue + 3600.6
    fc <- new_forecast(tibble::tibble(
      site_id = "kat", source = "openmeteo", model = "m",
      issue_time = issue, valid_time = valid,
      lead_time = as.difftime(3600.6, units = "secs"),
      member = NA_integer_, stat = NA_character_,
      variable = "temperature_2m", value = 1
    ))
    secs <- as.numeric(fc$lead_time, units = "secs")
    expect_equal(secs, round(secs))
  })

  it("still rejects a genuinely inconsistent lead_time", {
    issue <- as.POSIXct("2026-09-29 06:00:00", tz = "UTC")
    expect_error(
      new_forecast(tibble::tibble(
        site_id = "kat", source = "openmeteo", model = "m",
        issue_time = issue, valid_time = issue + 7200,
        lead_time = as.difftime(1, units = "hours"),
        member = NA_integer_, stat = NA_character_,
        variable = "temperature_2m", value = 1
      )),
      class = "meteoTidy_error_lead_inconsistent"
    )
  })

  it("met_forecast_archive() reads an archive written with a fractional clock", {
    root <- local_store()
    site <- make_test_site(store_root = root)
    issue <- as.POSIXct("2026-09-29 06:05:13.25", tz = "UTC")
    valid <- as.POSIXct("2026-09-29 07:00:00", tz = "UTC") + 3600 * 0:5
    store_write_forecast(root, tibble::tibble(
      site_id = site_id(site), source = "openmeteo", model = "best_match",
      issue_time = issue, valid_time = valid,
      lead_time = as.difftime(as.numeric(difftime(valid, issue, units = "hours")),
                              units = "hours"),
      member = NA_integer_, stat = NA_character_,
      variable = "temperature_2m", value = 20
    ))
    out <- met_forecast_archive(site, valid_from = min(valid), valid_to = max(valid))
    expect_equal(nrow(out), 6L)
  })
})
