# Plan 17 — BDD spec for wiring the post-correction consistency pass (item 4).

describe("item 4: correction ends with the physical-consistency pass", {
  it("clips gusts below mean wind and counts the violation", {

    root <- local_store()
    site <- make_test_site(store_root = root)
    issue <- as.POSIXct("2026-01-01 00:00", tz = "UTC")
    valid <- issue + 24 * 3600

    # A raw forecast where, post-correction, gusts (5) would sit BELOW mean wind
    # (9) at the same timestamp — a physical impossibility the pass must clip.
    fc <- tibble::tibble(
      site_id = "test", source = "openmeteo", model = "test_model",
      issue_time = issue, valid_time = valid,
      lead_time = as.difftime(24, units = "hours"),
      member = NA_integer_, stat = NA_character_,
      variable = c("wind_speed_10m", "wind_gusts_10m"), value = c(9, 5)
    )

    corrected <- correct_forecast(root, site, new_forecast(fc), now = issue)

    gust <- corrected$value[corrected$variable == "wind_gusts_10m"]
    speed <- corrected$value[corrected$variable == "wind_speed_10m"]
    expect_gte(gust, speed)                                   # clipped to satisfy gust >= speed
    expect_gte(attr(corrected, "n_violations") %||% 0L, 1L)  # and counted
  })
})
