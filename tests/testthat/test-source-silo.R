# Plan 06 — source_silo() over weatherOz (daily). All mocked; no live calls.

describe("source_silo() fetch → canonical daily obs", {
  it("maps a PatchedPoint frame to canonical rows with units converted", {
    site <- make_test_site()
    adapter <- source_silo(api_key_env = "SILO_EMAIL", dataset = "patched_point")
    win <- list(
      from = as.POSIXct("2026-01-01", tz = "UTC"),
      to = as.POSIXct("2026-12-31", tz = "UTC")
    )
    with_mocked_silo(make_silo_frame(), {
      out <- fetch(adapter, site, "temperature_2m", win)
    })
    expect_canonical_obs(out)
    expect_true(all(out$source == "silo"))
  })

  it("maps each SILO day to the documented 9am local-clock instant, DST-aware", {
    # SCOPING §3: the daily boundary is local clock time in the site IANA zone,
    # DST-inclusive (the 9am rainfall-day convention). Australia/Sydney is
    # UTC+11 in January (AEDT) and UTC+10 in July (AEST), so the *same* 9am
    # wall-clock maps to UTC instants an hour apart across the DST boundary.
    site <- make_test_site() # timezone Australia/Sydney
    adapter <- source_silo(api_key_env = "SILO_EMAIL", dataset = "patched_point")
    win <- list(
      from = as.POSIXct("2026-01-01", tz = "UTC"),
      to = as.POSIXct("2026-12-31", tz = "UTC")
    )
    frame <- make_silo_frame(dates = as.Date(c("2026-01-15", "2026-07-15")))
    with_mocked_silo(frame, {
      out <- fetch(adapter, site, "temperature_2m", win)
    })
    t <- out$datetime_utc[order(out$datetime_utc)]
    summer_9am <- as.POSIXct("2026-01-15 09:00", tz = "Australia/Sydney")
    winter_9am <- as.POSIXct("2026-07-15 09:00", tz = "Australia/Sydney")
    attr(summer_9am, "tzone") <- "UTC"
    attr(winter_9am, "tzone") <- "UTC"
    expect_true(as.POSIXct("2026-01-14 22:00", tz = "UTC") %in% out$datetime_utc)
    expect_true(as.POSIXct("2026-07-14 23:00", tz = "UTC") %in% out$datetime_utc)
  })

  it("reads the email from the named env var and never emits it in rows", {
    withr::local_envvar(SILO_EMAIL = "someone@example.org")
    site <- make_test_site()
    adapter <- source_silo(api_key_env = "SILO_EMAIL")
    win <- list(
      from = as.POSIXct("2026-01-01", tz = "UTC"),
      to = as.POSIXct("2026-12-31", tz = "UTC")
    )
    cap <- new.env()
    with_mocked_silo(make_silo_frame(),
      {
        out <- fetch(adapter, site, "temperature_2m", win)
      },
      capture = cap
    )
    expect_equal(cap$api_key, "someone@example.org") # passed to weatherOz
    leaked <- vapply(out, function(c) {
      any(grepl(
        "someone@example.org",
        as.character(c)
      ))
    }, logical(1))
    expect_false(any(leaked)) # but never in the data
  })
})

describe("source_silo() resolve_station()", {
  it("fills site@resolved$silo and returns a new site (functional)", {
    site <- make_test_site()
    adapter <- source_silo(api_key_env = "SILO_EMAIL", dataset = "patched_point")
    cat <- make_station_catalogue()
    resolved <- with_mocked_silo(cat, resolve_station(adapter, site))
    expect_false(is.na(site_resolved(resolved, c("silo", "station"))))
    # original untouched
    expect_true(is.na(site_resolved(site, c("silo", "station"))))
  })
})
