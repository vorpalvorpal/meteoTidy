# Plan 05 — Open-Meteo forecast products (forecast / ensemble / previous / hist).
# All mocked; no live calls. Issue time is the frozen `om_now()`.

describe("deterministic forecast", {
  it("builds canonical forecast rows with member/stat both NA", {
    out <- om_fetch("forecast", read_om_fixture("forecast.json"))
    expect_canonical_forecast(out)
    expect_true(all(is.na(out$member)))
    expect_true(all(is.na(out$stat)))
    expect_true(all(out$issue_time == om_now()))
  })

  it("derives lead_time as valid_time - issue_time", {
    out <- om_fetch("forecast", read_om_fixture("forecast.json"))
    lead_h <- as.numeric(out$lead_time, units = "hours")
    valid_h <- as.numeric(difftime(out$valid_time, out$issue_time, units = "hours"))
    expect_equal(lead_h, valid_h)
    expect_equal(min(lead_h), 0)          # first fixture step is issue hour
  })
})

describe("ensemble forecast", {
  it("emits an integer member per series and no stat", {
    out <- om_fetch("ensemble", read_om_fixture("ensemble.json"))
    expect_canonical_forecast(out)
    expect_type(out$member, "integer")
    expect_true(all(!is.na(out$member)))
    expect_true(all(is.na(out$stat)))
  })

  it("recovers every member series present in the body (member count parity)", {
    body <- read_om_fixture("ensemble.json")
    n_series <- sum(grepl("_member", names(body$hourly)))
    out <- om_fetch("ensemble", body)
    expect_equal(length(unique(out$member)), n_series)
    expect_setequal(unique(out$member), seq_len(n_series))
  })
})

describe("Previous Runs (daily-lead training pairs)", {
  it("expresses lead_time in whole days", {
    out <- om_fetch("previous_runs", read_om_fixture("previous-runs.json"))
    expect_canonical_forecast(out)
    lead_days <- as.numeric(out$lead_time, units = "days")
    expect_true(all(lead_days == round(lead_days)))   # whole-day granularity
    expect_setequal(unique(lead_days), c(1, 2))
  })

  it("marks the source so Plan 12 knows sub-daily lead resolution is absent", {
    # SCOPING §7.2: Previous Runs only pairs at daily lead; a provenance marker
    # must distinguish it from a lead-resolved source. Simplest stable contract:
    # lead_time is a whole-day difftime AND the model records the coarse cadence.
    out <- om_fetch("previous_runs", read_om_fixture("previous-runs.json"))
    expect_true("model" %in% names(out))
    expect_false(anyNA(out$lead_time))                # NOT the shortest-lead proxy
  })
})

describe("Historical Forecast (shortest-lead proxy)", {
  it("stamps lead_time = NA to mark the unresolved issue time", {
    # SCOPING §7.2: the stitched shortest-lead series has no resolvable issue
    # time; a lead_time-NA forecast row means "shortest-lead proxy". Plan 12
    # must never train lead-aware calibration on these rows.
    out <- om_fetch("historical_forecast",
                    read_om_fixture("historical-forecast.json"))
    expect_canonical_forecast(out)
    expect_true(all(is.na(out$lead_time)))
    expect_true(all(out$method %in% c("model_fill", NA_character_)) ||
                  !"method" %in% names(out))
  })

  it("pins the Plan 12 contract: lead-NA rows are the proxy marker", {
    out <- om_fetch("historical_forecast",
                    read_om_fixture("historical-forecast.json"))
    # Documented here, enforced in Plan 12's test-tier-emos.R ("lead_unresolved").
    is_proxy <- is.na(out$lead_time)
    expect_true(any(is_proxy))
  })
})
