# Plan 05 — Open-Meteo seasonal (EC46 + SEAS5 splice). Mocked; no live calls.
# Directly tests the SCOPING §5.2 fix: per-row underlying model, never the
# spliced product name.

describe("seasonal splice model attribution", {
  it("labels early-lead rows ec46 and late-lead rows seas5", {
    out <- om_fetch("seasonal", read_om_fixture("seasonal.json"),
                    window = list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                                  to = as.POSIXct("2026-04-01", tz = "UTC")))
    expect_canonical_forecast(out)
    lead_days <- as.numeric(out$lead_time, units = "days")
    early <- out$model[lead_days <= 46]
    late  <- out$model[lead_days >  46]
    expect_true(all(early == "ec46"))
    expect_true(all(late == "seas5"))
    # never the spliced product name
    expect_false(any(out$model == "seasonal"))
  })

  it("keeps the splice boundary a single documented constant (~46 days)", {
    out <- om_fetch("seasonal", read_om_fixture("seasonal.json"),
                    window = list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                                  to = as.POSIXct("2026-04-01", tz = "UTC")))
    lead_days <- as.numeric(out$lead_time, units = "days")
    # both sides of the boundary are present in the fixture
    expect_true(any(lead_days <= 46))
    expect_true(any(lead_days > 46))
  })
})

describe("seasonal member/summary shape", {
  it("emits 51 ensemble members with integer member and NA stat", {
    out <- om_fetch("seasonal", read_om_fixture("seasonal.json"),
                    window = list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                                  to = as.POSIXct("2026-04-01", tz = "UTC")))
    members <- out[!is.na(out$member), ]
    expect_type(members$member, "integer")
    expect_equal(length(unique(members$member)), 51)
    expect_true(all(is.na(members$stat)))
  })

  it("emits summary rows as stat with NA member, never both set", {
    out <- om_fetch("seasonal", read_om_fixture("seasonal.json"),
                    window = list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                                  to = as.POSIXct("2026-04-01", tz = "UTC")))
    summ <- out[!is.na(out$stat), ]
    expect_true(nrow(summ) > 0)
    expect_true(all(summ$stat %in% c("mean", "p10", "p50", "p90")))
    expect_true(all(is.na(summ$member)))
    # the revised member/stat rule: never both non-NA (also checked by helper)
    expect_false(any(!is.na(out$member) & !is.na(out$stat)))
  })
})
