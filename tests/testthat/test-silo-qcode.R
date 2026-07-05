# Plan 06 — SILO source/quality code → (method, qc_flag) mapping.
# The lookup is exhaustive: an unknown code fails loud (a SILO schema change
# must be caught, never silently defaulted).

describe("silo_qcode_map()", {
  it("maps observed codes to measured/ok", {
    m <- silo_qcode_map("0")            # 0 = observed (SILO convention)
    expect_equal(m$method, "measured")
    expect_equal(m$qc_flag, "ok")
  })

  it("maps interpolated/patched codes to imputed or model_fill, ok", {
    interp <- silo_qcode_map("25")      # 25 = interpolated from nearby stations
    expect_true(interp$method %in% c("imputed", "model_fill"))
    expect_equal(interp$qc_flag, "ok")

    grid <- silo_qcode_map("75")        # 75 = interpolated/grid (DataDrill)
    expect_true(grid$method %in% c("imputed", "model_fill"))
  })

  it("maps long-term-average fallback codes to suspect", {
    lta <- silo_qcode_map("35")         # long-term-average fallback
    expect_equal(lta$qc_flag, "suspect")
  })

  it("aborts unknown_silo_code rather than silently defaulting", {
    expect_error(silo_qcode_map("999"),
                 class = "meteoTidy_error_unknown_silo_code")
  })

  it("maps every code in the committed reference list (exhaustiveness)", {
    # The reference list is the documented SILO code set; each must resolve
    # without error and to a valid (method, qc_flag) pair.
    ref <- silo_qcode_reference()
    expect_true(nrow(ref) > 0)
    for (code in ref$code) {
      m <- silo_qcode_map(code)
      expect_true(m$method %in% METHOD_LEVELS)
      expect_true(m$qc_flag %in% QC_FLAG_LEVELS)
    }
  })
})

describe("code mapping applied through fetch()", {
  it("carries per-value quality codes into provenance method/qc_flag", {
    site <- make_test_site()
    adapter <- source_silo(api_key_env = "SILO_EMAIL")
    win <- list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                to = as.POSIXct("2026-12-31", tz = "UTC"))
    # one observed value, one long-term-average fallback value
    frame <- make_silo_frame(dates = as.Date(c("2026-01-15", "2026-01-16")),
                             value = c(30.5, 31.0), qcode = c("0", "35"))
    with_mocked_silo(frame, {
      out <- fetch(adapter, site, "temperature_2m", win)
    })
    out <- out[order(out$datetime_utc), ]
    expect_equal(out$method[1], "measured")
    expect_equal(out$qc_flag[1], "ok")
    expect_equal(out$qc_flag[2], "suspect")     # the LTA-fallback value
  })
})
