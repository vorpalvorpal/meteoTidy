# Acceptance tests for the post-implementation audit follow-ups.
#
# These encode behaviours the audit found MISSING. They are expected to FAIL
# (or, where a not-yet-existing seam is referenced, fail on an explicit
# existence assertion) until the implementer applies IMPLEMENTER_PROMPT.md.
# Each test names the prompt item it gates.

# --- ECMWF u/v -> wind speed/direction recombination (Plan 08) --------------
#
# The ECMWF adapter advertises `wind_speed_10m` / `wind_direction_10m` in its
# `provides`, but fetch_forecast() only delivers single-param (temperature)
# variables: the 10u/10v -> speed/direction recombination is unimplemented, so
# wind is advertised-but-silently-dropped.
#
# Required seam (see IMPLEMENTER_PROMPT.md, "ECMWF u/v"): a pure function
#   .ecmwf_uv_to_wind(field_tbl, values) -> tibble
# where `field_tbl` is a grib_field_table()-shaped tibble (columns band, param,
# unit, step, member) and `values` is the per-band extracted value vector
# (aligned to field_tbl$band). It pairs "10u"/"10v" bands on (step, member),
# and emits long rows with `variable` in {wind_speed_10m, wind_direction_10m},
# `value` = hypot(u, v) / meteorological from-direction, plus `member`/`step`.
# Reuse R/wind-uv.R's `uv_to_dir()` for the direction (do NOT re-derive it).

describe("ECMWF u/v recombination (Plan 08 gap)", {
  it("recombines 10u/10v bands into canonical wind_speed_10m / wind_direction_10m", {
    ns <- asNamespace("meteoTidy")
    expect_true(
      exists(".ecmwf_uv_to_wind", envir = ns, inherits = FALSE),
      info = "`.ecmwf_uv_to_wind()` seam not implemented — see IMPLEMENTER_PROMPT.md (ECMWF u/v)."
    )
    combine <- get0(".ecmwf_uv_to_wind", envir = ns, inherits = FALSE)

    if (!is.null(combine)) {
      field_tbl <- tibble::tibble(
        band   = 1:4,
        param  = c("10u", "10v", "10u", "10v"),
        unit   = rep("m/s", 4),
        step   = rep("24", 4),
        member = c(1L, 1L, 2L, 2L)
      )
      # member 1: u=3, v=4 -> speed 5 ; member 2: u=6, v=8 -> speed 10 (same dir)
      values <- c(3, 4, 6, 8)

      out <- combine(field_tbl, values)

      expect_setequal(
        unique(out$variable),
        c("wind_speed_10m", "wind_direction_10m")
      )

      sp1 <- out$value[out$variable == "wind_speed_10m" & out$member == 1L]
      sp2 <- out$value[out$variable == "wind_speed_10m" & out$member == 2L]
      expect_equal(sp1, 5)
      expect_equal(sp2, 10)

      # Direction must match the package's own u/v convention exactly, and be
      # identical for two collinear vectors (member 1 and member 2).
      di1 <- out$value[out$variable == "wind_direction_10m" & out$member == 1L]
      di2 <- out$value[out$variable == "wind_direction_10m" & out$member == 2L]
      expect_equal(di1, uv_to_dir(3, 4))
      expect_equal(di2, uv_to_dir(6, 8))
      expect_equal(di1, di2)
    }
  })
})

# --- Verification pair assembly reads the REAL store (Plan 13) ---------------
#
# `assemble_verification_pairs()` (R/verify.R) joins the archived forecasts with
# QC-clean observations to produce the (forecast, observation) pairs the skill
# verdict is computed on — and the SAME pairs the correction refit needs for
# fitting (see IMPLEMENTER_PROMPT.md item 1). But `test-verify-run.R` mocks this
# function out (`local_mocked_bindings(assemble_verification_pairs = ...)`), so
# its real store-read + join logic is exercised by no test. This closes that
# gap; it should PASS today.

describe("assemble_verification_pairs() real store-read (Plan 13 gap)", {
  it("joins archived forecasts to QC-clean obs on (variable, valid_time)", {
    root <- local_store()
    site <- make_test_site(store_root = root)

    fc <- make_forecast(n = 3, source = "openmeteo", variable = "temperature_2m",
                        value = c(20, 21, 22))
    store_write_forecast(root, new_forecast(fc))

    obs <- make_obs(n = 3, variable = "temperature_2m", source = "silo",
                    qc_flag = "ok")
    obs$datetime_utc <- fc$valid_time      # align obs to the forecast valid times
    obs$value <- c(19, 19, 19)
    store_write_obs(root, new_obs(obs))

    pairs <- assemble_verification_pairs(root, site, sources = "openmeteo")

    expect_true(all(c("forecast", "observation", "valid_time", "variable",
                      "lead_time") %in% names(pairs)))
    expect_equal(nrow(pairs), 3)
    pairs <- pairs[order(pairs$valid_time), , drop = FALSE]
    expect_equal(pairs$forecast, c(20, 21, 22))
    expect_equal(pairs$observation, c(19, 19, 19))
  })
})

# --- meteoHazard wide-emitter follow-ups (Plan 15) --------------------------

describe("wide emitter provenance + ensemble handling (Plan 15 gaps)", {
  it("threads the real per-variable correction tier into met_wide() provenance", {
    # `.met_wide_provenance()` (R/met-wide.R) hardcodes tier = "raw" for every
    # column, deferring "real per-variable tier" to Plan 16 — which never
    # threaded it. Coupled to IMPLEMENTER_PROMPT.md item 1 (once corrections
    # actually run, corrected columns must report their real tier, not "raw").
    skip("pending: real per-variable tier not threaded into met_wide() provenance — see IMPLEMENTER_PROMPT.md item 6")
  })

  it("does not silently collapse ensemble members when widening a forecast", {
    # `.widen_forecast()` keeps one value per (valid_time, variable) — first row
    # wins — silently dropping all but one ensemble member. The intended
    # wide-forecast semantics for ensembles are undecided (member column? mean?);
    # the implementer must decide and implement, then un-skip this.
    skip("pending: ensemble wide-forecast contract undecided — see IMPLEMENTER_PROMPT.md item 6")
  })
})

# --- DESCRIPTION dependency pins (Plan 06) ----------------------------------
#
# weatherOz 3.0.0 is the current CRAN release and carries a breaking DPIRD
# column restructure; the plan/scope require `>= 3.0.0`, but DESCRIPTION pins
# `>= 2.0.2`. This is RED until the pin is bumped (see IMPLEMENTER_PROMPT.md
# item 4 — and verify source_silo() against the real 3.0.0 API when bumping).

describe("DESCRIPTION dependency pins (audit)", {
  it("pins weatherOz to >= 3.0.0", {
    imports <- packageDescription("meteoTidy", fields = "Imports")
    imports <- gsub("[[:space:]]+", " ", imports)
    m <- regmatches(imports, regexpr("weatherOz \\(>= [0-9.]+\\)", imports))
    expect_length(m, 1L)
    ver <- sub(".*>= ([0-9.]+)\\)", "\\1", m)
    expect_true(package_version(ver) >= package_version("3.0.0"),
                info = paste("DESCRIPTION pins weatherOz >=", ver))
  })
})
