# Plan 00 — condition system.

describe("abort_meteo()", {
  it("attaches both the specific and umbrella error classes", {
    expect_error(
      abort_meteo("boom", class = "bad_units"),
      class = "meteoTidy_error_bad_units"
    )
    cnd <- catch_meteo(abort_meteo("boom", class = "bad_units"))
    expect_true(inherits(cnd, "meteoTidy_error_bad_units"))
    expect_true(inherits(cnd, "meteoTidy_error"))
  })

  it("reports the calling function as the call, not the helper itself", {
    f <- function() abort_meteo("from f", class = "bad_units")
    cnd <- catch_meteo(f())
    # the reported call must be f(), never abort_meteo()
    expect_match(rlang::expr_deparse(conditionCall(cnd)), "^f\\(")
  })

  it("fails fast when no class is supplied (misuse is an error)", {
    expect_error(abort_meteo("no class here"))
  })

  it("renders a multi-bullet cli message stably", {
    expect_snapshot(
      error = TRUE,
      abort_meteo(
        c("Bad thing happened.",
          "x" = "the value was {.val 42}",
          "i" = "try {.code met_help()}"),
        class = "demo"
      )
    )
  })
})

describe("warn_meteo()", {
  it("produces a warning carrying the parallel warning class", {
    expect_warning(
      warn_meteo("careful", class = "risky"),
      class = "meteoTidy_warning_risky"
    )
  })

  it("requires a class", {
    expect_error(warn_meteo("careful"))
  })
})

describe("inform_meteo()", {
  it("emits a message and allows an optional class", {
    expect_message(inform_meteo("fyi"))
    cnd <- catch_meteo(inform_meteo("fyi", class = "note"))
    expect_true(inherits(cnd, "meteoTidy_message_note") ||
                  inherits(cnd, "meteoTidy_condition_note") ||
                  inherits(cnd, "message"))
  })
})

describe("meteo_conditions()", {
  it("returns a discoverable, de-duplicated taxonomy under the package prefix", {
    tbl <- meteo_conditions()
    expect_true(all(c("class", "meaning") %in% names(tbl)))
    expect_false(any(duplicated(tbl$class)))
    expect_true(all(startsWith(tbl$class, "meteoTidy_")))
  })
})
