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

  # Audit drift-guard: every condition class actually raised via abort_meteo/
  # warn_meteo/inform_meteo must appear in the meteo_conditions() taxonomy, so
  # the user-facing catalogue of what can go wrong never silently drifts from
  # what the code throws. This scans the package source, so it only runs in a
  # dev/source tree (skipped when the source R/ dir is absent, e.g. an
  # installed-package check).
  it("registers every condition class the source actually raises", {
    candidates <- c(
      testthat::test_path("..", "..", "R"),
      file.path(find.package("meteoTidy"), "R")
    )
    r_dir <- candidates[dir.exists(candidates)]
    skip_if(length(r_dir) == 0, "package source R/ not available")
    r_dir <- r_dir[[1]]

    helpers <- c("abort_meteo", "warn_meteo", "inform_meteo")
    walk <- function(e, acc) {
      if (!is.call(e)) {
        return(acc)
      }
      fn <- e[[1L]]
      if (is.name(fn) && as.character(fn) %in% helpers) {
        arg <- as.list(e)[["class"]]  # named access; safe on empty arg slots
        if (is.character(arg) && length(arg) == 1L) acc <- c(acc, arg)
      }
      # Recurse positionally; tryCatch skips empty argument slots (the "missing"
      # symbol in calls like `x[, drop = FALSE]`), which are leaves anyway.
      for (i in seq_along(e)) {
        acc <- tryCatch(walk(e[[i]], acc), error = function(err) acc)
      }
      acc
    }

    raised <- character(0)
    for (f in list.files(r_dir, pattern = "[.]R$", full.names = TRUE)) {
      for (e in parse(f, keep.source = FALSE)) raised <- walk(e, raised)
    }
    raised <- sort(unique(raised))

    registered <- sub("^meteoTidy_(error|warning|message|condition)_", "",
                      meteo_conditions()$class)
    unregistered <- setdiff(raised, registered)

    expect_equal(
      unregistered, character(0),
      info = paste0(
        "Condition classes raised in R/ but missing from meteo_conditions(): ",
        paste(unregistered, collapse = ", ")
      )
    )
  })
})
