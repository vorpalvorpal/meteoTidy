# Helpers for Plan 01 — canonical schema builders + reusable expectations.
# These builders return *already-canonical* plain tibbles (no package call), so
# they double as valid inputs to `new_obs()` / `new_forecast()` and as fixtures
# for every later plan. The expectation helpers assert the full contract.

make_obs <- function(n = 3,
                     variable = "temperature_2m",
                     site_id = "test",
                     source = "test_src",
                     start = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
                     value = NULL,
                     method = "measured",
                     qc_flag = "ok") {
  times <- start + (seq_len(n) - 1L) * 3600
  if (is.null(value)) value <- seq(15, by = 0.5, length.out = n)
  tibble::tibble(
    site_id = site_id,
    datetime_utc = times,
    variable = variable,
    value = as.double(value),
    source = source,
    method = method,
    qc_flag = qc_flag
  )
}

make_forecast <- function(n = 3,
                          variable = "temperature_2m",
                          site_id = "test",
                          source = "openmeteo",
                          model = "ecmwf_ifs025",
                          issue_time = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
                          member = NA_integer_,
                          stat = NA_character_,
                          value = NULL) {
  lead_h <- seq_len(n) * 24L
  valid <- issue_time + lead_h * 3600
  if (is.null(value)) value <- seq(15, by = 1, length.out = n)
  tibble::tibble(
    site_id = site_id,
    source = source,
    model = model,
    issue_time = issue_time,
    valid_time = valid,
    lead_time = as.difftime(as.numeric(lead_h), units = "hours"),
    member = as.integer(member),
    stat = as.character(stat),
    variable = variable,
    value = as.double(value)
  )
}

make_forecast_aux <- function(n = 1,
                              site_id = "test",
                              source = "bom_forecast",
                              field = "short_text",
                              value_text = "Partly cloudy.") {
  issue <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  tibble::tibble(
    site_id = site_id,
    source = source,
    issue_time = issue,
    valid_time = issue + seq_len(n) * 86400,
    field = field,
    value_text = value_text
  )
}

# ---- reusable expectations -------------------------------------------------

expect_canonical_obs <- function(x) {
  cols <- c("site_id", "datetime_utc", "variable", "value",
            "source", "method", "qc_flag")
  testthat::expect_true(all(cols %in% names(x)),
                        label = "canonical obs has all required columns")
  testthat::expect_s3_class(x$datetime_utc, "POSIXct")
  testthat::expect_identical(attr(x$datetime_utc, "tzone"), "UTC")
  testthat::expect_type(x$value, "double")
  testthat::expect_true(all(x$qc_flag %in% QC_FLAG_LEVELS))
  testthat::expect_true(all(x$method %in% METHOD_LEVELS))
  key <- x[c("site_id", "datetime_utc", "variable", "source")]
  testthat::expect_false(any(duplicated(key)),
                         label = "obs key (site,time,variable,source) is unique")
  invisible(x)
}

expect_canonical_forecast <- function(x) {
  cols <- c("site_id", "source", "model", "issue_time", "valid_time",
            "lead_time", "member", "stat", "variable", "value")
  testthat::expect_true(all(cols %in% names(x)),
                        label = "canonical forecast has all required columns")
  testthat::expect_s3_class(x$issue_time, "POSIXct")
  testthat::expect_s3_class(x$valid_time, "POSIXct")
  testthat::expect_type(x$member, "integer")
  testthat::expect_type(x$stat, "character")
  testthat::expect_type(x$value, "double")
  # member and stat are never both non-NA (revised member/stat rule)
  testthat::expect_false(any(!is.na(x$member) & !is.na(x$stat)),
                         label = "member and stat never both set")
  key <- x[c("site_id", "source", "model", "issue_time",
             "valid_time", "member", "stat", "variable")]
  testthat::expect_false(any(duplicated(key)))
  invisible(x)
}

expect_canonical_forecast_aux <- function(x) {
  cols <- c("site_id", "source", "issue_time", "valid_time", "field", "value_text")
  testthat::expect_true(all(cols %in% names(x)))
  testthat::expect_type(x$value_text, "character")
  invisible(x)
}

# Restore the built-in variable dictionary after a test that registers into it,
# so registrations never leak across tests.
local_clean_dict <- function(env = parent.frame()) {
  withr::defer(dict_reset(), envir = env)
}
