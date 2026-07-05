# Plan 01 — forecast archive + forecast_aux tables.

.forecast_col_spec <- function() {
  list(
    col_spec("site_id", "character"),
    col_spec("source", "character"),
    col_spec("model", "character"),
    col_spec("issue_time", "POSIXct"),
    col_spec("valid_time", "POSIXct"),
    col_spec("lead_time", "difftime"),
    col_spec("member", "integer"),
    col_spec("stat", "character"),
    col_spec("variable", "character"),
    col_spec("value", "double")
  )
}

#' Stamp and validate a canonical forecast tibble
#'
#' Validates the forecast archive schema (SCOPING §3): one row per
#' `(site_id, source, model, issue_time, valid_time, member, stat,
#' variable)`.
#'
#' `member` (integer) and `stat` (character) are never both non-`NA` for the
#' same row (the member/stat rule): `NA`/`NA` for a deterministic forecast,
#' `member = k`/`NA` for ensemble member `k`, `NA`/`stat` for a summary
#' (e.g. `"p90"`). `lead_time` must equal `valid_time - issue_time` (in
#' hours). `model` may vary per row so that a seasonal splice can record the
#' underlying model (`"ec46"`/`"seas5"`) rather than a spliced product name.
#'
#' @param df A data frame with columns `site_id`, `source`, `model`,
#'   `issue_time`, `valid_time`, `lead_time`, `member`, `stat`, `variable`,
#'   `value`.
#' @return A validated tibble in canonical forecast form.
#' @keywords internal
#' @noRd
new_forecast <- function(df) {
  df <- tibble::as_tibble(df)
  assert_columns(df, .forecast_col_spec(), "forecast")

  unknown_vars <- setdiff(unique(df$variable), met_variables()$variable)
  if (length(unknown_vars) > 0) {
    abort_meteo(
      "forecast table references unknown variable{?s}: {.val {unknown_vars}}.",
      class = "unknown_variable"
    )
  }

  for (col in c("issue_time", "valid_time")) {
    tzone <- attr(df[[col]], "tzone")
    if (!identical(tzone, "UTC")) {
      abort_meteo(
        c(
          "{.field {col}} must be UTC.",
          "x" = "Found tzone {.val {tzone %||% \"\"}}."
        ),
        class = "non_utc_time"
      )
    }
  }

  conflict <- !is.na(df$member) & !is.na(df$stat)
  if (any(conflict)) {
    abort_meteo(
      "forecast rows must not set both {.field member} and {.field stat}.",
      class = "member_stat_conflict"
    )
  }

  expected_lead <- as.numeric(difftime(df$valid_time, df$issue_time, units = "hours"))
  actual_lead <- as.numeric(df$lead_time, units = "hours")
  if (!isTRUE(all.equal(expected_lead, actual_lead, tolerance = 1e-6))) {
    abort_meteo(
      c(
        "{.field lead_time} is inconsistent with {.code valid_time - issue_time}.",
        "i" = "Expected lead time{?s} (hours): {.val {expected_lead}}",
        "x" = "Got: {.val {actual_lead}}"
      ),
      class = "lead_inconsistent"
    )
  }

  key <- df[c(
    "site_id", "source", "model", "issue_time",
    "valid_time", "member", "stat", "variable"
  )]
  if (anyDuplicated(key) > 0) {
    abort_meteo(
      "forecast table has duplicate (site_id, source, model, issue_time, valid_time, member, stat, variable) keys.", # nolint: line_length_linter.
      class = "duplicate_key"
    )
  }

  df[c(
    "site_id", "source", "model", "issue_time", "valid_time",
    "lead_time", "member", "stat", "variable", "value"
  )]
}

.forecast_aux_col_spec <- function() {
  list(
    col_spec("site_id", "character"),
    col_spec("source", "character"),
    col_spec("issue_time", "POSIXct"),
    col_spec("valid_time", "POSIXct"),
    col_spec("field", "character"),
    col_spec("value_text", "character")
  )
}

#' Stamp and validate a canonical forecast_aux tibble
#'
#' The companion table for non-numeric forecast elements (précis text,
#' fire-danger / UV categories) that do not fit `new_forecast()`'s numeric
#' `value` column. Keyed `(site_id, source, issue_time, valid_time, field)`.
#'
#' @param df A data frame with columns `site_id`, `source`, `issue_time`,
#'   `valid_time`, `field`, `value_text`.
#' @return A validated tibble in canonical forecast_aux form.
#' @keywords internal
#' @noRd
new_forecast_aux <- function(df) {
  df <- tibble::as_tibble(df)
  assert_columns(df, .forecast_aux_col_spec(), "forecast_aux")

  for (col in c("issue_time", "valid_time")) {
    tzone <- attr(df[[col]], "tzone")
    if (!identical(tzone, "UTC")) {
      abort_meteo(
        c(
          "{.field {col}} must be UTC.",
          "x" = "Found tzone {.val {tzone %||% \"\"}}."
        ),
        class = "non_utc_time"
      )
    }
  }

  key <- df[c("site_id", "source", "issue_time", "valid_time", "field")]
  if (anyDuplicated(key) > 0) {
    abort_meteo(
      "forecast_aux table has duplicate (site_id, source, issue_time, valid_time, field) keys.",
      class = "duplicate_key"
    )
  }

  df[c("site_id", "source", "issue_time", "valid_time", "field", "value_text")]
}
