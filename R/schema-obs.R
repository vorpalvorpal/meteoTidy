# Plan 01 — canonical observation table.
#
# Design decision: canonical observation VALUES are stored as plain doubles
# in the variable's canonical unit, never as `units`-classed vectors. Parquet
# (Plan 03) has no native units type, so pinning the unit in the dictionary
# and enforcing it at every boundary (`to_canonical()` at ingest, the range
# check here) is the practical equivalent of a units-carrying column.

.obs_col_spec <- function() {
  list(
    col_spec("site_id", "character"),
    col_spec("datetime_utc", "POSIXct"),
    col_spec("variable", "character"),
    col_spec("value", "double"),
    col_spec("source", "character"),
    col_spec("method", "character"),
    col_spec("qc_flag", "character")
  )
}

#' Stamp and validate a canonical observation tibble
#'
#' Coerces/validates an incoming data frame into the canonical long-format
#' observation table (SCOPING §3): one row per `(site_id, datetime_utc,
#' variable, source)`.
#'
#' Validation, in order: required columns/types present
#' (`assert_columns()`); every `variable` known to the dictionary
#' ([met_variable()]); `qc_flag`/`method` are legal enum values; `datetime_utc`
#' is UTC; values are within `[min, max]` for their variable **unless** the
#' row is flagged `"fail"` or `"missing"` (an out-of-range value on those rows
#' is expected — e.g. a failed sensor reading — not a bug); and the key
#' `(site_id, datetime_utc, variable, source)` is unique.
#'
#' @param df A data frame with columns `site_id`, `datetime_utc`, `variable`,
#'   `value`, `source`, `method`, `qc_flag`.
#' @return A validated tibble in canonical observation form.
#' @keywords internal
#' @noRd
new_obs <- function(df) {
  df <- tibble::as_tibble(df)
  assert_columns(df, .obs_col_spec(), "obs")

  unknown_vars <- setdiff(unique(df$variable), met_variables()$variable)
  if (length(unknown_vars) > 0) {
    # met_variable() raises the standard "unknown_variable" abort; use the
    # first offender to produce the message (all are reported for context).
    abort_meteo(
      c(
        "obs table references unknown variable{?s}: {.val {unknown_vars}}.",
        "i" = "Register {cli::qty(length(unknown_vars))}{?it/them} first with {.code met_register_variable()}." # nolint: line_length_linter.
      ),
      class = "unknown_variable"
    )
  }

  validate_qc_flag(df$qc_flag)
  validate_method(df$method)

  tzone <- attr(df$datetime_utc, "tzone")
  if (!identical(tzone, "UTC")) {
    abort_meteo(
      c(
        "{.field datetime_utc} must be UTC.",
        "x" = "Found tzone {.val {tzone %||% \"\"}}."
      ),
      class = "non_utc_time"
    )
  }

  dict <- met_variables()
  ranges <- dict[match(df$variable, dict$variable), c("min", "max")]
  checkable <- df$qc_flag == "ok" & !is.na(df$value)
  below <- checkable & !is.na(ranges$min) & df$value < ranges$min
  above <- checkable & !is.na(ranges$max) & df$value > ranges$max
  bad <- below | above
  if (any(bad)) {
    idx <- which(bad)[1] # nolint: object_usage_linter. used via cli glue-interpolation below
    abort_meteo(
      c(
        "Value out of range for an {.val ok}-flagged row.",
        "x" = paste0(
          "variable {.val {df$variable[idx]}}: value {.val {df$value[idx]}} ",
          "outside [{.val {ranges$min[idx]}}, {.val {ranges$max[idx]}}]."
        ),
        "i" = "If this reading is genuinely bad, flag it {.val fail} or {.val missing} instead."
      ),
      class = "range_violation"
    )
  }

  key <- df[c("site_id", "datetime_utc", "variable", "source")]
  if (anyDuplicated(key) > 0) {
    abort_meteo(
      "obs table has duplicate (site_id, datetime_utc, variable, source) keys.",
      class = "duplicate_key"
    )
  }

  df[c("site_id", "datetime_utc", "variable", "value", "source", "method", "qc_flag")]
}

#' Pivot a canonical observation table to wide (Open-Meteo-named) form
#'
#' One row per `(site_id, datetime_utc)`; columns are the requested
#' `variables` (default: the full dictionary). A variable absent from `obs`
#' still produces an all-`NA` column, keeping the wide §3.1 shape stable
#' regardless of coverage. The time column stays named `datetime_utc`
#' internally; Plan 15's emitter does the `time` rename at the outer
#' boundary.
#'
#' @param obs A canonical observation tibble (see `new_obs()`).
#' @param variables Character vector of variable names to include as columns.
#'   Defaults to every variable currently in the dictionary.
#' @return A wide tibble with columns `site_id`, `datetime_utc`, and one
#'   column per requested variable.
#' @family schema
#' @export
#' @examples
#' # a canonical-shaped long obs tibble (the internal new_obs() validator is
#' # not exported; an already-canonical literal needs no re-validation)
#' widen_obs(tibble::tibble(
#'   site_id = "test",
#'   datetime_utc = as.POSIXct("2026-01-01", tz = "UTC"),
#'   variable = "temperature_2m",
#'   value = 20,
#'   source = "test_src",
#'   method = "measured",
#'   qc_flag = "ok"
#' ))
widen_obs <- function(obs, variables = NULL) {
  if (is.null(variables)) {
    variables <- met_variables()$variable
  }

  keep <- obs[obs$variable %in% variables, , drop = FALSE]
  base <- unique(obs[c("site_id", "datetime_utc")])
  base <- base[order(base$site_id, base$datetime_utc), , drop = FALSE]

  wide <- base
  for (v in variables) {
    sub <- keep[keep$variable == v, c("site_id", "datetime_utc", "value")]
    matched <- sub$value[match(
      paste(wide$site_id, wide$datetime_utc),
      paste(sub$site_id, sub$datetime_utc)
    )]
    wide[[v]] <- if (length(matched) == 0) rep(NA_real_, nrow(wide)) else matched
  }

  tibble::as_tibble(wide)
}

#' Pivot a wide observation table back to canonical long form
#'
#' The inverse of [widen_obs()]. `widen_obs(obs) |> narrow_obs()` is the
#' identity on `(site_id, datetime_utc, variable, value)` (round-trip
#' contract); other obs columns (`source`, `method`, `qc_flag`) are not
#' recoverable from the wide form and are filled with defaults
#' (`source`/`method`/`qc_flag` arguments) since the wide table does not
#' carry them.
#'
#' @param wide A wide tibble as produced by [widen_obs()].
#' @param source,method,qc_flag Defaults used to fill the long-form metadata
#'   columns that the wide table does not carry.
#' @return A canonical long-format observation tibble (not re-validated by
#'   `new_obs()`, since the fill-in metadata may not reflect the truth).
#' @family schema
#' @export
#' @examples
#' narrow_obs(widen_obs(tibble::tibble(
#'   site_id = "test",
#'   datetime_utc = as.POSIXct("2026-01-01", tz = "UTC"),
#'   variable = "temperature_2m",
#'   value = 20,
#'   source = "test_src",
#'   method = "measured",
#'   qc_flag = "ok"
#' )))
narrow_obs <- function(wide, source = NA_character_, method = "measured", qc_flag = "ok") {
  value_cols <- setdiff(names(wide), c("site_id", "datetime_utc"))
  long <- tidyr_like_pivot_longer(wide, value_cols)
  long <- long[!is.na(long$value), , drop = FALSE]
  long$source <- source
  long$method <- method
  long$qc_flag <- qc_flag
  cols <- c("site_id", "datetime_utc", "variable", "value", "source", "method", "qc_flag")
  tibble::as_tibble(long[cols])
}

# A minimal, dependency-free stand-in for tidyr::pivot_longer(), since tidyr
# is not in this plan's Imports. Stacks `value_cols` into `variable`/`value`.
tidyr_like_pivot_longer <- function(wide, value_cols) {
  n <- nrow(wide)
  reps <- length(value_cols)
  out <- tibble::tibble(
    site_id = rep(wide$site_id, times = reps),
    datetime_utc = rep(wide$datetime_utc, times = reps),
    variable = rep(value_cols, each = n),
    value = unlist(lapply(value_cols, function(v) wide[[v]]), use.names = FALSE)
  )
  out
}
