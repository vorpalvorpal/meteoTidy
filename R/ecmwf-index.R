# Plan 08 — the ECMWF Open Data `.index` sidecar: pure parse + message
# selection. No I/O here; `.http_get()` (via source-ecmwf.R) supplies the raw
# JSON-lines text and this file turns it into a tibble and filters it.
#
# ECMWF ships one `.index` line per GRIB message in the sibling `.grib2` file,
# e.g. (VERIFIED against a live `enfo` file, 2026-07-06 -- `number` is a JSON
# *string*, and the real open feed ships no `"type":"cf"` control member, only
# perturbed members `"1"..."50"`; see plans/08-acquisition-ecmwf.md):
#   {"domain": "g", "date": "20260702", "time": "0000", "expver": "0001",
#    "class": "od", "type": "pf", "stream": "enfo", "step": "24",
#    "levtype": "sfc", "number": "1", "param": "2t", "_offset": 0,
#    "_length": 655849}
# `number` is the ensemble member. We rename it to `member` in the parsed
# output because that's the name used throughout the rest of the package's
# forecast schema (`new_forecast()`'s `member` column) and by
# `ecmwf_select_messages()`'s `members` argument.

#' Parse an ECMWF Open Data `.index` sidecar
#'
#' Parses the JSON-lines text of an ECMWF Open Data `.index` file (one JSON
#' object per GRIB message, giving its byte range in the sibling `.grib2`
#' file) into a tibble. Pure function -- no I/O; callers read the lines (from
#' disk or via the package's internal `.http_get()` seam) and pass them in.
#'
#' The raw index's `number` field (the ensemble member) is renamed `member` in
#' the output, matching the `member` column used elsewhere in the package's
#' canonical forecast schema.
#'
#' @param lines A character vector, one JSON object per element (as returned
#'   by `readLines()` on a `.index` file).
#' @return A tibble with (at least) columns `param`, `step` (character, as
#'   ECMWF encodes it, e.g. `"24"`), `member` (double), `` `_offset` ``
#'   (double), `` `_length` `` (double), plus the other index fields
#'   (`domain`, `date`, `time`, `expver`, `class`, `type`, `stream`,
#'   `levtype`).
#' @keywords internal
#' @noRd
ecmwf_index_parse <- function(lines) {
  lines <- lines[nzchar(trimws(lines))]
  records <- lapply(lines, jsonlite::fromJSON, simplifyVector = TRUE)

  rows <- lapply(records, function(rec) {
    tibble::tibble(
      domain = as.character(rec$domain %||% NA_character_),
      date = as.character(rec$date %||% NA_character_),
      time = as.character(rec$time %||% NA_character_),
      expver = as.character(rec$expver %||% NA_character_),
      class = as.character(rec$class %||% NA_character_),
      type = as.character(rec$type %||% NA_character_),
      stream = as.character(rec$stream %||% NA_character_),
      step = as.character(rec$step %||% NA_character_),
      levtype = as.character(rec$levtype %||% NA_character_),
      param = as.character(rec$param %||% NA_character_),
      member = as.double(rec$number %||% NA_real_),
      `_offset` = as.double(rec$`_offset` %||% NA_real_),
      `_length` = as.double(rec$`_length` %||% NA_real_)
    )
  })

  vctrs::vec_rbind(!!!rows)
}

#' Select ECMWF GRIB messages matching requested params/steps/members
#'
#' Filters a parsed `.index` tibble (see `ecmwf_index_parse()`) down to the
#' rows matching all of: `param %in% params`, `step %in% steps` and
#' `member %in% members`. `step` is compared as character on both sides
#' (ECMWF's index stores it as a JSON string, e.g. `"24"`) so callers may pass
#' either `steps = 24` (numeric) or `steps = "24"` (character).
#'
#' @param idx A tibble as returned by `ecmwf_index_parse()`.
#' @param params Character vector of GRIB `param` short names to keep (e.g.
#'   `"2t"`).
#' @param steps Numeric or character vector of forecast steps (hours) to keep.
#' @param members Numeric vector of ensemble member numbers to keep (`0`= the
#'   control run).
#' @return The filtered tibble, same columns as `idx`.
#' @keywords internal
#' @noRd
ecmwf_select_messages <- function(idx, params, steps, members) {
  steps_chr <- as.character(steps)
  idx[
    idx$param %in% params &
      as.character(idx$step) %in% steps_chr &
      idx$member %in% members,
    ,
    drop = FALSE
  ]
}
