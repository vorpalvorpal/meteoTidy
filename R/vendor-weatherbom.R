# Vendored (trimmed) from mevers/weatherBOM, commit 4696bcf.
#
# Upstream `weatherBOM` (https://github.com/mevers/weatherBOM) is confirmed
# MIT-licensed: its DESCRIPTION declares `License: MIT + file LICENSE`, and
# its `LICENSE.md` carries the full MIT text, (c) 2021 "weatherBOM authors".
# This was verified directly against commit 4696bcf on 2026-07-05 (note: at
# the time of writing GitHub's own licence detector shows the repo as
# "NOASSERTION" only because the bare `LICENSE` stub file isn't
# machine-recognised by their heuristic -- cosmetic; the licence text itself
# is genuinely MIT). The functions below are trimmed/transcribed by value
# from `R/compass_angle.R` under the terms of that MIT licence; this notice
# and the attribution below must be retained.
#
# Primary author / maintainer credited upstream: Maurits Evers. He is listed
# in this package's `Authors@R` with role "ctb" for this vendored
# contribution (see DESCRIPTION).
#
# --- MIT License (as published upstream, mevers/weatherBOM, LICENSE.md) ---
# Copyright (c) 2021 weatherBOM authors
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# ---------------------------------------------------------------------------
#
# Scoping note (deliberate, not an oversight): this file vendors only the
# compass-direction helpers (`compass2angle()` / `angle2compass()`), NOT
# upstream's live-API-calling functions (`bom_forecasts()`,
# `bom_observations()`, `bom_location_info()`, `bom_search_station()`, the
# internal endpoint constant). meteoTidy's own `R/bom-parse.R` does the
# actual JSON/XML parsing against locally-recorded fixtures via the
# package's own `.http_get()`/`.ftp_get()` seams, so faithfully reproducing
# upstream's HTTP-calling function bodies (which this package cannot verify
# against a live API without violating the "no live calls in tests"
# convention) would add untested, unverifiable surface. The compass helpers
# are genuinely useful, self-contained, and verifiable by unit test, so they
# are vendored in full; everything else is re-implemented natively in
# `R/bom-parse.R`.

# The 16-point compass rose, in clockwise order starting at due north (0
# degrees), each point 22.5 degrees apart. Matches the compass abbreviations
# BOM's JSON/XML feeds use for `wind_dir` / `wind.direction`.
.compass_points <- function() {
  c(
    "N", "NNE", "NE", "ENE",
    "E", "ESE", "SE", "SSE",
    "S", "SSW", "SW", "WSW",
    "W", "WNW", "NW", "NNW"
  )
}

#' Convert a compass direction string to a degree angle
#'
#' Converts a 16-point compass abbreviation (e.g. `"N"`, `"SW"`, `"NNE"`) to
#' its degree angle, measured clockwise from north (`0` = N, `90` = E, `180`
#' = S, `270` = W). Vendored (trimmed/transcribed) from
#' `mevers/weatherBOM`'s `R/compass_angle.R` (MIT; see the file header for
#' the full notice).
#'
#' @param compass A character vector of compass abbreviations (case
#'   insensitive). `NA` and `"CALM"` map to `NA_real_` (calm wind has no
#'   direction).
#'
#' @return A numeric vector of degree angles in `[0, 360)`, the same length
#'   as `compass`.
#' @keywords internal
#' @noRd
compass2angle <- function(compass) {
  points <- .compass_points()
  idx <- match(toupper(compass), points)
  (idx - 1L) * 22.5
}

#' Convert a degree angle to the nearest 16-point compass direction
#'
#' The inverse of `compass2angle()`: rounds `angle` to the nearest 22.5-degree
#' compass point and returns its abbreviation. Vendored
#' (trimmed/transcribed) from `mevers/weatherBOM`'s `R/compass_angle.R` (MIT;
#' see the file header for the full notice).
#'
#' @param angle A numeric vector of degree angles (any real number; wrapped
#'   into `[0, 360)` before rounding). `NA` maps to `NA_character_`.
#'
#' @return A character vector of compass abbreviations, the same length as
#'   `angle`.
#' @keywords internal
#' @noRd
angle2compass <- function(angle) {
  points <- .compass_points()
  wrapped <- angle %% 360
  idx <- round(wrapped / 22.5) %% 16 + 1L
  points[idx]
}
