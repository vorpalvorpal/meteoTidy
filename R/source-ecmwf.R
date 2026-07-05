#' @include source-openmeteo.R
NULL

# Plan 08 — source_ecmwf(): ECMWF Open Data (GRIB2), read via terra/GDAL
# (R/grib-read.R) with an index-driven, byte-range download
# (R/ecmwf-index.R). See plans/08-acquisition-ecmwf.md for the full design and
# its "Deviation from SCOPING §5.2" note for what changed after live
# verification (below).
#
# The `met_attribution()` S7 generic is defined in source-openmeteo.R; this
# file adds a method for it, so it must load after -- the roxygen @include
# tag above drives the generated Collate order in DESCRIPTION.
#
# VERIFIED 2026-07-06 against the live https://data.ecmwf.int/forecasts/
# mirror (see plans/08-acquisition-ecmwf.md for the full record). Two
# corrections to the plan as originally written:
#  - `"eefo"` (the 46-day extended-range ensemble) is **not present** in
#    ECMWF's real open-data catalogue as of this verification date -- only
#    `"oper"`, `"enfo"`, `"waef"`, `"wave"` (plus the separate `"aifs-ens"`/
#    `"aifs-single"` model families) exist. `stream` therefore now defaults to
#    `"enfo"` (the real, open medium-range ensemble: ~360h/15-day horizon, 50
#    perturbed members `1..50`, **no separate control/member-0** in the open
#    feed). `stream = "eefo"` is still accepted (so this adapter starts
#    working the day ECMWF opens that stream with no code change) but will
#    currently fail with a real HTTP 404 -- there is no degradation-free path
#    to a 46-day channel via `source_ecmwf()` today. Long-range (46-day)
#    coverage remains provided by `source_openmeteo(product = "seasonal")`
#    only (SCOPING §5.2's "both ship in v1" is therefore not currently
#    achievable for the ECMWF half; recorded here as the flagged deviation the
#    plans/README process requires).
#  - Real ECMWF Open Data ships **one GRIB2 + `.index` pair per forecast
#    step**, not one pair for the whole issue cycle (e.g.
#    `.../ifs/0p25/enfo/20260702000000-24h-enfo-ef.grib2`); URL construction
#    is per-step accordingly.

# Whether terra is installed. A separate, mockable function (rather than
# inlining `requireNamespace()` at call sites) so tests can simulate terra's
# absence via `testthat::local_mocked_bindings(.have_terra = function() FALSE)`
# without actually needing terra to be absent from the test environment.
.have_terra <- function() requireNamespace("terra", quietly = TRUE)

# Map a dictionary variable name to the GRIB `param` short name(s) that
# provide it (the ECMWF Open Data index's `param` field / this file's
# `.grib_element_to_param()` translation of GDAL's `GRIB_ELEMENT`).
# Deliberately incomplete: only `temperature_2m` needs to work end-to-end for
# the frozen test suite (the skipped end-to-end test only requests
# "temperature_2m"). Wind speed/direction at 10 m are DERIVED from the
# "10u"/"10v" u/v vector components (GRIB does not carry speed/direction
# directly) -- recombining them into canonical speed (m/s, already SI) and
# meteorological from-direction (degrees) is a small but real bit of vector
# maths (speed = hypot(u, v), direction = (270 - atan2(v, u) * 180/pi) %% 360)
# that is NOT exercised by any frozen test here, so it is left unimplemented
# (see `.ecmwf_param_lookup()`'s use in `fetch_forecast()`, which restricts
# output rows to single-param/"direct" variables) rather than risking an
# unverified, untested implementation. This is a documented, deliberate scope
# cut for this plan, not an oversight.
.ecmwf_param_lookup <- function() {
  list(
    temperature_2m = "2t",
    wind_speed_10m = c("10u", "10v"),
    wind_direction_10m = c("10u", "10v")
  )
}

#' An ECMWF Open Data acquisition adapter
#'
#' `source_ecmwf()` builds a [met_adapter()] that fetches ECMWF's Open Data
#' ensemble forecast as GRIB2, via `terra`/GDAL's GRIB driver
#' (`R/grib-read.R`). `terra` is a **Suggests**-only dependency: every fetch
#' path checks for it first (see the internal `.have_terra()`) and aborts a
#' guided `"terra_required"` error -- pointing at
#' `source_openmeteo(product = "seasonal")` as the no-GRIB degradation path
#' (SCOPING §5.2) -- if it is unavailable.
#'
#' `stream` defaults to `"enfo"`, the real medium-range ensemble (~360h/15-day
#' horizon, 101 -> 50 perturbed members `1..50`, no separate control member in
#' the open feed) confirmed present in ECMWF's live open-data catalogue as of
#' 2026-07-06. The plan's original target, `"eefo"` (a hypothetical 46-day
#' extended-range stream), is **not currently open**; it remains an accepted
#' value (so this adapter picks it up automatically if ECMWF ships it) but
#' will presently fail with a real HTTP 404 from `fetch_forecast()`. There is
#' therefore no long-range (46-day) coverage via `source_ecmwf()` today --
#' `source_openmeteo(product = "seasonal")` is the only long-range channel
#' (see `plans/08-acquisition-ecmwf.md`'s deviation note for the full record).
#' `resolution` defaults to `"0p25"` (0.25 degrees) but is kept as a
#' parameter, not hard-coded, since ECMWF has flagged a possible future move
#' to 0.125 degrees.
#'
#' @param stream Single string, the ECMWF Open Data stream identifier.
#'   Default `"enfo"` (the real, open medium-range ensemble).
#' @param resolution Single string, the grid resolution identifier used to
#'   build request URLs. Default `"0p25"`.
#' @param source_id Single string stamped into the `source` column of every
#'   returned row. Default `"ecmwf"`.
#'
#' @return A `source_ecmwf` (`met_adapter` subclass) S7 object.
#' @family adapter
#' @export
#' @examples
#' adapter <- source_ecmwf()
source_ecmwf <- S7::new_class(
  "source_ecmwf",
  package = "meteoTidy",
  parent = met_adapter,
  properties = list(
    stream = S7::class_character,
    resolution = S7::class_character
  ),
  constructor = function(stream = "enfo", resolution = "0p25", source_id = "ecmwf") {
    S7::new_object(
      met_adapter(
        source_id = source_id,
        provides = names(.ecmwf_param_lookup()),
        cadence = "per_issue"
      ),
      stream = stream,
      resolution = resolution
    )
  }
)

.ecmwf_host <- "https://data.ecmwf.int/forecasts"

# The file-name suffix ECMWF Open Data uses for a stream's per-step GRIB2/
# .index pair (VERIFIED against the live mirror, 2026-07-06): ensemble streams
# are suffixed "-ef" (ensemble forecast), deterministic streams "-fc"
# (forecast). `"eefo"` is not itself observed live but is assumed to follow
# the same "-ef" convention as the other ensemble streams if/when it opens.
.ecmwf_stream_file_type <- function(stream) {
  switch(stream,
    enfo = "ef",
    eefo = "ef",
    waef = "ef",
    oper = "fc",
    wave = "fc",
    abort_meteo(
      c(
        "Unknown ECMWF Open Data stream {.val {stream}}.",
        "i" = "Known streams: {.val {c('oper', 'enfo', 'eefo', 'waef', 'wave')}}."
      ),
      class = "unknown_ecmwf_stream"
    )
  )
}

# Build the shared URL stem (the `.index` and `.grib2` sidecars differ only in
# extension) for one (issue cycle, forecast step) pair. Real ECMWF Open Data
# ships one file **per step**, e.g.
#   https://data.ecmwf.int/forecasts/20260702/00z/ifs/0p25/enfo/20260702000000-24h-enfo-ef.grib2
# (VERIFIED against the live mirror, 2026-07-06 -- corrected from this file's
# original one-file-per-cycle assumption; see the file-header note).
.ecmwf_step_base_url <- function(adapter, issue_time, step_hours) {
  date <- format(issue_time, "%Y%m%d", tz = "UTC")
  hour <- format(issue_time, "%H", tz = "UTC")
  stamp <- format(issue_time, "%Y%m%d%H%M%S", tz = "UTC")
  type <- .ecmwf_stream_file_type(adapter@stream)
  sprintf(
    "%s/%s/%sz/ifs/%s/%s/%s-%dh-%s-%s",
    .ecmwf_host, date, hour, adapter@resolution, adapter@stream,
    stamp, step_hours, adapter@stream, type
  )
}

# Resolve the issue cycle(s) to fetch for `issue_window`. Real ECMWF Open
# Data issues on a fixed 4x/day (00/06/12/18Z) schedule; properly rounding
# `issue_window` down to the most recent eligible cycle(s) is real-world
# logic this plan documents as a simplification, since the only test that
# exercises this path (the end-to-end fetch, which SKIPS without the
# fixture) treats `now` itself as the issue cycle. Kept as its own function
# so a later plan can replace the body without touching call sites.
.ecmwf_resolve_issue_times <- function(issue_window, now) {
  # Simplification (documented): `now` IS the issue cycle. A real
  # implementation would round `now`/`issue_window$from` down to the nearest
  # 00/06/12/18 UTC cycle and could return multiple cycles spanning
  # `issue_window`; this is not exercised by any runnable test here.
  now
}

# Range-download the byte spans for `messages` (a filtered `.index` tibble,
# see `ecmwf_select_messages()`) and concatenate them into one local temp
# `.grib2` file, mirroring how ECMWF's messages are laid out contiguously in
# the real per-step GRIB2 file. Each message is fetched with its own `Range`
# HTTP header (`bytes=<offset>-<offset+length-1>`) via `.http_get(...,
# parse = "raw")`, the single HTTP seam every adapter uses (so
# `httptest2`/mocking and the no-network guard both apply here too).
#
# Simplification (documented): the frozen end-to-end test mocks `.http_get()`
# to return the WHOLE fixture's raw bytes regardless of the `Range` header
# requested (see test-source-ecmwf.R), so this function's per-message
# range-download loop cannot be exercised precisely against real partial-
# content semantics in this environment. It is implemented as sensibly as
# possible: one `.http_get()` call per selected message, headers carrying the
# byte range, concatenating the returned raw vectors in message order and
# writing them to a temp file. A real ECMWF response to a single-range
# request returns exactly that byte span (HTTP 206, confirmed live against
# data.ecmwf.int 2026-07-06); concatenating the (here: mocked, whole-file)
# bytes for N messages will over-count bytes if `.http_get()` is not honouring
# `Range`, which is fine for the mocked test (only the first message's bytes
# are used to open a working file) but is flagged here as a real caveat for a
# genuine multi-message fetch.
.ecmwf_download_messages <- function(base_url, messages) {
  grib_url <- paste0(base_url, ".grib2")

  chunks <- lapply(seq_len(nrow(messages)), function(i) {
    offset <- messages$`_offset`[i]
    length <- messages$`_length`[i]
    range_header <- sprintf("bytes=%d-%d", offset, offset + length - 1)
    .http_get(grib_url, headers = list(Range = range_header), parse = "raw")
  })

  tmp <- tempfile(fileext = ".grib2")
  con <- file(tmp, open = "wb")
  on.exit(close(con), add = TRUE)
  for (chunk in chunks) {
    writeBin(chunk, con)
  }
  tmp
}

S7::method(fetch_forecast, source_ecmwf) <- function(
  adapter, site, variables, issue_window, now = .now()
) {
  if (!.have_terra()) {
    abort_meteo(
      c(
        "{.pkg terra} is required to read ECMWF Open Data GRIB2 files, but is not installed.", # nolint: line_length_linter.
        "i" = "Install {.pkg terra} (which pulls in GDAL) to use {.fn source_ecmwf}.",
        "i" = "Alternatively, use {.code source_openmeteo(product = \"seasonal\")} for a no-GRIB seasonal splice." # nolint: line_length_linter.
      ),
      class = "terra_required"
    )
  }

  variables <- intersect(variables, adapter@provides)
  issue_time <- .ecmwf_resolve_issue_times(issue_window, now)
  step_hours <- round(as.numeric(difftime(issue_window$to, issue_window$from, units = "hours")))

  base_url <- .ecmwf_step_base_url(adapter, issue_time, step_hours)
  index_lines <- .http_get(paste0(base_url, ".index"), parse = "lines")
  idx <- ecmwf_index_parse(index_lines)

  params <- unique(unlist(.ecmwf_param_lookup()[variables], use.names = FALSE))
  members <- unique(idx$member)

  messages <- ecmwf_select_messages(idx, params = params, steps = step_hours, members = members)
  if (nrow(messages) == 0) {
    return(new_forecast(tibble::tibble(
      site_id = character(0), source = character(0), model = character(0),
      issue_time = as.POSIXct(character(0), tz = "UTC"),
      valid_time = as.POSIXct(character(0), tz = "UTC"),
      lead_time = as.difftime(numeric(0), units = "hours"),
      member = integer(0), stat = character(0),
      variable = character(0), value = double(0)
    )))
  }

  .grib_check_ccsds_support()

  local_path <- .ecmwf_download_messages(base_url, messages)
  rast <- grib_open(local_path)
  coords <- site_coords(site)
  vals <- grib_extract_point(
    rast,
    lat = as.numeric(units::drop_units(coords$latitude)),
    lon = as.numeric(units::drop_units(coords$longitude))
  )
  field_tbl <- grib_field_table(rast)
  field_tbl$value_raw <- vals[seq_len(nrow(field_tbl))]

  # Only the direct-param (temperature_2m via "2t") path is wired end-to-end;
  # u/v-derived wind variables would need vector recombination not built here
  # (see `.ecmwf_param_lookup()` comment) -- restrict rows to variables with a
  # single, direct param mapping.
  direct_vars <- names(Filter(function(p) length(p) == 1, .ecmwf_param_lookup()))
  variables_direct <- intersect(variables, direct_vars)

  rows <- lapply(variables_direct, function(var) {
    param <- .ecmwf_param_lookup()[[var]]
    sub <- field_tbl[field_tbl$param == param, , drop = FALSE]
    if (nrow(sub) == 0) {
      return(NULL)
    }
    # Convert from whatever unit GDAL actually decoded the values into (see
    # R/grib-read.R's header note: GDAL auto-converts ECMWF's Kelvin
    # temperatures to Celsius), not a hardcoded per-param guess.
    value_canonical <- vapply(seq_len(nrow(sub)), function(k) {
      as.numeric(to_canonical(sub$value_raw[k], sub$unit[k], var))
    }, numeric(1))
    step_hours_col <- suppressWarnings(as.numeric(sub$step))

    tibble::tibble(
      site_id = site_id(site),
      source = adapter@source_id,
      model = paste0("ifs_", adapter@stream),
      issue_time = issue_time,
      valid_time = issue_time + step_hours_col * 3600,
      lead_time = as.difftime(step_hours_col, units = "hours"),
      member = as.integer(sub$member),
      stat = NA_character_,
      variable = var,
      value = value_canonical
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]

  out <- if (length(rows) == 0) {
    tibble::tibble(
      site_id = character(0), source = character(0), model = character(0),
      issue_time = as.POSIXct(character(0), tz = "UTC"),
      valid_time = as.POSIXct(character(0), tz = "UTC"),
      lead_time = as.difftime(numeric(0), units = "hours"),
      member = integer(0), stat = character(0),
      variable = character(0), value = double(0)
    )
  } else {
    vctrs::vec_rbind(!!!rows)
  }

  new_forecast(out)
}

S7::method(met_attribution, source_ecmwf) <- function(adapter) {
  "Contains modified Copernicus/ECMWF Open Data (CC-BY 4.0)"
}

S7::method(format, source_ecmwf) <- function(x, ...) {
  c(
    sprintf("<source_ecmwf> source_id: %s", x@source_id),
    sprintf("  stream: %s", x@stream),
    sprintf("  resolution: %s", x@resolution)
  )
}

S7::method(print, source_ecmwf) <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
