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
# `.grib_element_to_param()` translation of GDAL's `GRIB_ELEMENT`). Wind
# speed/direction at 10 m are DERIVED from the "10u"/"10v" u/v vector
# components (GRIB does not carry speed/direction directly) --
# `.ecmwf_uv_to_wind()` below recombines them.
.ecmwf_param_lookup <- function() {
  list(
    temperature_2m = "2t",
    wind_speed_10m = c("10u", "10v"),
    wind_direction_10m = c("10u", "10v")
  )
}

#' Recombine ECMWF 10u/10v GRIB bands into canonical wind speed/direction
#'
#' GRIB never carries wind speed/direction directly, only the u/v vector
#' components (`"10u"`/`"10v"` GRIB `param` short names, `grib_field_table()`,
#' R/grib-read.R). This pairs the `"10u"`/`"10v"` rows of `field_tbl` on
#' `(step, member)` and derives the canonical `wind_speed_10m`
#' (`hypot(u, v)`) and `wind_direction_10m` rows, reusing `uv_to_dir()`
#' (R/wind-uv.R) for the direction -- the package's one u/v-to-direction
#' formula, not re-derived here. `10u`/`10v` are already SI (m/s), so no unit
#' conversion is needed for either derived variable.
#'
#' @param field_tbl A `grib_field_table()`-shaped tibble: `band`, `param`,
#'   `unit`, `step`, `member`.
#' @param values Numeric vector, the per-band extracted value, aligned to
#'   `field_tbl$band` (`values[i]` is the value of `field_tbl[i, ]`'s band).
#' @return A long tibble (`variable`, `value`, `unit`, `step`, `member`):
#'   one `wind_speed_10m` and one `wind_direction_10m` row per matched
#'   `(step, member)` 10u/10v pair. An unmatched 10u or 10v row (no
#'   corresponding partner at the same step/member) contributes nothing.
#' @keywords internal
#' @noRd
.ecmwf_uv_to_wind <- function(field_tbl, values) {
  field_tbl$value_raw <- values[seq_len(nrow(field_tbl))]
  u_rows <- field_tbl[field_tbl$param == "10u", , drop = FALSE]
  v_rows <- field_tbl[field_tbl$param == "10v", , drop = FALSE]

  key <- function(df) paste(df$step, df$member, sep = "\r")
  m <- match(key(u_rows), key(v_rows))
  matched <- !is.na(m)
  u_rows <- u_rows[matched, , drop = FALSE]
  v_rows <- v_rows[m[matched], , drop = FALSE]

  u <- u_rows$value_raw
  v <- v_rows$value_raw

  vctrs::vec_rbind(
    tibble::tibble(variable = "wind_speed_10m", value = sqrt(u^2 + v^2),
                   unit = "m/s", step = u_rows$step, member = u_rows$member),
    tibble::tibble(variable = "wind_direction_10m", value = uv_to_dir(u, v),
                   unit = "degree", step = u_rows$step, member = u_rows$member)
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

# Real ECMWF Open Data issues on a fixed 4x/day schedule (VERIFIED against
# the live https://data.ecmwf.int/forecasts/ mirror, 2026-07-06: the 00z/06z/
# 12z cycles for the current day were already published; 18z was not yet).
.ecmwf_cycle_hours <- function() c(0L, 6L, 12L, 18L)

# Round `t` down to the most recent 00/06/12/18Z ECMWF issue cycle.
.ecmwf_round_down_to_cycle <- function(t) {
  hour <- as.integer(format(t, "%H", tz = "UTC"))
  cycle_hours <- .ecmwf_cycle_hours()
  cycle_hour <- max(cycle_hours[cycle_hours <= hour])
  cycle_date <- format(t, "%Y-%m-%d", tz = "UTC")
  as.POSIXct(paste(cycle_date, sprintf("%02d:00:00", cycle_hour)), tz = "UTC")
}

#' Resolve the ECMWF issue cycle(s) spanning `issue_window`
#'
#' Rounds every requested instant down to the real 00/06/12/18Z ECMWF issue
#' schedule (never up past `now`, since a cycle cannot be issued in the
#' future) and returns every eligible cycle from `issue_window$from` through
#' `min(issue_window$to, now)`, spaced 6 hours apart.
#'
#' **v1 scope decision:** `fetch_forecast()` only ever fetches the *last*
#' (most recent) cycle this returns -- it downloads one per-step GRIB2 file
#' per call (SCOPING §5.2's per-step file layout) and stamps a single
#' `issue_time` onto every output row. Genuinely fetching *multiple* cycles
#' in one `fetch_forecast()` call (each producing its own set of rows) would
#' need a per-cycle download loop this plan does not build, since no test
#' requires it and the pipeline's own hourly/daily cadence (Plan 16) is what
#' provides multi-cycle coverage over time -- each sync tick naturally
#' targets "the current cycle". This is a deliberate, documented v1 decision
#' (not a silent gap): `.ecmwf_resolve_issue_times()` itself is fully correct
#' for multiple cycles: callers needing genuine multi-cycle-per-request
#' support can loop over its full return value themselves.
#'
#' @param issue_window A list with `from`/`to`, both UTC `POSIXct` scalars.
#' @param now Injectable current time; caps how recent a returned cycle can
#'   be.
#' @return A `POSIXct` vector of eligible cycle instants (ascending), each
#'   exactly on a 00/06/12/18 UTC hour boundary.
#' @keywords internal
#' @noRd
.ecmwf_resolve_issue_times <- function(issue_window, now) {
  from <- issue_window$from %||% now
  to <- min(issue_window$to %||% now, now)

  from_cycle <- .ecmwf_round_down_to_cycle(from)
  to_cycle <- .ecmwf_round_down_to_cycle(to)
  if (to_cycle < from_cycle) {
    to_cycle <- from_cycle
  }

  cycles <- seq(from_cycle, to_cycle, by = 6 * 3600)
  as.POSIXct(cycles, tz = "UTC", origin = "1970-01-01")
}

# Range-download the byte spans for `messages` (a filtered `.index` tibble,
# see `ecmwf_select_messages()`) and concatenate them into one local temp
# `.grib2` file, mirroring how ECMWF's messages are laid out contiguously in
# the real per-step GRIB2 file. Each message is fetched with its own `Range`
# HTTP header (`bytes=<offset>-<offset+length-1>`) via `.http_get(...,
# parse = "raw")`, the single HTTP seam every adapter uses (so
# `httptest2`/mocking and the no-network guard both apply here too).
#
# VERIFIED LIVE (2026-07-06, against https://data.ecmwf.int/forecasts/'s real
# 20260706/00z enfo cycle): this function's exact per-message Range-download-
# and-concatenate loop, called unmocked through the real `.http_get()` seam,
# correctly reconstructs a valid multi-message GRIB2 file from two
# independently range-downloaded messages (byte counts matched the index's
# `_length` exactly for each chunk, and the reconstructed local file opened
# via `grib_open()`/`grib_field_table()` with the right `param`/`step`/
# `member` metadata for both messages). A real ECMWF HTTP 206 response to a
# single-range request returns exactly that byte span (never the whole
# file), so concatenating N such chunks in message order reconstructs the
# identical byte layout as a contiguous slice of the real per-step file --
# this is not a caveat in real use. The only place this cannot be verified
# is the frozen `test-source-ecmwf.R` end-to-end test, which mocks
# `.http_get()` to hand back the whole fixture's raw bytes for every call
# regardless of the `Range` header requested (so only the first message's
# bytes are meaningfully exercised there) -- a test-fixture limitation, not
# a defect in this function.
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
  # .ecmwf_resolve_issue_times() can return multiple eligible cycles spanning
  # issue_window; this fetch targets the most recent one (see that
  # function's roxygen for the documented v1 multi-cycle-per-call decision).
  issue_cycles <- .ecmwf_resolve_issue_times(issue_window, now)
  issue_time <- issue_cycles[[length(issue_cycles)]]
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

  # Direct-param variables (temperature_2m via "2t"): a single GRIB param
  # maps straight to the canonical variable.
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

  # Derived wind variables (wind_speed_10m/wind_direction_10m): recombined
  # from the "10u"/"10v" bands via .ecmwf_uv_to_wind() -- already canonical
  # (m/s / degree), no to_canonical() conversion needed.
  wind_vars_requested <- intersect(variables, c("wind_speed_10m", "wind_direction_10m"))
  if (length(wind_vars_requested) > 0 && any(field_tbl$param %in% c("10u", "10v"))) {
    uv <- .ecmwf_uv_to_wind(field_tbl, field_tbl$value_raw)
    uv <- uv[uv$variable %in% wind_vars_requested, , drop = FALSE]
    if (nrow(uv) > 0) {
      step_hours_col <- suppressWarnings(as.numeric(uv$step))
      rows[[length(rows) + 1]] <- tibble::tibble(
        site_id = site_id(site),
        source = adapter@source_id,
        model = paste0("ifs_", adapter@stream),
        issue_time = issue_time,
        valid_time = issue_time + step_hours_col * 3600,
        lead_time = as.difftime(step_hours_col, units = "hours"),
        member = as.integer(uv$member),
        stat = NA_character_,
        variable = uv$variable,
        value = uv$value
      )
    }
  }

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
