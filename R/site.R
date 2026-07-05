# Plan 02 — met_site S7 class + validator + resolved-ID cache.
#
# `met_site` is the domain object every stored row is keyed by (SCOPING §3).
# Dimensioned properties carry `units` at construction per house style; the
# `resolved` slot is a passive external-ID cache populated later by adapters
# (Plans 05-07) via `site_set_resolved()` — this plan only provides the shape
# and the functional setter/getter.

#' A single meteorological instrument
#'
#' An S7 class describing one sensor at a [met_site()]: what it measures, at
#' what height, and (for wind sensors) the roughness length and displacement
#' height needed for height-correction (SCOPING §3, §7.1).
#'
#' @param name Single string, e.g. `"anemometer"`, `"thermo"`, `"pyranometer"`.
#' @param variable Character vector of dictionary variable names this
#'   instrument measures (see [met_variable()]).
#' @param height A `units`-classed double (metres), sensor height above
#'   ground level.
#' @param roughness_length A `units`-classed double (metres) or `NA`, the
#'   aerodynamic roughness length `z0`. Required (non-`NA`) for any instrument
#'   measuring a wind variable.
#' @param displacement_height A `units`-classed double (metres), the
#'   displacement height `d`. Defaults to 0 m.
#'
#' @return A `met_instrument` S7 object.
#' @family site
#' @export
#' @examples
#' met_instrument(
#'   name = "anemometer",
#'   variable = c("wind_speed_10m", "wind_direction_10m"),
#'   height = units::set_units(10, "m"),
#'   roughness_length = units::set_units(0.03, "m")
#' )
met_instrument <- S7::new_class(
  "met_instrument",
  package = "meteoTidy",
  properties = list(
    name = S7::class_character,
    variable = S7::class_character,
    height = S7::class_double,
    roughness_length = S7::new_property(
      S7::class_double,
      default = quote(units::set_units(NA_real_, "m"))
    ),
    displacement_height = S7::new_property(
      S7::class_double,
      default = quote(units::set_units(0, "m"))
    )
  )
)

# Is `variable` (one or more dictionary names) a wind variable? Used to decide
# whether an instrument requires a non-NA roughness_length.
.is_wind_variable <- function(variable) {
  grepl("^wind_", variable)
}

#' A meteorological site
#'
#' An S7 class describing a single monitoring site: location, timezone,
#' instrumentation, source configuration, storage location, and a cache of
#' external station/grid identifiers resolved by adapters.
#'
#' @param site_id Single string, non-empty, matching `[A-Za-z0-9_-]+`. The
#'   join key used everywhere in the canonical store.
#' @param latitude,longitude A `units`-classed double (degrees). Latitude in
#'   `[-90, 90]`, longitude in `[-180, 180]`.
#' @param elevation A `units`-classed double (metres), station elevation.
#' @param timezone Single string, an IANA timezone name (validated against
#'   [OlsonNames()]).
#' @param instruments A list of [met_instrument()] objects.
#' @param sources A named list of raw source configs (opaque here; parsed by
#'   adapters in Plan 04).
#' @param store_root Single string, the filesystem path (or URI) for this
#'   site's Parquet tree.
#' @param resolved A list, the external-ID resolution cache. See
#'   [site_resolved()] / [site_set_resolved()]. Defaults to an empty
#'   `bom`/`ghcnh`/`silo` skeleton.
#'
#' @return A `met_site` S7 object.
#' @family site
#' @export
#' @examples
#' met_site(
#'   site_id = "example",
#'   latitude = units::set_units(-34.75, "degree"),
#'   longitude = units::set_units(148.20, "degree"),
#'   elevation = units::set_units(220, "m"),
#'   timezone = "Australia/Sydney",
#'   instruments = list(),
#'   sources = list(),
#'   store_root = tempfile()
#' )
met_site <- S7::new_class(
  "met_site",
  package = "meteoTidy",
  properties = list(
    site_id = S7::class_character,
    latitude = S7::class_double,
    longitude = S7::class_double,
    elevation = S7::class_double,
    timezone = S7::class_character,
    instruments = S7::class_list,
    sources = S7::class_list,
    store_root = S7::class_character,
    resolved = S7::new_property(S7::class_list, default = quote(.empty_resolved()))
  ),
  validator = function(self) {
    .validate_met_site(self)
  }
)

# The default (empty) shape of the `resolved` cache: bom/ghcnh/silo sub-lists
# with every field defaulting to NA. See plans/02-site-registry.md.
.empty_resolved <- function() {
  list(
    bom = list(geohash = NA_character_, aac = NA_character_, product = NA_character_),
    ghcnh = list(station_id = NA_character_, distance_km = NA_real_),
    silo = list(grid = NA_character_)
  )
}

# Validator body for met_site, factored out for readability. Returns NULL on
# success or aborts via abort_meteo() with a specific class on the first
# violation found (S7 validators may also return a character string, but we
# want classed conditions here, matching house style).
.validate_met_site <- function(self) {
  if (length(self@site_id) != 1 || is.na(self@site_id) ||
        !grepl("^[A-Za-z0-9_-]+$", self@site_id)) {
    abort_meteo(
      c(
        "{.field site_id} must be a non-empty string matching {.val [A-Za-z0-9_-]+}.",
        "x" = "Got {.val {self@site_id}}."
      ),
      class = "bad_site_id"
    )
  }

  .validate_coordinate(self@latitude, -90, 90, "latitude")
  .validate_coordinate(self@longitude, -180, 180, "longitude")

  if (!is.finite(as.numeric(self@elevation))) {
    abort_meteo(
      "{.field elevation} must be finite.",
      class = "bad_coordinates"
    )
  }

  if (length(self@timezone) != 1 || is.na(self@timezone) ||
        !(self@timezone %in% OlsonNames())) {
    abort_meteo(
      c(
        "{.field timezone} must be a valid IANA timezone name.",
        "x" = "Got {.val {self@timezone}}."
      ),
      class = "bad_timezone"
    )
  }

  if (length(self@store_root) != 1 || is.na(self@store_root) || !nzchar(self@store_root)) {
    abort_meteo("{.field store_root} must be a non-empty string.", class = "bad_store_root")
  }

  for (instrument in self@instruments) {
    .validate_instrument(instrument)
  }

  invisible(NULL)
}

.validate_coordinate <- function(value, lo, hi, label) {
  numeric_value <- as.numeric(value)
  if (length(numeric_value) != 1 || !is.finite(numeric_value) ||
        numeric_value < lo || numeric_value > hi) {
    abort_meteo(
      c(
        "{.field {label}} must be in [{lo}, {hi}].",
        "x" = "Got {.val {numeric_value}}."
      ),
      class = "bad_coordinates"
    )
  }
  invisible(NULL)
}

.validate_instrument <- function(instrument) {
  for (variable in instrument@variable) {
    met_variable(variable) # aborts class "unknown_variable" if unknown
  }

  if (any(.is_wind_variable(instrument@variable)) &&
        is.na(as.numeric(instrument@roughness_length))) {
    abort_meteo(
      c(
        "Instrument {.val {instrument@name}} measures a wind variable but has no {.field roughness_length}.", # nolint: line_length_linter.
        "i" = "Height correction is meaningless without {.field z0}; supply {.arg roughness_length}." # nolint: line_length_linter.
      ),
      class = "missing_roughness"
    )
  }

  invisible(NULL)
}

#' Accessors for `met_site` objects
#'
#' @param site A [met_site()] object.
#' @return
#' - `site_id()` — the site's `site_id` string.
#' - `site_coords()` — a list with `latitude`, `longitude`, `elevation`.
#' - `site_roughness()` — a named numeric (metres) of `roughness_length` by
#'   instrument name, `NA` where not applicable.
#' - `site_sources()` — the `sources` named list.
#' - `site_store_root()` — the `store_root` string.
#' - `site_instruments()` — the list of [met_instrument()] objects.
#' @family site
#' @export
#' @examples
#' site <- met_site(
#'   site_id = "example",
#'   latitude = units::set_units(-34.75, "degree"),
#'   longitude = units::set_units(148.20, "degree"),
#'   elevation = units::set_units(220, "m"),
#'   timezone = "Australia/Sydney",
#'   instruments = list(),
#'   sources = list(),
#'   store_root = tempfile()
#' )
#' site_id(site)
#' site_coords(site)
site_id <- function(site) {
  site@site_id
}

#' @rdname site_id
#' @export
site_coords <- function(site) {
  list(latitude = site@latitude, longitude = site@longitude, elevation = site@elevation)
}

#' @rdname site_id
#' @export
site_roughness <- function(site) {
  instruments <- site@instruments
  values <- vapply(instruments, function(i) as.numeric(i@roughness_length), double(1))
  names(values) <- vapply(instruments, function(i) i@name, character(1))
  values
}

#' @rdname site_id
#' @export
site_sources <- function(site) {
  site@sources
}

#' @rdname site_id
#' @export
site_store_root <- function(site) {
  site@store_root
}

#' @rdname site_id
#' @export
site_instruments <- function(site) {
  site@instruments
}

#' Read or set a site's resolved external-ID cache
#'
#' The `resolved` slot is a passive cache of external station/grid
#' identifiers (BOM geohash/AAC/product, nearest GHCNh station, SILO grid
#' cell) populated by adapters (Plans 05-07). This plan only provides the
#' accessors: `site_resolved()` reads (optionally at a `path` into the nested
#' list), and `site_set_resolved()` returns a **new** site with the value set
#' — `met_site` objects are never mutated in place (house style: functional
#' by default).
#'
#' @param site A [met_site()] object.
#' @param path Character vector of nested list names, e.g.
#'   `c("ghcnh", "station_id")`. `NULL` (the default for `site_resolved()`)
#'   returns the whole `resolved` list.
#' @param value The value to set at `path`.
#' @return `site_resolved()` returns the value at `path` (or the whole
#'   `resolved` list if `path = NULL`). `site_set_resolved()` returns a new
#'   `met_site` object with `resolved` updated; the original is unchanged.
#' @family site
#' @export
#' @examples
#' site <- met_site(
#'   site_id = "example",
#'   latitude = units::set_units(-34.75, "degree"),
#'   longitude = units::set_units(148.20, "degree"),
#'   elevation = units::set_units(220, "m"),
#'   timezone = "Australia/Sydney",
#'   instruments = list(),
#'   sources = list(),
#'   store_root = tempfile()
#' )
#' updated <- site_set_resolved(site, c("ghcnh", "station_id"), "ASN00072150")
#' site_resolved(updated, c("ghcnh", "station_id"))
site_resolved <- function(site, path = NULL) {
  if (is.null(path)) {
    return(site@resolved)
  }
  purrr_like_pluck(site@resolved, path)
}

#' @rdname site_resolved
#' @export
site_set_resolved <- function(site, path, value) {
  new_resolved <- site@resolved
  new_resolved <- assign_at_path(new_resolved, path, value)
  site@resolved <- new_resolved
  site
}

# A minimal, dependency-free stand-in for purrr::pluck() on nested lists.
purrr_like_pluck <- function(x, path) {
  for (name in path) {
    x <- x[[name]]
  }
  x
}

# Functional nested-list assignment: returns a new list with `value` set at
# `path`, without mutating `x` (base R's `[[<-` on a list already copies, but
# spelling this out keeps the functional contract explicit and testable).
assign_at_path <- function(x, path, value) {
  if (length(path) == 1) {
    x[[path]] <- value
    return(x)
  }
  head <- path[[1]]
  x[[head]] <- assign_at_path(x[[head]], path[-1], value)
  x
}
