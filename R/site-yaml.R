# Plan 02 — site registry YAML (de)serialisation.
#
# Schema is documented in full in plans/02-site-registry.md and mirrored in
# the fixtures under tests/testthat/_fixtures/sites/ (see one-site.yaml for
# a worked example): a top-level `sites` key holding a list of site entries,
# each with site_id/latitude/longitude/elevation/timezone/store_root, a list
# of `instruments` (name/variable/height/roughness_length/
# displacement_height), and a `sources` named list of opaque adapter configs.
#
# Numbers are unitless in the YAML (height/roughness/displacement in metres,
# lat/lon in degrees, elevation in metres) and have units attached on read;
# `write_sites_yaml()` strips them back to plain numbers so the file stays
# human-editable.

.site_top_level_keys <- c(
  "site_id", "latitude", "longitude", "elevation", "timezone",
  "store_root", "instruments", "sources", "resolved"
)

.instrument_keys <- c(
  "name", "variable", "height", "roughness_length", "displacement_height"
)

# Key names that look like secrets when their value is a literal (not a
# *_env / *_keyring reference). Heuristic only (SCOPING §11); full secret
# handling is Plan 14.
.secret_like_keys <- c("token", "api_key", "password", "secret", "key")

#' Read a site registry from YAML
#'
#' Parses a version-controlled site configuration file into a [met_sites()]
#' object. Numbers are assumed to be in the documented canonical unit (metres
#' for heights/elevation, degrees for lat/lon) and have `units` attached on
#' read. Source configs (`sources.*`) may only reference secrets by
#' `*_env`/`*_keyring` key names — a literal-looking secret value aborts
#' (class `"inline_secret"`); unrecognised top-level or per-site keys abort
#' (class `"unknown_config_key"`) so typos fail loudly.
#'
#' @param path Single string, path to a YAML file (see `vignette` schema in
#'   `plans/02-site-registry.md` and the package fixtures under
#'   `tests/testthat/_fixtures/sites/`).
#' @return A [met_sites()] object.
#' @family site
#' @export
#' @examples
#' path <- system.file(
#'   "extdata", "example-site.yaml",
#'   package = "meteoTidy"
#' )
#' if (nzchar(path)) read_sites_yaml(path)
read_sites_yaml <- function(path) {
  raw <- yaml::read_yaml(path)

  unknown_top <- setdiff(names(raw), "sites")
  if (length(unknown_top) > 0) {
    abort_meteo(
      c(
        "Unknown top-level key{?s} in site YAML: {.val {unknown_top}}.",
        "i" = "Recognised top-level key: {.val sites}."
      ),
      class = "unknown_config_key"
    )
  }

  site_specs <- raw$sites %||% list()
  sites <- lapply(site_specs, .site_from_yaml_spec)
  met_sites(sites)
}

.site_from_yaml_spec <- function(spec) {
  recognised <- .site_top_level_keys
  unknown <- setdiff(names(spec), recognised)
  if (length(unknown) > 0) {
    abort_meteo(
      c(
        "Unknown key{?s} in site YAML entry: {.val {unknown}}.",
        "i" = "Recognised keys: {.val {recognised}}."
      ),
      class = "unknown_config_key"
    )
  }

  instruments <- lapply(spec$instruments %||% list(), .instrument_from_yaml_spec)
  sources <- spec$sources %||% list()
  .check_no_inline_secrets(sources)

  args <- list(
    site_id = spec$site_id,
    latitude = units::set_units(as.numeric(spec$latitude), "degree"),
    longitude = units::set_units(as.numeric(spec$longitude), "degree"),
    elevation = units::set_units(as.numeric(spec$elevation), "m"),
    timezone = spec$timezone,
    instruments = instruments,
    sources = sources,
    store_root = spec$store_root
  )
  # `resolved` only appears when the file was written with
  # include_resolved = TRUE (write_sites_yaml()); the written shape already
  # carries the full bom/ghcnh/silo skeleton, so it is passed through as-is.
  if (!is.null(spec$resolved)) {
    args$resolved <- spec$resolved
  }
  do.call(met_site, args)
}

.instrument_from_yaml_spec <- function(spec) {
  recognised <- .instrument_keys
  unknown <- setdiff(names(spec), recognised)
  if (length(unknown) > 0) {
    abort_meteo(
      c(
        "Unknown key{?s} in instrument YAML entry: {.val {unknown}}.",
        "i" = "Recognised keys: {.val {recognised}}."
      ),
      class = "unknown_config_key"
    )
  }

  args <- list(
    name = spec$name,
    variable = as.character(spec$variable),
    height = units::set_units(as.numeric(spec$height), "m")
  )
  if (!is.null(spec$roughness_length)) {
    args$roughness_length <- units::set_units(as.numeric(spec$roughness_length), "m")
  }
  if (!is.null(spec$displacement_height)) {
    args$displacement_height <- units::set_units(as.numeric(spec$displacement_height), "m")
  }
  do.call(met_instrument, args)
}

# Walk a `sources` named list looking for a key that looks like a secret
# (SCOPING §11 heuristic) whose value is a literal string rather than a
# *_env/*_keyring reference key. Recurses into nested lists (each source
# entry is itself a named list of config fields).
.check_no_inline_secrets <- function(x, key = NULL) {
  if (is.list(x)) {
    for (name in names(x)) {
      .check_no_inline_secrets(x[[name]], name)
    }
    return(invisible(NULL))
  }

  if (is.null(key)) {
    return(invisible(NULL))
  }

  looks_like_secret_key <- tolower(key) %in% .secret_like_keys
  is_reference_key <- grepl("_env$|_keyring$", key)

  if (looks_like_secret_key && !is_reference_key) {
    abort_meteo(
      c(
        "Site YAML contains an inline secret value for key {.val {key}}.",
        "i" = "Reference secrets by name instead, e.g. {.val token_env} or {.val token_keyring}." # nolint: line_length_linter.
      ),
      class = "inline_secret"
    )
  }

  invisible(NULL)
}

#' Write a site registry to YAML
#'
#' The inverse of [read_sites_yaml()]: serialises a [met_sites()] (or a
#' single [met_site()]) back to the documented YAML schema. `units` are
#' stripped back to plain numbers in the documented canonical unit, so
#' `read_sites_yaml(path) |> write_sites_yaml(tmp) |> read_sites_yaml()`
#' round-trips to an equivalent `met_sites`. The `resolved` external-ID cache
#' is a runtime cache, not configuration, so it is excluded by default.
#'
#' @param sites A [met_sites()] or a single [met_site()].
#' @param path Single string, output file path.
#' @param include_resolved Logical, default `FALSE`. If `TRUE`, snapshot the
#'   `resolved` cache into the written file (useful for debugging only; the
#'   version-controlled config should not normally carry it).
#' @return `path`, invisibly.
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
#' tmp <- tempfile(fileext = ".yaml")
#' write_sites_yaml(met_sites(list(site)), tmp)
write_sites_yaml <- function(sites, path, include_resolved = FALSE) {
  sites <- as_met_sites(sites)
  spec <- list(
    sites = lapply(sites@sites, .site_to_yaml_spec, include_resolved = include_resolved)
  )
  yaml::write_yaml(spec, path)
  invisible(path)
}

.site_to_yaml_spec <- function(site, include_resolved) {
  spec <- list(
    site_id = site@site_id,
    latitude = as.numeric(site@latitude),
    longitude = as.numeric(site@longitude),
    elevation = as.numeric(site@elevation),
    timezone = site@timezone,
    store_root = site@store_root,
    instruments = lapply(site@instruments, .instrument_to_yaml_spec),
    sources = site@sources
  )
  if (include_resolved) {
    spec$resolved <- site@resolved
  }
  spec
}

.instrument_to_yaml_spec <- function(instrument) {
  spec <- list(
    name = instrument@name,
    variable = as.list(instrument@variable),
    height = as.numeric(instrument@height)
  )
  if (!is.na(as.numeric(instrument@roughness_length))) {
    spec$roughness_length <- as.numeric(instrument@roughness_length)
  }
  spec$displacement_height <- as.numeric(instrument@displacement_height)
  spec
}
