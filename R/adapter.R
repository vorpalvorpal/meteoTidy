# Plan 04 — the adapter contract: S7 base class, `fetch()`/`fetch_forecast()`/
# `resolve_station()` generics, and the return-contract checker every
# adapter's `fetch()` runs on its own output before returning.
#
# This is the interface Plans 05-08 (Open-Meteo, SILO/GHCNh, BOM, ECMWF)
# implement for specific external sources; it is also the interface a user
# can implement for a bespoke source (SCOPING §5).

#' The meteoTidy adapter contract
#'
#' `met_adapter` is the S7 base class every acquisition adapter extends
#' (`source_rest()`, `source_file()`, and provider-specific adapters added by
#' later plans). It carries only the properties shared by every adapter; the
#' actual fetch behaviour is implemented by [fetch()] methods on subclasses.
#'
#' @param source_id Single string, the `source` value stamped into every row
#'   returned by this adapter's [fetch()] (e.g. `"site_aws"`, `"silo"`,
#'   `"openmeteo"`).
#' @param provides Character vector of dictionary variable names this adapter
#'   can return.
#' @param cadence Single string, one of `"hourly"`, `"daily"`, `"subdaily"`,
#'   `"per_issue"` — a documentation/scheduling hint, not enforced.
#'
#' @return A `met_adapter` S7 object. In practice, users construct a subclass
#'   ([source_rest()], [source_file()]) rather than `met_adapter()` directly.
#' @family adapter
#' @export
#' @examples
#' met_adapter(source_id = "example", provides = "temperature_2m")
met_adapter <- S7::new_class(
  "met_adapter",
  package = "meteoTidy",
  properties = list(
    source_id = S7::class_character,
    provides = S7::class_character,
    cadence = S7::class_character
  )
)

#' Fetch canonical observations from an adapter
#'
#' The core of the adapter contract (SCOPING §5): every adapter implements a
#' `fetch()` method returning a canonical long observation tibble (see the
#' internal `new_obs()`) for the requested `variables` over `window`.
#'
#' Implementations must:
#' 1. Request/parse only the requested `variables` (intersected with
#'    `provides`).
#' 2. Convert every value to canonical units via `to_canonical()` **before**
#'    returning (SCOPING §3.1 unit pinning).
#' 3. Stamp `source = source_id`, an appropriate `method`, and
#'    `qc_flag = "ok"` (QC happens later; the adapter asserts nothing about
#'    quality beyond "as delivered").
#' 4. Return an empty (0-row) canonical table, not `NULL`, when the source has
#'    no data for `window`.
#'
#' @param adapter A [met_adapter()] subclass instance.
#' @param site A `met_site` object (see Plan 02).
#' @param variables Character vector of dictionary variable names requested.
#' @param window A list with `from`/`to`, both UTC `POSIXct` scalars.
#' @param now Injectable clock; defaults to `.now()`.
#'
#' @return A canonical long observation tibble.
#' @family adapter
#' @export
fetch <- S7::new_generic(
  "fetch", "adapter",
  function(adapter, site, variables, window, now = .now()) {
    S7::S7_dispatch()
  }
)

#' Fetch canonical forecasts from an adapter
#'
#' Like [fetch()], but for forecast products (see the internal
#' `new_forecast()`). The default method aborts class `"no_forecast_support"`;
#' only adapters that implement forecast retrieval (Open-Meteo, BOM, ECMWF —
#' Plans 05/07/08) override it.
#'
#' @inheritParams fetch
#' @param issue_window A list with `from`/`to`, both UTC `POSIXct` scalars,
#'   bounding the forecast issue times of interest.
#'
#' @return A canonical forecast tibble.
#' @family adapter
#' @export
fetch_forecast <- S7::new_generic(
  "fetch_forecast", "adapter",
  function(adapter, site, variables, issue_window, now = .now()) {
    S7::S7_dispatch()
  }
)

S7::method(fetch_forecast, met_adapter) <- function(
  adapter, site, variables, issue_window, now = .now()
) {
  abort_meteo(
    c(
      "{.arg adapter} ({.cls {class(adapter)[1]}}) does not support forecast retrieval.",
      "i" = "Only Open-Meteo/BOM/ECMWF adapters implement {.fn fetch_forecast}."
    ),
    class = "no_forecast_support"
  )
}

#' Resolve a site's external station/grid identifier for an adapter
#'
#' Some adapters (BOM, GHCNh, SILO — Plans 06-07) need to resolve a site's
#' coordinates to an external station or grid identifier before they can
#' fetch data, and cache the result on the site's `resolved` slot (see
#' `site_resolved()`/`site_set_resolved()`). The default method is a no-op:
#' it returns `site` unchanged, which is correct for adapters (like
#' `source_rest()`/`source_file()`) that need no such resolution.
#'
#' @param adapter A [met_adapter()] subclass instance.
#' @param site A `met_site` object.
#'
#' @return A `met_site` object (possibly with `resolved` updated).
#' @family adapter
#' @export
resolve_station <- S7::new_generic("resolve_station", "adapter", function(adapter, site) {
  S7::S7_dispatch()
})

S7::method(resolve_station, met_adapter) <- function(adapter, site) {
  site
}

#' Check that a fetched table honours the adapter contract
#'
#' An internal, belt-and-braces contract checker every adapter's `fetch()`
#' runs on its own output before returning: validates that `x` is a
#' well-formed canonical observation table, that `source` is uniformly
#' `adapter@source_id`, and that every `variable` present in `x` is one of
#' the requested `variables`. This is what guarantees a *user-written*
#' adapter that passes its tests actually honours the contract (SCOPING §5).
#'
#' @param x A tibble, the candidate canonical observation table.
#' @param adapter A [met_adapter()] subclass instance.
#' @param variables Character vector of the variables that were requested.
#'
#' @return `x`, invisibly, if it satisfies the contract.
#' @keywords internal
#' @noRd
check_fetch_result <- function(x, adapter, variables) {
  x <- new_obs(x)

  sources <- unique(x$source)
  if (length(sources) > 1) {
    abort_meteo(
      c(
        "{.arg x} must have a uniform {.field source} column.",
        "x" = "Found {.val {sources}}."
      ),
      class = "source_not_uniform"
    )
  }

  unrequested <- setdiff(unique(x$variable), variables)
  if (length(unrequested) > 0) {
    abort_meteo(
      c(
        "{cli::qty(length(unrequested))}{.arg x} contains variable{?s} that {?was/were} not requested.", # nolint: line_length_linter.
        "x" = "Unrequested: {.val {unrequested}}.",
        "i" = "Requested: {.val {variables}}."
      ),
      class = "unrequested_variable"
    )
  }

  invisible(x)
}

# Adapter names that are recognised (so they get a specific, tested stub
# error) but not yet implemented -- Plans 05-08 replace this stub with a real
# constructor for each.
.adapter_not_yet_implemented_names <- function() {
  c("openmeteo", "silo", "ghcnh", "bom_forecast", "bom_obs", "ecmwf")
}

# Build one met_adapter from a single named entry of site_sources(site),
# e.g. list(adapter = "rest", endpoint = ..., mapping = ...).
.adapter_from_source_config <- function(source_name, config) {
  kind <- config$adapter
  if (is.null(kind)) {
    abort_meteo(
      "Source {.val {source_name}} is missing an {.field adapter} field.",
      class = "unknown_adapter"
    )
  }

  if (kind == "rest") {
    return(source_rest(
      source_id = source_name,
      endpoint = config$endpoint,
      mapping = config$mapping,
      auth = config$auth %||% "none",
      token_env = config$token_env,
      provides = config$provides,
      cadence = config$cadence %||% "hourly"
    ))
  }

  if (kind == "file") {
    return(source_file(
      source_id = source_name,
      glob = config$glob,
      mapping = config$mapping,
      provides = config$provides,
      cadence = config$cadence %||% "daily"
    ))
  }

  if (kind %in% .adapter_not_yet_implemented_names()) {
    abort_meteo(
      c(
        "Adapter kind {.val {kind}} (source {.val {source_name}}) is not yet implemented.",
        "i" = "This is a temporary stub; a later plan replaces it with a real adapter."
      ),
      class = "adapter_not_yet_implemented"
    )
  }

  abort_meteo(
    c(
      "Source {.val {source_name}} declares unknown adapter kind {.val {kind}}.",
      "i" = "Recognised kinds: {.val {c('rest', 'file', .adapter_not_yet_implemented_names())}}."
    ),
    class = "unknown_adapter"
  )
}

#' Build the adapters configured for a site
#'
#' Reads a site's `sources` configuration (see `site_sources()`, populated
#' from YAML by Plan 02) and constructs one [met_adapter()] per entry,
#' dispatching on each source's `adapter` field: `"rest"` builds a
#' [source_rest()], `"file"` builds a [source_file()]. Source kinds reserved
#' for later plans (`"openmeteo"`, `"silo"`, `"ghcnh"`, `"bom_forecast"`,
#' `"bom_obs"`, `"ecmwf"`) abort a tested, temporary
#' `"adapter_not_yet_implemented"` stub; anything else aborts
#' `"unknown_adapter"`.
#'
#' @param site A `met_site` object.
#'
#' @return A named list of [met_adapter()] objects, named by source name.
#' @family adapter
#' @export
adapters_for_site <- function(site) {
  sources <- site_sources(site)
  adapters <- lapply(names(sources), function(nm) {
    .adapter_from_source_config(nm, sources[[nm]])
  })
  names(adapters) <- names(sources)
  adapters
}
