# Plan 02 — met_sites, a validated list of met_site objects.

#' A validated collection of `met_site` objects
#'
#' `met_sites()` wraps one or more [met_site()] objects in a list validated to
#' have unique `site_id`s, named by `site_id` for `[[`/`$` lookup.
#'
#' @param ... `met_site` objects, or a single list of `met_site` objects.
#' @return A `met_sites` S7 object (a list of `met_site`, named by `site_id`).
#' @family site
#' @export
#' @examples
#' site_a <- met_site(
#'   site_id = "a",
#'   latitude = units::set_units(-34.75, "degree"),
#'   longitude = units::set_units(148.20, "degree"),
#'   elevation = units::set_units(220, "m"),
#'   timezone = "Australia/Sydney",
#'   instruments = list(),
#'   sources = list(),
#'   store_root = tempfile()
#' )
#' sites <- met_sites(list(site_a))
#' site_ids(sites)
met_sites <- S7::new_class(
  "met_sites",
  package = "meteoTidy",
  properties = list(
    sites = S7::class_list
  ),
  validator = function(self) {
    .validate_met_sites(self)
  },
  constructor = function(...) {
    dots <- list(...)
    sites <- if (length(dots) == 1 && is.list(dots[[1]]) && !S7::S7_inherits(dots[[1]], met_site)) {
      dots[[1]]
    } else {
      dots
    }
    ids <- vapply(sites, function(s) s@site_id, character(1))
    names(sites) <- ids
    S7::new_object(S7::S7_object(), sites = sites)
  }
)

.validate_met_sites <- function(self) {
  sites <- self@sites
  if (!all(vapply(sites, S7::S7_inherits, logical(1), class = met_site))) {
    abort_meteo("Every element of {.arg sites} must be a {.cls met_site} object.", class = "bad_site_list") # nolint: line_length_linter.
  }
  ids <- vapply(sites, function(s) s@site_id, character(1))
  dup <- unique(ids[duplicated(ids)])
  if (length(dup) > 0) {
    abort_meteo(
      c(
        "{.arg site_id} values must be unique within a {.cls met_sites}.",
        "x" = "Duplicated: {.val {dup}}"
      ),
      class = "duplicate_site_id"
    )
  }
  invisible(NULL)
}

#' The site IDs in a `met_sites` collection
#'
#' @param sites A [met_sites()] object.
#' @return A character vector of `site_id` values, in order.
#' @family site
#' @export
#' @examples
#' site_ids(met_sites(list()))
site_ids <- function(sites) {
  vapply(sites@sites, function(s) s@site_id, character(1))
}

#' @export
`[[.meteoTidy::met_sites` <- function(x, i, ...) {
  x@sites[[i]]
}

#' @export
`$.meteoTidy::met_sites` <- function(x, name) {
  x@sites[[name]]
}

#' @export
`length.meteoTidy::met_sites` <- function(x) {
  length(x@sites)
}

#' Normalise a single site or a list of sites into `met_sites`
#'
#' Every multi-site verb accepts either a single [met_site()] or a
#' [met_sites()]; this normalises either into a `met_sites`.
#'
#' @param x A `met_site`, a `met_sites`, or a plain list of `met_site` objects.
#' @return A `met_sites` object.
#' @family site
#' @export
#' @examples
#' site <- met_site(
#'   site_id = "a",
#'   latitude = units::set_units(-34.75, "degree"),
#'   longitude = units::set_units(148.20, "degree"),
#'   elevation = units::set_units(220, "m"),
#'   timezone = "Australia/Sydney",
#'   instruments = list(),
#'   sources = list(),
#'   store_root = tempfile()
#' )
#' as_met_sites(site)
as_met_sites <- function(x) {
  if (S7::S7_inherits(x, met_sites)) {
    return(x)
  }
  if (S7::S7_inherits(x, met_site)) {
    return(met_sites(list(x)))
  }
  if (is.list(x)) {
    return(met_sites(x))
  }
  abort_meteo(
    "{.arg x} must be a {.cls met_site}, a {.cls met_sites}, or a list of {.cls met_site}.", # nolint: line_length_linter.
    class = "bad_site_list"
  )
}
