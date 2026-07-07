#' @include history-products.R store-obs.R store-forecast.R verify.R
NULL

# Plan 14 -- the stable, public read surface (SCOPING section 11): plain,
# canonical tibble-returning functions over the store. These four
# signatures are the VERSIONED PROMISE -- change them only with a
# deprecation cycle (the r-lib `lifecycle` conventions); the experimental
# `met_connect()` (R/connect.R) is the only place the physical schema
# itself is exposed. Every function accepts a single `met_site` or a
# `met_sites` collection (normalised via `as_met_sites()`, Plan 02) and
# row-binds across sites, and validates its return value with the Plan 01
# canonical helper (`new_obs()`/`new_forecast()`) before returning.

# Bridge to the curated history products (Plan 10's `build_history_daily()`/
# `build_history_hourly()`, which compute the product on demand rather than
# reading a separately-persisted table). Kept as its own named function
# (rather than inlined in `met_history()`) so tests can mock it directly to
# exercise `met_history()`'s own row-binding/validation logic without
# needing a populated store.
#
# `as_of` is threaded straight into both builders (Plan 17 item 9), which
# thread it on into their own single `store_read_obs()` call -- the
# aggregation/compositing each builder does is a pure function of the rows
# read, so a point-in-time read of the store reproduces the point-in-time
# product with no other change needed.
#
# The site object argument is named `met_site_obj`, NOT `site`: R's partial
# argument matching would otherwise treat a caller's `site = ` as an
# unambiguous abbreviation of the `site_id` formal (a real footgun hit
# during development -- `site = s` silently overwrote `site_id` with the
# whole `met_site` object instead of falling through to `...`).
store_read_history <- function(store_root, site_id, resolution = c("daily", "hourly"),
                               met_site_obj, from = NULL, to = NULL, as_of = NULL, ...) {
  resolution <- rlang::arg_match(resolution)
  window <- list(
    from = from %||% as.POSIXct("1970-01-01", tz = "UTC"),
    to = to %||% .now()
  )
  if (resolution == "daily") {
    build_history_daily(store_root, met_site_obj, window, as_of = as_of)
  } else {
    build_history_hourly(store_root, met_site_obj, window, as_of = as_of)
  }
}

#' The curated history record (`history_daily`/`history_hourly`)
#'
#' Returns the QC'd, gap-filled, curated observation history at daily or
#' hourly resolution (SCOPING §4): SILO-based `history_daily`, or hourly
#' aggregates for `history_hourly`, with provenance recording which leg
#' (SILO vs. the site's own AWS) served each value.
#'
#' @param site A [met_site()] or [met_sites()].
#' @param resolution Either `"daily"` (default) or `"hourly"`.
#' @param variables Optional character vector to restrict which variables
#'   are returned; `NULL` (default) returns every variable present.
#' @param from,to Optional UTC POSIXct bounds.
#' @param as_of Optional UTC POSIXct point-in-time read: reproduces the
#'   curated history the store would have produced at that point in time
#'   (Plan 03's revision policy, threaded through to the underlying
#'   `store_read_obs()` call the curated-product builders make).
#' @return A canonical long observation tibble (see `new_obs()`).
#' @family read-api
#' @export
#' @examples
#' \dontrun{
#' met_history(site, resolution = "daily")
#' }
met_history <- function(site, resolution = c("daily", "hourly"), variables = NULL,
                        from = NULL, to = NULL, as_of = NULL) {
  resolution <- rlang::arg_match(resolution)
  sites <- as_met_sites(site)

  rows <- lapply(sites@sites, function(s) {
    out <- store_read_history(site_store_root(s), site_id(s), resolution = resolution,
                              met_site_obj = s, from = from, to = to, as_of = as_of)
    if (!is.null(variables) && nrow(out) > 0) {
      out <- out[out$variable %in% variables, , drop = FALSE]
    }
    out
  })

  new_obs(vctrs::vec_rbind(!!!rows))
}

#' The site's "best available truth" curated record
#'
#' Returns the QC'd, gap-filled observation series for `site` -- the
#' curated record consumers should read by default (SCOPING §4).
#'
#' @param site A [met_site()] or [met_sites()].
#' @param variables Optional character vector to restrict which variables
#'   are returned; `NULL` (default) returns every variable present.
#' @param from,to Optional UTC POSIXct bounds.
#' @param as_of Optional UTC POSIXct instant: reproduces the value the store
#'   would have served at that point in time (Plan 03's revision policy),
#'   for reproducible reports.
#' @return A canonical long observation tibble (see `new_obs()`).
#' @family read-api
#' @export
#' @examples
#' \dontrun{
#' met_record(site)
#' }
met_record <- function(site, variables = NULL, from = NULL, to = NULL, as_of = NULL) {
  sites <- as_met_sites(site)

  rows <- lapply(sites@sites, function(s) {
    store_read_obs(site_store_root(s), site_id(s), variables = variables,
                   from = from, to = to, as_of = as_of)
  })

  new_obs(vctrs::vec_rbind(!!!rows))
}

#' The archived forecast record
#'
#' Returns archived forecasts for `site` (SCOPING §4/§9): every issuance is
#' archived on sync, deduplicated by issue time. Per-member ensemble
#' trajectories are retrievable by default.
#'
#' @param site A [met_site()] or [met_sites()].
#' @param source Optional character vector to filter the adapter `source`.
#' @param issue_from,issue_to Optional UTC POSIXct bounds on `issue_time`.
#' @param valid_from,valid_to Optional UTC POSIXct bounds on `valid_time`.
#' @param members Logical; if `FALSE`, drop per-member rows (keeping only
#'   deterministic/`stat`-summary rows). Default `TRUE`: member trajectories
#'   are retrievable by default (SCOPING §4).
#' @return A canonical forecast tibble (see `new_forecast()`).
#' @family read-api
#' @export
#' @examples
#' \dontrun{
#' met_forecast_archive(site)
#' }
met_forecast_archive <- function(site, source = NULL, issue_from = NULL, issue_to = NULL,
                                 valid_from = NULL, valid_to = NULL, members = TRUE) {
  sites <- as_met_sites(site)

  rows <- lapply(sites@sites, function(s) {
    store_read_forecast(site_store_root(s), site_id(s), source = source,
                        issue_from = issue_from, issue_to = issue_to,
                        valid_from = valid_from, valid_to = valid_to, members = members)
  })

  new_forecast(vctrs::vec_rbind(!!!rows))
}

#' The stored verification report
#'
#' Returns the Plan 13 verification report (`verify_run()`'s output) for
#' `site`: out-of-sample scores per `(source, variable, lead_bucket, tier)`.
#'
#' @param site A [met_site()] or [met_sites()].
#' @param source Optional character vector to filter the `source` column.
#' @param ... Reserved for future filtering (variable, lead bucket); unused.
#' @return A tibble (see `verify_run()`); zero rows (typed) if no report has
#'   been written yet.
#' @family read-api
#' @export
#' @examples
#' \dontrun{
#' met_verification(site)
#' }
met_verification <- function(site, source = NULL, ...) {
  sites <- as_met_sites(site)

  rows <- lapply(sites@sites, function(s) {
    report <- read_verification_report(site_store_root(s), site_id(s))
    if (!is.null(source) && nrow(report) > 0) {
      report <- report[report$source %in% source, , drop = FALSE]
    }
    report
  })

  vctrs::vec_rbind(!!!rows)
}
