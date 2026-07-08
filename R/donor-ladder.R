# Plan 10 -- the donor ladder: rank candidate donor stations for a gap-fill,
# deduplicated by physical station identity (the review fix -- SCOPING
# section 6).
#
# `available` (see `donor_catalogue()` in tests/testthat/helper-fill.R) is a
# named list, each element `list(source=, identity=, distance_km=)`: a
# candidate donor transport, not yet deduplicated or ordered. Two entries can
# describe the SAME physical station reached via two different transports
# (e.g. a BOM real-time feed and GHCNh both serving station "072150") -- the
# ladder must collapse those to one donor, keeping the higher-PRIORITY
# transport, not just whichever happens to sort first by distance.
#
# Reuses `.dedup_by_identity()` (R/station-resolve.R, Plan 06) directly
# rather than reimplementing dedup, per that function's own documented
# purpose ("This is what the gap-fill donor ladder (Plan 10) relies on to
# avoid double-counting a station reached by two routes"). `.dedup_by_identity()`
# keeps, per identity group, the row with the SMALLEST `distance_km` --
# ties are broken by `order()`'s stability, keeping whichever row sorts
# first in the input. So when two donors share both `identity` AND
# `distance_km` (the same physical station reached by two transports at
# identical distance, as in test-donor-ladder.R's fixture), we must sort
# candidates by `(priority, distance_km)` BEFORE calling
# `.dedup_by_identity()`, so the higher-priority transport sorts first and is
# what dedup keeps on a same-distance tie.

# Ladder priority order (SCOPING section 6): BOM real-time station -> GHCNh
# station -> ERA5 (Open-Meteo reanalysis) -> SILO-disaggregated. Lower number
# = higher priority = ranked first.
.donor_priority <- function(source) {
  priority <- c(bom_obs = 1L, ghcnh = 2L, openmeteo = 3L, silo = 4L)
  out <- priority[source]
  out[is.na(out)] <- max(priority) + 1L # unrecognised sources sort last
  unname(out)
}

#' Rank and deduplicate candidate donors for a gap-fill
#'
#' Builds an ordered, deduplicated donor list from `available` (a named list
#' of candidate donor transports, each `list(source=, identity=,
#' distance_km=)`): sorts candidates by `(priority, distance_km)` ascending,
#' where priority follows the documented ladder order (BOM -> GHCNh -> ERA5/
#' Open-Meteo -> SILO), then deduplicates by physical station `identity` via
#' `.dedup_by_identity()` (Plan 06) so a station reached by two transports
#' (e.g. BOM and GHCNh serving the same physical station) appears exactly
#' once -- as its higher-priority transport, even when both routes report the
#' identical distance.
#'
#' @param site A `met_site` object (reserved for future filtering, e.g.
#'   excluding donors with no coverage in `window`; not used to filter in
#'   this plan's tested scope).
#' @param variable A dictionary variable name (reserved for future
#'   variable-specific donor filtering, e.g. `cloud_cover` preferring METAR
#'   donors -- not used to filter beyond ordering in this plan's tested
#'   scope).
#' @param window A list with `from`/`to` POSIXct bounds (reserved for future
#'   filtering on donor coverage; not used to filter in this plan's tested
#'   scope).
#' @param available A named list of candidate donors, each
#'   `list(source=, identity=, distance_km=)` (see `donor_catalogue()` in the
#'   test helpers).
#' @return A list of donor metadata (same element shape as `available`),
#'   ordered by ladder priority then distance, deduplicated by `identity`.
#' @keywords internal
#' @noRd
rank_donors <- function(site, variable, window, available) {
  if (length(available) == 0) {
    return(list())
  }

  candidates <- tibble::tibble(
    key = names(available) %||% as.character(seq_along(available)),
    source = vapply(available, function(d) d$source, character(1)),
    identity = vapply(available, function(d) d$identity %||% NA_character_, character(1)),
    distance_km = vapply(available, function(d) d$distance_km %||% NA_real_, double(1))
  )
  candidates$priority <- .donor_priority(candidates$source)

  ord <- order(candidates$priority, candidates$distance_km)
  candidates <- candidates[ord, , drop = FALSE]

  deduped <- .dedup_by_identity(candidates)

  lapply(seq_len(nrow(deduped)), function(i) {
    available[[deduped$key[i]]]
  })
}
