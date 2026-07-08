# Plan 06 — shared nearest-station resolution for source_silo()/source_ghcnh().
#
# Both adapters need to turn a site's (latitude, longitude) into the nearest
# entry (or entries) in an external station catalogue. The haversine formula
# is implemented directly (not via geosphere::distHaversine, which defaults
# to the WGS84 equatorial radius, 6378.137 km) because the frozen test
# (`tests/testthat/test-station-resolve.R`) computes its own reference
# haversine with R = 6371 km (the IUGG mean earth radius) and expects our
# `distance_km` to match within `tolerance = 0.05`; the two radii differ by
# enough to blow that tolerance. Implementing by hand keeps the constant
# explicit and matches the test exactly.

# Great-circle distance (km) between (lat1, lon1) and (lat2, lon2), all in
# decimal degrees. R = 6371 km, the mean earth radius (matches the frozen
# test's reference calculation). `a` is clamped into [0, 1] before
# asin(sqrt(.)) because floating point can push it a hair over 1 for
# near-antipodal points, which would otherwise NaN.
.haversine_km <- function(lat1, lon1, lat2, lon2) {
  r <- 6371
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat / 2)^2 +
    cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  a <- pmin(pmax(a, 0), 1)
  2 * r * asin(sqrt(a))
}

# Collapse a catalogue to one row per physical station before ranking.
# Requires `catalogue$distance_km` to already be populated: if `identity` is
# present, rows sharing the same `identity` collapse to the row with the
# SMALLEST distance_km in that group (not merely the first in input order —
# a catalogue is not guaranteed to be pre-sorted by proximity). If `identity`
# is absent, every row is its own identity (station_id stands in for it),
# i.e. no dedup happens.
.dedup_by_identity <- function(catalogue) {
  if (!"identity" %in% names(catalogue)) {
    return(catalogue)
  }
  ord <- order(catalogue$identity, catalogue$distance_km)
  catalogue <- catalogue[ord, , drop = FALSE]
  keep <- !duplicated(catalogue$identity)
  catalogue[keep, , drop = FALSE]
}

#' Find the nearest stations in a catalogue to a point
#'
#' Computes the great-circle (haversine) distance from `(site_lat, site_lon)`
#' to every row of `catalogue`, deduplicates by physical station identity
#' (see Details), and returns the `n` nearest, ascending by distance.
#'
#' If `catalogue` has an `identity` column, rows sharing the same `identity`
#' value are treated as the same physical station (e.g. a station served via
#' both a real-time transport and GHCNh, under two different `station_id`s)
#' and collapsed to a single row — the nearest of the group — before the `n`
#' nearest are selected. This is what the gap-fill donor ladder (Plan 10)
#' relies on to avoid double-counting a station reached by two routes. When
#' `catalogue` has no `identity` column, no deduplication happens (every row
#' is its own station).
#'
#' @param site_lat,site_lon Single numeric degrees, the reference point.
#' @param catalogue A data frame with (at least) `station_id`, `latitude`,
#'   `longitude` columns, and optionally `identity` (see Details).
#' @param n Integer, how many nearest stations to return. If `catalogue` (after
#'   dedup) has fewer than `n` rows, all of them are returned.
#'
#' @return A data frame with the original `catalogue` columns plus
#'   `distance_km`, the `n` (or fewer) nearest rows ordered ascending by
#'   `distance_km`.
#' @family station-resolution
#' @export
#' @examples
#' cat <- data.frame(
#'   station_id = c("a", "b"),
#'   latitude = c(-34.76, -35.50),
#'   longitude = c(148.21, 149.00)
#' )
#' nearest_stations(-34.75, 148.20, cat, n = 1)
nearest_stations <- function(site_lat, site_lon, catalogue, n = 1) {
  catalogue$distance_km <- .haversine_km(
    site_lat, site_lon, catalogue$latitude, catalogue$longitude
  )
  # Dedup AFTER distance is computed, so a group's kept representative is
  # genuinely its nearest member, not just whichever row the caller happened
  # to list first.
  catalogue <- .dedup_by_identity(catalogue)
  ordered <- catalogue[order(catalogue$distance_km), , drop = FALSE]
  n <- min(n, nrow(ordered))
  ordered[seq_len(n), , drop = FALSE]
}
