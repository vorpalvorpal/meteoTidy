# Helpers for Plan 10 — gap-fill / transfer / aggregation builders.

# A canonical hourly series with a gap expressed as `missing`-flagged NA rows at
# the given (1-based) positions — the fill tiers' input shape.
series_with_gap <- function(variable, value, gap_at = integer(),
                            start = as.POSIXct("2026-01-01 00:00", tz = "UTC"),
                            site_id = "test", source = "test_src") {
  s <- make_obs(n = length(value), variable = variable, value = value,
                start = start, site_id = site_id, source = source)
  if (length(gap_at)) {
    s$value[gap_at] <- NA_real_
    s$qc_flag[gap_at] <- "missing"
    s$method[gap_at] <- "measured"
  }
  s
}

# Two realised series over a shared window with a known constant offset, for the
# transfer-engine tests (source = target + offset).
transfer_pair <- function(n = 200, offset = 2, seed = 1) {
  withr::local_seed(seed)
  target <- rnorm(n, mean = 15, sd = 3)
  source <- target + offset
  list(
    source = make_obs(n = n, variable = "temperature_2m", value = source,
                      source = "donor"),
    target = make_obs(n = n, variable = "temperature_2m", value = target,
                      source = "site")
  )
}

# A named list of candidate donors (each a small metadata list) for the ladder.
donor_catalogue <- function() {
  list(
    bom   = list(source = "bom_obs", identity = "072150", distance_km = 3),
    ghcnh = list(source = "ghcnh",   identity = "072150", distance_km = 3),
    era5  = list(source = "openmeteo", identity = "grid",  distance_km = 0),
    silo  = list(source = "silo",    identity = "silo_grid", distance_km = 0)
  )
}

# A 10-minute native-resolution series (6 samples/hour) for aggregation tests.
native_10min <- function(variable, value_per_slot,
                         start = as.POSIXct("2026-01-01 00:00", tz = "UTC")) {
  n <- length(value_per_slot)
  tibble::tibble(
    site_id = "test",
    datetime_utc = start + (seq_len(n) - 1L) * 600,   # 600s = 10 min
    variable = variable,
    value = as.double(value_per_slot),
    source = "test_src",
    method = "measured",
    qc_flag = "ok"
  )
}
