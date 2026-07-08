# Helpers for Plan 09 — QC engine builders.
#
# QC rules operate on canonical *plain* tibbles (as read back from the store) and
# may only DOWNGRADE qc_flag. These builders return plain make_obs() tibbles so a
# rule test reads as "series-in → flag-out"; they are never new_obs()-validated
# (the rule under test is what decides the flag).

# A single-variable hourly series with explicit values, all initially flagged ok.
qc_series <- function(variable, value,
                      start = as.POSIXct("2026-01-01 00:00", tz = "UTC"),
                      site_id = "test", source = "test_src",
                      method = "measured") {
  # nolint next: object_usage_linter. sibling helper
  make_obs(n = length(value), variable = variable, value = value,
           start = start, site_id = site_id, source = source, method = method)
}

# A wide, single-timestamp cross-variable frame for the physics-constraints
# module (internal-consistency + enforce mode). Columns are variable names.
qc_wide_row <- function(...) {
  tibble::tibble(
    site_id = "test",
    datetime_utc = as.POSIXct("2026-01-01 00:00", tz = "UTC"),
    ...
  )
}

# Steady donor stations for the spatial/buddy check: `n` hourly steps at a fixed
# offset from a nominal site value, one tibble per donor id.
qc_donor <- function(donor_id, value, variable = "temperature_2m",
                     start = as.POSIXct("2026-01-01 00:00", tz = "UTC")) {
  qc_series(variable, value, start = start, site_id = donor_id,
            source = donor_id)
}
