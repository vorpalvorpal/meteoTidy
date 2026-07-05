# Helpers for Plan 15 — classed-tibble (met_table) builders.

# A small wide §3.1-style tibble with a UTC `time` column and two value columns.
make_wide_tbl <- function(n = 3) {
  tibble::tibble(
    time = as.POSIXct("2026-01-01 00:00", tz = "UTC") + (seq_len(n) - 1L) * 3600,
    temperature_2m = seq(15, by = 0.5, length.out = n),
    wind_speed_10m = seq(3, by = 0.1, length.out = n)
  )
}

# A matching per-variable provenance table.
make_provenance <- function(variables = c("temperature_2m", "wind_speed_10m"),
                            tier = "qmap", source = "openmeteo",
                            train_overlap = 24) {
  tibble::tibble(
    variable = variables,
    tier = tier,
    train_overlap = train_overlap,
    source = source
  )
}

# A complete, valid met_table for the class/dplyr/hash tests.
make_met_table <- function(x = make_wide_tbl(),
                           provenance = make_provenance(),
                           keys = list(site_id = "test",
                                       from = as.POSIXct("2026-01-01", tz = "UTC"),
                                       to = as.POSIXct("2026-01-02", tz = "UTC")),
                           versions = list(schema_version = "1.0.0",
                                           calibration_manifest_version = 3L)) {
  new_met_table(x, provenance = provenance, keys = keys, versions = versions)
}
