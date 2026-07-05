# Plan 06 — Acquisition: SILO & GHCNh adapters

## Objective

Implement `source_silo()` (daily Australian series via `weatherOz`) and
`source_ghcnh()` (official-quality hourly obs via `worldmet` ≥ 1.0.0,
`import_ghcn_hourly()`). Both are
observation adapters; both also implement `resolve_station()` to populate the
site’s resolved-ID cache. SILO’s per-value quality codes must be **carried into
provenance** (review fix), and SILO’s retrospective revisions are handled by the
Plan 03 re-fetch window.

## Scope

**In:**
- `source_silo()` over `weatherOz` (≥ 3.0.0) PatchedPoint / DataDrill (daily).
- `source_ghcnh()` over `worldmet` (≥ 1.0.0) `import_ghcn_hourly()` (hourly).
- `resolve_station()` for both (SILO grid/station; nearest GHCNh station).
- SILO quality-code → `(method, qc_flag)` mapping.

**Out:**
- The per-site donor-coverage *audit* (Plan 16 `met_backfill()`); here we provide
  the resolution + a `station_coverage()` helper it calls.
- Disaggregation of SILO daily → hourly (that is a fill concern, Plan 10).

## Prerequisites

Plans 00–04.

## Background

SCOPING §5 (SILO via weatherOz; DPIRD restructure in 3.0.0 → **pin min version**;
API key = email = PII, not a secret, §11; **ingest SILO source/quality codes into
provenance**; PatchedPoint revises retrospectively → Plan 03 re-fetch window;
GHCNh via worldmet, official-quality, **NCEI's official ISD replacement** — ISD
hourly feed frozen after 2025-08-24, ISD services retire 2026-07-31; **updated
daily but no published real-time latency figure** (treat as best-effort, up to
~a week); feeds `history_hourly` backfill + fallback ladder, not the live head),
§13 (GHCNh effective per-site latency and nearest-station completeness still
audited per site; GHCNh *does* include non-airport/cooperative stations).

## File layout

```
R/source-silo.R
R/source-ghcnh.R
R/silo-qcode.R              # SILO source/quality code → (method, qc_flag) lookup
R/station-resolve.R         # nearest-station helpers shared by silo/ghcnh (haversine)
tests/testthat/test-source-silo.R
tests/testthat/test-source-ghcnh.R
tests/testthat/test-silo-qcode.R
tests/testthat/test-station-resolve.R
tests/testthat/_fixtures/silo/*.rds|csv     # recorded weatherOz return frames
tests/testthat/_fixtures/ghcnh/*.rds        # recorded worldmet return frames
```

Add `weatherOz (>= 3.0.0)` and `worldmet (>= 1.0.0)` to `Imports` (GHCNh support
landed in worldmet 1.0.0; current CRAN is 1.1.0. Use `Imports`, not `Suggests` —
they are core to the Australian use case). Add `geosphere` (or a hand-rolled
haversine) for nearest-station distance.

## Detailed design

### SILO (`R/source-silo.R`)

`source_silo(api_key_env, dataset = c("patched_point","data_drill"),
source_id = "silo")`:

- The SILO “API key” is an **email address**, read from the env var
  `api_key_env` at fetch time; it is **PII, not a secret** — keep it out of
  committed config, but an env var suffices, and it may appear in `weatherOz`
  calls (SCOPING §11). Never write it into provenance/Parquet.
- `fetch()` calls the appropriate `weatherOz` function for the window/variables,
  then maps columns → dictionary variables with `to_canonical()`. SILO is daily,
  so `datetime_utc` is the daily-boundary instant in **local clock time**
  (the site’s IANA timezone, DST-inclusive) represented in UTC (SCOPING §3
  timezone rule) — document precisely which instant a SILO “day” maps to (the
  9am rainfall-day convention, which shifts by an hour under DST) and keep it
  consistent with Plan 10’s daily aggregation.
- **Quality codes → provenance:** each SILO value carries a source/quality code.
  Map it (via `R/silo-qcode.R`) to `method` and `qc_flag`: observed codes →
  `method = "measured"`, `qc_flag = "ok"`; interpolated/patched codes →
  `method = "imputed"` (or `"model_fill"` for grid-interpolated), `qc_flag = "ok"`;
  long-term-average-fallback codes → `qc_flag = "suspect"`. The lookup table is
  filled from SILO’s documented codes (cite the SILO API docs in a comment) and
  is exhaustive — an **unknown code aborts** class `"unknown_silo_code"` rather
  than silently defaulting (so a SILO schema change is caught).
- `resolve_station(site)`: PatchedPoint resolves the nearest BOM station number;
  DataDrill resolves the grid cell for the site lat/lon. Fills
  `site@resolved$silo`.

### GHCNh (`R/source-ghcnh.R`)

`source_ghcnh(source_id = "ghcnh")`:

- `resolve_station(site)`: find the **nearest** GHCNh station to the site (great-
  circle distance), store `station_id` + `distance_km` in `site@resolved$ghcnh`.
  Provide `n` nearest as an option (the fill ladder may want more than one donor).
- `fetch()` calls **`worldmet::import_ghcn_hourly(station, year, ...)`** (the
  GHCNh accessor; note the spelling is `import_ghcn_hourly`, not
  `import_ghcnh_*`) for the resolved station over the window, maps → canonical
  hourly obs. `source = source_id`, `method = "measured"`, `qc_flag` mapped from
  any worldmet quality field (default `"ok"`). GHCNh is **updated daily but
  publishes no real-time latency figure** — treat it as best-effort backfill (up
  to ~a week behind), not the live head (SCOPING §5.1/§13); expose this as
  `adapter@cadence` metadata so the pipeline (Plan 16) never expects GHCNh to
  fill the live window.
- `station_coverage(adapter, site, window)` → a small tibble of per-variable
  completeness for the nearest station(s), the input to Plan 16’s donor-coverage
  audit (SCOPING §13). No live probing here beyond the mocked fetch.

### Shared station resolution (`R/station-resolve.R`)

- `nearest_stations(site_lat, site_lon, catalogue, n = 1)` — haversine distance,
  returns the `n` nearest with distances. `catalogue` is the station list each
  adapter obtains (mock it in tests). Deduplicate by station identity (a station
  present under multiple ids/transports collapses to one) — this is also what
  Plan 10 relies on to avoid double-counting BOM/GHCNh overlap (SCOPING §6).

## Test requirements

All tests replay recorded `weatherOz`/`worldmet` return frames via
`local_mocked_bindings()` on the specific wrapper calls — **no live calls**.

### `test-source-silo.R`
- A recorded PatchedPoint frame → canonical daily obs; `expect_canonical_obs()`;
  units converted; the daily-boundary instant is the documented 9am
  local-clock-time mapping — include a DST-period date where the UTC instant
  shifts by an hour relative to winter.
- `resolve_station()` fills `site@resolved$silo` and returns a new site.
- The email is read from the named env var and never appears in the output rows.

### `test-silo-qcode.R`
- Observed / interpolated / long-term-average codes map to the documented
  `(method, qc_flag)`; an **unknown code aborts** `"unknown_silo_code"`.
- The mapping table covers every code in a committed reference list.

### `test-source-ghcnh.R`
- A recorded worldmet frame → canonical hourly obs; `method == "measured"`.
- `resolve_station()` picks the nearest station and records `distance_km`; asking
  for `n = 3` returns three distinct stations ordered by distance.
- `adapter@cadence` marks the lag so the pipeline won’t use GHCNh for the live
  head (assert the metadata).
- `station_coverage()` returns per-variable completeness for a fixture window.

### `test-station-resolve.R`
- `nearest_stations()` returns correct ordering for a hand-built catalogue with
  known coordinates (assert distances within tolerance of a reference haversine).
- Duplicate stations (same identity, two ids) collapse to one (the dedup the
  fill ladder needs).

## Definition of done

Shared skeleton plus:
- `source_silo()`, `source_ghcnh()`, `resolve_station()` methods, and
  `station_coverage()` exported/documented; min versions pinned in DESCRIPTION.
- SILO quality codes are provably carried into provenance; unknown codes fail loud.
- `adapters_for_site()` resolves `"silo"` and `"ghcnh"`.
- New condition classes registered in `meteo_conditions()`.
