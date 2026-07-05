# Plan 03 — Storage layer

## Objective

Implement the on-disk store: hive-partitioned Parquet datasets for observations
and forecasts, the read/write functions with partition pruning, the watermark
mechanism, the **observation-revision policy** (re-fetch window +
supersede-not-overwrite), and the calibration store (manifest + coefficient
tables persisted *as data*, never `.rds`). Optionally expose a DuckDB read path
behind the same read API.

## Scope

**In:**
- Directory layout + dataset open/write helpers over `arrow`.
- `store_write_obs()`, `store_read_obs()`, `store_write_forecast()`,
  `store_read_forecast()`, `store_read_forecast_aux()`.
- Watermarks: `store_get_watermark()`, `store_set_watermark()`.
- Revision handling: supersede logic + version columns.
- Calibration store: `calib_write()`, `calib_read()`, `calib_manifest()`.
- Partition compaction: `store_compact()` (SCOPING §8 — run monthly by Plan 16's
  `met_refit()`).
- Optional `store_connect()` returning a DuckDB/arrow connection (experimental).

**Out:**
- Deciding *what* to write (adapters/curation/correction — later plans). This
  plan is pure IO + bookkeeping given already-canonical tables (Plan 01).
- The public tibble read API (`met_history()` etc., Plan 14) — that wraps these
  internal `store_*` functions.

## Prerequisites

Plans 00, 01, 02.

## Background

SCOPING §8 (Parquet layout, arrow default, optional DuckDB, monthly compaction),
§9 (watermarks, idempotency, **observation-revision re-fetch window +
supersede-not-overwrite** from the review), §7.1 (calibrations persisted **as
data**, versioned, manifest-tracked — the review’s anti-`.rds` fix).

## File layout

```
R/store.R             # layout constants, dataset path builders, open/write core
R/store-obs.R         # observation read/write + revision/supersede logic
R/store-forecast.R    # forecast + forecast_aux read/write
R/store-watermark.R   # watermark get/set
R/store-calib.R       # calibration manifest + coefficient-table IO
R/store-connect.R     # optional DuckDB/arrow connection (Suggests: duckdb)
tests/testthat/helper-store.R     # local_store() building a temp store
tests/testthat/test-store-obs.R
tests/testthat/test-store-forecast.R
tests/testthat/test-store-watermark.R
tests/testthat/test-store-revision.R
tests/testthat/test-store-calib.R
tests/testthat/test-store-compact.R
tests/testthat/test-store-connect.R
```

Add `arrow` to `Imports`; `duckdb`, `jsonlite` to relevant places (`jsonlite`
Imports for the manifest; `duckdb` Suggests).

## Detailed design

### Layout (`R/store.R`)

One directory tree per deployment, rooted at each site’s `store_root` (Plan 02).
Because `store_root` is per-site, the `site_id=` partition in SCOPING §8 is
redundant *within* a single-site root but retained so multiple sites can share a
root and so a dataset is self-describing. Support both: the path builder takes
`store_root` and always writes `site_id=<id>` under it.

```
<store_root>/observations/site_id=<id>/year=<yyyy>/part-*.parquet
<store_root>/forecasts/source=<src>/site_id=<id>/issue_date=<yyyy-mm-dd>/part-*.parquet
<store_root>/forecast_aux/source=<src>/site_id=<id>/issue_date=<yyyy-mm-dd>/part-*.parquet
<store_root>/calibrations/site_id=<id>/manifest.json
<store_root>/calibrations/site_id=<id>/<variable>-<source>-v<ver>.parquet
<store_root>/watermarks/site_id=<id>/watermarks.json
```

- `dataset_path(store_root, table, ...)` builds and (on write) creates the
  partition directory.
- All datetimes are UTC; partition columns (`year`, `issue_date`) are **derived
  from** the UTC timestamps at write time and validated to agree on read (guard
  against a caller hand-setting a wrong partition).

### Observation IO + revision policy (`R/store-obs.R`)

Add two bookkeeping columns to the stored observation schema (beyond Plan 01’s
canonical columns):

- `ingested_at` — UTC POSIXct, when this row was written (`= now`, injected).
- `superseded` — logical; `FALSE` for the current value, `TRUE` for a prior value
  kept for audit. The *current truth* is `superseded == FALSE`.

`store_write_obs(store_root, obs, now = .now(), mode = c("append","supersede"))`:
- Validates `obs` with `new_obs()` (Plan 01) first.
- `mode = "append"` — plain append (used for brand-new windows).
- `mode = "supersede"` — the revision path (SCOPING §9): for each incoming key
  `(site_id, datetime_utc, variable, source)` that already exists with a
  **different `value`/`qc_flag`/`method`**, mark the existing current row
  `superseded = TRUE` and append the incoming row with `superseded = FALSE` and
  fresh `ingested_at`. If the incoming value is **identical**, write nothing
  (idempotency — re-running a sync must not duplicate rows). Return, invisibly, a
  small summary `(n_new, n_superseded, n_unchanged)`.
- Because Parquet files are immutable, “marking superseded” means rewriting the
  affected partition file(s) — implement by reading the affected `year`
  partition, updating in memory, and rewriting that partition atomically
  (write to a temp path in the same dir, then rename). Only touch partitions that
  contain affected keys.

`store_read_obs(store_root, site_id, variables = NULL, from = NULL, to = NULL,
include_superseded = FALSE, as_of = NULL)`:
- Opens the arrow dataset, **pushes down** the `year` partition filter derived
  from `[from, to]` and the `variable`/`site_id` filters, collects a tibble.
- Default returns only current rows (`superseded == FALSE`).
- `as_of` (a UTC instant) reconstructs *what the store would have served at that
  time*: return the row for each key with the greatest `ingested_at <= as_of`
  (this is what makes regulator-facing reports reproducible — SCOPING §9/§3.2).
- Returns a `new_obs()`-valid tibble; passes `expect_canonical_obs()`.

### Forecast IO (`R/store-forecast.R`)

`store_write_forecast(store_root, fc, now = .now())` — validate with
`new_forecast()`, partition by `source` and `issue_date` (= `as.Date(issue_time)`
in UTC). **Deduplicate on `(source, model, issue_time)`** (SCOPING §9): if an
issuance is already archived, writing it again is a no-op (idempotent archiving).
Ensemble members and deterministic rows share the dataset (one schema). Mirror
for `store_write_forecast_aux()`.

`store_read_forecast(store_root, site_id, source = NULL, issue_from = NULL,
issue_to = NULL, valid_from = NULL, valid_to = NULL, members = TRUE)` — partition
pruning on `source`/`issue_date`; `members = FALSE` drops per-member rows and
returns only `stat`-summary rows. **Per-member trajectories always remain
retrievable** (SCOPING §4) — the default keeps them.

### Watermarks (`R/store-watermark.R`)

A watermark records, per `(site_id, table, source)`, the UTC instant through
which data has been processed. Stored as one JSON file per site.

- `store_get_watermark(store_root, site_id, table, source)` → POSIXct or `NA`.
- `store_set_watermark(store_root, site_id, table, source, t)` — write/replace.
- `store_effective_fetch_window(store_root, site_id, table, source, refetch =
  <duration>)` → the `[from, to]` a sync should fetch: `from = watermark -
  refetch` (the **re-fetch window** for revisions, SCOPING §9), `to = now`.
  Default `refetch` per source is a parameter later plans pass (SILO months,
  BOM/GHCNh days). If watermark is `NA`, `from = NULL` (full history).

### Calibration store (`R/store-calib.R`)

**No `.rds`.** A fitted calibration is persisted as (a) one row in the manifest
and (b) a coefficient/mapping **Parquet** table whose columns depend on tier:

- `calib_manifest(store_root, site_id)` → tibble of
  `(variable, source, version, tier, fit_date, train_start, train_end, n_pairs,
  lead_bucket, path)`.
- `calib_write(store_root, site_id, variable, source, tier, coeffs, meta, now)`
  — bumps `version`, writes `<variable>-<source>-v<ver>.parquet`, appends the
  manifest row atomically. `coeffs` is a tidy tibble the tier defines (Plan 12):
  e.g. mean-bias = harmonic coefficients; qmap = the (source_quantile,
  target_quantile) mapping table; emos = regression coefficients per lead bucket.
- `calib_read(store_root, site_id, variable, source, version = "current")` →
  the coefficient tibble + its manifest row. `"current"` = highest version.

This plan does not fit anything; it defines the storage contract Plan 12 writes
into and Plan 11/12 read from.

### Partition compaction (`R/store.R`)

Incremental syncs leave many small Parquet files per partition. `store_compact(
store_root, tables = c("observations","forecasts","forecast_aux"))` rewrites
each partition that contains more than one file into a single file, atomically
(write to a temp path in the partition dir, then swap). It must not change the
readable rows in any way — current, superseded, and `as_of` reads are identical
before and after (that invariant is the test). Called monthly by `met_refit()`
(Plan 16); never runs implicitly.

### Optional DuckDB (`R/store-connect.R`)

`store_connect(store_root, backend = c("arrow","duckdb"))` returns a connection
/ `dbplyr` source over the same Parquet tree (SCOPING §8, §11 — experimental).
Guard `duckdb` with `rlang::check_installed()`. Read-only; it must see exactly
the rows `store_read_*` sees (same current-vs-superseded default via a view).

## Test requirements

### `helper-store.R`
- `local_store(env = parent.frame())` — creates a temp `store_root` under
  `withr::local_tempdir()` and returns it; auto-cleaned.
- Reuse Plan 01 builders for canonical inputs.

### `test-store-obs.R`
- Write then read returns the same rows (round-trip), passing
  `expect_canonical_obs()`.
- Partition pruning: a `from/to` spanning one year reads only that year’s data
  (assert via the returned rows; optionally assert files opened if feasible).
- Writing the **same** obs twice in `supersede` mode is idempotent: row count
  unchanged, summary reports `n_unchanged` = all (SCOPING §9 idempotency).

### `test-store-revision.R` (directly tests the review fix)
- Write v1 of a value; write a **changed** value for the same key in `supersede`
  mode. Then:
  - default read returns only the new value (one current row).
  - `include_superseded = TRUE` returns both.
  - `as_of = <between the two ingests>` returns the **old** value — proving
    point-in-time reproducibility for audit (SCOPING §3.2/§9).
- An unchanged re-write does not create a superseded row.

### `test-store-forecast.R`
- Deterministic + ensemble-member + `stat`-summary rows write to one dataset and
  read back valid.
- Re-archiving the same `(source, model, issue_time)` is a no-op (dedup).
- `members = FALSE` returns only summary rows; members remain retrievable by
  default.
- Partition pruning on `source` and `issue_date`.

### `test-store-watermark.R`
- get after set returns the set instant; unset returns `NA`.
- `store_effective_fetch_window()` yields `from = watermark - refetch`; with no
  watermark yields `from = NULL`.

### `test-store-calib.R`
- `calib_write()` twice bumps version; `calib_read(version = "current")` returns
  the second; manifest has two rows with monotonic versions.
- Coefficients round-trip through Parquet exactly (no `.rds`; assert file
  extension is `.parquet` and no `.rds` is created anywhere under `calibrations/`).

### `test-store-compact.R`
- Write a partition in several small appends (multiple files), `store_compact()`,
  then assert: file count per partition is 1, and `store_read_obs()` (default,
  `include_superseded = TRUE`, and an `as_of` read) returns exactly the same rows
  as before compaction.
- Compacting an already-compacted store is a no-op (idempotent).

### `test-store-connect.R`
- `skip_if_not_installed("duckdb")`; the DuckDB path returns the same current
  rows as `store_read_obs()` for a small fixture (parity test).

## Definition of done

Shared skeleton plus:
- All `store_*` and `calib_*` functions exist; those the public API needs are
  documented (keep most internal — Plan 14 exports the public surface).
- The revision/supersede + `as_of` semantics are proven by `test-store-revision.R`.
- No `.rds` anywhere in the calibration path (grep test).
- New condition classes registered in `meteo_conditions()`.
