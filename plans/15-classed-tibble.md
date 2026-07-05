# Plan 15 — Classed-tibble contract + wide emitter

## Objective

Implement the meteoHazard interface: the wide, Open-Meteo-named §3.1 hourly table
emitted as a **classed tibble** carrying validated provenance metadata, with
`dplyr` reconstruction methods that keep the class alive through wrangling and
**downgrade visibly** when metadata is invalidated — plus **per-column content
hashing** so the honestly-scoped guarantee (metadata authoritative only at
validated boundaries) is enforceable, not aspirational (the review fix).

## Scope

**In:**
- The classed tibble `met_table` (S3, extends `tbl_df`) + constructor/validator.
- Per-variable provenance attribute (correction tier, training-overlap length,
  source), site/window keys, schema + calibration-manifest versions.
- `dplyr_reconstruct()` / `dplyr_row_slice()` / `dplyr_col_modify()` methods; the
  visible-downgrade behaviour; content hashing.
- `met_wide()` — the §3.1 wide emitter (with the `time` rename and unit pinning).
- The meteoHazard **dual-accept** boundary validator (`met_ingest()`).

**Out:**
- Changing meteoHazard itself (it takes the dependency or a shim — out of this
  repo). This plan provides the class and emitter meteoHazard consumes.
- Any storage/statistics (earlier plans).

## Prerequisites

Plans 01 (schemas/units), 03 (provenance columns), 14 (read API — `met_wide`
builds on `met_record`/`met_forecast_archive`).

## Background

SCOPING §3.2 (classed tibble extending `tbl_df`, the sf/tsibble pattern — **not**
an opaque S7 wrapper; carries per-variable provenance + keys + versions;
`dplyr_reconstruct` keeps class/attributes; invalidating operations **downgrade
visibly**; **`dplyr_reconstruct` cannot detect in-place column mutation** →
metadata authoritative only at validated boundaries, **content hashes** make
staleness detectable; meteoHazard **dual-accepts** — classed input trusted after
one boundary validation, plain tibble validated on entry; concrete uses: single-
provenance-class derived indices, uncertainty display, audit trail), §3.1 (the
exact wide column set + **units pinned**, `time` in UTC), §10 (the one-call
meteoHazard interface).

## File layout

```
R/met-table.R             # new_met_table(), validator, print/format, attribute accessors
R/met-table-dplyr.R       # dplyr_reconstruct / row_slice / col_modify + downgrade logic
R/met-table-hash.R        # per-column content hashing + staleness detection
R/met-wide.R              # met_wide() §3.1 emitter
R/met-ingest.R            # dual-accept boundary validator for consumers
tests/testthat/test-met-table.R
tests/testthat/test-met-table-dplyr.R
tests/testthat/test-met-table-hash.R
tests/testthat/test-met-wide.R
tests/testthat/test-met-ingest.R
```

Add `vctrs`, `pillar` (nice printing) if not already; `digest` (or `rlang::hash`)
for content hashing.

## Detailed design

### `met_table` (`R/met-table.R`)

An S3 subclass of `tbl_df` (so dplyr/ggplot2 work untouched — SCOPING §3.2).
`new_met_table(x, provenance, keys, versions)`:

- `x` — the underlying wide tibble (§3.1 columns).
- `provenance` — a per-variable table: `(variable, tier, train_overlap, source)`.
- `keys` — `site_id` + the window (`from`, `to`).
- `versions` — `schema_version` + `calibration_manifest_version`.
- Stored as attributes; a validator checks provenance covers every value column
  and that versions are present.
- `print`/`format` show a compact provenance banner (tiers per column) so the
  metadata is visible, not hidden.
- Accessors: `met_provenance(x)`, `met_keys(x)`, `met_versions(x)`.

### Content hashing (`R/met-table-hash.R`) — the review fix

`dplyr_reconstruct` **cannot** detect `mutate(temperature_2m = temperature_2m +
1)` — the class and per-variable provenance survive while the values silently
change (SCOPING §3.2). To make the guarantee real:

- On construction and at every validation boundary, compute a **per-column content
  hash** and store it in the attribute.
- `met_validate_boundary(x)` recomputes hashes; a mismatch means a value column
  was mutated in place since the metadata was set → **downgrade the affected
  column’s provenance to `unverified`** (visible), not silently trust it.
- This is what lets Plan 14’s consumers rely on “metadata authoritative **at the
  boundary**”, not through arbitrary wrangling.

### dplyr methods (`R/met-table-dplyr.R`)

- `dplyr_reconstruct.met_table(data, template)` — reattach class + attributes when
  the operation preserves them (column-preserving verbs: `filter`, `arrange`,
  `slice`, `mutate` that doesn’t touch value columns).
- `dplyr_col_modify` / `dplyr_row_slice` methods likewise.
- **Visible downgrade** (SCOPING §3.2): operations that genuinely invalidate the
  metadata (dropping a value column, `bind_rows` mixing incompatible provenance, a
  join that changes keys) **downgrade the object to a plain tibble with a warning**
  (`warn_meteo(class = "met_table_downgraded")`) rather than silently carrying
  stale attributes — the failure mode of a plain attribute-carrying tibble.

### Wide emitter (`R/met-wide.R`)

`met_wide(site, window, kind = c("forecast","record"), variables = <§3.1 set>,
now = .now())` — the **one-call meteoHazard interface** (SCOPING §10):

- `kind = "forecast"` → corrected forecast (Plan 12) for prediction; `kind =
  "record"` → curated record (Plans 09–10) for hindcast.
- Returns the §3.1 wide table: exactly the columns in SCOPING §3.1, **units
  pinned** (canonical), `datetime_utc` renamed to **`time`** at this outer
  boundary only, wrapped as a `met_table` with provenance/keys/versions.
- Missing variables still appear as all-`NA` columns (stable §3.1 shape, Plan 01
  `widen_obs`).
- The model-only subset carries tier `raw` (or the experimental marker) in
  provenance; `cloud_cover`/radiation carry their partial-anchoring tier
  (SCOPING §3.1).

### Dual-accept boundary (`R/met-ingest.R`)

`met_ingest(x)` — the validator a consumer (meteoHazard) calls once at its
boundary (SCOPING §3.2):

- If `x` is a `met_table`: run `met_validate_boundary()` (hash check) **once**;
  trust the provenance thereafter (no defensive re-checking downstream); enforce
  provenance rules (e.g. **warn when a derived index would mix correction tiers** —
  the single-provenance-class rule, SCOPING §3.2 use #1).
- If `x` is a plain tibble: validate the §3.1 schema on entry and treat provenance
  as **unverified**.
- Provide `met_assert_single_tier(x, variables)` implementing the enforce-able
  “each derived quantity computed within one provenance class” rule (warn/abort on
  mixing corrected + uncorrected inputs to one derived index).

## Test requirements

### `test-met-table.R`
- `new_met_table()` builds a valid classed tibble; dplyr verbs (`filter`,
  `arrange`, `select` of value cols) preserve the class and provenance.
- `print` shows the provenance banner (snapshot).
- Validator rejects a table whose provenance misses a value column.

### `test-met-table-dplyr.R`
- A metadata-preserving `mutate` (new derived column, value cols untouched) keeps
  the class.
- An invalidating op (dropping a value column / incompatible `bind_rows`)
  **downgrades to a plain tibble with `met_table_downgraded` warning** — proving
  visible, not silent, degradation.

### `test-met-table-hash.R` (the review fix)
- In-place mutation of a value column (`mutate(temperature_2m = temperature_2m +
  1)`) is **detected at the boundary**: `met_validate_boundary()` marks that
  column’s provenance `unverified` (the honest-scoping guarantee).
- An untouched table passes the boundary with provenance intact.

### `test-met-wide.R`
- Output has exactly the §3.1 columns, `time` (UTC) not `datetime_utc`, canonical
  units (assert wind is m/s — the §3.1 pinning), and is a `met_table`.
- Missing variables appear as all-`NA` columns.
- `kind = "forecast"` routes through corrected forecast; `kind = "record"` through
  the curated record (mock the sources; assert the routing).

### `test-met-ingest.R`
- A classed input is validated **once** and trusted; a plain tibble is validated
  and marked unverified.
- `met_assert_single_tier()` warns when a derived index mixes a corrected 10 m
  wind with a raw 80 m wind (SCOPING §3.2 use #1 — the physically-impossible-shear
  guard), and is silent when all inputs share a tier.

## Definition of done

Shared skeleton plus:
- `met_wide()`, `met_ingest()`, `new_met_table()` and accessors exported/
  documented as the meteoHazard interface; the dplyr methods registered.
- Content hashing detects in-place mutation at the boundary (the review fix),
  proven by test; invalidating ops downgrade visibly.
- The §3.1 wide contract (names + units + `time`) is emitted exactly and tested.
- New condition classes registered in `meteo_conditions()`.
