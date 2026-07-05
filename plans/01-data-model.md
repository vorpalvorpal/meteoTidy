# Plan 01 — Canonical data model

## Objective

Define, in code, the fixed vocabulary the whole package is built on: the table
schemas, the variable dictionary (with units, ranges, and the two class axes),
the closed enumerations (QC flag, production method, correction tier), and the
`member`/`stat` rule. Provide constructors that *stamp and validate* a table into
canonical form, plus the schema-assertion test helpers every later plan reuses.

Nothing here does IO or statistics. This plan is pure structure and validation.

## Scope

**In:**
- The variable dictionary: built-ins + a user-extension API.
- Canonical table schemas + validating constructors + widen/narrow helpers.
- Enumerations: `qc_flag`, `method`, `tier`, `measurability_class`,
  `statistical_class`.
- Unit definitions per variable and a canonicalising converter.
- Test helpers `expect_canonical_obs()`, `expect_canonical_forecast()`, etc.

**Out:**
- Reading/writing to disk (Plan 03).
- The *classed* tibble with provenance attributes and `dplyr_reconstruct`
  (Plan 15). Here the tables are **plain tibbles** with validated columns.
- Any adapter or correction logic.

## Prerequisites

Plan 00.

## Background

SCOPING §3 (canonical data model, incl. the member/stat rule — *this plan*),
§3.1 (wide contract + units note — this plan implements the unit pinning),
§3.2 (the classed tibble — Plan 15, not here), §6/§7 (statistical &
measurability classes drive dispatch — this plan only *stores* them).

## File layout

```
R/dict.R              # variable dictionary + registration API
R/dict-builtin.R      # the built-in variable definitions (data)
R/enums.R             # qc_flag, method, tier, class enums + validators
R/units.R             # canonical-unit conversion helpers (uses `units` pkg)
R/schema-obs.R        # canonical observation table: constructor, validator, widen/narrow
R/schema-forecast.R   # forecast archive + forecast_aux constructors/validators
R/schema.R            # shared schema utilities (column spec type, assert_columns)
tests/testthat/helper-schema.R   # expect_canonical_*() + make_obs()/make_forecast()
tests/testthat/test-dict.R
tests/testthat/test-enums.R
tests/testthat/test-units.R
tests/testthat/test-schema-obs.R
tests/testthat/test-schema-forecast.R
```

Add `units`, `tibble`, `vctrs` to `Imports`.

## Detailed design

### Enumerations (`R/enums.R`)

Represent each closed enum as a character vector constant plus a validator. Use a
factory to avoid repetition.

- `QC_FLAG_LEVELS <- c("ok", "suspect", "fail", "missing")`
  **Note the deliberate removal of `estimated`** (SCOPING §3): production method
  is *not* a QC state.
- `METHOD_LEVELS <- c("measured", "aggregated", "donor_fill", "model_fill",
  "imputed", "disaggregated", "derived")` — how a value was produced. Extendable
  in a later plan only by editing this vector with a plan reference.
- `TIER_LEVELS <- c("raw", "physical", "mean_bias", "qmap", "emos")`
  (SCOPING §3.2/§7.1). Ordered: a helper `tier_rank()` returns the integer rank
  so “higher tier” comparisons are unambiguous.
- `STAT_CLASS_LEVELS <- c("linear", "circular", "bounded", "intermittent",
  "clear_sky_indexed")` (SCOPING §3, §6).
- `MEASURABILITY_LEVELS <- c("site_measurable", "derived_measurable",
  "donor_observable", "model_only")` (SCOPING §3).

For each: `validate_<enum>(x)` returns `x` invisibly or aborts with class
`"invalid_<enum>"` listing the offending values (via `abort_meteo`). Register
these classes in `meteo_conditions()`.

### Units (`R/units.R`)

- `canonical_unit(variable)` → the `units`-package unit string for a dictionary
  variable (looked up from the dictionary).
- `to_canonical(x, from, variable)` — given a numeric/`units` vector `x` currently
  in unit `from`, convert to the variable’s canonical unit. If `x` already carries
  units, honour them and ignore `from` (but warn if they disagree). Aborts with
  class `"bad_units"` when `from` is not convertible to canonical (e.g. someone
  maps a temperature column to `wind_speed_10m`).
- **The km/h footgun (SCOPING §3.1):** canonical wind unit is `m/s`. A helper
  test proves that a value supplied as `km/h` is converted, not passed through.

### Variable dictionary (`R/dict.R`, `R/dict-builtin.R`)

The dictionary is the single lookup table that QC, fill, and correction dispatch
on. Represent it as an environment-backed registry so users can extend it at
runtime (SCOPING §3), seeded with the built-ins.

Row fields per variable:

| field | type | notes |
|---|---|---|
| `variable` | chr | canonical internal name = the §3.1 Open-Meteo column name where one exists |
| `unit` | chr | canonical `units` string |
| `min`, `max` | dbl (canonical unit) | valid physical range; `NA` if unbounded on a side |
| `statistical_class` | enum | drives QC + correction dispatch |
| `measurability_class` | enum | drives the fill ladder (SCOPING §6) |
| `circular_period` | dbl or NA | 360 for direction, else NA |
| `description` | chr | one line |

API:
- `met_variables()` — return the current dictionary as a tibble (exported).
- `met_register_variable(variable, unit, min, max, statistical_class,
  measurability_class, circular_period = NA, description)` — validate and add/replace.
  Aborts (class `"duplicate_variable"`) on an attempt to *silently* redefine a
  built-in unless `overwrite = TRUE`. Validates enums and that `unit` parses.
- `met_variable(variable)` — single-row lookup; abort class `"unknown_variable"`
  if absent (message suggests `met_register_variable()`).
- Internal `dict_reset()` for tests to restore built-ins (used via a `withr`
  helper so tests never leak registrations).

**Built-in variables** (`R/dict-builtin.R`) — at minimum the §3.1 contract plus
the §3 risk-model variables. Fill `min`/`max` from a cited authority (WMO / BSRN
for radiation) in a comment; the values below are the *starting* ranges — the
implementer confirms them against the cited source:

| variable | unit | min | max | stat class | measurability |
|---|---|---|---|---|---|
| `temperature_2m` | degC | -50 | 60 | linear | site_measurable |
| `relative_humidity_2m` | % | 0 | 100 | bounded | site_measurable |
| `dewpoint_2m` | degC | -60 | 40 | linear | derived_measurable |
| `surface_pressure` | hPa | 700 | 1100 | linear | site_measurable |
| `pressure_msl` | hPa | 870 | 1085 | linear | derived_measurable |
| `precipitation` | mm | 0 | 500 | intermittent | site_measurable |
| `cloud_cover` | % | 0 | 100 | bounded | donor_observable |
| `direct_radiation` | W/m2 | 0 | 1400 | clear_sky_indexed | derived_measurable |
| `diffuse_radiation` | W/m2 | 0 | 1000 | clear_sky_indexed | derived_measurable |
| `wind_speed_10m` | m/s | 0 | 120 | linear | site_measurable |
| `wind_direction_10m` | degree | 0 | 360 | circular | site_measurable |
| `wind_gusts_10m` | m/s | 0 | 150 | linear | site_measurable |
| `wind_speed_80m` | m/s | 0 | 150 | linear | model_only |
| `wind_direction_80m` | degree | 0 | 360 | circular | model_only |
| `wind_speed_120m` | m/s | 0 | 150 | linear | model_only |
| `wind_direction_120m` | degree | 0 | 360 | circular | model_only |
| `wind_speed_180m` | m/s | 0 | 150 | linear | model_only |
| `wind_direction_180m` | degree | 0 | 360 | circular | model_only |
| `boundary_layer_height` | m | 0 | 5000 | linear | model_only |
| `soil_moisture_0_to_1cm` | m3/m3 | 0 | 1 | bounded | model_only |
| `soil_moisture_1_to_3cm` | m3/m3 | 0 | 1 | bounded | model_only |
| `cape` | J/kg | 0 | 8000 | linear | model_only |
| `uv_index` | 1 | 0 | 20 | bounded | model_only |

(The hub-height wind directions and the two layered soil-moisture variables
were added when the §3.1 contract was re-verified against meteoHazard's
sources, 2026-07-05 — `odour_hazard()` requires the soil layers and
`pressure_msl`; `ventilation_state()` optionally consumes the directions.)

Set `circular_period = 360` for **every `wind_direction_*` variable**. Record
in a comment that wind direction is corrected as joint u/v components, never
quantile-mapped as an angle (SCOPING §6) — Plan 12 enforces it; here it is
just flagged as circular.

### Canonical observation table (`R/schema-obs.R`)

Long format (SCOPING §3):

| column | type |
|---|---|
| `site_id` | chr |
| `datetime_utc` | POSIXct, tz UTC |
| `variable` | chr, must be in dictionary |
| `value` | dbl (canonical unit; stored unitless dbl, unit known via dictionary) |
| `source` | chr |
| `method` | chr in `METHOD_LEVELS` |
| `qc_flag` | chr in `QC_FLAG_LEVELS` |

Design decision to state in code comments: values are stored as **plain doubles
in canonical units**, not `units` vectors (Parquet has no units type). Units live
in the dictionary; `to_canonical()` is applied at ingest (Plan 04), and the
schema validator checks *range* against the dictionary, which is the units
contract in practice.

- `new_obs(df)` — coerce/validate an incoming data frame into the canonical
  observation tibble. Checks: all required columns present and typed; `variable`
  values all known; `qc_flag`/`method` in their enums; `datetime_utc` is UTC;
  values within `[min, max]` for their variable **or** flagged `fail`/`missing`
  (range violations on an `ok` row are a bug → abort class `"range_violation"`);
  key `(site_id, datetime_utc, variable, source)` is unique. Returns a tibble;
  aborts with a specific class per failure.
- `widen_obs(obs, variables = NULL)` — pivot to the wide, Open-Meteo-named hourly
  table (SCOPING §3.1). One row per `(site_id, datetime_utc)`; columns are the
  requested variables (default: the full §3.1 set). Missing variables become
  all-`NA` columns (so the wide contract shape is stable regardless of coverage).
  Column `datetime_utc` renamed to `time` only at the very outer boundary — keep
  it `datetime_utc` internally; Plan 15’s emitter does the `time` rename.
- `narrow_obs(wide, ...)` — inverse pivot back to long. `widen_obs |> narrow_obs`
  is identity on `(site_id, datetime_utc, variable, value)` (round-trip test).

### Forecast archive (`R/schema-forecast.R`)

`(site_id, source, model, issue_time, valid_time, lead_time, member, stat,
variable, value)` (SCOPING §3, revised member/stat rule):

- `model` nullable chr (BOM edited product has none); for spliced seasonal
  products it records the **underlying** model (`ec46`/`seas5`), never the
  spliced name (SCOPING §5.2).
- `issue_time`, `valid_time` UTC POSIXct; `lead_time` a `difftime` (or integer
  hours) = `valid_time - issue_time`; validator checks consistency.
- **`member` is integer, `stat` is character**, and **never both non-NA**
  (SCOPING §3, revised member/stat rule): `member` = `NA` + `stat` = `NA` for deterministic;
  `member` = k, `stat` = `NA` for ensemble member k; `member` = `NA`, `stat` in
  `c("mean","p10","p50","p90",...)` for summaries. Abort class
  `"member_stat_conflict"` if both non-NA.
- `value` numeric only. Non-numeric BOM edited-product elements (précis text,
  fire-danger / UV categories) go in a **companion `forecast_aux`** table keyed
  `(site_id, source, issue_time, valid_time, field, value_text)` —
  `new_forecast_aux(df)` validates it.
- `new_forecast(df)` validates all of the above; key uniqueness on
  `(site_id, source, model, issue_time, valid_time, member, stat, variable)`.

### Shared schema utilities (`R/schema.R`)

- A tiny column-spec type: `col_spec(name, type, ...)`.
- `assert_columns(df, spec, table_name)` — the one validator all `new_*()`
  constructors call; produces uniform, class-tagged errors
  (`"schema_missing_column"`, `"schema_bad_type"`).

## Test requirements

### `helper-schema.R` (reused by many later plans)
- Builders: `make_obs(n = 3, variable = "temperature_2m", ...)`,
  `make_forecast(...)`, `make_forecast_aux(...)` returning already-canonical
  tibbles with sensible defaults, so later plans get one-line valid inputs.
- Expectation helpers: `expect_canonical_obs(x)`, `expect_canonical_forecast(x)`,
  `expect_canonical_forecast_aux(x)` — assert the full column/type/enum/key
  contract. Every later plan that returns one of these tables must call the
  matching helper.
- `local_clean_dict()` — snapshot + `withr::defer(dict_reset())` so registration
  tests don’t leak.

### `test-enums.R`
- Each validator accepts every legal level and rejects an illegal one with the
  documented class.
- `tier_rank("raw") < tier_rank("emos")`; ranks are total and match `TIER_LEVELS`
  order.
- Assert `"estimated"` is **not** a legal `qc_flag` (guards against regressing to
  the pre-review enum).

### `test-units.R`
- `to_canonical(10, "km/h", "wind_speed_10m")` ≈ `2.7778` m/s — the footgun test.
- Round-trip: convert canonical→other→canonical within `1e-9`.
- Mapping an incompatible unit (`"degC"` into a wind variable) aborts class
  `"bad_units"`.
- A `units`-carrying input whose unit disagrees with the `from` argument warns
  (class `"units_conflict"`) and trusts the carried unit.

### `test-dict.R`
- Every built-in row: unit parses under `units::as_units`; `min <= max`;
  enums valid; every `wind_direction_*` variable has `circular_period == 360`
  and every other variable has `NA`.
- The §3.1 contract set is fully present (assert each of the 20 wide variable
  columns is a dictionary variable; `time` itself is not one).
- `met_register_variable()` adds a variable that `met_variable()` then finds;
  redefining a built-in without `overwrite` aborts `"duplicate_variable"`;
  `met_variable("nope")` aborts `"unknown_variable"`.
- Registration is undone by `local_clean_dict()` (no leak across tests).

### `test-schema-obs.R`
- `new_obs(make_obs())` passes `expect_canonical_obs()`.
- An `ok` row out of range aborts `"range_violation"`; the same value flagged
  `fail` is accepted.
- Unknown variable, bad enum value, non-UTC datetime, duplicate key — each aborts
  its specific class.
- **Round-trip:** `make_obs()` |> `widen_obs()` |> `narrow_obs()` equals the
  original on `(site_id, datetime_utc, variable, value)` (order-insensitive).
- `widen_obs()` with a variable absent from the data still emits that column, all
  `NA`, preserving the §3.1 shape.

### `test-schema-forecast.R`
- `new_forecast(make_forecast())` passes `expect_canonical_forecast()`.
- Deterministic (both `member`/`stat` NA), ensemble (member set), and summary
  (stat set) rows all validate; a row with **both** `member` and `stat` set
  aborts `"member_stat_conflict"`.
- `lead_time` inconsistent with `valid_time - issue_time` aborts.
- A seasonal-splice fixture where `model` is `"ec46"` for early leads and
  `"seas5"` for late leads validates (proving per-row model is representable).
- `new_forecast_aux()` accepts précis text rows and rejects numeric-only misuse.

## Definition of done

Shared skeleton plus:
- `met_variables()`, `met_variable()`, `met_register_variable()` exported and
  documented; the `new_*`, `widen_obs`, `narrow_obs`, `to_canonical` helpers
  exist (export `widen_obs`/`narrow_obs`; keep `new_*` internal for now — Plan 04
  calls them).
- Every condition class introduced here is listed in `meteo_conditions()`.
- The reusable test helpers exist and are used by this plan’s own tests, proving
  their shape for later plans.
