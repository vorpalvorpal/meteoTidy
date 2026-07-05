# Plan 02 — Site registry & station-ID resolution

## Objective

Implement the `met_site` domain object (S7) that every stored row is keyed by,
its YAML (de)serialisation for version-controlled deployment config, and the
resolved-external-ID cache. Multi-site is a list of these.

## Scope

**In:**
- S7 class `met_site` with validator, and `met_sites` (a validated list).
- Construction from YAML and serialisation back to YAML.
- The resolved-ID cache structure (BOM geohash / AAC / product codes, nearest
  GHCNh station, SILO grid cell) as *data on the object*, populated later by
  adapters — this plan defines the slots and accessors, not the resolution calls.
- Accessors: `site_id()`, `site_coords()`, `site_roughness()`, `site_sources()`,
  `site_store_root()`, `site_resolved()`.

**Out:**
- Actually calling BOM/GHCNh/SILO to resolve IDs (that lives with each adapter,
  Plans 05–07). Here the cache is a passive slot with a setter.
- Adapter objects themselves (Plan 04) — the site holds *source configs*
  (plain named lists from YAML); adapters are constructed from them in Plan 04.

## Prerequisites

Plans 00, 01.

## Background

SCOPING §3 (site registry fields, incl. the **added `z0`/displacement height**
from the review), §11 (config in version-controlled YAML; secrets referenced by
name, resolved in Plan 14 — here we only store the *reference*, never a secret).

## File layout

```
R/site.R              # S7 met_site class + validator + accessors
R/site-list.R         # met_sites list wrapper + validator (unique site_id)
R/site-yaml.R         # read_sites_yaml() / write_sites_yaml()
tests/testthat/helper-site.R      # make_test_site(), make_test_sites()
tests/testthat/test-site.R
tests/testthat/test-site-yaml.R
tests/testthat/_fixtures/sites/one-site.yaml
tests/testthat/_fixtures/sites/multi-site.yaml
tests/testthat/_fixtures/sites/bad-*.yaml
```

Add `S7` and `yaml` to `Imports`.

## Detailed design

### `met_site` S7 class (`R/site.R`)

Properties (S7 `class_*` typed where possible; use `units`-typed doubles for
dimensioned quantities per house style):

| property | type | notes |
|---|---|---|
| `site_id` | character(1) | non-empty, `[A-Za-z0-9_-]+`; the join key everywhere |
| `latitude` | double(1), units degree | −90..90 |
| `longitude` | double(1), units degree | −180..180 |
| `elevation` | double(1), units m | station elevation |
| `timezone` | character(1) | IANA name; validated against `OlsonNames()` |
| `instruments` | list of `met_instrument` | sensor height + `z0` + displacement per instrument |
| `sources` | named list | raw source configs from YAML (opaque here; Plan 04 parses) |
| `store_root` | character(1) | filesystem path (or URI) for this site’s Parquet tree |
| `resolved` | list | cache of external IDs; see below; default empty list |

Add a small nested S7 class `met_instrument`:

| property | type | notes |
|---|---|---|
| `name` | chr(1) | e.g. `"anemometer"`, `"thermo"`, `"pyranometer"` |
| `variable` | chr(1) or chr(n) | dictionary variable(s) it measures |
| `height` | double(1), units m | sensor height AGL |
| `roughness_length` | double(1), units m, optional | `z0`; **required for any wind instrument** (SCOPING §3/§7.1 — height correction is meaningless without it) |
| `displacement_height` | double(1), units m, optional | `d`; default 0 |

**Validator** (`S7::validator`): non-empty `site_id` matching the pattern;
lat/lon/elevation finite and in range; `timezone %in% OlsonNames()`; every
instrument’s `variable` is a known dictionary variable (Plan 01); **any
instrument whose variable has `statistical_class == "linear"` and is a wind
variable, or more simply any wind instrument, must have a non-NA
`roughness_length`** → else abort class `"missing_roughness"`. `store_root`
non-empty.

`resolved` slot shape (all optional, filled by adapters later):
```
list(
  bom = list(geohash = NA_character_, aac = NA_character_, product = NA_character_),
  ghcnh = list(station_id = NA_character_, distance_km = NA_real_),
  silo = list(grid = NA_character_)
)
```
Provide `site_set_resolved(site, path, value)` returning a *new* site
(functional; S7 objects are copied, not mutated) and `site_resolved(site,
path = NULL)` to read. Adapters call the setter and the pipeline persists the
updated site (Plan 16); this plan just provides the accessors.

### `met_sites` (`R/site-list.R`)

- `met_sites(...)` / `met_sites(list_of_sites)` — validates it is a list of
  `met_site` and that `site_id`s are **unique** (abort class `"duplicate_site_id"`).
- `site_ids(sites)`, `[[`/`$` by `site_id` (name the list by `site_id`).
- Every multi-site verb in later plans accepts either a single `met_site` or a
  `met_sites`; provide `as_met_sites()` to normalise.

### YAML (`R/site-yaml.R`)

`read_sites_yaml(path)` → `met_sites`. Schema (document it in the roxygen and in a
vignette stub):

```yaml
sites:
  - site_id: piggery_north
    latitude: -34.75
    longitude: 148.20
    elevation: 220
    timezone: Australia/Sydney
    store_root: /data/meteo/piggery_north
    instruments:
      - name: anemometer
        variable: [wind_speed_10m, wind_direction_10m, wind_gusts_10m]
        height: 10
        roughness_length: 0.03
        displacement_height: 0
      - name: thermo
        variable: [temperature_2m, relative_humidity_2m]
        height: 2
    sources:
      silo: { adapter: silo, api_key_env: SILO_EMAIL }   # note: reference by name
      site_aws:
        adapter: rest
        endpoint: "https://aws.example/api?site={site}&from={from}&to={to}"
        token_env: PIGGERY_AWS_TOKEN
        # ...response mapping parsed in Plan 04
```

Rules enforced on read:
- Units attach on read (`height: 2` → `set_units(2, "m")`). Numbers are assumed
  in the documented unit; the YAML carries no unit strings to keep it human-editable.
- **No secret values inline.** A `sources.*` entry may only reference secrets by
  `*_env` / `*_keyring` key names. If a value looks like a token (heuristic:
  a key named `token`/`api_key`/`password` with a literal value rather than a
  `*_env` reference), abort class `"inline_secret"` (SCOPING §11). This is a
  safety rail, not full secret handling (Plan 14).
- Unknown top-level keys abort `"unknown_config_key"` (typos fail loudly).

`write_sites_yaml(sites, path)` — round-trips: `read |> write |> read` yields an
equivalent `met_sites` (units normalised). Never writes the `resolved` cache into
the version-controlled file by default (it is a runtime cache); provide
`include_resolved = FALSE` default, `TRUE` to snapshot for debugging.

## Test requirements

### `helper-site.R`
- `make_test_site(site_id = "test", with_wind = TRUE, with_pyranometer = FALSE,
  store_root = withr::local_tempdir())` → valid `met_site`.
- `make_test_sites(n = 2)` → valid `met_sites`.

### `test-site.R`
- A minimal valid site constructs and passes its validator.
- Out-of-range lat/lon, unknown timezone, empty `site_id`, bad `site_id`
  characters — each aborts its specific class.
- **A wind instrument with no `roughness_length` aborts `"missing_roughness"`**;
  adding `z0` fixes it. (Directly tests the review fix.)
- An instrument measuring an unknown dictionary variable aborts.
- `site_set_resolved()` returns a new site with the value set and **does not
  mutate** the original (assert the original’s `resolved` is unchanged).
- `met_sites()` with duplicate `site_id` aborts `"duplicate_site_id"`; lookup by
  `site_id` works.

### `test-site-yaml.R`
- `read_sites_yaml("_fixtures/sites/one-site.yaml")` yields a site whose
  `height` slots carry `units` of metres.
- **Round-trip:** read → `write_sites_yaml(tmp)` → read is equivalent.
- `bad-inline-secret.yaml` (a literal `token: abc123`) aborts `"inline_secret"`.
- `bad-unknown-key.yaml` aborts `"unknown_config_key"`.
- `multi-site.yaml` yields a `met_sites` of length 2 with unique ids.
- `resolved` is excluded from the written file by default and included when asked.

## Definition of done

Shared skeleton plus:
- `met_site`, `met_sites`, `read_sites_yaml`, `write_sites_yaml`, and the
  accessors are exported and documented; `met_instrument` documented.
- New condition classes registered in `meteo_conditions()`.
- The fixtures exist and are minimal.
