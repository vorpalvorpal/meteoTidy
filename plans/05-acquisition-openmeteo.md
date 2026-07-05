# Plan 05 — Acquisition: Open-Meteo adapter

## Objective

Implement `source_openmeteo()`: one adapter covering all the Open-Meteo products
the package uses — Forecast, Ensemble, Historical Weather (ERA5), Historical
Forecast, Previous Runs, Single Runs, and Seasonal — returning canonical
observation *or* forecast tables. This migrates the request plumbing currently in
meteoHazard and adds the archive/training endpoints.

## Scope

**In:**
- `source_openmeteo(product, ...)` producing a `met_adapter` per product.
- `fetch()` (observation-shaped products: Historical Weather / ERA5) and
  `fetch_forecast()` (forecast/ensemble/seasonal/previous/single-runs).
- The ensemble → `member` and seasonal-summary → `stat` mapping (Plan 01).
- The seasonal splice: **per-row underlying model** (`ec46`/`seas5`), never the
  spliced product name (SCOPING §5.2).
- API-key handling that **never assumes the free tier** (SCOPING §10).
- Provenance tagging that marks Historical-Forecast rows as **shortest-lead
  proxy** so correction (Plan 12) treats them correctly.

**Out:**
- Deciding *when* to archive (Plan 16 pipeline). This adapter fetches a window of
  issuances on request; the pipeline calls it and writes via Plan 03.
- Correction/calibration (Plans 11–13). Here we only acquire.

## Prerequisites

Plans 00–04.

## Background

SCOPING §5 (product list), §5.2 (seasonal EC46+SEAS5 splice; snapshot every
issuance because there is no seasonal issue-time archive), §7.2 (what each
training endpoint can support: Previous Runs = daily-lead pairs from ~2024;
Single Runs = shallow, retention unverified → §14 task; Historical Forecast =
shortest-lead stitched series, issue time not resolvable), §10 (licensing:
free tier non-commercial < 10 000 calls/day no key; commercial use needs a paid
plan + key, with historical/climate/ensemble/satellite-radiation on Professional
and above *among the paid plans*; CC-BY attribution; **accept a key cleanly,
document the boundary, never assume free**), §3.1 (request canonical units —
km/h footgun).

## File layout

```
R/source-openmeteo.R        # constructor + fetch/fetch_forecast dispatch
R/openmeteo-endpoints.R     # per-product endpoint URLs, param builders, model lists
R/openmeteo-parse.R         # response → canonical (obs and forecast); ensemble/member/stat
tests/testthat/test-source-openmeteo-obs.R
tests/testthat/test-source-openmeteo-forecast.R
tests/testthat/test-source-openmeteo-seasonal.R
tests/testthat/test-source-openmeteo-licensing.R
tests/testthat/_fixtures/openmeteo/*.json   # one recorded body per product
```

No new hard dependencies (uses Plan 04 `.http_get()` + `httr2`).

## Detailed design

### Constructor

`source_openmeteo(product = c("forecast","ensemble","historical","historical_forecast",
"previous_runs","single_runs","seasonal"), models = NULL, api_key_env = NULL,
source_id = "openmeteo", ...)`:

- `product` selects the endpoint + shape. `models` picks the underlying NWP
  model(s) where the product supports it (Ensemble includes at least
  `ecmwf_ifs025`, `ecmwf_aifs025`, `icon_seamless`, `gfs_seamless`, `gem_global`,
  MOGREPS, ACCESS-GE — the full Open-Meteo roster is larger and changes, so keep
  the model list in `openmeteo-endpoints.R` as the single extensible source of
  truth, and never present the built-in list as exhaustive).
- `api_key_env` names the env var holding a commercial key. **The request always
  sends the key when the env var is set** (and targets the `customer-` host);
  when unset, it uses the free host and the object records `commercial = FALSE`
  with a one-time `inform_meteo()` note that the free tier is licensed for
  **non-commercial use only** (SCOPING §10). **No product aborts for lack of a
  key** — the free tier serves every product this adapter wraps, including
  historical and ensemble; SCOPING §10's "Professional tier and above" is a
  boundary *within the commercial plans* (which paid plan a commercial
  deployment needs), not a technical key gate. Spell the boundary out in the
  roxygen: commercial deployments need a paid plan, and the historical /
  climate / ensemble / satellite-radiation APIs need the Professional tier or
  above among those plans. (This corrects an earlier draft of this plan that
  aborted `"openmeteo_key_required"` on keyless historical/ensemble requests —
  that behaviour contradicted SCOPING §2 "anonymous/free channels only in v1"
  and §9's day-0 ERA5 / Previous-Runs backfill, and misread §10.)
- `provides` = the dictionary variables Open-Meteo can serve for that product.

### Endpoints & params (`R/openmeteo-endpoints.R`)

- One base URL per product (see the Appendix links in SCOPING). Free host vs
  `customer-` (commercial) host selected by key presence.
- Param builder: always requests **canonical units** explicitly. The wind
  parameter is **`wind_speed_unit=ms`** (the current Open-Meteo spelling —
  **not** the legacy `windspeed_unit`, which a rename could silently drop,
  leaving wind in the km/h default and defeating the whole guard; meteoHazard
  already uses `wind_speed_unit=ms`). Also `temperature_unit=celsius`,
  `precipitation_unit=mm`. The parser still runs `to_canonical()` as a
  belt-and-braces check (SCOPING §3.1).
- Variable-name mapping: Open-Meteo names already equal our dictionary names for
  the §3.1 set (that is why §3.1 is Open-Meteo-named) — assert this in a test so a
  future dictionary rename can’t silently break the request.

### Parsing (`R/openmeteo-parse.R`)

- **Observation products** (Historical Weather / ERA5): `hourly` block → long
  canonical obs. `source = source_id`, `method = "model_fill"` for ERA5 (it is
  reanalysis, not a site measurement — mark it honestly), `qc_flag = "ok"`.
- **Forecast products**: build canonical forecast rows with `issue_time`,
  `valid_time`, `lead_time`.
  - **Ensemble**: each member series → a row with integer `member`; `stat = NA`.
  - **Seasonal**: 51 members → `member`; probability/summary products → `stat`
    (`"mean"`, `"p10"`, …). **Set `model` per row to the underlying model**: EC46
    for leads ≤ ~46 d, SEAS5 beyond (SCOPING §5.2). The splice boundary is a
    documented constant; a test asserts early leads carry `ec46` and late leads
    `seas5`.
  - **Previous Runs**: issue×valid pairs at **daily** lead granularity — set
    `lead_time` in whole days; mark provenance so Plan 12 knows sub-daily lead
    resolution is unavailable from this source (SCOPING §7.2).
  - **Single Runs**: per-run forecasts by init time. Record the init time as
    `issue_time`. (Retention is unverified — §14; the adapter returns what it
    gets, and Plan 16’s backfill audits coverage.)
  - **Historical Forecast**: stitched shortest-lead series — issue time is **not**
    resolvable. Stamp these rows with `model` = the model and a **provenance
    marker** (`method = "model_fill"` plus a `lead_kind = "shortest"` note in a
    way Plan 12 can read — simplest: set `lead_time = NA` and document that a
    `lead_time`-`NA` forecast row means “shortest-lead proxy, not lead-resolved”).
    Plan 12 must not train lead-aware calibration on these (SCOPING §7.2).
- All forecast parsing runs `new_forecast()` and `check_fetch_result`-equivalent
  validation before returning.

### Licensing guardrails

- The key is read from the env var **by name at request time**; never stored on
  the object, never logged, never written to provenance (SCOPING §10/§11).
- CC-BY attribution string is exposed via `met_attribution(adapter)` (a small
  generic added here, default `NA`, Open-Meteo returns the required credit) so
  dashboards/reports can surface it.

## Test requirements

All tests replay recorded fixtures; **no live calls** (Plan 04 no-net guard).

### `test-source-openmeteo-obs.R`
- Historical/ERA5 fixture → canonical obs; `expect_canonical_obs()`; ERA5 rows
  have `method == "model_fill"`.
- Units: a fixture is (deliberately) served with a km/h wind and the output is
  m/s — proving both the explicit unit request path and the `to_canonical` check.
- Dictionary-name parity assertion (request variable names == dictionary names).

### `test-source-openmeteo-forecast.R`
- Forecast fixture → canonical forecast; deterministic rows have `member`/`stat`
  both `NA`.
- Ensemble fixture → integer `member` per series; member count matches the model.
- Previous Runs fixture → `lead_time` in whole days; a marker distinguishes it
  from lead-resolved sources.
- Historical Forecast fixture → `lead_time` is `NA` (shortest-lead proxy) and a
  test asserts Plan 12’s contract expectation (documented here, enforced there).

### `test-source-openmeteo-seasonal.R`
- Seasonal fixture spanning the splice → early-lead rows `model == "ec46"`,
  late-lead rows `model == "seas5"` (directly tests the §5.2 fix).
- 51 members present; summary `stat` rows validate; `member`/`stat` never both set.

### `test-source-openmeteo-licensing.R`
- A keyless historical/ensemble request targets the **free** host and succeeds
  (no key gate), emitting the one-time non-commercial notice.
- With `api_key_env` set (`withr::local_envvar`), the request targets the
  commercial host and sends the key; the key is **absent** from the adapter’s
  print/`format()` output and from any returned provenance.
- Free-tier forecast with no key emits the non-commercial `inform_meteo()` note
  once (snapshot it).

## Definition of done

Shared skeleton plus:
- `source_openmeteo()` and `met_attribution()` exported and documented, with the
  licensing boundary spelled out in the roxygen (SCOPING §10).
- Every product has a fixture and a passing parse test; the seasonal splice and
  Historical-Forecast provenance markers are proven.
- New condition classes registered in `meteo_conditions()`.
- `adapters_for_site()` (Plan 04) now resolves `"openmeteo"`.
