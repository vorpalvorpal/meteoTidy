# Plan 04 — Acquisition: adapter contract + generic adapters

## Objective

Define the exported adapter contract — an S7 class with a single `fetch()`
generic returning a canonical long observation table — and ship the two
source-agnostic built-ins, `source_rest()` and `source_file()`, plus the shared
HTTP seam, the response-mapping spec, and unit enforcement at the boundary. This
plan fixes the interface that Plans 05–08 implement for specific sources.

## Scope

**In:**
- S7 `met_adapter` base class + `fetch()` generic + the fetch return contract.
- `source_rest()` (generic REST AWS) and `source_file()` (logger CSV/TSV drops).
- The response-mapping spec (JSON paths / CSV columns → dictionary variables,
  with units and sensor heights).
- The internal HTTP seam (`.http_get()`) with the no-network test guard and
  retry/error classification.
- Adapter construction from a site’s `sources` YAML config (Plan 02).

**Out:**
- Any specific external API (Open-Meteo/SILO/GHCNh/BOM/ECMWF — Plans 05–08).
- Forecast fetching contract details beyond noting adapters may *also* implement
  a `fetch_forecast()` generic (defined here, implemented in 05/07/08).
- QC, fill, correction, storage-writing (later plans). `fetch()` returns a
  validated in-memory table; the pipeline (Plan 16) writes it.

## Prerequisites

Plans 00, 01, 02, 03.

## Background

SCOPING §5 (adapter = S7 class implementing `fetch(site, variables, window)` →
canonical long table; contract exported so users can add their own), §5 table
(`source_rest`, `source_file` constrained scope — §13), §3.1 (**unit enforcement
at the boundary** — the review’s unit-pinning fix), §11 (secrets referenced by
name, resolved from env — full resolution is Plan 14, but the REST adapter must
read a token from an env var *by name*).

## File layout

```
R/adapter.R           # S7 met_adapter base, fetch()/fetch_forecast() generics, return contract checker
R/adapter-mapping.R   # response-mapping spec type + apply_mapping()
R/http.R              # .http_get() seam, retry, error classification, no-net guard
R/source-rest.R       # source_rest() adapter
R/source-file.R       # source_file() adapter
tests/testthat/helper-adapter.R
tests/testthat/test-adapter-contract.R
tests/testthat/test-http.R
tests/testthat/test-source-rest.R
tests/testthat/test-source-file.R
tests/testthat/_fixtures/rest/*.json        # recorded/synthetic API bodies
tests/testthat/_fixtures/file/*.csv
```

Add `httr2` and `readr` to `Imports`; `httptest2` already in Suggests (Plan 00).

## Detailed design

### The adapter contract (`R/adapter.R`)

S7 base class `met_adapter` with common properties:
- `source_id` — chr(1), the `source` string stamped into every returned row
  (e.g. `"site_aws"`, `"silo"`, `"openmeteo"`).
- `provides` — chr(n), the dictionary variables this adapter can return.
- `cadence` — chr(1), one of `c("hourly","daily","subdaily","per_issue")`
  (documentation/scheduling hint).

Generics (S7 `new_generic`):
- `fetch(adapter, site, variables, window, now = .now())` → **canonical long
  observation tibble** (Plan 01 `new_obs()`-valid), for the requested
  `variables` (intersected with `provides`) over `window` (a `[from, to]` UTC
  pair). Must:
  1. Request/parse only the requested variables.
  2. Convert every value to canonical units via `to_canonical()` **before**
     returning (SCOPING §3.1 unit pinning). A bare-number source with a declared
     source unit is converted; a source that can request units (Open-Meteo) is
     asked for canonical units *and* the result is still checked.
  3. Stamp `source = source_id`, `method = "measured"` (raw obs) or the
     appropriate method, `qc_flag = "ok"` (QC happens later, Plan 09 — the
     adapter asserts nothing about quality beyond “as delivered”).
  4. Return an empty (0-row) canonical table, not `NULL`, when the source has no
     data for the window (callers rely on the shape).
- `fetch_forecast(adapter, site, variables, issue_window, now = .now())` →
  canonical **forecast** tibble (Plan 01 `new_forecast()`). Default method aborts
  class `"no_forecast_support"`; only forecast adapters (05/07/08) implement it.
- `resolve_station(adapter, site)` → an updated `met_site` with the `resolved`
  cache filled for this source (default: return site unchanged). Implemented by
  BOM/GHCNh/SILO adapters later.

`check_fetch_result(x, adapter, variables)` — an internal contract checker every
adapter’s `fetch()` runs on its own output before returning (belt-and-braces):
validates `new_obs()`, that `source` is uniformly `source_id`, and that returned
variables ⊆ requested. This is what guarantees a *user-written* adapter that
passes its tests actually honours the contract.

### Response-mapping spec (`R/adapter-mapping.R`)

A declarative spec, built from YAML (Plan 02) or in code, describing how to turn
a parsed response into canonical rows. Represent as an S7 class `met_mapping` or
a validated list; fields:

- `time` — how to find the timestamp: a JSON path or CSV column name, plus a
  format/timezone (source timestamps are converted to UTC here).
- `variables` — a list, one entry per source field:
  `list(variable = "temperature_2m", path/column = "...", unit = "degC",
  height = set_units(2,"m"))`. `unit` is the **source** unit; `apply_mapping()`
  calls `to_canonical()`. `height` lets a source expose, e.g., wind at a
  non-standard height (recorded but height-correction is Plan 11).
- `format` — `"json"` or `"csv"`.

`apply_mapping(parsed, mapping, site, source_id, now)` → canonical long tibble.
Pure function (no IO): takes an already-parsed body. This is the unit-tested core;
the adapters are thin IO wrappers around it. JSON paths resolved with a tiny
internal path walker (`x[["hourly"]][["temperature_2m"]]`) — document the
supported path syntax; keep it deliberately simple (SCOPING §13 constrains scope).

### HTTP seam (`R/http.R`)

`.http_get(url, headers = list(), query = list(), retry = 3, now = .now())`:
- Built on `httr2`. This is the **only** function that performs a live request;
  every adapter goes through it, so `httptest2` and the no-network guard have one
  seam to intercept.
- **No-network guard:** if `Sys.getenv("METEOTIDY_NO_NET") == "1"` (set in tests,
  Plan 00), abort class `"network_disabled"` immediately. This makes an
  un-mocked request in a test a loud failure, not a silent live call.
- Retry policy (SCOPING §5.1 circuit-breaker spirit, per-request half):
  transient failures (timeout, 429, 5xx) retry with backoff up to `retry`;
  **persistent** failures (404/410/DNS) do **not** retry — they abort class
  `"http_gone"` so the transport ladder (Plan 07) can trip to the next rung.
  Other 4xx abort class `"http_client_error"`. Classify and attach the status.
- Returns the parsed body (JSON→list via `httr2::resp_body_json`, or raw for
  CSV/GRIB callers to parse).

### `source_rest()` (`R/source-rest.R`)

`source_rest(source_id, endpoint, mapping, auth = c("none","header","basic"),
token_env = NULL, provides = NULL, cadence = "hourly")`:
- `endpoint` is a template with `{site}`, `{from}`, `{to}`, `{var}` placeholders
  interpolated per request (document the exact placeholder set — SCOPING §5
  “user supplies endpoint template”).
- `auth`: `"none"`; `"header"` sends `Authorization: <token>` where the token is
  read **from the env var named `token_env`** at fetch time (never stored on the
  object, never logged — SCOPING §11); `"basic"` uses `httr2::req_auth_basic`
  with user/pass from two env vars.
- `provides` defaults to the variables named in `mapping`.
- **Constrained by design (SCOPING §13):** single-page responses, JSON or CSV,
  no OAuth, no pagination. If a response looks paginated (a `next`/`cursor` field
  is present), abort class `"unsupported_response"` pointing the user at writing
  their own adapter.
- `fetch()` = interpolate endpoint → `.http_get()` → parse → `apply_mapping()` →
  `check_fetch_result()`.

### `source_file()` (`R/source-file.R`)

`source_file(source_id, glob, mapping, provides = NULL, cadence = "daily")`:
- Reads local logger exports matched by `glob` (CSV/TSV), concatenates, applies
  the same `mapping` machinery (`format = "csv"`), returns canonical long.
- Handles the common messiness minimally: configurable delimiter, header row,
  `na` strings, and a `skip` for preamble lines — documented; anything beyond
  that is a user adapter.
- No network; no HTTP seam. Reads only files under paths the caller supplies.

### Building adapters from site config

`adapters_for_site(site)` → named list of `met_adapter` built from
`site@sources` (Plan 02 YAML). Dispatch on `sources.<name>.adapter`:
`"rest"` → `source_rest()`, `"file"` → `source_file()`, and (stubbed here,
implemented later) `"openmeteo"`, `"silo"`, `"ghcnh"`, `"bom_forecast"`,
`"bom_obs"`, `"ecmwf"`. Unknown adapter name aborts `"unknown_adapter"`. This is
the function the pipeline (Plan 16) uses; here, only `rest`/`file` resolve, the
rest abort `"adapter_not_yet_implemented"` (a temporary, tested stub that Plans
05–08 replace).

## Test requirements

### `helper-adapter.R`
- `make_rest_mapping()`, `make_csv_mapping()` builders.
- `local_no_net()` — sets `METEOTIDY_NO_NET=1` for a test (usually already global).
- `with_recorded_api(fixture)` — `httptest2` wrapper replaying a fixture body.

### `test-adapter-contract.R`
- `check_fetch_result()` accepts a valid canonical table and rejects: a table
  whose `source` isn’t uniform, one containing an unrequested variable, one that
  fails `new_obs()`.
- `fetch_forecast()` on a non-forecast adapter aborts `"no_forecast_support"`.
- A trivial hand-written test adapter (in the test file) that returns canonical
  rows passes the contract — proving third parties can implement it (SCOPING §5).

### `test-http.R`
- With `METEOTIDY_NO_NET=1`, `.http_get()` aborts `"network_disabled"` (proves
  the guard; guarantees no test hits the network).
- A mocked 500 then 200 (via `httptest2`) succeeds after retry; a mocked 404
  aborts `"http_gone"` **without** retrying (assert the request count); a 401
  aborts `"http_client_error"`.

### `test-source-rest.R` (all mocked — no live calls)
- `apply_mapping()` on a recorded JSON body produces canonical rows: correct
  variable names, UTC timestamps, and **units converted** — feed a fixture whose
  wind is in km/h and assert the output is m/s (the §3.1 footgun, end-to-end).
- Endpoint template interpolation puts `from`/`to`/`site` in the right places
  (assert the URL the mocked seam received).
- `auth = "header"` reads the token from the named env var
  (`withr::local_envvar`) and sends it; the token never appears in the returned
  object or in any message (assert it’s absent from `format()`/print output).
- A paginated-looking body aborts `"unsupported_response"`.
- Empty response → 0-row canonical table (not `NULL`).

### `test-source-file.R`
- A fixture CSV maps to canonical rows; delimiter/skip/na options honoured.
- Multiple files matched by glob concatenate; ordering by time is deterministic.
- Unit conversion applied identically to the REST path (shared `apply_mapping`).

## Definition of done

Shared skeleton plus:
- `met_adapter`, `fetch`, `fetch_forecast`, `resolve_station`, `source_rest`,
  `source_file`, and `met_mapping`/`apply_mapping` are exported and documented,
  with a vignette-stub note that the contract is user-implementable.
- No test performs a live request (the no-net guard proves it); every adapter
  test replays a fixture.
- `adapters_for_site()` resolves `rest`/`file` and cleanly stubs the rest with a
  tested `"adapter_not_yet_implemented"` error, ready for Plans 05–08 to fill in.
- New condition classes registered in `meteo_conditions()`.
