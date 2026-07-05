# Plan 14 — Read API + config & secrets

## Objective

Ship the **stable, public read surface** — tibble-returning functions over the
store — plus the experimental `met_connect()` SQL path, and the configuration /
secret-resolution layer (non-secret YAML in version control; secrets referenced
by name and resolved from env/keyring at use time, never persisted).

## Scope

**In:**
- `met_history()`, `met_record()`, `met_forecast_archive()`, `met_verification()`
  (tibble-returning; the versioned contract).
- `met_connect()` (DuckDB/arrow connection; **experimental**).
- Config loading built on Plan 02 YAML + a global-deployment config (store roots,
  refetch windows, adapter defaults).
- Secret resolution: `resolve_secret(ref)` reading env/keyring by name.

**Out:**
- The classed-tibble wide emitter for meteoHazard (Plan 15) — `met_history()` here
  returns plain tibbles; Plan 15 adds the classed wide table.
- Pipeline verbs (Plan 16).

## Prerequisites

Plans 00–03 (reads the store), 13 (verification report).

## Background

SCOPING §11 (**ship both**: default tibble functions = stable, versioned,
backend-agnostic contract; `met_connect()` exposes the physical schema →
**experimental**; non-secret config in version-controlled YAML; secrets in
`.Renviron`/keyring referenced by name, never inlined; SILO email = PII not a
secret; **never write secrets into Parquet/manifests/provenance**), §4 (the
curated products these functions serve), §8 (arrow default, duckdb optional).

## File layout

```
R/read-api.R              # met_history/met_record/met_forecast_archive/met_verification
R/connect.R               # met_connect() (wraps Plan 03 store_connect)
R/config.R                # deployment config loader (global + per-site YAML)
R/secrets.R               # resolve_secret(); env + keyring
tests/testthat/test-read-api.R
tests/testthat/test-connect.R
tests/testthat/test-config.R
tests/testthat/test-secrets.R
tests/testthat/_fixtures/config/deployment.yaml
```

`keyring` in `Suggests` (env vars are the default; keyring is optional).

## Detailed design

### Read API (`R/read-api.R`) — the stable contract

All return plain, canonical tibbles (Plan 01) and wrap the internal `store_read_*`
(Plan 03). These signatures are the **versioned promise** — change them only with
deprecation (the r-lib `lifecycle` conventions):

- `met_history(site, resolution = c("daily","hourly"), variables = NULL,
  from = NULL, to = NULL, as_of = NULL)` → `history_daily` / `history_hourly`
  (SCOPING §4), curated + provenance columns. `as_of` exposes Plan 03’s point-in-
  time read for reproducible reports.
- `met_record(site, variables = NULL, from = NULL, to = NULL, as_of = NULL)` →
  the “best available truth” curated record (the QC’d, gap-filled observation
  series).
- `met_forecast_archive(site, source = NULL, issue_from = NULL, issue_to = NULL,
  valid_from = NULL, valid_to = NULL, members = TRUE)` → archived forecasts;
  **members retrievable by default** (SCOPING §4). Includes `forecast_aux` join
  helper for the non-numeric BOM elements.
- `met_verification(site, source = NULL, ...)` → the Plan 13 report table.

Each accepts a `met_site` or `met_sites` (multi-site → row-bound with `site_id`).
Each validates its output with the Plan 01 canonical helper before returning.

### `met_connect()` (`R/connect.R`) — experimental

`met_connect(site, backend = c("duckdb","arrow"))` → a `dbplyr`/DBI source over
the same Parquet tree (Plan 03 `store_connect`). **Marked experimental** in
roxygen with `lifecycle::badge("experimental")`: it exposes the physical schema,
so only the tibble surface is a stability promise (SCOPING §11). It must return
the same current rows as the tibble API (parity test). Guard `duckdb` with
`check_installed`.

### Deployment config (`R/config.R`)

A global deployment config layered over the per-site YAML (Plan 02):

- `read_deployment_config(path)` → a validated config object: the `met_sites`,
  the store root(s), per-source **refetch windows** (Plan 03 revision policy),
  adapter defaults (e.g. `allow_web_api`), and Open-Meteo key env-var names.
- Unknown keys abort `"unknown_config_key"` (typos fail loud, as in Plan 02).
- **No secret values** appear here — only `*_env` / `*_keyring` references
  (reuse Plan 02’s inline-secret guard).

### Secrets (`R/secrets.R`)

- `resolve_secret(ref)` where `ref` is `list(env = "VAR")` or
  `list(keyring = "service")`: reads the env var (default) or the keyring entry
  at **use time**; returns the value to the caller that needs it (adapters).
  Never caches it on an object, never logs it, never writes it anywhere.
- A `redact()` helper used in all `print`/`format` methods so a secret can’t leak
  through object display.
- **Guard:** a function `assert_no_secrets_in(df)` used before any store write
  (Plan 03 write paths call it) that aborts `"secret_leak"` if a column value
  matches a resolved secret — a belt-and-braces check that secrets never reach
  Parquet/manifests/provenance (SCOPING §11).

## Test requirements

### `test-read-api.R`
- Each function returns a canonical tibble passing the Plan 01 helper; multi-site
  input row-binds with correct `site_id`s.
- `as_of` reproduces a historical view (ties to Plan 03’s revision test).
- `met_forecast_archive(members = TRUE/FALSE)` includes/excludes member rows;
  members retrievable by default.
- The signatures are snapshot-tested (a `formals()` snapshot) so an accidental
  breaking change to the stable contract is caught in review.

### `test-connect.R`
- `skip_if_not_installed("duckdb")`; `met_connect()` returns the **same** current
  rows as `met_record()`/`met_history()` for a fixture (parity).
- Carries the experimental badge (documented).

### `test-config.R`
- A valid `deployment.yaml` loads to a config with the expected sites, store
  roots, and refetch windows; an unknown key aborts `"unknown_config_key"`.
- An inline secret in the deployment config aborts (reusing Plan 02’s guard).

### `test-secrets.R`
- `resolve_secret(list(env=...))` reads the env var (`withr::local_envvar`) and
  does not persist it.
- `redact()` hides the value in print output.
- `assert_no_secrets_in()` aborts `"secret_leak"` when a resolved secret value
  appears in a data frame bound for the store — proving secrets can’t reach disk.

## Definition of done

Shared skeleton plus:
- The four `met_*` read functions and `met_connect()` exported/documented; the
  tibble surface marked stable, `met_connect()` experimental (`lifecycle`).
- Secrets are resolved by name, redacted in display, and blocked from the store by
  an enforced guard — proven by test.
- The stable signatures are snapshot-guarded.
- New condition classes registered in `meteo_conditions()`.
