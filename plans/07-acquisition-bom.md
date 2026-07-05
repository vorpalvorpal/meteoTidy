# Plan 07 — Acquisition: BOM adapters + vendored weatherBOM + transport ladder

## Objective

Implement the two BOM adapters — `source_bom_forecast()` and `source_bom_obs()` —
over a **configurable transport ladder with a circuit breaker** (SCOPING §5.1),
vendoring the trimmed weatherBOM functions (MIT). BOM forecasts are archived on
every sync because BOM keeps no public forecast archive; the non-numeric elements
of the edited product go to `forecast_aux`.

## Scope

**In:**
- `source_bom_forecast()` (daily précis 7-day via official feeds; hourly ~2-day
  via the unofficial web API, opt-in) and `source_bom_obs()` (rolling 72-h
  station JSON; web API fallback + geohash/location search).
- The transport ladder: ordered rungs, circuit-breaker state persisted across
  runs, provenance recording which transport served each value.
- Vendored weatherBOM code (MIT), trimmed, notice retained.
- `forecast_aux` population (précis text, fire-danger/UV categories).
- `resolve_station()` → BOM geohash / AAC / product codes, cached in the site.

**Out:**
- Paid/registration-gated services (Registered User Services) — **out of v1**
  (SCOPING §5.1); do not implement rung 3’s ADFD.
- The archive-scheduling loop (Plan 16); this adapter fetches issuances on demand.

## Prerequisites

Plans 00–04.

## Background

SCOPING §5 (adapter roles), §5.1 (channel inventory + **BOM transport ladder**:
official product feeds first, then `api.weather.bom.gov.au/v1`, then
`api.bom.gov.au/apikey/v1` new gateway with a browser-like User-Agent, then
source substitution to Open-Meteo; circuit breaker on persistent failure; every
value’s provenance records the transport), §2 (vendor weatherBOM; **upstream
`mevers/weatherBOM` confirmed MIT** — commit `4696bcf`, DESCRIPTION + LICENSE.md,
© 2021 "weatherBOM authors"; credit Maurits Evers `ctb`), §10 (vendored function
list; the compass helpers are `compass2angle()`/`angle2compass()`, **not**
`compass_angle()`), §3 (`forecast_aux`), §13 (BOM web-API dependence is the top
risk; graceful degradation).

> The user has explicitly de-scoped CRAN acceptance, so the browser-User-Agent
> rung stays in. Keep it **opt-in and documented as at-your-own-risk** anyway —
> that is a correctness/ethics posture, not a packaging one.

## File layout

```
R/vendor-weatherbom.R       # trimmed MIT-licensed functions; header notice + attribution
R/bom-transport.R           # transport ladder, rung definitions, circuit breaker
R/bom-breaker-state.R       # persist/read breaker state under the store
R/source-bom-forecast.R
R/source-bom-obs.R
R/bom-parse.R               # BOM JSON/XML → canonical forecast/obs + forecast_aux
tests/testthat/test-bom-transport.R
tests/testthat/test-bom-breaker.R
tests/testthat/test-source-bom-forecast.R
tests/testthat/test-source-bom-obs.R
tests/testthat/_fixtures/bom/ftp-precis-*.xml
tests/testthat/_fixtures/bom/obs-72h-*.json
tests/testthat/_fixtures/bom/webapi-*.json
tests/testthat/_fixtures/bom/gateway-*.json
```

Add `xml2` to `Imports` (précis XML). Reuse Plan 04 `.http_get()`; FTP fetch is a
thin `.ftp_get()` seam added here (curl-based), mockable the same way.

## Detailed design

### Vendored weatherBOM (`R/vendor-weatherbom.R`)

- Copy in **only** the needed functions (SCOPING §10): `bom_forecasts()`
  (`R/bom_forecasts.R`), `bom_observations()`, `bom_location_info()`,
  `bom_search_station()`, the **internal** endpoint constant — value
  `"https://api.weather.bom.gov.au/v1/"` from `R/endpoint.R`, copied **by value**
  (it is unexported upstream; do **not** rely on `weatherBOM:::endpoint`) — and
  the compass helpers **`compass2angle()` and `angle2compass()`** (both in
  `R/compass_angle.R`; **there is no `compass_angle()` function** — that name was
  a scoping-doc error). Trim to what the adapters use.
- **Licence:** upstream `mevers/weatherBOM` is **confirmed MIT** (DESCRIPTION:
  `License: MIT + file LICENSE`; `LICENSE.md` full MIT text, © 2021 "weatherBOM
  authors"; verified at commit `4696bcf`, 2026-07-05). Vendoring is clean. The
  file header carries the MIT notice and records this verification (commit +
  URL); `Authors@R` credits Maurits Evers `ctb` (Plan 00). (Note: GitHub's API
  shows the repo licence as `NOASSERTION` only because the bare `LICENSE` stub
  isn't machine-recognised — cosmetic; the licence is genuinely MIT.)
- These functions are internal (not exported); the adapters call them.

### Transport ladder (`R/bom-transport.R`)

A transport is `list(id, kind, fetch_fn, applies_to)` where `applies_to` says
which products it can serve. The ladder is an **ordered** vector of transports,
configurable per adapter. Rungs (SCOPING §5.1):

1. `ftp_feeds` — anonymous FTP `ftp.bom.gov.au/anon/gen/fwo/` + HTTP mirrors on
   `reg.bom.gov.au`: 7-day précis XML, warnings, rolling 72-h obs JSON.
   **Preferred wherever its products suffice.** Lowest ToS exposure.
2. `web_api` — `api.weather.bom.gov.au/v1` (undocumented). **Opt-in** (a
   constructor flag `allow_web_api = FALSE` default). Only free channel for
   *hourly* forecasts and for **geohash/location search**.
3. `gateway` — `api.bom.gov.au/apikey/v1` (new gateway). Same opt-in flag.
   Requires a browser-like `User-Agent` and its own endpoint/schema map. Built in
   advance so failover is config, not an emergency patch. Document plainly that
   the UA is a deliberate disguise and this is the most legally awkward rung.
4. `substitute` — **source substitution, not transport substitution**: if all
   BOM web channels are gone, FTP précis still supplies the regulator-cited
   product at *daily* resolution; sub-daily granularity falls to Open-Meteo
   (Plan 05), which is model output — flagged in provenance as a **different
   source with separate calibration**, a quality fallback not a compliance one.

`ladder_fetch(ladder, request, breaker, now)`:
- Try rungs in order; **skip** rungs whose breaker is currently tripped.
- On a rung’s success, stamp provenance `transport = rung$id` on every row and
  return.
- On **persistent** failure (`"http_gone"` from Plan 04: 404/410/DNS), record a
  strike against that rung in the breaker; on **transient** failure
  (`"http_client_error"` transient/timeout/5xx) retry within the rung (Plan 04
  already retries) then, if still failing, move on without a persistent strike.
- If every applicable rung fails, abort class `"bom_all_transports_failed"`
  (Plan 16 turns this into degraded-service behaviour, not a crash).

### Circuit-breaker state (`R/bom-breaker-state.R`)

The breaker must persist **across runs** (SCOPING §5.1: “persistent failure …
across several consecutive runs trips fallback”). Store a small JSON under the
store: per `(rung_id)` a strike count and last-failure time.

- `breaker_read(store_root)` / `breaker_write(...)`.
- `breaker_trip?(rung, threshold = 3)` — tripped when consecutive strikes ≥
  threshold. A success resets the count to 0.
- Tripped rungs are retried occasionally (a cooldown, e.g. re-probe after N
  hours) so recovery is automatic; record trip/reset events so switches are
  visible and datable (SCOPING §5.1).

### `source_bom_forecast()` / `source_bom_obs()`

- Constructors take the `ladder` (default order above), `allow_web_api` (default
  `FALSE`), and `store_root` (for breaker state).
- `fetch_forecast()` (forecast adapter): daily précis via `ftp_feeds`; hourly via
  `web_api`/`gateway` only when `allow_web_api = TRUE`. Returns canonical forecast
  rows with `model = NA` (the edited BOM product has **no model name**, Plan 01)
  and `forecast_aux` rows for précis `short_text`/`extended_text`, fire-danger
  and UV **categories** (archived verbatim for reports).
- `fetch()` (obs adapter): rolling 72-h station JSON via `ftp_feeds`; web API only
  as opt-in fallback. Canonical obs, `method = "measured"`.
- `resolve_station()`: the web API is keyed by **geohash, not lat/lon**
  (SCOPING §5.1) — resolve and cache geohash + AAC/product codes in
  `site@resolved$bom`. Geohash search itself needs a rung that supports it (web
  API); if `allow_web_api = FALSE` and no geohash is cached, abort
  `"bom_geohash_unavailable"` with guidance.
- **Archive-every-sync note:** BOM keeps no public forecast archive, so any
  issuance not fetched is lost, and **BOM gaps cannot be backfilled** (SCOPING §9).
  The adapter simply returns the current issuance(s); Plan 16 dedups on
  `(source, model, issue_time)` and warns in its vignette that hourly scheduling
  captures more issue cycles.

## Test requirements

All tests replay fixtures against the `.http_get()`/`.ftp_get()` seams — **no
live calls**.

### `test-bom-transport.R`
- Rung order is honoured: with rung 1 mocked to succeed, rungs 2+ are never
  called (assert call counts).
- A rung returning `"http_gone"` falls through to the next rung; the served rows
  carry the **fallback** rung’s `transport` in provenance.
- `allow_web_api = FALSE` makes rungs 2/3 unavailable; an hourly-forecast request
  that only they can serve aborts with actionable guidance.
- Source substitution: all BOM web rungs gone → daily précis still served from
  FTP; a sub-daily request falls to the Open-Meteo substitute and is flagged as a
  different source in provenance.

### `test-bom-breaker.R`
- Three consecutive persistent failures trip the rung; the tripped rung is
  skipped on the next `ladder_fetch`. A success resets the strike count.
- Breaker state **persists across a simulated run boundary** (write, re-read,
  still tripped) — the cross-run requirement.
- Transient failures do **not** accumulate persistent strikes.

### `test-source-bom-forecast.R`
- Précis XML fixture → canonical forecast with `model == NA`; `forecast_aux` rows
  carry the précis text and fire-danger/UV categories verbatim.
- `resolve_station()` caches the geohash; with web API disabled and no cached
  geohash, `"bom_geohash_unavailable"` is raised.

### `test-source-bom-obs.R`
- 72-h obs JSON fixture → canonical obs; `transport == "ftp_feeds"`.
- Web-API fallback path (opt-in on) serves obs when FTP is mocked gone, with the
  fallback transport recorded.

### Licence test
- A test asserts the vendored file header contains the MIT notice and the Maurits
  Evers attribution (guards against a future edit stripping it).

## Definition of done

Shared skeleton plus:
- `source_bom_forecast()`, `source_bom_obs()`, `resolve_station()` exported/
  documented; the web-API rungs documented as opt-in / at-your-own-risk.
- Circuit-breaker state persists across runs and is proven by test.
- Vendored code carries the MIT notice; upstream MIT verified and recorded.
- `adapters_for_site()` resolves `"bom_forecast"` and `"bom_obs"`.
- New condition classes registered in `meteo_conditions()`.
