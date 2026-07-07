# meteoTidy — scoping document

**Status:** scoping / discussion — no code yet.
**Date:** 2026-07-05.
**Revised:** 2026-07-05 — renamed `tidyMeteo` → `meteoTidy`; correction-,
QC-, schema- and verification-layer revisions following technical review; then
a research pass resolving the §14 open questions (name availability, weatherBOM
licence, meteoHazard units, worldmet/GHCNh, Open-Meteo APIs, ECMWF/GRIB) with
findings folded into §5–§14.
**Revised again:** 2026-07-05 (post-plan review) — daily boundary corrected
from local standard time to **local clock time** (§3, matching BOM/SILO
practice); §3.1 wide contract completed against a fresh meteoHazard clone
(`pressure_msl`, layered soil moisture, hub-height wind directions); CRAN
submission de-scoped (§12); curated-product assembly assigned to Plan 10.
**Repository:** <https://github.com/vorpalvorpal/meteoTidy> (tracked in-repo as of 2026-07-05).

---

## 1. What the package is

A site-weather data layer for the tidyWaste family, with four jobs:

1. **Acquire** — adapters for site AWS (generic REST / file drops), BOM
   observations & forecasts, SILO, Open-Meteo (forecast, historical, seasonal),
   GHCNh, and optionally ECMWF Open Data.
2. **Curate** — QC-flag, gap-fill, and provenance-track observations into a
   continuous "best available truth" per site.
3. **Correct** — fit and apply site-specific bias corrections to forecasts
   (hourly, lead-time-aware) and to SILO (daily), tiered by available overlap.
4. **Serve** — tidy, keyed, query-fast tables for meteoHazard, dashboards,
   and operational reports (e.g. the daily site rainfall email), plus a
   locally maintained forecast archive.

Australian use is the primary case and Australian-specific helpers (BOM, SILO)
are provided, but the bulk of the machinery is source-agnostic: any
user-provided source fits the same exported adapter contract.

**Positioning.** `weatherOz` (CRAN, rOpenSci-reviewed) already provides
Australian weather-API clients. meteoTidy is not a competing client: it *uses*
weatherOz for SILO transport and differentiates on everything downstream — QC,
gap-fill, bias correction, forecast archiving, verification, and a storage
layer. The DESCRIPTION should make this split explicit.

## 2. Settled decisions

| Topic | Decision |
|---|---|
| Package boundary | Sibling package in the tidyWaste family. meteoHazard consumes the wide hourly table (§3.1); dashboards consume the long tables and the forecast archive. |
| meteoHazard contract | meteoHazard is pre-v0; its ingestion evolves to accept the **classed-tibble contract** (§3.2). The wide column set stays Open-Meteo-named. |
| AWS ingestion | Generic: works with any AWS via a user-specified REST endpoint or file export per measurement, normalised into the canonical shape. Pyranometer optional. |
| Sites | Single or multiple; part of an automated pipeline; **scheduling left to the user** (vignette, not package code). |
| BOM forecasts | Required as *content* (the product Australian regulators cite). Official transports preferred where they exist (§5.1); the unofficial web API is opt-in but a core capability. |
| Forecast archiving | **Every sync verb archives any newly issued forecast, deduplicated by issue time** (§9). BOM keeps no public archive of its edited forecast, so anything not fetched is lost. |
| Long-range forecasts | **First-class v1 product**: acquired, archived, and calibrated (§5.2, §7.2). Both the Open-Meteo seasonal splice and `source_ecmwf()` ship in v1; seasonal calibration is prototyped against hindcasts during v1. |
| Paid services | **None in v1** — anonymous/free channels only; BOM Registered User Services deferred post-v1 (§5.1). |
| Latency | Long daily history: lag fine. Live hourly head: **best-effort** near-real-time, gated by the weakest channel (§5.1, §13). |
| History depth | Must work from day 0 and scale to decade+ (the correction tier system is load-bearing). |
| Storage | Hive-partitioned Parquet via `arrow`; optional DuckDB query layer (§8). |
| Model-only variables | Raw pass-through with explicit provenance by default; opt-in experimental profile rescaling (§7.3). |
| weatherBOM code | Vendor the needed functions. Licence: **upstream `mevers/weatherBOM` confirmed MIT** (DESCRIPTION + LICENSE.md, © 2021 "weatherBOM authors"; commit `4696bcf`), so vendoring is clean; retain the MIT notice and credit Maurits Evers (`role = "ctb"`). Note the compass helpers are `compass2angle()`/`angle2compass()`, not `compass_angle()` (§10). |
| Name | **`meteoTidy`** — camelCase retained as the family style, and the `meteo*` prefix matches meteoHazard directly; the lowercase typo tax is a recorded, accepted trade-off (§12). Availability checks must be re-run — the original sweep tested `tidyMeteo` (§12, §14). |

## 3. Canonical data model

- **Site registry** — S7 object (matching meteoHazard's S7-plus-`units`
  constructor idiom): `site_id`, lat/lon, elevation, timezone, attached
  sources, sensor heights per instrument, surface roughness length `z0` per
  wind instrument (plus displacement height where relevant — the §6/§7.1
  height corrections are meaningless without it), storage root, and a cache of resolved
  external station identifiers (BOM geohash and AAC/product codes, nearest
  GHCNh station, SILO grid cell). Multi-site = a list of these; every stored
  row is keyed by `site_id`.
- **Canonical observation table** — long format:
  `(site_id, datetime_utc, variable, value, source, method, qc_flag)`,
  with a widening helper that emits the Open-Meteo-named wide hourly table
  meteoHazard consumes (§3.1). Observations are stored at native resolution
  (e.g. 10-minute logger data); the curation layer aggregates to the hourly
  and local-day daily products. Long format is what makes multi-source provenance,
  Parquet partitioning, and dashboard queries clean.
- **Canonical variable dictionary** — internal names + canonical units + valid
  ranges + **statistical class** (linear / circular / bounded / intermittent /
  clear-sky-indexed) + **measurability class**, used by QC, fill, and
  correction dispatch. Measurability classes:
  - *site-measurable* — a site AWS instrument can observe it (temperature,
    RH, pressure, precipitation, 10 m wind/gusts);
  - *derived-measurable* — observable indirectly at the site (direct/diffuse
    radiation via a pyranometer's global irradiance plus a split model, §7.3);
  - *donor-observable* — not measurable by a typical site AWS but observed at
    nearby professional stations (cloud cover via airport METAR/ceilometer);
  - *model-only* — no surface network observes it (80/120/180 m winds,
    boundary-layer height).

  The dictionary is **user-extensible** (registering a variable supplies
  name, units, ranges, and both classes); built-ins cover the §3.1 contract
  plus the variables the site risk models already consume (layered soil
  moisture, CAPE, UV index, MSL pressure).
- **Forecast archive table** —
  `(site_id, source, model, issue_time, valid_time, lead_time, member, variable, value)`.
  `model` is nullable (the edited BOM forecast product has no model name).
  `member` is a nullable *integer*: `NA` for deterministic forecasts, the
  member number for ensembles. Summary statistics (`"mean"`, `"p10"`, …) live
  in a separate `stat` column (never both non-NA; deterministic rows have both
  `NA`) —
  overloading `member` with reserved strings would force the column to
  string type and conflate data with derived statistics. The member/stat
  scheme is what lets long-range ensemble products (§5.2) share the
  schema. `value` is numeric; the non-numeric elements of the BOM edited
  product (précis `short_text`/`extended_text`, fire-danger and UV
  categories) are archived in a companion `forecast_aux` table keyed the same
  way — downstream reports quote them verbatim.
- **QC flag** — closed enumeration (`ok`, `suspect`, `fail`, `missing`), not
  free text, so downstream filters are stable. QC status and production
  method are deliberately separate axes: gap-filled values are identified by
  `method` (e.g. `donor_fill`, `model_fill`), never by a QC state — an
  `estimated` QC value would conflate "how was this produced" with "is it
  trustworthy" and make measured-data-only filters depend on two columns
  agreeing.
- **Timezone rule** — storage and arithmetic in UTC. Site-local time appears
  only for display and at the daily-aggregate boundary, where it is **local
  clock time (civil time, DST-inclusive — corrected 2026-07-05; an earlier
  draft wrongly said local standard time)**: BOM's 9am observation has been
  made at local clock time since 1973/74 (BOM's DST-observing-practice notes
  record the earlier variations) and SILO inherits that convention, so the
  9am rain day shifts by an hour with DST in the states that observe it.
  Daily aggregation therefore uses the site's IANA timezone directly — DST
  transitions give 23/25-hour days, matching the source products — whereas a
  fixed standard-time offset would sit one hour off the SILO/BOM day for the
  whole DST half-year. Pre-1970s records observed under other conventions
  are a known historical wrinkle, not a design driver.

### 3.1 The meteoHazard wide contract (load-bearing)

meteoHazard's odour / dust / litter pipelines consume a wide hourly table with
these exact Open-Meteo-named columns (verified against meteoHazard's current
`R/` sources on 2026-07-05, and **re-verified against a fresh clone the same
day — the re-check found the original list incomplete**: `odour_hazard()`
*requires* `pressure_msl` and the layered `soil_moisture_0_to_1cm` /
`soil_moisture_1_to_3cm` (litter/dust hazard also use the 0–1 cm layer), and
`ventilation_state()` optionally consumes `wind_direction_80m/120m/180m` for
overnight residual-wind. `direct_radiation` and `diffuse_radiation` are
required by `odour-ventilation.R`, `odour_exposure.R`, `odour_hazard.R`, and
`generate_twl.R`):

```
time (UTC), temperature_2m, relative_humidity_2m, surface_pressure,
pressure_msl, precipitation, cloud_cover, direct_radiation,
diffuse_radiation, wind_speed_10m, wind_direction_10m, wind_gusts_10m,
wind_speed_80m, wind_direction_80m, wind_speed_120m, wind_direction_120m,
wind_speed_180m, wind_direction_180m, boundary_layer_height,
soil_moisture_0_to_1cm, soil_moisture_1_to_3cm
```

The contract pins **units as well as names** (confirmed against meteoHazard's
`R/openmeteo.R` and downstream sources, 2026-07-05 — wind **m/s**, temperature
**°C**, pressure **hPa**, precipitation **mm**, radiation **W/m²**, RH/cloud
**%**, direction **degrees**, BLH **m**, soil moisture **m³/m³**; these match
the variable dictionary):
Open-Meteo's API **defaults to km/h** wind speeds, so a name-only contract
carries a silent unit-mismatch failure mode — adapters must request
`wind_speed_unit=ms` explicitly (meteoHazard already hardcodes exactly this),
and the widening helper enforces canonical units from the variable dictionary.
Note meteoHazard consumes both `surface_pressure` (via `generate_twl()`) and
`pressure_msl` (via `odour_hazard()`), both in hPa; both are in the wide
contract and the dictionary.

**Model-only subset:** `wind_speed_80m/120m/180m`,
`wind_direction_80m/120m/180m`, `boundary_layer_height`, and the
`soil_moisture_*` layers (Open-Meteo model soil layers — a typical site AWS
has no matching probe; the dictionary is user-extensible if a site gains
one). No site AWS can measure these, so "best available truth" is undefined
for them; the correction policy is in §7.3.
`cloud_cover` is donor-observable (airport METAR via GHCNh) and
`direct_radiation`/`diffuse_radiation` are derived-measurable where a
pyranometer exists — both get partial site anchoring rather than none.

### 3.2 Contract evolution (meteoHazard changes are in scope)

The wide emitter returns a **classed tibble** — an S3 class extending
`tbl_df` (the sf/tsibble pattern), *not* an opaque S7 wrapper. It is still a
tibble, so dplyr/ggplot2 work untouched, but it carries validated metadata:
per-variable provenance (correction tier applied — `raw`, `physical`,
`mean_bias`, `qmap`, `emos` — training-overlap length, source), the
site/window keys, and the schema + calibration-manifest versions that
produced it. `dplyr_reconstruct()` methods keep the class and attributes
alive through wrangling; operations that genuinely invalidate the metadata
downgrade the object *visibly* rather than dropping attributes silently (the
failure mode of a plain attribute-carrying tibble under `bind_rows()`, joins,
etc.). One honest limit: `dplyr_reconstruct()` cannot detect in-place
mutation of a provenance-carrying column — `mutate(temperature_2m = …)`
keeps the class and now-stale per-variable provenance — so the metadata is
**authoritative only at validated boundaries** (meteoHazard re-validates on
entry; per-column content hashes carried in the attribute make staleness
detectable there), not through arbitrary wrangling. S7 is reserved for
domain objects (site registry, adapters); tabular
data stays a classed tibble.

meteoHazard **dual-accepts**: the classed input is trusted (validated once at
the boundary, no defensive re-checking) with provenance rules enforced; a
plain tibble is still accepted but validated on entry, with provenance
treated as unverified. Concrete uses of the class:

1. **Single-provenance-class derived indices.** Research on operational
   post-processing (§7.3) says mixed corrected/uncorrected tables are normal
   (NOAA's National Blend, BoM's IMPROVER, EPA's regulatory MMIF pathway all
   ship them) and the accepted mitigation is documentation plus consistent
   *use*: each derived quantity should be computed within one provenance class
   — e.g. stability from corrected surface variables; a ventilation index from
   raw model wind × raw model BLH — rather than mixing a corrected 10 m wind
   with an uncorrected 80 m wind, which can imply physically impossible shear.
   With guaranteed provenance, meteoHazard can *enforce* this (warn when a
   derived index mixes tiers) instead of merely documenting it.
2. **Uncertainty display.** Dashboards can badge values by tier.
3. **Audit trail.** Calibration-manifest versions and forecast issue times
   ride along into regulator-facing hazard reports, recording exactly which
   data produced which assessment.

The Open-Meteo request plumbing currently inside meteoHazard
(`fetch_openmeteo()`, `om_request()`, `om_perform()` in `R/openmeteo.R`)
migrates into meteoTidy; meteoHazard keeps a thin shim or takes the
dependency.

## 4. Curated products

- **`history_daily`** — century+ daily series per site. Base: SILO (gap-free
  by construction), site-corrected against AWS daily aggregates; AWS values
  win where present and QC-clean. The most recent ~weeks may be served before
  full cleaning. Consumers: history dashboard, climatological bounds for QC,
  calibration priors. **Not a homogenized climate record**: the AWS
  installation date introduces a step change (corrected SILO before, measured
  AWS after) — fit for operational bounds and priors, unfit for trend
  analysis; the product documentation must say so.
- **`history_hourly`** — hourly series from the beginning of AWS records.
  Base: AWS, QC-corrected and gap-filled from surrounding stations (§6). The
  most recent ~weeks may be served before full cleaning.
- **`forecast_hourly`** / **`forecast_daily`** — the coming ~fortnight
  (including today), from BOM and/or Open-Meteo (or any configured source),
  corrected for past skill at predicting `history_hourly` / `history_daily`.
- **`forecast_longrange`** — weekly-to-seasonal horizon (out to ~7 months)
  from ensemble sources (§5.2), served as calibrated percentiles rather than
  deterministic values. **Per-member trajectories stay retrievable alongside
  the summaries** (for all ensemble products, short-range included): correct
  cumulative uncertainty bands require accumulating each member's path and
  *then* taking quantiles — daily percentiles cannot be validly summed across
  days.
- **Calibration training store** — matched `(archived_forecast, observation)`
  pairs. Consumer: `met_refit()`. **Retention is independent of any rolling
  data window** and must cover at least the longest calibration training
  window (≥ 2 yr for the EMOS tier) — training pairs are never expired by the
  policies that trim the live record.

All curated products carry per-value provenance `(source, method, qc_flag)`
plus the correction tier where a correction was applied.

## 5. Acquisition layer (adapters)

Adapter = S7 class implementing `fetch(site, variables, window)` → canonical
long table. The contract is exported so users can write adapters for anything
else (Davis WeatherLink, Campbell, vendor clouds) without touching package
internals. Built-ins:

| Adapter | Wraps | Cadence | Notes |
|---|---|---|---|
| `source_rest()` | generic REST AWS | hourly | User supplies endpoint template (site/window/var interpolation), auth (none / static header token / basic), response mapping (JSON paths or CSV columns → variable dictionary), units, sensor heights, timezone. Deliberately constrained (§13). |
| `source_file()` | logger CSV/TSV export drops | daily | Same mapping spec, file glob instead of endpoint. Many AWS only export files. |
| `source_openmeteo()` | migrated meteoHazard code + new endpoints | hourly | Forecast, **Ensemble** (multi-model members incl. ECMWF IFS 0.25°/AIFS, ICON Seamless, GEM, MOGREPS-G, ACCESS-GE — the full roster is larger, keep the model list extensible), Historical Weather (ERA5 1940+, ~5-day lag), Historical Forecast, Previous Runs, Single Runs, Seasonal (§5.2, §7.2). Licensing note in §10. |
| `source_bom_forecast()` | official product feeds + vendored weatherBOM code | per issue cycle | Daily précis forecasts (7-day) via the **official anonymous-FTP/product feeds**; hourly forecasts (~2 days) only via the unofficial web API (opt-in, §5.1). Fetched **and archived** every sync — BOM keeps no public forecast archive. |
| `source_bom_obs()` | official 72-h station JSON feeds + vendored weatherBOM code | hourly | Primary transport: the official per-station rolling 72-h observation JSON product feeds; web API as opt-in fallback and for geohash/location search. |
| `source_silo()` | `weatherOz` (≥ 3.0.0) PatchedPoint / DataDrill | daily | Free; API key = email (PII, not a secret, §11). Note weatherOz 3.0.0 made breaking changes (DPIRD column restructure, BOM ag-bulletin functions removed) — pin the minimum version. SILO's per-value source/quality codes are ingested into provenance, and PatchedPoint revises values retrospectively — handled by the §9 re-fetch window. |
| `source_ghcnh()` | `worldmet` (≥ 1.0.0) `import_ghcn_hourly()` | daily update; obs latency undocumented | Official-quality hourly obs. **GHCNh has superseded NOAA ISD** (NCEI's stated replacement): the ISD hourly feed's last update was 2025-08-24 and ISD services retire 2026-07-31. GHCNh is updated daily but publishes no real-time lag figure. Feeds `history_hourly` backfill and the fallback ladder, not the live head. |
| `source_ecmwf()` | ECMWF Open Data | medium-range ensemble (`enfo` stream), per issue | **v1 (medium-range only).** Fully open (CC-BY 4.0) since 2025-10-01. **Live-verified 2026-07-06:** the free open-data catalogue exposes `oper`/`enfo`/`waef`/`wave` (+ `aifs-*`), **not** the 46-day extended-range `eefo` stream — so the adapter ships defaulting to `enfo` (~15-day/360 h, 50 perturbed members `1..50`, no separate control) and `eefo` remains an accepted-but-currently-404 value. **46-day long-range coverage is therefore provided only by `source_openmeteo(product = "seasonal")`** (EC46+SEAS5 splice). GRIB2 read via terra/GDAL's GRIB driver — needs GDAL built with **libaec/CCSDS** support (§13); heavy deps in Suggests. |

### 5.1 BOM channels — facts as of July 2026

There is still no free, official, programmatic channel that covers everything
wanted, but the channel inventory is more differentiated than "official vs
scraped":

1. **Official product feeds (anonymous FTP `ftp.bom.gov.au/anon/gen/fwo/` and
   HTTP mirrors on `reg.bom.gov.au`)** — free, non-commercial, no SLA, listed
   in BOM's own data-feeds catalogue. Carry the 7-day précis forecast XML,
   warnings, and rolling 72-h per-station observation JSON. Live-verified
   2026-07-05. **This is the preferred transport wherever its products
   suffice** — it is an official channel, unlike the website API.
2. **`api.weather.bom.gov.au/v1` (undocumented website API)** — live-verified
   working 2026-07-05. What the community around the reference Python
   implementations (tonyallan/weather-au, the Home Assistant
   bremor/bureau_of_meteorology integration) has established:
   - The embedded *"You must not use, copy or share it"* metadata has been
     there since **~April 2022** — four years with zero enforcement; no IP
     ban, User-Agent block, or cease-and-desist against the JSON API has ever
     been confirmed.
   - BOM's written position (May 2022, quoted in bremor issue #109): the API
     "is not intended for direct access"; the sanctioned free channel is the
     anonymous FTP (item 1); public APIs were promised "no definite
     timeframe" and remain unfulfilled.
   - BOM's redesigned website (2025-10-22) runs entirely on a separate
     gateway (`api.bom.gov.au/apikey/v1/…`) — currently usable *without* a
     key but User-Agent-gated and CORS-restricted, and the community
     consensus is not to build on it because real authentication could be
     switched on at any moment. The first amputation of the old stack has
     already happened: its radar/mapping tiles were shut off without notice
     in **Dec 2025**.
   - Community consensus: the old API survives only as long as BOM's legacy
     mobile/app backends do — "run it until it dies, keep FTP as the
     lifeboat," which is exactly this package's posture.

   This is the same class of access that got `bomrang` archived. Treat as:
   **opt-in, documented as at-your-own-risk, with graceful degradation.** It
   is the only free channel for *hourly* BOM forecasts and for
   geohash/location search (the API is keyed by geohash, not lat/lon —
   resolve and cache in the site registry).
3. **Registered User Services** (register via webreg@bom.gov.au under the Data
   Licence Agreement) — ADFD / ACCESS gridded NWP and other products, mix of
   free and charged. The closest thing to a guaranteed channel; credentials
   are secrets (§11). **v1 uses no paid or registration-gated services** —
   investigating ADFD gridded products is deferred to post-v1.
4. **GHCNh** (via `worldmet`) — official-quality hourly observations, updated
   daily but with no published real-time latency figure (treat as best-effort,
   up to ~a week); the backfill spine and last-resort fallback.

**BOM transport ladder.** The BOM adapters try transports in configured
order, with a circuit breaker: persistent failure (404/410/DNS-gone across
several consecutive runs) trips fallback to the next rung; transient
5xx/timeouts only retry. Every stored value's provenance records the
transport that served it, so switches are visible and datable.

1. **Official product feeds first wherever equivalent** (72-h obs JSON, daily
   précis XML) — lowest ToS exposure and the channels most likely to persist.
   The rest of the ladder applies to products nothing official carries:
   hourly forecasts and geohash/location search.
2. **`api.weather.bom.gov.au/v1`** (current web API) — opt-in; run until it
   dies.
3. **`api.bom.gov.au/apikey/v1`** (new gateway) — same opt-in flag; built in
   advance so fallback is config, not an emergency patch. Requires a
   browser-like User-Agent (a deliberate disguise — the most legally awkward
   rung; documented plainly) and its own endpoint map (paths and schemas
   differ). Dies the day BOM enables real authentication.
4. **Source substitution, not transport substitution:** if all BOM web
   channels are gone, the FTP précis still supplies the regulator-cited
   product at *daily* resolution; only sub-daily granularity falls to
   Open-Meteo, which is model output — a different source with separate
   calibration fits, flagged as such in provenance. A quality fallback, not
   a compliance fallback.

Degradation story: the site AWS is the primary near-real-time source; BOM
feeds only fill AWS outages in the live head. If every BOM real-time channel
breaks, live-head gap-fill quality degrades for up to ~a week until GHCNh
catches up (GHCNh's operational latency is undocumented — treat that window as
an upper estimate). Near-real-time is therefore a **best-effort** target, not a
guarantee.

### 5.2 Long-range sources

- **Open-Meteo Seasonal API** — rebuilt on ECMWF in Nov 2025: **EC46**
  (46 days, daily issuance) spliced with **SEAS5** (7 months, monthly
  issuance — Open-Meteo's docs flag a move to twice-monthly during 2026),
  51 ensemble members, ~36 km, with daily/weekly/monthly
  aggregations, anomalies, and probability products. The splice means the
  underlying model switches at day ~46 of every issuance: archive rows must
  record the underlying model (`ec46` / `seas5`) per row, and calibrations
  are fitted per underlying model, never per spliced product. **Critical caveat:
  individual members are retained only ~1 month and there is no issue-time
  archive product for the seasonal API** — past issuances are effectively
  unretrievable, so meteoTidy must snapshot every issuance itself (which the
  archive-on-every-sync policy already does). This is the v1 live source: JSON
  transport consistent with the other adapters.
- **ECMWF Open Data** — the real-time catalogue is open (CC-BY 4.0) since
  2025-10-01 at 0.25° with an AWS mirror. **Live verification (2026-07-06)
  corrected the scope here:** the *free* open-data catalogue exposes the
  medium-range `enfo` ensemble (~15-day/360 h, 50 perturbed members) plus
  `oper`/`waef`/`wave` and the `aifs-*` families — but **not** the 46-day
  extended-range `eefo` stream (which is a real MARS/dissemination stream, just
  not in the free feed). So `source_ecmwf()` ships defaulting to `enfo`;
  `eefo` remains an accepted parameter value that currently 404s until/unless
  ECMWF opens it. 0.25° is the current open resolution; ECMWF has flagged a
  future move to 0.125°/9 km, so don't hard-code the grid. **In scope for v1**
  as a medium-range channel: it is the only channel whose issue-time archive
  the deployment fully controls. **The 46-day long-range need is met by the
  Open-Meteo seasonal splice alone (§5.2), not by ECMWF, in v1.** Real
  open-data ships **one GRIB2 + `.index` pair per forecast step** (not one per
  cycle); each `.index` is JSON-lines (per-message `_offset`/`_length`)
  enabling per-field/per-member HTTP range fetches. GRIB2 is the implementation
  cost — plan on terra/GDAL's GRIB driver rather than an ecCodes system
  dependency, but the driver needs GDAL built with **libaec/CCSDS** support
  (§13).
- **BOM long-range outlooks (ACCESS-S)** — *no open programmatic channel
  exists*: public pages serve only PNG map products; the operational gridded
  outlook is Registered User (likely paid) territory; the new website's outlook
  pages sit behind the key-gated gateway. Out of scope until an open data
  channel exists. **ACCESS-S2 hindcasts (1981–2018) are freely available on NCI
  THREDDS/OPeNDAP** (`s2dprediction.nci.org.au`) — useful for
  calibration/verification research, not a live feed.

## 6. Curation layer (QC + gap-fill)

- ~10 WMO-style QC rules (range, step, persistence, internal consistency,
  climatological bounds from `history_daily`, and **spatial/buddy checks
  against the configured donor stations** — the deployment already knows its
  donors, and spatial consistency is the highest-power test for the slow
  sensor drift that range/step/persistence all miss), custom-built (R's QC
  package landscape is thin), dispatched per statistical class from the
  variable dictionary. Solar QC names its clear-sky model (McClear or
  Ineichen–Perez) and applies BSRN-style limits.
- Tiered fill: micro-gaps ≤ 2–3 h via `imputeTS` (smooth variables only);
  medium gaps via the best bias-corrected donor (BOM station → GHCNh station →
  ERA5 → SILO disaggregated, with donors **deduplicated by physical station
  identity** — BOM real-time feeds and GHCNh serve largely the same stations,
  so the ladder ranks transports and must not count one station twice);
  macro-gaps / pre-installation via corrected
  model series.
- **Model-only variables skip the donor ladder entirely** — they are always
  the (raw or profile-rescaled) model value, per the §7.3 policy. Cloud cover
  uses the donor ladder via airport METAR where a suitable donor exists, else
  raw model.
- Gap-fill and forecast correction share one transfer engine for the
  transform machinery, but they are **not the same statistical problem**:
  gap-fill maps one *realized* observation to another, while forecast
  correction is conditional on lead time and must model skill decay — the
  forecast side adds lead-dependent shrinkage on top of the shared
  transforms (§7.1).
- Per-variable statistical treatment: dewpoint for RH; wind direction
  corrected as joint u/v (vector) components — never quantile-mapped as an
  angle; occurrence + amount for rain; clear-sky index for solar; height
  correction (via registry `z0`) before any wind statistics.
- **Incremental**: each run processes only the window since the last watermark
  (stored alongside the data); idempotent on re-run.

## 7. Correction layer (tiered calibration)

### 7.1 Tiers

| Overlap with site truth (see gate note below) | Method |
|---|---|
| Day 0 (none) | Physical adjustments only: sensor height, altitude/lapse, wind-profile height correction. Always applied; never fitted; superseded by fitted corrections once overlap exists. Known-crude and documented as such: a fixed environmental lapse rate is wrong under nocturnal inversions — exactly the stable inland regime §7.3 flags as most consequential — and the log-wind profile assumes neutral stratification and requires the registry's `z0`; extrapolating a 2–3 m farm mast to 10 m through a neutral log law is likely the largest day-0 wind error. |
| 1–6 months | Mean bias + variance scaling with **harmonic day-of-year and hour-of-day covariates** (sin/cos pairs), not raw hour-of-day bins — a fit from one season applied unshrunk year-round can carry the wrong sign by the opposite season. |
| 6 mo – 2 yr | Empirical quantile mapping per hour block (`qmap`) with **cross-season pooling/shrinkage** (six months of overlap leaves whole seasons untrained) and an explicit **tail policy**: constant-shift extrapolation beyond training support, never unbounded. |
| ≥ 2 yr + archive | Regression MOS/EMOS per lead-time bucket (`crch`), optional multivariate (`MBC`). |

- Tier selection is automatic per `(site, source, variable)` and enforced —
  the tier gates guard against small-sample overfitting. Promotion to a
  higher tier additionally requires **out-of-sample skill improvement** over
  the incumbent (rolling-origin verification surviving the block bootstrap,
  §7.4): data volume alone never demonstrates that the more complex method
  has stopped overfitting.
- **Forecast application adds lead-dependent shrinkage.** Empirical QM is
  variance-preserving — it keeps full forecast variance at every lead —
  while the correct behaviour as skill decays is shrinkage toward
  climatology; at day 5–7 unshrunk QM can verify *worse* than mean-bias or
  raw. Applied to forecasts, the QM tier therefore blends toward climatology
  with a per-lead-bucket weight set from verified skill; EMOS supersedes it
  partly because regression handles this natively. Gap-fill (both series
  realized, no skill decay) uses the unshrunk transforms.
- **Post-correction consistency pass.** Independent univariate corrections
  can emit physically impossible combinations: gusts below mean wind,
  dewpoint above temperature, RH > 100 %, direct + diffuse above the
  clear-sky ceiling. Every correction application ends with a cheap
  constraint-enforcement pass; violations are clipped and *counted* — a
  rising violation rate is itself a verification red flag (§7.4).
- **What gates the tiers is training-pair availability, not AWS age per se.**
  For Open-Meteo sources the Previous Runs archive supplies ~2 yr of
  issue×valid pairs at daily leads from day 0 (§7.2), so daily-lead EMOS can
  start immediately — but its truth side is then the SILO-anchored
  `history_daily`: interpolated pseudo-truth, not the site AWS. This is
  deliberate, and distinct from the ERA5-as-pseudo-truth rejection in §7.3 —
  SILO is station-interpolated observation, not model output, so there is no
  model-vs-model circularity — but day-0 fits inherit SILO's site error and
  are refit as AWS overlap accrues. Hourly correction has no such shortcut:
  the Historical Forecast API is shortest-lead-only, so early hourly
  corrections are short-lead-trained and applied across all leads —
  documented as overconfident at long leads until the local archive matures.
- Fitted calibrations are versioned and persisted **as data** — coefficient
  and mapping tables (Parquet/JSON) under the site's storage root, tracked by
  a manifest (version, fit date, training window, tier) — never as serialised
  `.rds` model objects, which are fragile across R and package versions on a
  decade-scale deployment. The daily pipeline only *applies* the current
  version; the monthly job *refits* and bumps the manifest (§9).
- SILO correction is a separate daily-scale QM fit.

### 7.2 Training data — what each archive can actually support

Verified against Open-Meteo documentation and live probes, 2026-07-05:

- **Open-Meteo Previous Runs API** — issue×valid pairs at **daily lead
  granularity (0–7 days)**, most models back to ~Jan 2024 (GFS 2 m temperature
  to 2021, JMA to 2018; ACCESS-G is **not** separately dated — it inherits the
  generic Jan-2024 baseline, so don't assert a per-model ACCESS-G date). This
  is the only ready-made source of
  lead-aware training pairs at adoption time: **EMOS at daily lead buckets
  works from day 0** for Open-Meteo sources — trained against
  `history_daily` pseudo-truth until AWS overlap accrues (see the tier-gate
  note in §7.1). Sub-daily issue cycles collapse
  into daily offsets, so hourly-lead-resolved calibration cannot be trained
  from it.
- **Open-Meteo Single Runs API** — full per-run forecasts queryable by
  initialisation time (exact "what did the forecast say at T for V"), but the
  archive is shallow: most models from 2026-04-02, ECMWF IFS from 2024-03-14
  (dates confirmed against the docs, 2026-07-05).
  Whether every daily cycle is retained is **not documented** (confirmed absent
  from the docs) — verify empirically at backfill before relying on it.
  **The long game is local archiving; start it immediately.**
- **Open-Meteo Historical Forecast API** — a stitched series of shortest-lead
  values (ACCESS-G since 2024-01-18, ECMWF IFS HRES since 2017, GFS since
  2021); issue time is not resolvable, so it serves as a short-lead proxy /
  backfill for the training store, not for lead-aware training. Docs caution
  against long series across model-version changes.
- **ACCESS-G raw model ≠ BOM edited forecast.** Open-Meteo archives BOM's raw
  ACCESS-G numerical output; the edited BOM forecast product regulators cite
  is a different object with **no public archive** — its training pairs exist
  only from the day local archiving starts.
- **Seasonal calibration** is hindcast-anchored: SEAS5 hindcasts via the
  Copernicus CDS and ACCESS-S2 hindcasts via NCI THREDDS provide the
  historical forecast–observation pairs that no live archive can (monthly
  issuance means locally accumulated pairs accrue far too slowly). Note the
  member-count mismatch: SEAS5 hindcasts run 25 members against 51
  operational, so spread calibration fitted on hindcasts does not transfer
  directly — one more reason this layer ships flagged experimental. v1 ships
  issuance archiving, verification, and a simple lead-dependent
  mean/variance calibration against `history_daily`; anything richer is
  explicitly experimental. This is the least mature layer — flagged as such.

### 7.3 Model-only variables (research-informed policy)

The design question: surface variables get site-specific correction, but
`wind_speed_80m/120m/180m` and `boundary_layer_height` have no site truth, so
consumers would ingest a mix of corrected and uncorrected values. Findings
from operational practice and the literature:

- Operational systems (NOAA National Blend, BoM's own IMPROVER, EPA's
  regulatory MMIF/AERMOD pathway) all ship mixed calibrated/uncalibrated
  products; the accepted mitigation is documentation and consistent use, not
  forced correction. EPA explicitly endorses passing raw model boundary-layer
  height into dispersion modelling.
- 10 m→hub-height inference (log/power law, MCP, ML) needs target-height truth
  to be reliable, and fails worst in stable/decoupled conditions — nocturnal
  low-level jets are frequent inland Australia and are exactly the regime that
  matters most for odour dispersion. A site's 10 m bias is largely local
  siting/roughness signal with no physical claim on the flow at 120–180 m.
- Univariate correction of some variables while leaving related ones raw can
  break inter-variable physics (implied shear); the multivariate
  post-processing literature (Schaake shuffle, ensemble copula coupling, MBC)
  resolves this by *keeping the model's dependence structure and correcting
  the marginals*.
- ERA5-as-pseudo-truth is rejected for correction: model-vs-model circularity,
  ~31 km resolution, ~5-day latency, and documented BLH biases in stable
  conditions. Permitted only for clearly labelled climatological sanity
  checks.

Policy:

1. **Default: pass model-only variables through raw**, with the correction
   tier recorded as `raw` in the §3.2 provenance attribute. Never worse than
   the model; matches operational precedent.
2. **Opt-in, experimental: `profile_rescale`** — multiply 80/120/180 m winds
   by the (corrected 10 m)/(raw 10 m) ratio, damped with height, capped, and
   suppressed under stable stratification (e.g. night + low corrected 10 m
   wind). Preserves the model's shear ratios — the deterministic analogue of
   the "correct marginals, keep model dependence" principle — but no published
   method does exactly this, so it ships as an experiment to verify per site,
   not established practice.
3. **Optional diagnostic BLH** — an AERMET-style boundary-layer height
   recomputed from *corrected* surface variables (heat-flux / friction-velocity
   based), served alongside the raw model BLH. This is the construction
   regulatory dispersion modelling already trusts, and gives consumers a
   surface-consistent alternative.
4. **Radiation:** where a pyranometer exists, correct global irradiance via
   the clear-sky index, then re-split direct/diffuse preserving the model's
   split ratio (or a decomposition model, e.g. BRL); without one, raw model
   values with `raw` provenance.
5. **Roadmap:** if a site ever gets a lidar/sodar campaign, target-height
   observations slot into the same tier machinery as first-class truth —
   design the correction API so measurement height is data, not schema.

### 7.4 Verification

Produced by the monthly refit as a report dashboards can render, built on
`scoringRules`: MAE/CRPS by lead time, per variable, before/after
correction. Design requirements:

- **Out-of-sample by construction** — rolling-origin evaluation; every score
  is computed outside the fit's training window. A refit verified on its own
  training period inflates skill and would corrupt the §7.1 tier gates,
  which this machinery enforces.
- **Against baselines, not just before/after** — raw model, persistence, and
  climatology. "Corrected beats raw" is not evidence of value at leads where
  climatology already wins.
- **Calibration diagnostics for probabilistic products** — rank
  histograms/PIT and spread–error ratio alongside CRPS (CRPS alone conflates
  sharpness with reliability), plus Brier scores for rain occurrence.
- **Uncertainty on skill differences** — block bootstrap (verification
  series are autocorrelated); tier promotion requires the improvement to
  survive it (§7.1).

Long-range products verified as probabilistic forecasts (CRPS, PIT against
`history_daily`).

## 8. Storage

**`arrow` Parquet datasets, hive-partitioned, one directory tree per
deployment:**

```
<store>/observations/site_id=X/year=2026/…parquet
<store>/forecasts/source=bom/site_id=X/issue_date=2026-07-04/…parquet
<store>/calibrations/site_id=X/manifest.json + <variable>-<source>-<ver>.parquet
```

Forecast files carry `issue_time`, `valid_time`, and `member` columns, so
deterministic short-range and ensemble long-range products share one dataset.
Rationale: file-based (no server — fits user-scheduled, per-site deployment),
append-friendly, partition-pruned time-range queries fast enough for
dashboards, readable directly by DuckDB (`duckdb` + `arrow` zero-copy) when
dashboards want SQL, and portable to S3/GCS later without an API change.
DuckDB-as-primary-store is the alternative (single file, real SQL, concurrent
readers) — slightly better for a future central multi-site server, slightly
worse for rsync-able simplicity. Default arrow; allow a DuckDB backend behind
the same read API if needed. Partition compaction runs monthly (§9).

## 9. Pipeline cadence

Exported verbs; the user schedules them (cron / GitHub Actions /
`taskscheduleR` — vignette, not package code):

| Cadence | Verb | Work |
|---|---|---|
| hourly (optional) | `met_sync_live()` | Fetch AWS + near-real-time BOM head; QC + fill the live window; apply calibrations; archive any newly issued forecasts. |
| daily | `met_sync_daily()` | Fetch/refresh all forecasts (Open-Meteo incl. seasonal, BOM) and archive new issuances; extend `history_hourly`; pull GHCNh backfill; refresh the `history_daily` tail from SILO. |
| monthly | `met_refit()` | Refit calibrations, tier re-check, verification report, Parquet compaction. |
| ad hoc | `met_backfill()` | Day-0 bootstrap: full SILO + ERA5 + Open-Meteo Previous-Runs/Historical-Forecast pulls; ingest historical AWS exports and any pre-existing ad-hoc forecast archives; initial fits; per-site donor-coverage audit (incl. GHCNh completeness for the nearest stations). |

- All verbs are multi-site (`sites` argument), incremental (watermarks), and
  idempotent.
- **Forecast archiving policy:** every sync verb checks each configured
  forecast source for new issuances and archives them, deduplicating on
  `(source, model, issue_time)`. Users running hourly sync capture most BOM
  issue cycles; daily-only users get daily snapshots. Open-Meteo gaps
  self-heal via Previous/Single Runs backfill; **BOM gaps cannot be
  backfilled** (no public archive) — document this trade-off in the
  scheduling vignette. EC46 (daily) and SEAS5 (monthly) issuances are
  captured by `met_sync_daily()`.
- **Observation revision policy:** watermarks alone never see retroactive
  changes, and sources make them — SILO revises PatchedPoint values as
  station QC lands, BOM observation feeds carry corrections, GHCNh is
  reprocessed. Each source therefore gets a rolling **re-fetch window**
  behind the watermark (sized per source; SILO revisions can reach months
  back), and a re-fetched value that differs **supersedes rather than
  overwrites**: the prior value is retained and marked, with provenance
  recording the revision. This is what makes the §3.2 audit trail real — a
  regulator-facing report stays reproducible as "what the store served at
  report time" even after upstream revisions. Calibrations are versioned
  (§7.1); without this, observations would not be.

## 10. Package structure & dependency posture

- **meteoTidy owns everything above.** `fetch_openmeteo()` / `om_request()` /
  `om_perform()` migrate out of meteoHazard (which keeps a thin
  backwards-compat shim or takes the dependency).
- **meteoHazard's interface:** one call — "give me the wide hourly table
  (§3.1) for site X, window W" (corrected forecast for prediction, curated
  record for hindcast), returning the provenance attribute (§3.2).
- **Dashboards' interface:** the long tables + forecast archive +
  verification reports (§11).
- **Vendored weatherBOM code (MIT, upstream `mevers/weatherBOM` confirmed MIT —
  commit `4696bcf`):** `bom_forecasts()`, `bom_observations()`,
  `bom_location_info()`, `bom_search_station()`, the **internal** endpoint
  constant `"https://api.weather.bom.gov.au/v1/"` (vendored **by value**, not
  via `weatherBOM:::endpoint`), and the compass helpers **`compass2angle()` /
  `angle2compass()`** (there is no `compass_angle()` — the earlier name was
  wrong) — trimmed to what the adapters need; MIT notice retained; Maurits Evers
  (© 2021 "weatherBOM authors") credited in `Authors@R`.
- **Data access:** `weatherOz` ≥ 3.0.0 (SILO), `worldmet` ≥ 1.0.0 (GHCNh —
  `import_ghcn_hourly()`; GHCNh support landed in 1.0.0, current CRAN 1.1.0).
  **Statistics:** `qmap`, `MBC`, `crch`, `imputeTS`, `circular`,
  `scoringRules`. **Storage:** `arrow` (+ `duckdb` in Suggests). **Objects:**
  S7 + `units`, matching the meteoHazard idiom. Heavy/optional things in
  Suggests where feasible.
- **Open-Meteo licensing (compliance flag):** the free tier is
  **non-commercial only** (< 10 000 calls/day, no key); commercial use
  requires a paid plan with an API key, and the historical, climate, ensemble,
  and satellite-radiation APIs all sit on the Professional tier and above. Data
  is CC-BY 4.0 (attribute); the server is AGPLv3 and self-hostable as an escape
  hatch. Deployments at commercial
  livestock operations plausibly count as commercial use — the package should
  accept an API key cleanly, document the boundary prominently, and never
  assume the free tier.

## 11. Read API, config & secrets

**Read API — ship both, with a clear stability boundary:**

- **Default: tibble-returning functions** (`met_history()`, `met_record()`,
  `met_forecast_archive()`, `met_verification()`). The stable, versioned
  contract: no connection lifecycle, trivially testable, backend-agnostic.
  Cost: materialises results in R memory.
- **Opt-in: `met_connect()`** returning a DuckDB/arrow connection (or `dbplyr`
  source) over the same Parquet tree, for dashboards needing SQL pushdown.
  Exposes the physical schema, so **marked experimental** — only the tibble
  surface is a promise, which relieves the API-stability pressure of having
  two dashboard consumers before maturity.

**Config & secrets:**

- **Non-secret config → version-controlled YAML** in the site's deployment:
  site registry, adapter endpoint templates and response mappings, storage
  root, resolved station IDs. Safe to commit.
- **Secrets → environment (`.Renviron`) or keyring**, referenced *by name*
  from the YAML (`token_env: MYSITE_AWS_TOKEN`), never inlined: generic-REST
  tokens, BOM registered-user credentials, Open-Meteo commercial API keys.
  The SILO "API key = email" is **PII, not a secret** — keep it out of public
  repos, but an env var suffices.
- Never write secrets into Parquet, manifests, or provenance fields.

## 12. Naming & release

The package is **`meteoTidy`** (decided 2026-07-05, superseding the earlier
`tidyMeteo` candidate): camelCase keeps the family style, and the `meteo*`
prefix matches meteoHazard directly. With `tidy` demoted to a suffix, the
earlier concern about implying official tidyverse affiliation largely
dissolves; the soft expectation the element still carries — tidy long
tibbles, pipe-friendly composition — is already the design.

**CRAN submission is de-scoped** (decided 2026-07-05): distribution is GitHub
(optionally r-universe); no CRAN submission is planned. The availability and
casing notes below are retained for collision avoidance and in case that
decision is ever revisited — at which point the §5.1 web-API/gateway rungs
would need a policy re-check first.

- **Availability re-checked for `meteoTidy` (2026-07-05):** verified free on
  CRAN (current + archive, both casings — CRAN's case-insensitive uniqueness is
  satisfied), Bioconductor, r-universe, GitHub (only this repo), and PyPI, with
  no trademark or unfortunate-meaning collisions (nearest names — Meteomatics,
  Meteodyn, generic "Tidy" — don't collide). The earlier sweep had tested the
  old `tidyMeteo` name; this supersedes it.
- **Casing:** camelCase goes against the R Packages (2e) / rOpenSci
  preference for lowercase — a recorded, accepted trade-off. With CRAN
  de-scoped the `install.packages("meteotidy")` failure mode is moot; the
  README/pkgdown pages still lead with the exact
  `pak::pak("vorpalvorpal/meteoTidy")` install command (GitHub installs are
  case-sensitive too).
- **Nearest neighbours:** `tidyweather` (CRAN, Feb 2026, agricultural weather
  analysis) remains the closest semantic neighbour — expect occasional
  confusion; `weatherOz` is the functional overlap — differentiate in
  DESCRIPTION (QC/correction/archiving vs API client) and interoperate rather
  than compete.
- **Pre-submission (only if CRAN is ever revisited):** run
  `available::available("meteotidy")` / `pak::pkg_name_check()` as a
  freshness check.

## 13. Risks

- **BOM web-API dependence.** The unofficial endpoint works today; its
  forbidding metadata has gone unenforced since ~2022, but BOM has stated the
  API is not for direct access, has already shut off the old radar tiles
  without notice (Dec 2025), and runs its new website on a separate gateway —
  shutdown with no notice is the realistic failure mode (§5.1). Mitigations:
  official product feeds preferred where they suffice, web API opt-in, GHCNh
  fallback, graceful degradation. The degradation window is up to ~1 week
  (GHCNh's latency is undocumented — treat as an upper estimate), so never
  promise real-time BOM data.
- **GHCNh unknowns.** GHCNh is updated **daily** and *does* include
  non-airport/cooperative stations (network codes C/M/N/L, HCN/GSN flags), but
  NCEI publishes no real-time latency figure and no airport-vs-cooperative
  proportion, and the network skews to hourly-reporting METAR/SYNOP sites — so
  `met_backfill()` still performs a per-site donor-coverage audit at onboarding
  rather than assuming coverage.
- **Open-Meteo licensing.** Commercial deployments need a paid plan (historical
  APIs: Professional tier). Design for keys; document; self-hosting is the
  escape hatch.
- **Seasonal layer maturity.** No third party archives issued seasonal
  forecasts; local snapshots accrue slowly; hindcast-anchored calibration is
  the only viable path. Shipped flagged experimental with verification-first
  emphasis.
- **Model-only variables.** Raw by default (defensible, precedented);
  `profile_rescale` is unvalidated synthesis — never default, verify per site.
- **Generic REST adapter scope creep.** v1 is constrained to: no-auth / static
  header token / basic auth, JSON or CSV bodies, single-page responses;
  everything else is a user-written adapter against the exported contract.
- **Two dashboard consumers → premature API-stability pressure.** Tibble read
  API minimal and stable; `met_connect()` experimental (§11).
- **GRIB2 in R** — `source_ecmwf()` is in v1, so this cost is committed.
  Mitigation: read GRIB2 through terra's GDAL GRIB driver (no ecCodes system
  dependency), with terra in Suggests and an informative error when absent.
  Confirmed on the dev machine (terra 1.8.70 / GDAL 3.8.5 reads GRIB2 without
  ecCodes). Two real-world caveats remain: (a) ECMWF's GRIB2 uses **CCSDS/AEC**
  compression, so the GDAL build must include **libaec** (older builds error —
  gdal #8108); the adapter runs a runtime GDAL-capability check. (b) GDAL
  exposes ensemble members as **flat bands** with member identity only in PDS
  metadata (`perturbationNumber`), so the adapter must demux members itself
  (`stars` may help). Spike this early; if the driver proves insufficient for
  the ensemble files, the adapter degrades to the Open-Meteo seasonal splice and
  full ECMWF support moves post-v1.
- **Timezone/DST** at the daily boundary (mitigated by matching the BOM/SILO
  9am local-clock-time convention, §3);
  **small-sample overfitting** (mitigated by enforced tier gates plus
  skill-gated promotion, §7.1/§7.4);
  **SILO daily↔hourly disaggregation** remains genuinely hard — donor-ladder
  position reflects that.

## 14. Verification tasks

All design questions are resolved. The July-2026 open questions were then
researched (2026-07-05); results are folded into §5–§13. What remains are
runtime/empirical checks and site-level experiments, not design decisions.

**Resolved by research (2026-07-05):**
- **Name availability** — `meteoTidy` verified free on CRAN (both casings +
  archive), Bioconductor, r-universe, GitHub, and PyPI, no collisions (§12).
- **weatherBOM licence** — upstream `mevers/weatherBOM` confirmed MIT
  (commit `4696bcf`; §2, §10). Correction folded in: the compass helper is
  `compass2angle()`/`angle2compass()`, not `compass_angle()`.
- **§3.1 unit pinning** — confirmed against meteoHazard's `R/openmeteo.R`; the
  dictionary units match, and Open-Meteo's wind parameter is
  `wind_speed_unit=ms` (meteoHazard hardcodes it) (§3.1).
- **worldmet** — GHCNh support is `import_ghcn_hourly()` in `worldmet` ≥ 1.0.0,
  current CRAN 1.1.0 (§10). GHCNh is NCEI's official ISD replacement, updated
  daily, and includes non-airport stations (§13).
- **ECMWF stream** — *superseded by live verification (2026-07-06):* the
  46-day extended-range `eefo` stream is a real MARS stream but is **not in the
  free open-data catalogue** (which carries `enfo`/`oper`/`waef`/`wave` +
  `aifs-*`). `source_ecmwf()` therefore ships the medium-range `enfo` ensemble
  (~15-day, 50 members) and the 46-day need is met by the Open-Meteo seasonal
  splice only (§5.2). Real open-data ships one GRIB2 + `.index` pair **per
  step**; terra/GDAL reads GRIB2 locally (given a libaec/CCSDS build).

**Remaining runtime/empirical checks:**
1. **Single Runs retention** — the docs do not document whether every issue
   cycle is retained (confirmed absent, 2026-07-05); verify empirically at
   backfill before relying on it for archive backfill (§7.2).
2. **GHCNh per-site coverage** — cadence and station-type inclusion are now
   known (§13); the *per-site* nearest-station completeness and effective
   latency are still audited by `met_backfill()` at onboarding.
3. **GRIB CCSDS / ensemble demux** — terra/GDAL reads GRIB2 on the dev machine;
   confirm the GDAL build handles ECMWF's **CCSDS/libaec** compression on a real
   ensemble file and that member demux via `perturbationNumber` works, early in
   v1 (§13).
4. **Experiments needing site-level verification** before being recommended:
   `profile_rescale` (§7.3) and the seasonal-calibration prototype (§7.2).

## Appendix — key sources

- Open-Meteo: [Historical Forecast API](https://open-meteo.com/en/docs/historical-forecast-api) ·
  [Previous Runs API](https://open-meteo.com/en/docs/previous-runs-api) ·
  [Single Runs API](https://open-meteo.com/en/docs/single-runs-api) ·
  [Seasonal API](https://open-meteo.com/en/docs/seasonal-forecast-api) ·
  [Terms](https://open-meteo.com/en/terms) · [Pricing](https://open-meteo.com/en/pricing)
- BOM: [data services](https://www.bom.gov.au/resources/data-services) ·
  [data-feeds catalogue](https://reg.bom.gov.au/catalogue/data-feeds.shtml) ·
  [registered users](http://reg.bom.gov.au/reguser/reguser.shtml) ·
  [ACCESS NWP data](http://reg.bom.gov.au/nwp/doc/access/NWPData.shtml) ·
  [unofficial API docs](https://github.com/trickypr/bom-weather-docs) ·
  [copyright-string & BOM-contact history](https://github.com/bremor/bureau_of_meteorology/issues/109) ·
  [new-gateway endpoint map](https://github.com/bremor/bureau_of_meteorology/issues/235)
- Observations: [GHCNh](https://www.ncei.noaa.gov/products/global-historical-climatology-network-hourly) ·
  [worldmet](https://cran.r-project.org/package=worldmet) ·
  [SILO API](https://www.longpaddock.qld.gov.au/silo/api-documentation/) ·
  [weatherOz](https://cran.r-project.org/package=weatherOz)
- Long-range: [ECMWF open data announcement](https://www.ecmwf.int/en/about/media-centre/news/2025/ecmwf-makes-its-entire-real-time-catalogue-open-all) ·
  [ACCESS-S2 hindcasts on NCI](https://s2dprediction.nci.org.au/)
- Model-only variable policy: [NBM verification](https://www.weather.gov/media/mdl/AMS2017-NBMVerification.pdf) ·
  [IMPROVER](https://journals.ametsoc.org/view/journals/bams/104/3/BAMS-D-21-0273.1.xml) ·
  [EPA MMIF guidance](https://gaftp.epa.gov/Air/aqmg/SCRAM/models/related/mmif/MMIF_Guidance.pdf) ·
  [AERMOD formulation](https://gaftp.epa.gov/aqmg/SCRAM/models/preferred/aermod/aermod_mfd.pdf) ·
  [ERA5 BLH evaluation (Guo et al. 2021)](https://acp.copernicus.org/articles/21/17079/2021/) ·
  [nocturnal LLJs & wind power](https://wes.copernicus.org/articles/7/1575/2022/)
- Naming: [R Packages (2e) on names](https://r-pkgs.org/workflow101.html) ·
  [rOpenSci dev guide](https://devguide.ropensci.org/pkg_building.html)
