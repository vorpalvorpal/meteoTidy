# Plan 10 — Curation: gap-fill / transfer engine

> **Extended (2026-07-08, see [`18-eccodes-and-derivation.md`](18-eccodes-and-derivation.md)).**
> A **deterministic derivation tier** is inserted in the fill ladder **between
> micro and donor**: a coupled variable (RH ↔ dewpoint ↔ temperature) whose
> physical inputs are all co-observed and QC-clean at a gap timestamp is
> computed exactly (`method = "derived"`) before any donor fetch. The
> micro/donor/model tiers below are otherwise unchanged.

## Objective

Implement tiered gap-filling and the shared **transfer engine** that both gap-fill
and forecast correction (Plan 12) build on — while keeping the review’s
distinction sharp: gap-fill maps one *realized* series to another (no forecast
skill decay), so it uses the transfer transforms **without** the lead-dependent
shrinkage that Plan 12 adds. Donors are deduplicated by physical station identity.
Model-only variables skip the donor ladder.

This plan also **assembles the curated products** (added 2026-07-05 — this work
previously had no home in the series): native-resolution → hourly aggregation,
hourly → local-day daily aggregation, the SILO+AWS compositing that builds
`history_daily` (SCOPING §4), and the SILO daily→hourly disaggregation the
donor ladder’s last rung needs (punted here by Plan 06).

## Scope

**In:**
- Gap detection (missing / `fail` / `missing`-flagged spans) per variable.
- The fill tiers: micro (interpolation), medium (donor), macro (model).
- The donor ladder with **dedup by station identity** (review fix).
- The shared transfer engine (`R/transfer.R`): fit a bias-correction transform on
  overlap, apply it — reused by Plan 12.
- Per-variable statistical treatment (dewpoint space, circular, occurrence+amount,
  clear-sky index, height correction before wind transfer).
- Aggregation: native-resolution → hourly, and hourly → daily on the local-day
  boundary (SCOPING §3).
- History-product assembly: `history_hourly` and `history_daily` (SILO base,
  AWS wins where present and QC-clean — SCOPING §4).
- SILO daily→hourly disaggregation (the donor ladder’s last rung).

**Out:**
- Forecast correction and its shrinkage (Plan 12) — but the transfer engine it
  shares is built here.
- Model-only correction policy (`profile_rescale`, diagnostic BLH — Plan 12); here
  model-only variables are simply the raw model value.

## Prerequisites

Plans 00–03, 09 (fills operate on QC’d data), 06 (donors).

## Background

SCOPING §6 (tiered fill: micro ≤ 2–3 h via `imputeTS` on smooth variables; medium
via best bias-corrected donor **BOM → GHCNh → ERA5 → SILO-disaggregated, donors
deduplicated by physical station identity**; macro/pre-install via corrected model;
**model-only skip the donor ladder**; cloud cover uses donor ladder via airport
METAR else raw model; gap-fill & forecast correction share one transfer engine but
are **not** the same statistical problem — the forecast side adds lead-dependent
shrinkage; per-variable treatment; incremental + idempotent), §7.3 (model-only
policy), §3 (local-clock-time daily boundary; `method` values), §4 (the
`history_hourly` / `history_daily` product definitions, the AWS-wins
compositing rule, and the not-a-homogenized-record caveat), §13 (SILO
daily↔hourly disaggregation is genuinely hard — its last-rung position
reflects that).

## File layout

```
R/fill.R                  # fill_run(): gap detection, tier routing, watermark, idempotency
R/fill-tiers.R            # micro/medium/macro implementations
R/transfer.R              # SHARED transfer engine: fit_transfer(), apply_transfer()
R/fill-treatments.R       # per-variable treatment (dewpoint/circular/rain/solar/height)
R/donor-ladder.R          # donor selection + dedup by station identity
R/aggregate.R             # native→hourly + hourly→local-day daily aggregation
R/history-products.R      # history_hourly / history_daily assembly (SILO+AWS compositing)
R/silo-disagg.R           # SILO daily→hourly disaggregation (donor ladder last rung)
tests/testthat/test-transfer.R
tests/testthat/test-fill-tiers.R
tests/testthat/test-fill-treatments.R
tests/testthat/test-donor-ladder.R
tests/testthat/test-fill-run.R
tests/testthat/test-aggregate.R
tests/testthat/test-history-products.R
tests/testthat/test-silo-disagg.R
```

Add `imputeTS` and `circular` to `Imports`.

## Detailed design

### The shared transfer engine (`R/transfer.R`)

The single statistical primitive both curation and correction use (SCOPING §6):

- `fit_transfer(source_series, target_series, method = c("mean_bias","qmap"),
  by = NULL, treatment = NULL)` → a transfer object (a plain list / small tibble
  of parameters, *not* an `.rds` model). `by` allows conditioning (e.g. hour
  block); `treatment` names the per-variable statistical space (below).
- `apply_transfer(transfer, source_series)` → corrected series.
- **No skill decay here.** The engine assumes both series are realized
  observations over a shared window. Plan 12 wraps these to add lead-dependent
  shrinkage for forecasts; gap-fill calls them **directly** (this is the review’s
  “opposite direction, not the same problem” distinction made concrete).

### Per-variable treatment (`R/fill-treatments.R`)

Dispatched from the dictionary’s `statistical_class`; each provides a
`to_space()` / `from_space()` pair so transfer/interpolation happen in the right
space (SCOPING §6):

- **RH** → convert to **dewpoint**, operate there, convert back (avoids impossible
  RH and respects temperature dependence).
- **circular** (`wind_direction`) → operate on `sin`/`cos` (or u/v), recombine to
  an angle; never interpolate the raw angle across the 0/360 wrap.
- **intermittent** (rain) → split into **occurrence** (did it rain) and **amount**;
  fill/transfer each separately; never linearly interpolate rainfall (it smears a
  dry period into drizzle).
- **clear_sky_indexed** (solar) → divide by modelled clear-sky irradiance to a
  clear-sky **index**, operate on the index (well-behaved, bounded), multiply back.
  Reuse the Plan 09 clear-sky model.
- **wind speed** → apply **height correction** (log-wind profile using the
  instrument `z0`/displacement from the registry, Plan 02) to a common reference
  height **before** any cross-station wind statistics; document the neutral-
  stability assumption and the inversion caveat (SCOPING §7.1 review note).

### Donor ladder + dedup (`R/donor-ladder.R`)

`rank_donors(site, variable, window, available)` → an ordered, **deduplicated**
donor list. Order: BOM station → GHCNh station → ERA5 → SILO-disaggregated
(SCOPING §6). **Dedup by physical station identity** (review fix): BOM real-time
feeds and GHCNh frequently serve the *same* station via different transports —
collapse them to one donor (use the resolved station identity from Plan 06’s
`nearest_stations` dedup) so the ladder ranks *transports of distinct stations*,
never counts one station twice. Each donor is bias-corrected to the site via
`fit_transfer` on the overlap before use.

### Fill tiers (`R/fill-tiers.R`)

Route each gap by length (SCOPING §6):

- **micro** (≤ 2–3 h, **smooth variables only** — not rain, not direction): fill
  with `imputeTS` interpolation in the variable’s treatment space. `method =
  "imputed"`.
- **medium**: fill from the **best available deduplicated donor**, transfer-
  corrected to the site. `method = "donor_fill"`, `source =` the donor’s identity.
- **macro / pre-installation**: fill from the corrected model series (Open-Meteo /
  ERA5). `method = "model_fill"`.
- **cloud_cover**: donor ladder via airport METAR where a suitable donor exists,
  else raw model (SCOPING §6).
- **model-only variables** (`wind_speed_80m/120m/180m`, `boundary_layer_height`):
  **skip the donor ladder entirely** — always the raw model value here (Plan 12
  may later apply the experimental `profile_rescale`). `method = "model_fill"`,
  correction tier `raw`.

Every filled value records `source` and `method` per the Plan 01 method/QC
separation: the `method` (`imputed`/`donor_fill`/`model_fill`) is what
identifies a value as filled, never a QC state. The filled row's `qc_flag` is
`"ok"` (or `"suspect"` where the fill itself is dubious, e.g. a distant donor)
— it must **not** inherit the `missing`/`fail` flag of the gap it replaces, or
every downstream `qc_flag == "ok"` filter would drop the fills. Measured-only
consumers filter on `method == "measured"`, not on the flag (SCOPING §3).

### Aggregation (`R/aggregate.R`)

- `aggregate_hourly(obs)` — native resolution (e.g. 10-minute logger data) →
  hourly, dispatched per statistical class from the dictionary: mean for
  linear/bounded, **vector (u/v) mean for circular**, sum for intermittent
  (rain), mean for radiation (W/m² is already a rate). Only `qc_flag == "ok"`
  rows contribute; an hour with less than a documented completeness threshold
  (default ≥ 75 % of expected native records) is left **missing** for the fill
  tiers to handle. Output rows carry `method = "aggregated"`.
- `aggregate_daily(obs_hourly, site)` — hourly → daily on the **local-day
  boundary** (SCOPING §3: local clock time via the site’s IANA timezone; DST
  transition days are 23/25-hour days by construction). Per-variable
  day-window and date-assignment conventions are **table-driven and must match
  SILO’s documented definitions** (rain: 24 h to 9am, assigned to the day of
  observation; temperature min/max: SILO’s assignment — cite the SILO
  climate-variables page in a comment). Do not hard-code one convention across
  variables: the §4 compositing and the SILO QM correction (Plan 11) both
  depend on the day windows agreeing with SILO’s.

### History products (`R/history-products.R`)

- `build_history_hourly(store_root, site, window)` — the QC’d, gap-filled,
  hourly-aggregated AWS record; this *is* `history_hourly` (SCOPING §4).
  Incremental over the product watermark; idempotent.
- `build_history_daily(store_root, site, window)` — base = SILO daily
  (gap-free by construction), overlaid with the site’s AWS daily aggregates
  wherever present **and QC-clean** (the AWS-wins rule, SCOPING §4).
  Provenance records which leg served each value; once Plan 11/12’s
  daily-scale SILO QM exists the SILO leg is the site-corrected series (tier
  in provenance; raw SILO until then). The roxygen **must carry the §4
  caveat** verbatim in spirit: the AWS installation date introduces a step
  change — fit for operational bounds and priors, unfit for trend analysis.

### SILO disaggregation (`R/silo-disagg.R`)

- `disaggregate_silo(daily, shape)` — daily → hourly as the donor ladder’s
  **last** rung (SCOPING §6/§13: genuinely hard, which the position reflects).
  Take a **diurnal shape** from the best available sub-daily reference for the
  same window (a donor station, else a model series, else the site’s
  climatological diurnal cycle), then scale it in the variable’s treatment
  space so the disaggregated hours **exactly reproduce the daily value** (sum
  for rain, min/max for temperature, mean otherwise). Never invents sub-daily
  structure beyond the shape source; `method = "disaggregated"`, with the
  shape source recorded in provenance.

### `fill_run` (`R/fill.R`)

`fill_run(store_root, site, variables = NULL, now = .now(), donors = NULL)`:
- **Incremental** over the fill watermark with a look-back; **idempotent**
  (re-running yields identical fills — seed any stochastic step). Writes filled
  rows via `store_write_obs(mode = "supersede")` so a later, better donor can
  supersede an earlier fill, with the earlier one retained for audit (Plan 03).

## Test requirements

### `test-transfer.R`
- `fit_transfer(method="mean_bias")` on a known offset recovers it; `apply_transfer`
  removes it. `qmap` on a known distributional shift maps quantiles correctly.
- **The shared-engine invariant:** `apply_transfer` on a realized series has **no
  lead argument and no shrinkage** — assert the engine is skill-decay-free (a
  contract test Plan 12 relies on when it adds shrinkage as a wrapper).

### `test-fill-treatments.R`
- RH filled via dewpoint space never produces RH > 100 or < 0.
- Circular interpolation across 350°→10° yields values near 0°/360°, never ~180°.
- Rain occurrence+amount: a dry gap between dry neighbours stays dry (no smeared
  drizzle); a wet gap distributes amount without inventing a dry spell.
- Solar filled via clear-sky index stays within physical bounds and is 0 at night.
- Wind height correction brings a 2 m and a 10 m donor to a common reference
  before differencing (assert the corrected values, given known `z0`).

### `test-donor-ladder.R`
- Ladder order is BOM → GHCNh → ERA5 → SILO for a fixture with all present.
- **Dedup:** a station present as both a BOM feed and a GHCNh station appears
  **once** in the ranked donors (the review fix), and the ladder still fills.

### `test-fill-tiers.R`
- A 2 h gap in temperature is micro-filled (`imputed`); a 2 h gap in **rain** is
  **not** micro-interpolated (routed to donor/model instead).
- A 2-day gap uses a transfer-corrected donor (`donor_fill`).
- A pre-installation span uses model fill (`model_fill`).
- **model-only** variable gaps are filled with raw model, **bypassing** the donor
  ladder (assert no donor transfer was attempted).

### `test-fill-run.R`
- **Idempotency:** two runs over the same window produce identical filled values
  and no duplicate rows.
- A better donor arriving later supersedes an earlier fill; the earlier fill is
  retrievable via `include_superseded = TRUE`.

### `test-aggregate.R`
- A 10-minute fixture aggregates to hourly with the per-class rule: rain sums,
  temperature means, direction vector-means (a 350°/10° mix averages near 0°,
  never ~180°); an hour below the completeness threshold stays missing.
- Daily aggregation over a **DST-transition fixture**: the 9am boundary follows
  local clock time (one 23-hour and one 25-hour day at the transitions), and
  the rain-day total matches a hand-computed SILO-convention value.

### `test-history-products.R`
- Compositing: where an AWS daily aggregate is present and QC-clean, it wins;
  where AWS is absent or `fail`, SILO serves; provenance records the leg per
  value (the SCOPING §4 rule, directly).
- Across a simulated AWS installation date, the two legs are distinguishable
  from provenance (the step-change auditability the §4 caveat rests on).

### `test-silo-disagg.R`
- Disaggregated hourly rain sums **exactly** to the daily total; an all-dry day
  disaggregates to all-dry hours (no invented drizzle).
- Disaggregated temperature reproduces the daily min/max; rows carry
  `method = "disaggregated"` and the shape source in provenance.

## Definition of done

Shared skeleton plus:
- `fill_run()` exported/documented; `fit_transfer`/`apply_transfer` exported (Plan
  12 and advanced users need them) and documented as the shared, **skill-decay-
  free** primitive.
- Donor dedup by station identity is proven by test.
- Model-only variables provably skip the donor ladder.
- `history_hourly`/`history_daily` builders exist with the AWS-wins compositing
  proven; daily aggregation conventions match SILO’s documented day windows
  (DST-transition test passing); SILO disaggregation reproduces daily values
  exactly and its `history_daily` docs carry the §4 step-change caveat.
- New condition classes registered in `meteo_conditions()`.
