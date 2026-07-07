# Plan 17 — Correction serve-wiring, verification enrichment, and refactors

## Objective

Close the gap found in the post-implementation review: the fitted-correction and
verification machinery (Plans 11–13) exists as tested leaf functions, but almost
nothing **calls** it on the path a consumer actually reads. As shipped, corrected
values never reach `met_wide()`, `history_daily`, or any dashboard; the provenance
attached to the meteoHazard interface names correction tiers that were never
applied. This plan wires the existing pieces together, enriches the verification
report to what SCOPING §7.4 requires, and applies a handful of small correctness
fixes and safe refactors.

**This plan adds almost no new science.** Every statistical primitive it needs
(`fit_mean_bias`/`apply_mean_bias`, `fit_qmap`/`apply_qmap`, `fit_emos`/`apply_emos`,
`shrink_to_climatology`, `consistency_pass`, `baseline_persistence`,
`baseline_climatology`, `rank_histogram`, `spread_error_ratio`, `brier_score`,
`score_crps`) already exists and is unit-tested. The work is *composition*: read
the right rows, call the right leaf, write/return the result, and record honest
provenance.

## How to work this plan (read this first)

- Work the items **in the numbered order**. Items 1–5 are load-bearing; 6–10 are
  correctness fixes; 11–13 are behaviour-preserving refactors.
- Each item lists **Problem**, the **exact files** to touch, the **exact change**
  (signatures + algorithm), and a **Done when** line.
- The BDD acceptance specs already exist in `tests/testthat/test-plan17-*.R`.
  Every `it()` block starts with a `skip("plan 17 item N: … — un-skip when
  implementing")` line. Your job for each item is: **delete that one skip line
  and make the block pass** without weakening the assertion. Do not delete a
  skip until you are implementing that exact item.
- Do **not** mock the function under test to fake a pass (the anti-pattern the
  earlier audit called out). Mock only *seams you own and are not testing*
  (e.g. an acquisition seam) with `testthat::local_mocked_bindings()`.
- After each item: `devtools::document()` if you added/changed roxygen, then
  `devtools::test()` and `lintr::lint_package()`. The r-science `verify` gate
  must be green before you move on.
- When a change is behaviour-changing for an already-green test (Items 7 and 12
  in particular), update that test in the **same** commit and say so in the
  message — never leave the suite red between commits.

## Prerequisites

Plans 00–16 merged (they are). No new hard dependencies. `scoringRules`,
`crch`, `qmap` are already in `Imports`.

## Background — where the design says each piece belongs

- SCOPING §7.1 — tiers, the "daily pipeline only applies the current version;
  the monthly job refits", the post-correction consistency pass ("violations are
  clipped and *counted*"), lead-dependent shrinkage on the forecast path only.
- SCOPING §7.4 — verification: out-of-sample by construction, **against
  baselines** (raw / persistence / climatology, not just before/after), PIT /
  rank histogram / spread–error and Brier for probabilistic products.
- SCOPING §4 — `history_daily` is SILO-based, **site-corrected against AWS**,
  AWS wins where clean; the correction tier is recorded in provenance.
- SCOPING §10 — the one-call meteoHazard interface returns the wide table with
  a **truthful** provenance attribute.
- SCOPING §5.1 — "every stored value's provenance records the transport that
  served it".

## The target architecture (a decision — do not deviate)

**Correction is applied at *serve* time, never materialised into the raw
observation store or the raw forecast archive.** The archive and the raw
observation record stay immutable. The current calibration (the highest manifest
version for that `(variable, source)`) is applied when a consumer reads corrected
data — in `met_wide()` and in `build_history_daily()`. Reproducibility comes from
the `calibration_manifest_version` already stamped into `met_wide()`'s `versions`
attribute.

> **Flagged deviation from SCOPING §9.** §9 lists "apply calibrations" as work
> `met_sync_live()` does. Materialising corrected values into the store instead
> would (a) force the heavy supersede rewrite on every sync, (b) go stale the
> moment the monthly refit changes coefficients (a stored "corrected" value would
> silently reflect an old fit), and (c) make store corruption easy for a
> follow-on implementer. Serve-time application keeps the raw record canonical,
> makes provenance honest, and is version-pinnable. This mirrors how
> `history_daily` is already *computed on demand* rather than persisted. Recorded
> here, per plans/README's "flag, don't silently pick" rule.

Consequently, the discarded `correct_apply()` calls currently inside
`met_sync_live()` are **removed** (Item 1c): they compute nothing that survives
the function, and under this architecture the live sync's job is to acquire, QC,
fill, and archive — not to correct.

---

## Item 1 — [BLOCKING] Apply the calibration to forecasts at serve time; make `met_wide()` provenance honest

### Problem
`met_wide(kind = "forecast")` reads the **raw** archive and widens it, but stamps
each column's provenance with the tier the manifest *claims* is fitted
(`R/met-wide.R`, `.met_wide_provenance()`). So meteoHazard receives uncorrected
model values labelled `qmap`/`emos`. Nothing anywhere applies a fitted
calibration to a forecast.

### Files
- **New:** `R/correct-forecast.R`
- Edit: `R/met-wide.R`
- Edit: `R/met-sync-live.R` (remove the discarded correction calls — 1c)
- Roxygen/NAMESPACE regen.

### Exact change

**1a. New serve-time forecast corrector.** Add:

```r
# correct_forecast(store_root, site, fc, now = .now()) -> fc with corrected
# `value` and a per-row `tier` column.
#
# `fc` is a canonical forecast tibble (as read from the archive: deterministic,
# per-member, and stat rows). Correction is applied per (variable, source):
#   - model_only variable (met_variable(v)$measurability_class == "model_only")
#       -> value unchanged, tier "raw"   (SCOPING §7.3)
#   - no calibration on file for (variable, source)
#       -> value unchanged, tier "physical" (day-0 floor; a forecast has no
#          site instrument to height/lapse-correct, so physical == identity)
#   - calibration present
#       -> apply the manifest tier's apply_*() to the forecast values, using
#          newdata = tibble(issue_time = fc$issue_time, valid_time = fc$valid_time,
#                           forecast = fc$value); stamp that tier.
# Then, on the forecast path only, shrink toward climatology per lead bucket
# with a verified skill weight (see serve_shrink_weight()); stamp nothing extra
# (tier already recorded). Returns fc with `value` replaced and `tier` added.
```

- Reuse `.correct_apply_fitted(obs, coeffs, tier)` from `R/correct.R` — it already
  dispatches `mean_bias`/`qmap`/`emos` on a `(issue_time, forecast)`-shaped
  `newdata`. Rename its parameter reads to accept the forecast frame, or add a
  sibling `.apply_fitted_values(coeffs, tier, newdata)` returning a numeric
  vector (preferred: a pure vector-in/vector-out helper both `correct_apply()`
  and `correct_forecast()` call, so there is one apply path).
- Lead bucket per row: `.verify_lead_bucket(fc$lead_time)` (already exists,
  `R/verify.R`).

**1b. Verified per-lead shrink weight.** Add to `R/correct-forecast.R`:

```r
# serve_shrink_weight(store_root, site_id, source, variable, lead_bucket) -> [0,1]
#
# Reads the stored verification report (read_verification_report()). If it holds
# both a fitted-tier row and a "climatology" baseline row for
# (source, variable, lead_bucket), weight = clamp(skill_score(tier_rmse,
# climatology_rmse), 0, 1): high skill vs climatology -> trust the correction
# (weight -> 1); no skill over climatology -> shrink to climatology (weight 0).
# If the report has no such rows yet (Item 5 not landed, or no history), fall
# back to a tier-based weight: 1 for a fitted tier (mean_bias/qmap/emos), else 0.
```

`skill_score()` and `clamp` (`pmin(pmax(x,0),1)`) already exist / are trivial.
The climatology series for the shrink target is
`baseline_climatology(build_history_daily(...), valid_time, variable)$mean` per
row — the same lookup `.correct_forecast_climatology()` in `R/correct.R` already
does; factor that into a shared helper rather than duplicating it.

**1c. Wire `met_wide()` + drop the dead sync calls.**
- In `R/met-wide.R`, forecast branch: after `.latest_issuance(long)`, call
  `long <- correct_forecast(site_store_root(s), s, long, now = now)`, then widen.
  Build provenance from the **applied** `tier` column of `long` (one tier per
  variable — take the modal/first tier per variable from the corrected long
  table), not from a fresh manifest re-derivation. Keep `train_overlap` from the
  manifest as you already do.
- In `R/met-sync-live.R`, delete the `correct_apply(..., target = "record")` call
  in the obs loop and the whole `for (source in config$forecast_sources)
  correct_apply(..., target = "forecast")` block. Update the roxygen: live sync
  acquires, QCs, fills, archives — correction is applied at serve time
  (`met_wide()`), per Plan 17's architecture note.

### Done when
`test-plan17-serve-correction.R` passes: a site with a promoting `qmap`
calibration written for `temperature_2m`/`openmeteo` gets **corrected** values
out of `met_wide(kind = "forecast")` (not the raw archive value), the provenance
tier for that column reads `"qmap"`, and a variable with no calibration reads
`"physical"` / a model-only variable reads `"raw"`. A high-lead bucket with no
verified skill shrinks toward climatology.

---

## Item 2 — [BLOCKING] Site-correct the SILO leg of `history_daily`

### Problem
SCOPING §4 says `history_daily` is SILO "site-corrected against AWS daily
aggregates", with the correction tier recorded in provenance. `build_history_daily()`
(`R/history-products.R`) currently composites **raw** SILO under AWS with no
correction, and stamps no tier.

### Files
- Edit: `R/history-products.R`
- Reuse: `R/correct.R` apply path, `R/store-calib.R`.

### Exact change
- Before compositing, apply the current `(variable, "silo")` calibration to the
  SILO leg via the same serve-time apply path as Item 1 (`.apply_fitted_values()`),
  keyed on `datetime_utc` as the value time. Where no SILO calibration exists,
  leave SILO raw (tier `"physical"`/`"raw"` per the model-only rule) — never error.
- Add a `tier` column to the returned tibble recording, per row, which tier
  produced it: the applied SILO tier for SILO-served rows, and `"measured"`→
  represent AWS-served rows as tier `"physical"` is wrong; AWS is measured truth,
  so use tier `"raw"` for AWS-served rows (no model correction applied) and the
  applied tier for SILO rows. (The `source` column already records the leg; the
  new `tier` column records the correction state, matching §4.)
- The SILO daily QM is fitted by `correct_refit()` for `source = "silo"` once
  Item 3 iterates it; until a fit exists this item is a no-op that still stamps
  the tier column, so it is safe to land before Item 3.

### Done when
`test-plan17-record-correction.R` passes: with a `qmap` calibration on
`(temperature_2m, silo)`, a SILO-served day in `history_daily` shows the
**corrected** value and `tier == "qmap"`; an AWS-served day shows the raw AWS
value and `tier == "raw"`; with no calibration the SILO value is unchanged.

---

## Item 3 — [HIGH] Fit calibrations for forecast sources in `met_refit()`

### Problem
`met_refit()` (`R/met-refit.R`) calls `correct_refit()` only for
`config$obs_sources`. `correct_refit()` assembles pairs by filtering the
**forecast archive** to that source name (`assemble_verification_pairs(sources =
source)`). In a realistic config (obs: `site_aws`/`silo`; forecasts:
`openmeteo`/`bom_forecast`) that join is empty, so **no forecast calibration is
ever fitted in production**.

### Files
- Edit: `R/met-refit.R`

### Exact change
- Iterate `correct_refit()` over `unique(c(config$obs_sources,
  config$forecast_sources))`, not just `obs_sources`. (Obs sources stay included
  for the SILO daily-QM record correction, Item 2, whose "source" is an obs
  source; forecast sources are the ones with archived forecasts to calibrate.)
- Update the roxygen note at `R/met-refit.R:12-23` — the "not-yet-fully-wired
  concern" it describes is exactly what this item wires; remove the stale caveat.

### Done when
`test-plan17-refit-wiring.R`'s "fits a forecast-source calibration" block passes:
after seeding overlapping archived `openmeteo` forecasts + QC-clean obs and
running `met_refit()`, `calib_manifest()` has a row for
`(temperature_2m, openmeteo)` at a fitted tier.

---

## Item 4 — [HIGH] Run the post-correction consistency pass and count violations

### Problem
`consistency_pass()` (`R/consistency-pass.R`) — clip physically impossible
combinations, count them — has **zero callers**. SCOPING §7.1: "every correction
application ends with a cheap constraint-enforcement pass; violations are clipped
and *counted* — a rising violation rate is itself a verification red flag."

### Files
- Edit: `R/correct-forecast.R` (Item 1), `R/history-products.R` (Item 2).

### Exact change
- After a correction application produces per-variable corrected values for a
  common set of timestamps, **widen** to one row per `(site_id, datetime_utc)` /
  per `valid_time` (columns = variable names), run each row through
  `consistency_pass(wide_row)`, sum the `n_violations`, and **narrow** back to the
  corrected long/forecast shape. Attach the total as
  `attr(result, "n_violations")`.
- In `correct_forecast()`, do this once over the corrected forecast frame before
  returning. In `build_history_daily()`, do this over the composited daily frame.
- Surface the count: `verify_run()` (Item 5) reads
  `attr(., "n_violations")` where available and records a
  `consistency_violation_rate` column on the report (violations / rows). If
  threading the attribute is awkward, a minimally-invasive alternative is to have
  `correct_forecast()` return the count in an attribute and have `met_wide()`
  ignore it (the count matters for verification, not for the served table).

### Done when
`test-plan17-consistency.R` passes: a corrected forecast that would emit
`wind_gusts_10m < wind_speed_10m` is clipped so gusts ≥ speed on output, and the
number of clipped relations is retrievable (via the returned attribute).

---

## Item 5 — [HIGH] Enrich the verification report: baselines, tier comparison, calibration diagnostics

### Problem
`verify_run()` (`R/verify.R`) persists only a single raw-tier MAE/RMSE row per
group. The §7.4 requirements — persistence and climatology **baselines**, the
**corrected tier** scored beside raw, and calibration diagnostics (rank/PIT,
spread–error, Brier) for probabilistic products — all exist as tested leaf
functions (`R/verify-baselines.R`, `R/verify-calibration.R`, `R/verify-scores.R`)
that `verify_run()` never calls.

### Files
- Edit: `R/verify.R`
- Reuse: `verify-baselines.R`, `verify-calibration.R`, `verify-scores.R`,
  `build_history_daily()`.

### Exact change
- Per `(source, variable, lead_bucket)` group, emit **one report row per method**
  in `{"raw", "persistence", "climatology", <fitted tier if a calibration
  exists>}`, each scored out-of-sample via `rolling_origin_score()` with the
  appropriate `apply_fn`:
  - `raw` → identity (`.verify_identity_apply`, already used).
  - `persistence` → `baseline_persistence(score_set$observation)` as the forecast.
  - `climatology` → per-row `baseline_climatology(hist, valid_time, variable)$mean`,
    where `hist = build_history_daily(store_root, site, <trailing window>)`.
  - fitted tier → the incumbent calibration's `apply_*()` (reuse Item 1's
    `.apply_fitted_values()`).
  - Keep the existing `tier` column to carry the method name; widen the report
    schema's allowed `tier` values accordingly (no enum change — the report is a
    bespoke table, not a canonical one).
- **Calibration diagnostics (probabilistic).** When the group's source has
  ensemble members in the archive (member rows present), additionally compute and
  persist, to a companion `verification_diagnostics` dataset under
  `<store_root>/verification_diagnostics/site_id=<id>/`:
  `histogram_flatness(rank_histogram(member_matrix, truth))`,
  `spread_error_ratio(member_matrix, truth)`, and, for `precipitation`,
  `brier_score(prob_of_rain, outcome)`. Build `member_matrix` by pivoting the
  archived member rows to one column per member at each `valid_time`. This is a
  *secondary* product; if a group has no members, write no diagnostics row for it.
- `read_verification_report()` and `met_verification()` keep working (they rbind
  the report dataset); add `read_verification_diagnostics()` mirroring it.

### Done when
`test-plan17-verification.R` passes: `verify_run()` over a seeded archive+obs
writes report rows for `raw`, `persistence`, and `climatology` (all
out-of-sample); and an ensemble source additionally yields a diagnostics row
carrying a finite spread–error ratio.

---

## Item 6 — [MEDIUM] Compare a candidate refit against the *fitted* incumbent, not raw

### Problem
`correct_refit()` (`R/correct.R`, `.correct_refit_variable()`) scores the
candidate tier's out-of-sample errors against `.verify_identity_*` (raw). SCOPING
§7.1 requires promotion to beat **the incumbent** (the currently-fitted
calibration), so a marginally-better `emos` can displace a already-good `qmap`
only if it actually beats that `qmap`, not merely raw.

### Files
- Edit: `R/correct.R` (`.correct_refit_variable()`).

### Exact change
- Before scoring, read the current calibration for `(variable, source)` via
  `calib_read(..., version = "current")` inside `tryCatch` (may not exist).
- Build the incumbent's `fit_fn`/`apply_fn` from its tier (reuse
  `.correct_refit_fit_apply(incumbent_tier)`); if no incumbent exists, keep
  `.verify_identity_*` (raw) as today.
- Score `incumbent_errs` with that incumbent apply, `candidate_errs` with the
  candidate. The block-bootstrap gate on `incumbent_errs - candidate_errs` then
  measures improvement over the real incumbent. Everything downstream is
  unchanged.

### Done when
`test-plan17-refit-wiring.R`'s "does not promote a candidate that fails to beat
the fitted incumbent" block passes: with an incumbent that already fits well,
a candidate whose out-of-sample errors are not significantly smaller is **not**
written (manifest version does not advance).

---

## Item 7 — [MEDIUM] Key the mean-bias harmonics on the value's own time

### Problem
`fit_mean_bias()` builds its seasonal/diurnal harmonics from `pairs$issue_time`,
but `apply_mean_bias()` builds them from `newdata$issue_time` while `correct_apply()`
feeds it `issue_time = obs$datetime_utc` (the *valid*/observation time). For a
daily-lead forecast, issue and valid differ by the lead, so the diurnal
(hour-of-day) harmonic is fit on the issuance clock and applied on the valid
clock — a phase mismatch that grows with lead. The seasonal/diurnal bias of a
forecast is a property of the **time the value is about** (valid time), not when
it was issued.

### Files
- Edit: `R/tier-mean-bias.R`.

### Exact change
- Add an internal `.mean_bias_time(df)` that returns `df$valid_time` when that
  column is present, else `df$issue_time`. Use it in both `fit_mean_bias()`
  (replace `time <- pairs$issue_time`) and `apply_mean_bias()` (replace
  `newdata$issue_time`). Callers already pass `valid_time` in the pairs
  (`assemble_verification_pairs()` includes it) and in the forecast `newdata`
  (Item 1 passes `valid_time`).
- This is **behaviour-changing**: any test asserting a specific mean-bias number
  on `issue_time`-keyed harmonics must be updated in the same commit. Re-fit is
  required in production (a manifest version bump), but that happens naturally on
  the next `met_refit()`.

### Done when
`test-plan17-serve-correction.R`'s "mean-bias harmonics track valid time" block
passes: a fit on pairs whose seasonal bias is a function of **valid** day-of-year
recovers and removes that bias when applied to new forecasts at the matching
valid times.

---

## Item 8 — [MEDIUM] Persist the BOM serving transport in provenance

### Problem
The BOM ladder stamps a `transport` column (`ftp_feeds` / `web_api`) on its
returned frame, but `new_obs()` strips every non-canonical column, so the store
never records which transport served a value — contradicting SCOPING §5.1
("every stored value's provenance records the transport that served it").

### Files
- **New:** `R/store-obs-transport.R` (a small companion table, mirroring the
  `qc_log` pattern in `R/qc-log.R`).
- Edit: `R/source-bom-obs.R`, `R/source-bom-forecast.R` (return the transport for
  the writer to pick up), and the sync verbs' obs-write step so the transport, when
  present on the fetched frame, is written to the companion table keyed by
  `(site_id, datetime_utc, variable, source)`.

### Exact change
- Companion dataset `<store_root>/obs_transport/site_id=<id>/year=<yyyy>/…parquet`
  with columns `(site_id, datetime_utc, variable, source, transport, ingested_at)`.
  Provide `obs_transport_write(store_root, df, now)` (dedup on the key, latest
  `ingested_at` wins) and `obs_transport_read(store_root, site_id, from, to)`.
- The acquisition seam `.acquire_obs()` (`R/pipeline.R`) returns a canonical obs
  frame; when the underlying fetch carried a `transport` attribute/column, pass it
  through so the sync verb can call `obs_transport_write()` alongside
  `store_write_obs()`. Do **not** widen the canonical obs schema.
- This is additive; nothing reads it yet beyond the new reader, so it cannot break
  existing behaviour.

### Done when
`test-plan17-provenance-readapi.R`'s "BOM transport is recorded" block passes: a
BOM obs fetch served via the FTP rung writes an `obs_transport` row with
`transport == "ftp_feeds"` for the same key, retrievable via
`obs_transport_read()`.

> If the companion-table surface proves too large to land safely, the acceptable
> reduced scope is: write the transport but do **not** add a public reader — the
> DoD test then asserts the parquet file exists with the right column. Decide and
> record which you did in the commit; do not leave it half-wired.

---

## Item 9 — [MEDIUM] Honour `as_of` in `met_history()`

### Problem
`met_history(as_of = …)` is accepted but silently ignored: `store_read_history()`
(`R/read-api.R`) does not thread `as_of` into the curated-product builders, which
have no revision concept. Documented as a gap, but a reproducible regulator-facing
report needs the daily/hourly history to reflect the store *as it was*.

### Files
- Edit: `R/history-products.R` (`build_history_daily`/`build_history_hourly`),
  `R/read-api.R` (`store_read_history`).

### Exact change
- Give both builders an `as_of = NULL` argument and thread it into their single
  `store_read_obs(...)` call (which already supports `as_of`). No other change:
  the aggregation/compositing is a pure function of the rows read, so a
  point-in-time read reproduces the point-in-time product.
- Thread `as_of` from `met_history()` → `store_read_history()` → the builders.
  Remove the "currently has no effect" caveat from the roxygen.

### Done when
`test-plan17-provenance-readapi.R`'s "history honours as_of" block passes: an obs
value superseded by a later revision is reflected in `met_history()` at its old
value when `as_of` predates the revision, and the new value otherwise.

---

## Item 10 — [LOW] Give the day-0 lapse a real elevation delta

### Problem
`.correct_temperature_rows()` (`R/correct-physical.R`) always passes
`elevation_delta = 0`, so the documented lapse adjustment is a permanent no-op —
there is no "reference/grid elevation" anywhere for it to key on.

### Files
- Edit: `R/correct-physical.R`; possibly `R/site.R` (a per-source grid-elevation
  accessor) — but keep it data-driven, not schema-breaking.

### Exact change
- Add an **optional** `grid_elevation` to the source/adapter metadata (or accept
  it as a `correct_physical(..., grid_elevation = NULL)` argument threaded from
  the caller). When present, `elevation_delta = site_elevation - grid_elevation`
  and the lapse applies; when absent, keep the current no-op (still stamps
  `"physical"`). Do not invent a grid-elevation source — this item only makes the
  adjustment *possible* and *tested*, wired for the day a caller supplies one.

### Done when
`test-plan17-physical.R` passes: `correct_physical(obs, site, grid_elevation = g)`
adjusts temperature by `-0.0065 * (site_elev - g)`, and with `grid_elevation =
NULL` the value is unchanged (still tier `"physical"`).

---

## Item 11 — [REFACTOR] Hoist `qc_run()`/`fill_run()` out of the per-source loop

### Problem
`met_sync_live()` (`R/met-sync-live.R`) calls `qc_run()` and `fill_run()` **inside**
the `for (source in config$obs_sources)` loop, so with N obs sources they each run
N times over the same window — redundant, and the last run's result is the only
one that matters. (`met_sync_daily()` already runs them once, after the loop —
copy that shape.)

### Files
- Edit: `R/met-sync-live.R`.

### Exact change
- Move the `qc_run()` / `fill_run()` (and, now that Item 1c removed the per-source
  `correct_apply`, nothing else) below the obs-acquisition loop, matching
  `met_sync_daily()`. Behaviour-preserving: QC/fill see the same fully-written
  window either way; only the redundant repeats go away.

### Done when
`test-plan17-refactors.R`'s "live sync QCs/fills once regardless of obs-source
count" block passes: with two obs sources, `qc_run` is invoked once per site per
`met_sync_live()` call (assert via a call-counting mock), and stored output is
unchanged from before.

---

## Item 12 — [REFACTOR] Vectorise `aggregate_hourly()` / `aggregate_daily()` grouping

### Problem
Both aggregators (`R/aggregate.R`) use triple-nested `for` loops over
variable × site × bucket, rebuilding a one-row tibble per bucket and `rbind`-ing
them. That is O(rows × buckets)-ish and allocation-heavy — a drag on decade-scale
backfills.

### Files
- Edit: `R/aggregate.R`.

### Exact change
- Replace the per-bucket loop with a single split-apply over the grouping key
  (`interaction(site_id, variable, bucket)` or `vctrs::vec_group_id()`), applying
  `.aggregate_value()` per group and assembling one tibble with `vctrs::vec_rbind`
  / `tibble` column construction. Keep `.aggregate_value()`, the completeness
  threshold, the circular mean, and the per-variable day convention **exactly** as
  they are — this is a pure performance rewrite with identical output.
- The existing `test-aggregate.R` must stay green unchanged; the new spec adds an
  explicit equivalence check.

### Done when
`test-plan17-refactors.R`'s "aggregation output is unchanged by the refactor"
block passes: on a fixed multi-variable, multi-site, DST-spanning fixture, the new
`aggregate_hourly()`/`aggregate_daily()` produce rows identical (same keys, values
within 1e-9) to a captured reference, and `test-aggregate.R` still passes.

---

## Item 13 — [REFACTOR] Reset the `met_table` downgrade flag defensively

### Problem
`vec_ptype2.met_table.met_table()` (`R/met-table-dplyr.R`) sets a package-private
`.met_table_state$downgrade_pending <- TRUE` that the next
`dplyr_reconstruct.met_table()` reads and clears. If a `bind_rows()` errors
*between* those two calls (e.g. incompatible column types), the flag is left
`TRUE` and leaks into the **next, unrelated** `bind_rows()`, silently downgrading a
perfectly compatible result.

### Files
- Edit: `R/met-table-dplyr.R`.

### Exact change
- In `vec_ptype2.met_table.met_table()`, set the flag with
  `withr::defer()`-style safety is awkward across the two generics, so instead:
  clear the flag at the **start** of every `vec_ptype2.met_table.met_table()` call
  before deciding, so a stale `TRUE` from an aborted prior combine cannot survive
  into a fresh operation. (The set-then-read handshake within one successful
  `bind_rows()` is unaffected.) Add a brief comment explaining the leak this
  guards against.

### Done when
`test-plan17-refactors.R`'s "a failed bind_rows does not poison the next" block
passes: force a `vec_ptype2` that sets the flag then an error, then a clean
`bind_rows()` of two compatible `met_table`s — the clean combine keeps the class
and does **not** emit the `met_table_downgraded` warning.

---

## Definition of done (whole plan)

- Every `it()` in `tests/testthat/test-plan17-*.R` is un-skipped and green; no
  assertion was weakened to get there.
- `devtools::test()` — all pass, only the pre-existing CCSDS/eccodes environment
  skips remain.
- `devtools::document()` committed; `lintr::lint_package()` clean.
- `R CMD check` adds no new ERROR/WARNING/NOTE beyond the environmental ones.
- `NEWS.md` gains a bullet: forecast/record corrections are now applied at serve
  time; verification reports baselines + diagnostics; `met_history(as_of=)` works;
  BOM transport is recorded.
- The flagged SCOPING §9 deviation (serve-time correction) stays documented here
  and in `met_sync_live()`'s roxygen — do not silently revert it.
