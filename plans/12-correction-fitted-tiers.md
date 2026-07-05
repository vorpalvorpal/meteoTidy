# Plan 12 — Correction: fitted tiers

## Objective

Implement the fitted correction tiers and the review’s statistical fixes:
mean-bias with **harmonic day-of-year + hour-of-day covariates** (not raw bins);
quantile mapping with **cross-season pooling/shrinkage** and an **explicit tail
policy**; EMOS per lead bucket; **lead-dependent shrinkage toward climatology**
when correcting forecasts; wind direction via **joint u/v** (never QM’d as an
angle); a **post-correction physical-consistency pass** (clip + count); and the
model-only experiments (`profile_rescale`, diagnostic BLH, radiation re-split),
all opt-in/experimental.

## Scope

**In:**
- `fit`/`apply` for tiers `mean_bias`, `qmap`, `emos` (+ optional MBC multivariate).
- Lead-dependent shrinkage wrapper for the forecast path.
- Wind-direction u/v correction.
- Post-correction consistency pass (reusing Plan 09’s `physics-constraints`).
- Model-only: `profile_rescale`, diagnostic BLH, radiation re-split — all opt-in.
- Writing calibrations as data via the Plan 03 calib store.

**Out:**
- Tier *selection* and lifecycle orchestration (Plan 11).
- Skill computation / promotion verdict (Plan 13) — consumed here for shrinkage
  weights.

## Prerequisites

Plans 00–03, 09 (`physics-constraints`), 10 (`R/transfer.R`), 11 (framework).

## Background

SCOPING §7.1 (tier methods + the review’s harmonic/pooling/tail edits; skill-
gated promotion; lead-dependent shrinkage; calibrations as data), §6 (wind
direction as joint u/v; post-correction consistency pass — clip and count),
§7.3 (model-only policy: raw default; `profile_rescale` opt-in experimental —
damped with height, capped, suppressed under stable stratification; diagnostic
AERMET-style BLH from corrected surface vars; radiation clear-sky-index correct
then re-split preserving model ratio or BRL), §7.2 (do **not** train lead-aware
calibration on Historical-Forecast `lead_time = NA` rows).

## File layout

```
R/tier-mean-bias.R        # fit/apply: harmonic covariates
R/tier-qmap.R             # fit/apply: pooled qmap + tail policy
R/tier-emos.R             # fit/apply: crch per lead bucket
R/tier-mbc.R              # optional multivariate (MBC) — Suggests
R/shrinkage.R             # lead-dependent blend toward climatology (forecast only)
R/wind-uv.R               # direction <-> u/v correction
R/consistency-pass.R      # post-correction enforce (reuses physics-constraints)
R/model-only.R            # profile_rescale, diagnostic BLH, radiation re-split
tests/testthat/test-tier-mean-bias.R
tests/testthat/test-tier-qmap.R
tests/testthat/test-tier-emos.R
tests/testthat/test-shrinkage.R
tests/testthat/test-wind-uv.R
tests/testthat/test-consistency-pass.R
tests/testthat/test-model-only.R
```

Add `qmap`, `crch` to `Imports`; `MBC` to `Suggests`.

## Detailed design

Each tier exposes `fit_<tier>(pairs, ...) -> coeffs_tibble` and
`apply_<tier>(coeffs, newdata, ...) -> corrected`. `coeffs_tibble` is tidy data
persisted by `calib_write` (Plan 03) — **never an `.rds` model object**.

### mean_bias (`R/tier-mean-bias.R`)

Fit bias (and a variance-scaling factor) as a smooth function of **day-of-year and
hour-of-day via harmonic (sin/cos) covariates**, not raw hour-of-day bins
(review fix — a one/two-season fit applied unshrunk year-round can be wrong-signed
by the opposite season). Model: `resid ~ sin/cos(2π·doy/365.25) [+ 2nd harmonic] +
sin/cos(2π·hod/24)`; store the regression coefficients. `apply` evaluates the
harmonics at the target time. Document the number of harmonics as a parameter with
a sensible default.

### qmap (`R/tier-qmap.R`)

Empirical quantile mapping per hour block, with two review fixes:

- **Cross-season pooling/shrinkage:** with only 6 months of overlap some seasons
  are untrained; borrow strength across seasons (e.g. a pooled base map shrunk
  toward the season-specific map by sample size) rather than leaving empty cells.
- **Explicit tail policy:** outside the training support, **extrapolate by constant
  shift** (the shift at the nearest trained quantile), **never unbounded**. When a
  new record beyond training support arrives, the correction is bounded and
  documented. Store the mapping table + the tail-shift constants.

### emos (`R/tier-emos.R`)

`crch` heteroscedastic regression per **lead-time bucket** → a predictive
distribution (mean + spread) per lead. Store coefficients per bucket. **Do not
fit on `lead_time = NA` rows** (Historical-Forecast shortest-lead proxy — SCOPING
§7.2); assert this in code and test. EMOS supersedes qmap partly because it models
skill decay natively.

### Lead-dependent shrinkage (`R/shrinkage.R`) — forecast path only

The review’s core statistical fix. Empirical QM/mean-bias are variance-preserving:
applied at long lead they keep full forecast variance when the skilful thing is to
**shrink toward climatology**. `shrink_to_climatology(corrected, climatology,
weight)` blends `w·corrected + (1−w)·climatology`, where `w` per lead bucket is set
from **verified skill** (Plan 13): high skill → `w≈1`; skill ≤ climatology → `w≈0`.
Applied **only** when correcting forecasts (`target = "forecast"`); gap-fill and
record correction (realized series, no skill decay) never shrink (Plan 10
contract). EMOS handles this natively, so shrinkage mainly guards the `qmap`/
`mean_bias` tiers at long lead.

### Wind direction u/v (`R/wind-uv.R`)

Correct wind direction as **joint u/v components** (or vector mean bias), never
quantile-map an angle (SCOPING §6). `dir_to_uv(speed, dir)` / `uv_to_dir(u, v)`;
correction operates on u/v then recombines. A test asserts an angle is never
passed to `qmap`.

### Post-correction consistency pass (`R/consistency-pass.R`)

After univariate corrections, enforce the physical relations (SCOPING §6 review
fix) using **Plan 09’s `physics-constraints` in `enforce` mode**: `gusts ≥ wind`,
`dewpoint ≤ temperature`, `RH ≤ 100`, `direct + diffuse ≤ clear-sky ceiling`.
Violations are **clipped** to the constraint boundary and **counted**; the
violation rate is returned so Plan 13 can surface a rising rate as a red flag.

### Model-only experiments (`R/model-only.R`) — all opt-in, experimental

Per SCOPING §7.3 (default remains raw pass-through, tier `raw`, done in Plan 11):

- **`profile_rescale`** — multiply 80/120/180 m winds by
  `(corrected 10 m)/(raw 10 m)`, **damped with height**, **capped**, and
  **suppressed under stable stratification** (e.g. night + low corrected 10 m
  wind). Ships flagged experimental; never default; verify per site (SCOPING §7.3,
  §14).
- **Diagnostic BLH** — an AERMET-style boundary-layer height recomputed from
  *corrected* surface variables, served **alongside** the raw model BLH (not
  replacing it).
- **Radiation re-split** — where a pyranometer exists: correct global irradiance
  via the clear-sky index, then re-split direct/diffuse preserving the model’s
  split ratio (or a decomposition model, e.g. BRL). Without a pyranometer: raw
  model, tier `raw`.

Each is behind an explicit opt-in argument, defaults off, and stamps a provenance
marker that the value is experimental.

## Test requirements

### `test-tier-mean-bias.R`
- A synthetic seasonally-varying bias (sign flips summer↔winter) is recovered by
  the harmonic fit and removed on apply; a **raw-bin** fit on 4 months would
  mis-sign the opposite season — assert the harmonic version does not (the review
  fix, shown by construction).

### `test-tier-qmap.R`
- A known distributional shift is corrected within tolerance.
- **Pooling:** a season with no training data is still corrected (via the pooled
  base), not left unmapped.
- **Tail policy:** an input beyond the training max is corrected by the constant
  nearest-quantile shift and never returns an unbounded/NA value.

### `test-tier-emos.R`
- `crch` fit per lead bucket yields a predictive mean+spread; CRPS on held-out
  data improves over raw.
- Fitting **refuses `lead_time = NA`** rows (`"lead_unresolved"`), proving
  Historical-Forecast proxy rows can’t contaminate lead-aware training.

### `test-shrinkage.R`
- With `weight = 1`, output equals the corrected value; with `weight = 0`, it
  equals climatology; intermediate weights blend linearly.
- **The forecast/record distinction:** `target = "record"` never shrinks;
  `target = "forecast"` at a low-skill lead pulls strongly toward climatology
  (directly tests the variance-preserving-QM fix). A long-lead unshrunk QM has
  higher error than the shrunk version on a synthetic skill-decaying series.

### `test-wind-uv.R`
- Correcting a direction via u/v handles the 0/360 wrap (a bias across north is
  corrected correctly); assert `qmap` is never called on the raw angle.

### `test-consistency-pass.R`
- Post-correction `gusts < wind`, `dewpoint > temperature`, `RH > 100`,
  `direct+diffuse > ceiling` are all clipped to the boundary and **counted**; a
  clean set is unchanged with a zero violation count.
- Reuses the **same** `physics-constraints` module as Plan 09 (assert identical
  relations by calling both modes on one fixture).

### `test-model-only.R`
- `profile_rescale` is **off by default** (model-only stays raw, tier `raw`).
- When enabled: rescale is damped with height (180 m rescaled less than 80 m),
  capped, and **suppressed** under a stable-stratification fixture (returns raw).
- Diagnostic BLH is served alongside, not instead of, the raw model BLH.
- Radiation re-split without a pyranometer returns raw model values (tier `raw`).

## Definition of done

Shared skeleton plus:
- All three core tiers fit/apply and persist **as data** (no `.rds`); MBC guarded
  behind `Suggests`.
- Lead-dependent shrinkage applies to forecasts only and is proven to reduce
  long-lead error vs unshrunk QM.
- Wind direction never QM’d as an angle; consistency pass shares Plan 09’s module.
- Model-only experiments are opt-in, default-off, and provenance-marked experimental.
- New condition classes registered in `meteo_conditions()`.
