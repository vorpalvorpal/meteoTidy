# Plan 11 — Correction: physical adjustments & tier framework

## Objective

Build the correction *framework*: the always-on day-0 physical adjustments, the
tier-selection logic (data-availability gate **plus** the skill gate from
Plan 13), and the apply/refit lifecycle that reads and writes the Plan 03
calibration store. The fitted-tier statistics themselves are Plan 12; this plan
is the scaffolding they slot into, and it ships the physical (day-0) tier
end-to-end.

## Scope

**In:**
- Physical adjustments (sensor height, altitude/lapse, log-wind profile) — always
  applied, never fitted, superseded once fitted corrections exist.
- `tier_select()` — choose the tier per `(site, source, variable[, lead_bucket])`
  from available training pairs **and** the skill gate (Plan 13 result).
- The correction lifecycle: `correct_apply()` (daily; applies current calibration)
  and the `correct_refit()` skeleton (monthly; delegates fitting to Plan 12).
- Per-value provenance: record the correction `tier` (`raw`/`physical`/…).

**Out:**
- The fitted statistics (mean-bias, qmap, EMOS, MBC), shrinkage, consistency pass,
  and model-only experiments — **all Plan 12**.
- Verification/skill computation — **Plan 13** (this plan *consumes* its verdict).

## Prerequisites

Plans 00–03, 10 (shares `R/transfer.R`). Interlocks with Plans 12 and 13.

## Background

SCOPING §7.1 (tier table; day-0 physical adjustments always applied, never fitted;
**fixed lapse rate is wrong under inversions — document**; log-wind profile needs
`z0`; tier selection automatic and **enforced**; **skill-gated promotion**;
calibrations persisted **as data**, manifest-tracked; daily applies, monthly
refits; SILO correction a separate daily-scale QM), §7.3 (model-only default =
raw pass-through with tier `raw`).

## File layout

```
R/correct.R               # correct_apply(), correct_refit() skeleton, provenance stamping
R/correct-physical.R      # day-0 physical adjustments
R/tier-select.R           # tier gates: data-availability + skill gate
tests/testthat/test-correct-physical.R
tests/testthat/test-tier-select.R
tests/testthat/test-correct-apply.R
```

## Detailed design

### Physical adjustments (`R/correct-physical.R`)

Deterministic, always applied at day 0 (SCOPING §7.1), superseded by fitted
corrections once overlap exists:

- **Sensor-height / log-wind profile** — bring wind between the instrument height
  and a reference (10 m, and to model heights for comparison) via the log-wind
  profile using the registry `z0` and displacement height (Plan 02). **Requires
  `z0`** — abort `"missing_roughness"` if absent (the registry already enforces
  this, but re-check here). Document the **neutral-stability assumption** and that
  it fails in stable/decoupled regimes (nocturnal inversions inland — the regime
  §7.3 flags as most consequential).
- **Altitude / lapse** — adjust temperature for elevation difference (site vs
  source grid) using the standard environmental lapse rate. **Document that a
  fixed lapse rate is wrong under inversions** (review note) — it is a day-0
  crutch, superseded by fitting.
- **Pressure reduction** — surface ↔ MSL using the standard atmosphere where a
  variable pair requires it.

Each returns corrected values tagged tier `physical`. These are the *only*
correction available before overlap accrues and for any variable/lead the gates
keep at the physical tier.

### Tier selection (`R/tier-select.R`)

`tier_select(site, source, variable, lead_bucket = NA, training_summary,
skill_verdict)` → one of `TIER_LEVELS`. Two gates, **both** must pass to use a
higher tier (SCOPING §7.1, with the review’s skill gate):

1. **Data-availability gate** (the original tier ladder):
   - `< overlap/pairs for mean_bias` → `physical`.
   - `1–6 mo` → `mean_bias`.
   - `6 mo – 2 yr` → `qmap`.
   - `≥ 2 yr + archive` → `emos`.
   `training_summary` supplies the pair counts / overlap length per
   `(source, variable, lead_bucket)` (from the SCOPING §4 calibration training
   store — training pairs are assembled at refit time by joining the Plan 03
   forecast archive with the curated observations; Plan 16's backfill seeds
   both sides). **For Open-Meteo sources, daily-lead pairs exist from day 0
   via Previous Runs (SCOPING §7.2)** — so the availability gate can reach `emos`
   at daily leads immediately, trained against `history_daily` pseudo-truth; the
   truth-source distinction is recorded in provenance (SCOPING §7.1 review note).
2. **Skill gate** (review fix): promotion to the more complex tier is allowed only
   if Plan 13’s rolling-origin verification shows an **out-of-sample skill
   improvement over the incumbent that survives the block bootstrap**. If the
   skill verdict is “no significant improvement”, **stay at the lower tier** even
   when data volume permits the higher one. Data volume alone never demonstrates
   the complex method stopped overfitting.

The choice is **enforced**, not advisory: `correct_apply` uses exactly the tier
`tier_select` returns and refuses to apply a calibration whose manifest tier
disagrees (abort `"tier_mismatch"`).

### Correction lifecycle (`R/correct.R`)

- `correct_apply(store_root, site, source, target, variables, leads = NULL,
  now = .now())` — for each `(variable[, lead_bucket])`: read the current
  calibration (Plan 03), apply it (delegating the tier’s `apply` fn to Plan 12;
  the `physical` tier is applied here), stamp the correction `tier` into
  provenance, and return corrected values. `target` distinguishes correcting a
  **forecast** (adds Plan 12 shrinkage) from correcting a **donor/record** (no
  shrinkage) — the same distinction as Plan 10.
- `correct_refit(store_root, site, source, variables, now)` — the **monthly** job
  skeleton: assemble training pairs, call `tier_select`, delegate fitting to Plan
  12’s per-tier `fit` fns, request the Plan 13 skill verdict, write the new
  calibration + manifest bump (Plan 03) **only if** the skill gate passes. This
  plan implements the orchestration and the `physical`-tier path; the fitted tiers
  are Plan 12 hooks.
- **Model-only variables** default to tier `raw` (pass-through), provenance `raw`
  (SCOPING §7.3). The `profile_rescale` experiment is Plan 12.
- **SILO correction** is registered as a separate daily-scale QM target
  (`source = "silo"`, daily) using the same lifecycle.

## Test requirements

### `test-correct-physical.R`
- Log-wind profile brings a known 2 m wind to 10 m correctly for a given `z0`
  (assert against a hand-computed value); absent `z0` aborts `"missing_roughness"`.
- Lapse adjustment shifts temperature by the expected amount for an elevation
  delta; a code comment / roxygen documents the inversion caveat (assert the
  documented behaviour, and that the function exposes the lapse rate as a
  parameter so a site can override it).
- Physical-tier output is tagged tier `physical`.

### `test-tier-select.R`
- Availability gate returns the right tier for each overlap band.
- **Skill gate:** with sufficient data but a “no significant improvement” skill
  verdict, `tier_select` stays at the **lower** tier (directly tests the review
  fix). With a passing verdict, it promotes.
- Open-Meteo daily-lead path can reach `emos` from day 0 (data gate), and the
  provenance records the pseudo-truth training source.
- `correct_apply` refuses a calibration whose manifest tier ≠ selected tier
  (`"tier_mismatch"`).

### `test-correct-apply.R`
- Day-0 (no calibration) path applies only physical adjustments and stamps tier
  `physical`.
- Model-only variables pass through raw with tier `raw`.
- `correct_apply(target = "forecast")` routes through the shrinkage wrapper hook
  (mock Plan 12) while `target = "record"` does not — proving the framework
  respects the forecast/record distinction.

## Definition of done

Shared skeleton plus:
- `correct_apply()`, `correct_refit()` (skeleton), `tier_select()`, and the
  physical adjustments exist; the public verbs stay in Plan 16, so keep these
  internal-but-documented.
- The skill gate is wired so a failing verdict **blocks promotion** — proven by
  test even though Plan 13 supplies the real verdict.
- Physical adjustments document their stability caveats.
- New condition classes registered in `meteo_conditions()`.
