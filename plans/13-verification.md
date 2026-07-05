# Plan 13 — Verification engine

## Objective

Implement the verification engine the review rebuilt: **rolling-origin,
out-of-sample** scoring against baselines, calibration diagnostics for
probabilistic products, and **block-bootstrap uncertainty on skill differences** —
which together produce the **skill verdict** that gates tier promotion (Plan 11)
and sets the shrinkage weights (Plan 12). Run by the monthly refit as a report.

## Scope

**In:**
- Rolling-origin evaluation over archived `(forecast, observation)` pairs.
- Deterministic scores (MAE, RMSE) and probabilistic scores (CRPS via
  `scoringRules`) by lead bucket, per variable, per tier.
- Baselines: raw model, persistence, climatology; skill scores relative to them.
- Calibration diagnostics: PIT/rank histogram, spread–error ratio, Brier score +
  reliability for rain occurrence.
- Block bootstrap for CIs on score differences.
- `skill_verdict()` — the promote/keep decision consumed by Plan 11, and the
  per-lead shrinkage weights consumed by Plan 12.
- A renderable verification report table.

**Out:**
- Fitting corrections (Plan 12) and selecting tiers (Plan 11) — this plan only
  *scores* and *judges*.
- Dashboard rendering (consumers read the report table via Plan 14).

## Prerequisites

Plans 00–03, 11, 12.

## Background

SCOPING §7.4 (the rebuilt section: **out-of-sample by construction** via rolling
origin; **against baselines** raw/persistence/climatology; **calibration
diagnostics** PIT/rank/spread–error/Brier; **uncertainty on skill differences**
block bootstrap; tier promotion requires improvement surviving the bootstrap),
§4 (long-range verified as probabilistic — CRPS, PIT against `history_daily`;
per-member trajectories needed for cumulative bands), §7.1 (skill-gated promotion;
the consistency-pass violation rate is a red flag to surface here).

## File layout

```
R/verify.R                # verify_run(): assemble pairs, rolling origin, scores, report
R/verify-scores.R         # MAE/RMSE/CRPS + skill scores vs baselines
R/verify-baselines.R      # persistence, climatology, raw model baselines
R/verify-calibration.R    # PIT, rank histogram, spread-error, Brier + reliability
R/verify-bootstrap.R      # block bootstrap of score differences
R/verify-verdict.R        # skill_verdict(): promote/keep + shrinkage weights
tests/testthat/test-verify-scores.R
tests/testthat/test-verify-baselines.R
tests/testthat/test-verify-calibration.R
tests/testthat/test-verify-bootstrap.R
tests/testthat/test-verify-verdict.R
tests/testthat/test-verify-run.R
```

Add `scoringRules` to `Imports`.

## Detailed design

### Rolling-origin evaluation (`R/verify.R`)

The load-bearing correctness property (SCOPING §7.4): **every score is computed on
data outside the fit’s training window.** Given the calibration training store and
archive:

- Walk origins forward in time. At each origin `t`: use only pairs with
  `issue_time < t − buffer` to represent “the calibration in force at `t`”, and
  score its predictions on pairs issued at `t..t+step`. Never score a calibration
  on its own training window (a monthly refit verified on its training period
  inflates skill and would corrupt the Plan 11 gate).
- Aggregate scores by `(variable, lead_bucket, tier)`.

### Scores (`R/verify-scores.R`)

- Deterministic: MAE, RMSE per `(variable, lead_bucket)`, before and after
  correction.
- Probabilistic: **CRPS** via `scoringRules` for ensemble/EMOS predictive
  distributions; use **per-member trajectories** for cumulative quantities so
  bands are accumulated per member *then* quantiled (SCOPING §4 — daily percentiles
  can’t be validly summed).
- Skill score `= 1 − score/score_baseline` against each baseline.

### Baselines (`R/verify-baselines.R`)

- **Raw model** — the uncorrected forecast (isolates whether correction adds value).
- **Persistence** — last observation carried forward.
- **Climatology** — the `history_daily` seasonal distribution (also the shrinkage
  target for Plan 12). Without baselines, “after beats before” can’t distinguish a
  real gain from a lead where climatology already wins (review point).

### Calibration diagnostics (`R/verify-calibration.R`)

CRPS alone conflates sharpness and reliability, so also compute (SCOPING §7.4):

- **PIT histogram** (continuous predictive) / **rank histogram** (ensemble) —
  flat = calibrated; U/∩ shapes = under/over-dispersed.
- **Spread–error ratio** — ensemble spread vs RMSE of the mean.
- **Brier score + reliability diagram** for **rain occurrence** (probability of
  precipitation).

### Block bootstrap (`R/verify-bootstrap.R`)

Verification series are autocorrelated, so a naive CI is too tight. Implement a
**moving-block bootstrap** on the score-difference series (corrected − incumbent,
and model − baseline), returning a CI and a significance flag. Block length is a
documented parameter (default from the series autocorrelation).

### The verdict (`R/verify-verdict.R`)

`skill_verdict(scores, bootstrap)` → per `(variable, lead_bucket)`:

- `promote` (TRUE/FALSE) — TRUE only if the candidate tier’s **out-of-sample
  improvement over the incumbent survives the block bootstrap** (the review’s
  skill gate; consumed by Plan 11 `tier_select`).
- `shrink_weight` (per lead bucket, 0..1) — from the skill score vs climatology:
  skill ≤ 0 → weight 0 (fall back to climatology); high skill → weight ≈ 1
  (consumed by Plan 12 `shrinkage`).
- `consistency_violation_rate` — surfaced from Plan 12’s consistency pass; a rising
  rate is flagged (SCOPING §7.1 red flag).

### Report

`verify_run(store_root, site, sources, now)` runs the above and writes a renderable
report (a tidy tibble, plus the diagnostics as list-columns or a companion table)
into the store, for dashboards (Plan 14 `met_verification()`).

## Test requirements

### `test-verify-scores.R`
- MAE/RMSE/CRPS match hand-computed values on tiny fixtures.
- Skill score is 0 when equal to baseline, positive when better, negative when
  worse.
- Cumulative-quantity CRPS uses per-member accumulation (a fixture where summing
  daily percentiles gives a *different, wrong* answer proves the per-member path).

### `test-verify-baselines.R`
- Persistence and climatology baselines are computed correctly; at a long lead on
  a low-skill fixture, climatology **beats** the raw model (so “correction helps”
  must be judged against it, not just against raw).

### `test-verify-calibration.R`
- A calibrated ensemble yields a ~flat rank histogram; an under-dispersed one
  yields a U-shape (assert the shape statistic).
- Spread–error ratio ≈ 1 for a well-calibrated fixture.
- Brier score + reliability computed correctly for a PoP fixture.

### `test-verify-bootstrap.R`
- On an autocorrelated series, the moving-block CI is **wider** than a naive iid
  bootstrap (proving the autocorrelation handling matters).
- A tiny-but-noisy improvement is judged **not significant**; a large consistent
  one is significant.

### `test-verify-verdict.R`
- `skill_verdict` returns `promote = FALSE` when the improvement doesn’t survive
  the bootstrap (feeds Plan 11’s gate) and `TRUE` when it does.
- `shrink_weight` is 0 at/below climatology skill and near 1 at high skill (feeds
  Plan 12).

### `test-verify-run.R`
- **Rolling-origin correctness:** a deliberately over-fit calibration scores well
  *in-sample* but the rolling-origin evaluation reports its true (poor)
  out-of-sample skill — proving no training-window leakage (the central review fix).
- The report table is well-formed and readable back via the store.

## Definition of done

Shared skeleton plus:
- `verify_run()` and `skill_verdict()` exist; `skill_verdict` is wired into Plan 11
  (`tier_select`) and Plan 12 (`shrinkage`) — end-to-end, a failing verdict blocks
  promotion and pulls forecasts toward climatology.
- Rolling-origin (no in-sample leakage), baselines, calibration diagnostics, and
  block bootstrap are each proven by a dedicated test.
- The verification report is stored and retrievable (Plan 14 reads it).
- New condition classes registered in `meteo_conditions()`.
