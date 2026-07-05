# Plan 16 — Pipeline verbs

## Objective

Wire everything together into the four exported, user-scheduled verbs —
`met_sync_live()`, `met_sync_daily()`, `met_refit()`, `met_backfill()` — each
multi-site, incremental (watermarks), and idempotent, with the archive-on-every-
sync forecast policy and the graceful-degradation posture. This is the top of the
stack: it composes adapters (04–08) → curation (09–10) → correction (11–12) →
verification (13) → storage (03), and does no new science itself.

## Scope

**In:**
- The four verbs and their orchestration.
- Forecast archiving policy (dedup on `(source, model, issue_time)`) on every sync.
- Incrementality (watermarks + refetch windows) and idempotency across all verbs.
- Degradation handling (BOM transport failures, ECMWF/terra absence) — continue,
  flag, don’t crash.
- The per-site donor-coverage audit in `met_backfill()`.
- A scheduling **vignette** (cron / GitHub Actions / `taskscheduleR`) — code stays
  out of the package (SCOPING §9).

**Out:**
- Any new statistics/IO primitives — all live in earlier plans; this plan only
  sequences calls to them.

## Prerequisites

Plans 00–15 (this is the integration layer).

## Background

SCOPING §9 (the four verbs + their work; all multi-site, incremental, idempotent;
**forecast archiving policy** — every sync archives new issuances, dedup on
`(source, model, issue_time)`; hourly users capture most BOM cycles, daily users
get daily snapshots; Open-Meteo gaps self-heal via Previous/Single Runs, **BOM
gaps cannot be backfilled**; EC46 daily + SEAS5 monthly captured by daily sync;
Parquet compaction monthly), §7.2 (`met_backfill` day-0 bootstrap: SILO + ERA5 +
Previous-Runs/Historical-Forecast pulls; ingest historical AWS exports + pre-
existing ad-hoc forecast archives; initial fits; **per-site donor-coverage audit
incl. GHCNh completeness**), §5.1 (best-effort near-real-time; degradation window
~1 week to GHCNh), §13 (risks the pipeline must degrade around).

## File layout

```
R/pipeline.R              # shared orchestration helpers (per-site loop, error isolation)
R/met-sync-live.R
R/met-sync-daily.R
R/met-refit.R
R/met-backfill.R
R/archive-forecasts.R     # the archive-on-every-sync helper (dedup)
vignettes/scheduling.Rmd  # cron / GH Actions / taskscheduleR examples (not package code)
tests/testthat/test-met-sync-live.R
tests/testthat/test-met-sync-daily.R
tests/testthat/test-met-refit.R
tests/testthat/test-met-backfill.R
tests/testthat/test-archive-forecasts.R
tests/testthat/test-pipeline-idempotency.R
```

## Detailed design

### Orchestration helpers (`R/pipeline.R`)

- `for_each_site(sites, fn, on_error = c("isolate","stop"))` — run `fn` per site;
  default **isolate** so one site’s failure (a dead BOM channel) doesn’t abort the
  others (SCOPING §5.1 degradation). Collect per-site status into a returned
  summary tibble `(site_id, verb, status, messages)`.
- All verbs take `sites` (a `met_site` or `met_sites`), `now = .now()`, and a
  config (Plan 14). All are **idempotent** — re-running does not duplicate rows
  (Plan 03 dedup/supersede) or double-advance watermarks.

### `archive_forecasts()` (`R/archive-forecasts.R`)

The shared archive-on-every-sync helper (SCOPING §9): for each configured forecast
source, fetch the current issuance window, and write via
`store_write_forecast()` which **dedups on `(source, model, issue_time)`** (Plan
03). Called by both sync verbs. Records that **BOM gaps cannot be backfilled**
(no-op-with-note when an issuance was missed) while Open-Meteo gaps are left for
`met_backfill` to self-heal via Previous/Single Runs.

### `met_sync_live()` (`R/met-sync-live.R`) — hourly, optional

Per site (SCOPING §9): fetch AWS (`source_rest`/`source_file`) + near-real-time
BOM head (`source_bom_obs`, opt-in transports); QC (`qc_run`) + fill (`fill_run`)
the **live window** only; apply current calibrations (`correct_apply`, `target =
"forecast"` for the forecast head, `"record"` for obs); `archive_forecasts()`.
GHCNh is **not** used for the live head (its ~1-week lag — Plan 06 cadence
metadata); near-real-time is **best-effort** (SCOPING §5.1). Advance the live
watermark.

### `met_sync_daily()` (`R/met-sync-daily.R`) — daily

Per site (SCOPING §9): fetch/refresh **all** forecasts (Open-Meteo incl. seasonal
EC46+SEAS5, BOM) and `archive_forecasts()`; extend `history_hourly` (AWS + QC +
fill); pull GHCNh backfill (respecting its lag) into `history_hourly`; refresh the
`history_daily` tail from SILO — using the **refetch window** so SILO revisions
supersede (Plan 03/06). Advance the daily watermarks per source.

### `met_refit()` (`R/met-refit.R`) — monthly

Per site (SCOPING §9): `correct_refit()` (Plan 11) which fits candidate tiers
(Plan 12), runs `verify_run()` (Plan 13) for the **skill verdict**, and bumps the
calibration manifest **only if the skill gate passes**; write the verification
report; run **Parquet compaction** (Plan 03 partition compaction). Idempotent: a
second run in the same month re-verifies but only re-writes calibrations if the
data changed.

### `met_backfill()` (`R/met-backfill.R`) — ad hoc, day-0 bootstrap

Per site (SCOPING §7.2, §9): full SILO + ERA5 + Open-Meteo Previous-Runs /
Historical-Forecast pulls; ingest historical AWS exports (`source_file`) and any
pre-existing ad-hoc forecast archives; initial calibration fits; and a **per-site
donor-coverage audit** — `station_coverage()` (Plan 06) for the nearest GHCNh (and
BOM) stations, returned so the operator sees coverage gaps **before** relying on
the donor ladder (SCOPING §13). Self-heals Open-Meteo forecast gaps via
Previous/Single Runs; explicitly **cannot** backfill BOM forecast gaps (documents
it in the returned summary).

### Scheduling vignette (`vignettes/scheduling.Rmd`)

Shows cron, GitHub Actions, and `taskscheduleR` recipes for the four verbs, and
the trade-off note: **hourly sync captures most BOM issue cycles; daily-only users
get daily snapshots and miss intra-day BOM issuances that can’t be backfilled**
(SCOPING §9). This is documentation, not package code (scheduling is the user’s).

## Test requirements

Everything mocked (adapters replay fixtures; store under `local_tempdir()`); **no
live calls**, **frozen clock**.

### `test-archive-forecasts.R`
- New issuances are archived; re-running with the **same** issuances is a no-op
  (dedup on `(source, model, issue_time)`).
- A missed BOM issuance is noted as non-backfillable; a missed Open-Meteo issuance
  is flagged for Previous/Single-Runs self-heal.

### `test-met-sync-live.R`
- Live window is QC’d, filled, corrected, and forecasts archived; GHCNh is **not**
  called for the live head (assert no GHCNh fetch).
- A dead BOM channel degrades gracefully (site status `degraded`, not an error;
  other sites unaffected via `for_each_site(on_error = "isolate")`).

### `test-met-sync-daily.R`
- Forecasts (incl. seasonal) archived; `history_hourly`/`history_daily` extended;
  SILO refetch window supersedes a revised value (ties to Plan 03/06).

### `test-met-refit.R`
- Refit fits candidates, runs verification, and **only bumps the manifest when the
  skill gate passes** (mock `skill_verdict` both ways — the end-to-end skill-gated
  promotion from Plans 11/13).
- Compaction runs and reduces partition file count without changing readable rows.

### `test-met-backfill.R`
- Day-0 bootstrap pulls history, ingests an AWS export fixture, makes initial
  fits, and returns a **donor-coverage audit** flagging a variable with no nearby
  GHCNh coverage (SCOPING §13).
- BOM forecast gaps are reported non-backfillable; Open-Meteo gaps self-heal.

### `test-pipeline-idempotency.R`
- Each verb run **twice** over the same inputs/clock yields identical store
  contents (row counts, watermarks) — the cross-cutting idempotency guarantee
  (SCOPING §9).
- Multi-site: a two-site `met_sites` processes both; one site failing leaves the
  other complete and reports per-site status.

## Definition of done

Shared skeleton plus:
- `met_sync_live()`, `met_sync_daily()`, `met_refit()`, `met_backfill()` exported/
  documented; all multi-site, incremental, idempotent — proven by
  `test-pipeline-idempotency.R`.
- Archive-on-every-sync dedup, skill-gated manifest bumps, SILO-revision
  supersession, and the donor-coverage audit are each proven end-to-end.
- Degradation is graceful: a dead channel or absent `terra` yields a flagged,
  continued run, never a crash.
- The scheduling vignette builds and documents the BOM backfill trade-off.
- `NEWS.md` records the v1 pipeline; this completes the 00–16 series.
