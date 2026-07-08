# Implementer prompt — meteoTidy post-audit fixes

You are fixing defects found in a post-implementation audit of `meteoTidy`
(branch `implementation/plans-00-16`). The package builds and its suite passes,
but several plan requirements are unmet or wired incorrectly. Work through the
items below **in priority order**. After each item, run the relevant tests.

Acceptance tests for these fixes already exist and are currently **red or
skipped** on purpose — your job is to make them green:

- `tests/testthat/test-conditions.R` → "registers every condition class the
  source actually raises" (**red**).
- `tests/testthat/test-audit-followups.R` → ECMWF u/v recombination (**red**).
- `tests/testthat/test-audit-followups.R` → `assemble_verification_pairs()` real
  store-read (**passes today** — keep it passing; do not re-mock it away).
- `tests/testthat/test-audit-followups.R` → wide-emitter provenance + ensemble
  handling (**2 skips**; un-skip and make pass — item 6).
- `tests/testthat/test-met-refit.R` → "fits+writes a calibration through the
  REAL correct_refit()…" (**skipped**; un-skip and make it pass).

Do **not** weaken a test to make it pass. Do **not** mock the function under
test to fake coverage (that is exactly the anti-pattern item 1 fixes).

---

## 1. [BLOCKING] Wire the correction / verification pipeline

**Problem.** The whole fitted-correction + skill-gated-promotion + shrinkage
system exists only as isolated, unit-tested leaf functions. The trunk is
stubbed, and the stub comments are stale ("Plan 13 does not exist yet" — it
does). Net effect today: **no calibration is ever fitted or written**, so
`correct_apply()` always falls through to the day-0 `physical` tier, the
`qmap`/`emos` tiers are dead outside their own tests, and forecast shrinkage is
a no-op.

Evidence:
- `R/correct.R:212` `correct_refit()` is `invisible(NULL)` — a total stub. It is
  called by `met_refit()` (`R/met-refit.R:54`) and `met_backfill()`
  (`R/met-backfill.R:61`), so **both scheduled verbs fit nothing**.
- `R/correct.R:27` `.correct_forecast_climatology()` calls
  `shrink_to_climatology(value, climatology = value, weight = 1)` — a no-op.
  Plan 12's "core statistical fix" (lead-dependent shrinkage toward
  climatology) never happens.
- `R/correct.R:61` `.correct_apply_fitted()` uses an "until Plan 12 lands"
  offset-only fallback and never delegates to `apply_mean_bias()` /
  `apply_qmap()` / `apply_emos()`.
- `calib_write()` is never called from executable code — the only `calib_write`
  that runs inside `met_refit()` is the one **inside the test's mock of
  `correct_refit`** (`tests/testthat/test-met-refit.R`).
- `apply_emos()`/`fit_emos()` are called **nowhere** outside `R/tier-emos.R`.
  (Note: `R/transfer.R` has its own private `.fit_mean_bias`/`.fit_qmap` for the
  gap-fill path — those are a *separate* family and are correctly wired; do not
  confuse them with Plan 12's `fit_mean_bias`/`fit_qmap`.)

**What to change.**

1. Implement `correct_refit(store_root, site, source, variables, now)` to do the
   flow already documented in its own roxygen (`R/correct.R:192`):
   a. Assemble training pairs by joining the Plan 03 forecast archive with
      curated observations (SCOPING §4). **This already exists** —
      `assemble_verification_pairs()` (`R/verify.R:67`) produces exactly the
      `forecast_obs_pairs()`-shaped (forecast, observation) tibble you need;
      reuse it (don't write a parallel assembler). **Caveat:** its real
      store-read path was previously exercised by no test (`test-verify-run.R`
      mocks it out) — `test-audit-followups.R` now covers it; keep that test
      green. **Exclude Historical-Forecast `lead_time = NA` rows** from any
      lead-aware training (SCOPING §7.2 — `fit_emos()` already refuses them;
      keep that contract).
   b. Summarise pairs into a `training_summary` and call `tier_select()`.
   c. Fit the selected tier via Plan 12's `fit_mean_bias()`/`fit_qmap()`/
      `fit_emos()`.
   d. Obtain the Plan 13 skill verdict (`verify_run()` / the leaf
      `skill_verdict()`), and `calib_write()` **only if the gate promotes**;
      otherwise keep the incumbent calibration.
2. Make `.correct_apply_fitted()` (`R/correct.R:61`) delegate to the real
   per-tier `apply_*()` function for the manifest's tier, not the offset-only
   fallback.
3. Make `.correct_forecast_climatology()` (`R/correct.R:27`) read a real
   climatology series and a **verified per-lead-bucket skill weight** (Plan 13)
   and pass them to `shrink_to_climatology()`. Delete the stale "Plan 13 does
   not exist yet" comments here and at `R/correct.R:19`.
4. In `tests/testthat/test-met-refit.R`: keep an orchestration-shell test if you
   like, but the **"end-to-end Plans 11/13" claim must be honoured by mocking
   the leaf `skill_verdict` (both ways), not `correct_refit`**. Un-skip the
   acceptance test "fits+writes a calibration through the REAL correct_refit()"
   and seed it with overlapping archived-forecast + curated-obs pairs so the
   real fit has data.

**Done when:** the un-skipped met-refit acceptance test passes; `met_refit()`
with a promoting verdict writes exactly one calibration and `correct_apply()`
then applies it (not `physical`); a failing verdict writes nothing.

---

## 2. [HIGH] Add ECMWF u/v → wind speed/direction recombination

**Problem.** `source_ecmwf()` advertises `wind_speed_10m` / `wind_direction_10m`
in `provides`, but `fetch_forecast()` restricts output to single-param
("direct") variables, so a request for ECMWF wind returns **zero rows and no
error** (`R/source-ecmwf.R`, `.ecmwf_param_lookup()` + the `direct_vars`
restriction). Everything needed already exists: `R/wind-uv.R:34` `uv_to_dir()`
is the exact from-direction formula (and is unit-tested), speed is
`sqrt(u²+v²)`, `grib_field_table()` already decodes `10u`/`10v` bands with
`step`/`member`, and the index/download path already selects them.

**What to change.**
- Add a pure seam `.ecmwf_uv_to_wind(field_tbl, values)` where `field_tbl` is a
  `grib_field_table()`-shaped tibble (`band`, `param`, `unit`, `step`, `member`)
  and `values` is the per-band extracted vector (aligned to `field_tbl$band`).
  It pairs `"10u"`/`"10v"` bands on `(step, member)` and returns long rows with
  `variable ∈ {wind_speed_10m, wind_direction_10m}`, `value` = `hypot(u,v)` /
  `uv_to_dir(u,v)`, carrying `member`/`step`. **Reuse `uv_to_dir()` — do not
  re-derive the angle.**
- Call it from `fetch_forecast()` for the derived wind variables and `vec_rbind`
  with the direct-param rows. `10u`/`10v` are already SI (`m/s`), so no unit
  conversion; wind speed canonical unit is `m/s`, direction `degree`.
- The contract is pinned by `tests/testthat/test-audit-followups.R` — make it
  pass. (End-to-end verification against a real CCSDS GRIB needs a libaec-
  enabled GDAL; the unit test above does **not**, so it must pass here.)

**Done when:** the u/v acceptance test is green and requesting ECMWF wind yields
canonical `wind_speed_10m`/`wind_direction_10m` rows per member.

---

## 3. [HIGH] Register the four missing condition classes

**Problem.** These classes are raised via `abort_meteo()` but are absent from
`meteo_conditions()`, so the user-facing error taxonomy is incomplete (every
plan's DoD requires registration):
- `secret_unresolved` (`R/secrets.R:28`)
- `bad_secret_ref` (`R/secrets.R:44`)
- `secret_leak` (`R/secrets.R:95`)
- `unknown_ecmwf_stream` (`R/source-ecmwf.R:141`)

**What to change.** Add all four to the `meteo_conditions()` registry
(`R/conditions.R`) with a one-line `meaning` each.

**Done when:** the new drift-guard test in `test-conditions.R` ("registers every
condition class the source actually raises") is green. That test scans the
source on every run, so it will catch any future unregistered class too.

---

## 4. [HIGH] Fix the weatherOz version pin (and verify against the real API)

**Problem.** `DESCRIPTION` pins `weatherOz (>= 2.0.2)`, but the plan and scope
require `>= 3.0.0` — and **3.0.0 is the current CRAN release** (the machine that
built this branch merely has a stale 2.0.2 installed). weatherOz 3.0.0 shipped a
**breaking DPIRD column restructure** and removed BOM ag-bulletin functions, so
`source_silo()` and its recorded test fixtures may be written against the wrong
(2.0.2) API shape.

**What to change.**
- Bump `DESCRIPTION` Imports to `weatherOz (>= 3.0.0)`.
- Install weatherOz 3.0.0 and verify `source_silo()` against its actual
  PatchedPoint/DataDrill return shape; re-record `tests/testthat/_fixtures/silo/`
  frames if the columns changed. Confirm the SILO quality-code → provenance
  mapping still matches 3.0.0's fields.

**Done when:** the package works against weatherOz 3.0.0 with the pin corrected.

---

## 5. [MEDIUM] ECMWF adapter completeness (post-scaffolding)

`source_ecmwf()` currently only fetches temperature at a single, caller-supplied
instant. Two documented simplifications should be finished (or explicitly marked
post-v1 in roxygen, your call — but decide, don't leave them silent):
- `.ecmwf_resolve_issue_times()` (`R/source-ecmwf.R`) treats `now` as *the*
  issue cycle. Implement rounding to the real 00/06/12/18Z schedule and support
  multiple cycles spanning `issue_window`.
- `.ecmwf_download_messages()` over-counts bytes when `Range` is not honoured; it
  works only because the test mocks whole-file responses. Verify against a real
  HTTP 206 partial response.

The stream default is now correctly `enfo` (see the "Deviation from SCOPING §5.2"
note in `plans/08` — do not change it back to `eefo`).

---

## 6. [LOW] meteoHazard wide-emitter provenance and ensemble handling

Both are covered by skipped acceptance tests in `test-audit-followups.R`
("wide emitter provenance + ensemble handling") — un-skip and make them pass.

- `R/met-wide.R:58` `.met_wide_provenance()` hardcodes `tier = "raw"` for every
  column. Once item 1 lands, thread the **real per-variable correction tier**
  through to the wide `met_table` provenance (SCOPING §3.1).
- `R/met-wide.R:36` `.widen_forecast()` keeps one value per `(valid_time,
  variable)` (first row wins), silently collapsing ensemble members. Decide the
  intended wide-forecast semantics for ensembles (member column? mean?) and
  implement, or document the single-value contract explicitly.

---

## 7. [LOW] Two documented simplifications from the 09–16 audit

Not blocking, but decide-and-mark rather than leave silent:

- `R/source-ghcnh.R:47` `.ghcnh_qc_flag()` always returns `"ok"`. Plan 06 says
  the `qc_flag` should be **mapped from worldmet's quality field**. Once the
  GHCNh/worldmet quality vocabulary is confirmed, replace the constant with a
  real lookup (and add a fixture row exercising a non-`ok` code).
- `R/model-only.R:62` the diagnostic BLH is "a simple placeholder", not the
  AERMET-style scheme Plan 12 §7.3 describes. It is experimental / off by
  default, so this is acceptable for v1 — but say so explicitly in the roxygen
  (mark it experimental-approximation) rather than implying a full scheme.

---

## Notes / environment

- **CCSDS/libaec:** real ECMWF Open Data GRIB2 uses CCSDS compression; the GRIB
  end-to-end tests (`test-grib-read.R`) `skip` unless GDAL was built with
  libaec. The CRAN macOS `terra` binary bundles a GDAL **without** libaec, so
  those stay skipped locally — verify the ECMWF end-to-end path on CI or a
  source `terra` built against a libaec-enabled GDAL. The u/v unit test (item 2)
  needs none of this and must pass everywhere.
- After all items: `devtools::document()`, `devtools::test()`, `R CMD check`.
  Target state: zero failures, zero warnings, and only the intentional
  CCSDS/CRAN skips remaining.
