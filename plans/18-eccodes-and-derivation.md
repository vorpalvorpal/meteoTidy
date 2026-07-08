# Plan 18 — eccodes-only GRIB read, and a deterministic derivation fill tier

Two independent post-v1 changes agreed after the plan-17 review. They touch
disjoint files and can be implemented in parallel.

> **Supersedes** the terra/GDAL GRIB-read design in
> [`08-acquisition-ecmwf.md`](08-acquisition-ecmwf.md) (Part A below), and
> **extends** the fill ladder in
> [`10-curation-gapfill.md`](10-curation-gapfill.md) (Part B below). SCOPING §6,
> §10, §13 carry dated pointers to this plan.

Work each part **tests first**, then implementation, then the r-science `verify`
gate (load_all clean, `devtools::test()` green, `document()` no diff,
`lintr::lint_package()` clean, `R CMD check` no new ERROR/WARNING).

---

## Part A — eccodes-only GRIB read (drop terra/GDAL from the ECMWF path)

### Why
`grib_field_table()` reads GRIB band metadata through `terra::meta()`, whose
`GRIB_ELEMENT` string (an NCEP-table translation) **varies across GDAL
versions** — the drift SCOPING §13's post-audit addendum records. On the dev
GDAL 3.8.5 it is `"TMP"` → mapped to `"2t"`; on CI's newer GDAL it resolves
differently, so `param`/`member` come out wrong and the ECMWF tests had to be
`skip_on_ci()`'d. This is a brittle-coupling bug on our side triggered by
legitimate GDAL version variance.

**Decision:** stop using terra/GDAL for GRIB entirely and read everything
through **eccodes** (ECMWF's own library, already provisioned by
`ecmwf_install_eccodes()`). eccodes reads ECMWF-native identifiers
(`shortName`, `step`, `perturbationNumber`, `units`) with no NCEP-table
translation to drift, decodes CCSDS/AEC natively (no libaec-GDAL needed), and
does nearest-gridpoint extraction — all in **one** `grib_ls` call. This makes
the GRIB path deterministic and GDAL-version-independent.

**Accepted consequences (see SCOPING §10/§13):**
- eccodes becomes a **hard requirement** for `source_ecmwf()` (was an opt-in
  fallback). A fetch with no eccodes aborts `eccodes_required`, pointing at
  `ecmwf_install_eccodes()`.
- CI installs eccodes so the GRIB path is *tested*, not skipped.
- `terra` is dropped from the GRIB path (and from `Suggests` unless still used
  elsewhere — it is not).

### Design
The existing eccodes seam (`R/ecmwf-eccodes.R`) already provides
`.eccodes_grib_ls_path()`, `.have_eccodes()`, and
`.eccodes_extract_point(path, lat, lon)` (which shells `grib_ls -j -p
perturbationNumber -l lat,lon,1`). Generalise it to also carry the
**metadata** each band needs, so one call replaces terra's open + meta +
extract:

1. **New unified reader** in `R/ecmwf-eccodes.R` (or a renamed
   `R/grib-read.R`): `grib_point_table(path, lat, lon)` → a tibble, one row per
   GRIB message, with columns
   `band` (integer, file order), `param` (`shortName`, e.g. `"2t"`/`"10u"`/
   `"10v"`), `unit` (raw eccodes unit → udunits via
   `.eccodes_unit_to_udunits()`), `step` (character, forecast lead in hours),
   `member` (integer, `perturbationNumber`; `NA` if not applicable), and
   `value` (double, nearest-gridpoint, in eccodes' native unit).
   Implement by extending the existing `grib_ls` args to
   `-j -p shortName,step,perturbationNumber -l lat,lon,1` and extending
   `.eccodes_parse_nearest_json()` to pull `shortName`/`step` from `msg$keys`.
   `step` may be reported as e.g. `"24"` or `"24h"` — normalise to plain hours.

2. **Rewrite `grib-read.R`'s public seams onto eccodes** (keep the names
   `source-ecmwf.R` already calls, to minimise churn there):
   - `grib_field_table(rast)` and `grib_extract_point(rast, lat, lon)` are
     replaced by the single `grib_point_table(path, lat, lon)` above. Update
     `R/source-ecmwf.R`'s `fetch_forecast()` to open nothing via terra: it
     already downloads a local `.grib2` (range-fetch via `.index`); pass that
     path straight to `grib_point_table()`. The u/v recombination
     (`.ecmwf_uv_to_wind()`) consumes the same `param`/`step`/`member` columns,
     so it needs only the column names kept stable.
   - Delete `grib_open()`, `.grib_band_tag()`, `.grib_element_to_param()`,
     `.grib_band_member()`, `.grib_check_ccsds_support()`, `.have_terra()`, and
     every `terra::` call. The CCSDS capability guard is gone — eccodes always
     decodes CCSDS, so there is no "this build can't decode" branch; absence of
     eccodes is the only failure mode (`eccodes_required`).
   - `GRIB unit auto-conversion`: terra auto-converted 2t Kelvin→Celsius;
     eccodes returns native Kelvin. `fetch_forecast()` already routes every
     value through `to_canonical(value, unit, variable)` (the eccodes-fallback
     path did), so keep that — `unit` now always comes from eccodes.

3. **Conditions** (`R/conditions.R`): remove `terra_required` and
   `grib_ccsds_unsupported` from `meteo_conditions()` **iff** no code raises
   them any more (the drift-guard test will catch a stale entry). Keep
   `eccodes_required`, `eccodes_*`.

4. **DESCRIPTION**: remove `terra` from `Suggests` (confirm no other use:
   `grep -rl 'terra::' R/` must be empty after the rewrite). `SystemRequirements`
   already lists eccodes — reword from "optional … only needed as a fallback"
   to "required for `source_ecmwf()`".

5. **CI** (`.github/workflows/R-CMD-check.yaml`): add a step before the check
   that installs eccodes CLI tools, e.g.
   `sudo apt-get update && sudo apt-get install -y libeccodes-tools`
   (provides `grib_ls`). This lets the real end-to-end GRIB test run on CI.

### Tests first (`test-grib-read.R`, `test-source-ecmwf.R`, `test-ecmwf-eccodes.R`)
- **Remove** every `skip_on_ci()` added for GDAL-metadata drift, and the
  `skip_unless_grib_ready()`/`ecmwf_ccsds_supported()` terra gates.
- **Deterministic (mocked) coverage** — mock the eccodes seam
  (`.eccodes_grib_ls_path`/`system2` or, cleaner, mock `grib_point_table` /
  the JSON-returning seam with canned `grib_ls -j` output for the committed
  fixture) so `grib_point_table()` yields `param == "2t"`, `unit`, `step ==
  "24"`, `member ∈ {1,2,3}`, finite `value` — on **every** platform, no GDAL.
  This is the contract that was GDAL-version-fragile; it must now be
  version-independent.
- **`grib_point_table()` parses `grib_ls -j` JSON** into the documented tibble
  (unit test against canned JSON, mirroring `.eccodes_parse_nearest_json`).
- **Real end-to-end**, gated `skip_unless_eccodes_ready()`: against the
  committed CCSDS fixture, `grib_point_table()` decodes real values and
  `fetch_forecast()` yields canonical `wind_speed_10m`/`wind_direction_10m` per
  member. This now *runs on CI* (eccodes installed).
- **`fetch_forecast()` aborts `eccodes_required`** (mock `.have_eccodes() ->
  FALSE`) with a message naming `ecmwf_install_eccodes()`.

### Done when
terra appears nowhere in `R/`; the GRIB tests pass with no `skip_on_ci`; CI
installs eccodes and runs the real GRIB path green; `fetch_forecast()` decodes
via eccodes and aborts cleanly when eccodes is absent.

---

## Part B — deterministic derivation fill tier (physics before donors)

### Why
The gap-fill ladder (micro → donor → model) never uses the **other variables
observed at the same site and timestamp**. For thermodynamically-coupled
quantities the relationship is *exact physics*, and computing the missing one
from co-observed inputs beats a donor station (no inter-station bias) — and is
strictly better than interpolation. The Magnus helpers already exist
(`.rh_from_dewpoint`, `.dewpoint_from_rh`, `R/fill-treatments.R`); this tier
formalises them into an explicit **derivation** step that runs **before** the
donor tier.

Scope now: the RH ↔ dewpoint ↔ temperature triangle (exact, reuses existing
helpers). The registry is extensible; the direct/diffuse radiation split (from
global irradiance + solar geometry, e.g. BRL) is noted as a **future** entry,
not implemented here.

### Design
1. **A derivation registry** (`R/fill-derive.R`): a small table mapping a
   `target` variable to its `inputs` (other variable names) and a pure
   `fn(inputs...) -> numeric`:
   - `relative_humidity_2m` from (`temperature_2m`, `dewpoint_2m`) via
     `.rh_from_dewpoint(temp_c, dewpoint_c)`.
   - `dewpoint_2m` from (`temperature_2m`, `relative_humidity_2m`) via
     `.dewpoint_from_rh(temp_c, rh_pct)`.
2. **`fill_derive(obs, dict)`**: for each derivable target variable with gap
   rows, at each gap timestamp where **every** input variable has a QC-clean
   (`qc_flag == "ok"`, non-`NA`) value in `obs`, compute the target
   deterministically. Stamp `method = "derived"`, `qc_flag = "ok"`, keep the
   site's own `source`. Timestamps missing an input are left as gaps for the
   donor/model tiers. `obs` here is the **full** multi-variable frame (the
   derivation needs cross-variable lookup), so this runs at the `fill_tier()`
   level, not inside the per-variable `.fill_tier_one_variable()`.
3. **Wire into `fill_tier()`** (`R/fill-tiers.R`): run `fill_derive()` on the
   gap rows **after micro, before the donor/model tiers** — i.e. a derived
   value pre-empts a donor fetch. Order per gap: micro (short smooth) →
   **derive** (exact physics) → donor → model. Model-only variables are
   unaffected (never derivable from surface obs). Update the `fill_tier()`
   roxygen ladder description and the SCOPING §6 tier list.

### Tests first (`test-fill-derive.R`, and extend `test-fill-tiers.R`)
- **`fill_derive()` computes RH from co-observed T + dewpoint exactly**: seed a
  timestamp with `temperature_2m` and `dewpoint_2m` present (QC-clean) and
  `relative_humidity_2m` missing; assert the filled RH equals
  `.rh_from_dewpoint(T, dewpoint)` within tolerance, `method == "derived"`.
- **and dewpoint from T + RH** (the reverse).
- **a gap with a missing input is NOT derived** (left for donor/model): drop
  the dewpoint at one timestamp; that RH stays a gap out of `fill_derive()`.
- **routing: derivation pre-empts the donor tier** — via `fill_tier()` with a
  donor available for RH but T+dewpoint co-observed, the RH gap is filled
  `method == "derived"` (not `"donor_fill"`); with an input missing, it falls
  through to `"donor_fill"`.
- **derived values survive the QC/consistency invariants** (RH ∈ [0, 100]).

### Done when
`fill_tier()` fills a coupled variable by exact derivation before consulting
donors, stamps `method = "derived"`, and falls through to donor/model only
where an input is absent — proven by the routing tests.

---

## Definition of done (whole plan)

- Both parts green under the r-science `verify` gate; `R CMD check` adds no new
  ERROR/WARNING; CI (with eccodes installed) passes the real GRIB path.
- `NEWS.md` bullets: GRIB read is eccodes-only (terra dropped; eccodes required
  for `source_ecmwf()`); gap-fill derives coupled variables from co-observed
  inputs before donors.
- SCOPING §6/§10/§13 pointers to this plan are accurate; `08`/`10` note the
  superseded/extended sections.
