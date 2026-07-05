# Plan 08 — Acquisition: ECMWF Open Data (GRIB2)

## Objective

Implement `source_ecmwf()` over ECMWF Open Data (CC-BY): the 46-day extended-range
ensemble at 0.25°, read from GRIB2 via terra/GDAL’s GRIB driver, extracted at the
site point, returned as canonical forecast rows. This is the one long-range
channel whose issue-time archive the deployment fully controls (SCOPING §5.2).
The plan front-loads the **GRIB spike** (SCOPING §13/§14).

## Scope

**In:**
- `source_ecmwf()` `met_adapter` with `fetch_forecast()`.
- GRIB2 download (index-driven, only the needed fields) + terra read + nearest-
  gridpoint extraction + variable/units mapping.
- Ensemble member → `member`; CC-BY attribution.
- Graceful absence of `terra`, and a documented degradation path to the Open-Meteo
  seasonal splice (Plan 05) if the driver proves insufficient.

**Out:**
- Seasonal calibration (Plan 13); here we only acquire the ensemble.
- Any GRIB *writing*; we only read.

## Prerequisites

Plans 00–04 (and it interoperates with Plan 05 as the degradation target).

## Background

SCOPING §5 (`source_ecmwf` wraps ECMWF Open Data; 46-day ensemble per issue;
CC-BY since 2025-10-01; GRIB2 via terra/GDAL, **heavy deps in Suggests**), §5.2
(in scope for v1; the only long-range channel whose issue-time archive the
deployment controls; plan on terra/GDAL rather than an ecCodes system dep; if the
driver is insufficient for ensemble files, degrade to the Open-Meteo seasonal
splice and move full ECMWF support post-v1), §13 (GRIB2-in-R risk; terra in
Suggests + informative error when absent; **spike this early**), §14 (GRIB-spike
verification task).

## File layout

```
R/source-ecmwf.R
R/ecmwf-index.R            # parse the .index sidecar; select needed messages by byte range
R/grib-read.R             # terra-based GRIB open + point extract; the isolable "spike" seam
tests/testthat/test-grib-read.R          # the spike test (skips if terra absent)
tests/testthat/test-source-ecmwf.R
tests/testthat/_fixtures/ecmwf/small.grib2        # tiny GENUINE ECMWF GRIB2 (CCSDS-compressed, few fields, small grid)
tests/testthat/_fixtures/ecmwf/small.index        # its JSON-lines index sidecar
```

Add `terra` to **Suggests** (heavy; GDAL system dep). No hard dependency; all
terra use is guarded.

## Detailed design

### `terra` + GDAL-capability guard

Every entry point that reaches GRIB reading calls
`rlang::check_installed("terra", reason = "to read ECMWF Open Data GRIB2 files")`
and, on absence, aborts class `"terra_required"` with a message that (a) says how
to install terra/GDAL and (b) points at `source_openmeteo(product = "seasonal")`
as the no-GRIB alternative (SCOPING §5.2 degradation path).

**CCSDS/libaec check (verified necessary, 2026-07-05).** Real ECMWF open-data
GRIB2 messages use **CCSDS/AEC** compression, which GDAL can only decode when
built with **libaec**; older GDAL builds error out (OSGeo/gdal #8108). So the
guard also confirms the GDAL build handles CCSDS: the cheapest reliable check is
to attempt reading the committed real-ECMWF fixture (`small.grib2`, which *is*
CCSDS-compressed) once and, on failure, abort class `"grib_ccsds_unsupported"`
with (a) the GDAL/libaec explanation and a suggested minimum GDAL version, and
(b) the same seasonal-splice fallback pointer. Do **not** assume a synthetic
round-trip proves CCSDS support — the fixture must be a genuine CCSDS file. (Dev
machine as of 2026-07-05: terra 1.8.70 / GDAL 3.8.5, GRIB read/write driver
present; confirm CCSDS specifically on a real ECMWF file.)

### GRIB read seam (`R/grib-read.R`) — the spike

Kept in its own file so the §13/§14 spike is a single isolable unit:

- `grib_open(path)` → a terra `SpatRaster` (each band a message/field).
- `grib_extract_point(rast, lat, lon)` → the nearest-gridpoint value(s) per band
  (terra `extract` with the site point). Document that we take the nearest
  gridpoint, not bilinear interpolation, and why (a 0.25° cell is coarse; nearest
  is defensible and avoids smearing across land/sea — note this as a decision).
- `grib_field_table(rast)` → a tibble of `(band, shortName/paramId, level,
  step/lead, member)` decoded from GRIB metadata, so mapping to dictionary
  variables is explicit and testable. **Ensemble caveat (verified):** GDAL
  exposes each GRIB message as a **flat band** — there is no labelled ensemble
  dimension. Member identity lives only in the band's PDS metadata (read
  `perturbationNumber` from `GRIB_PDS_TEMPLATE_ASSEMBLED_VALUES` / the band's
  metadata), so `grib_field_table()` must **demux members itself** from that
  metadata rather than expecting a member axis. (`stars` can present multi-dim
  GRIB more tidily than raw terra if the flat-band bookkeeping gets unwieldy —
  it is already installed on the dev machine.)

If `grib_extract_point` on the committed fixture does not yield sane values, the
spike has failed — the plan’s degradation clause (below) applies.

### Index-driven download (`R/ecmwf-index.R`)

ECMWF Open Data ships each GRIB with a `.index` sidecar listing every message and
its byte range. To avoid downloading whole ensemble files:

- `ecmwf_index_parse(index_lines)` → tibble of messages `(param, level, step,
  number(member), _offset, _length)`.
- Select the messages matching the requested variables/leads/members; issue HTTP
  **range requests** (via `.http_get()` with a `Range` header) for only those
  byte spans; concatenate into a local `.grib2` temp file; read with the seam.
- This keeps `source_ecmwf()` feasible on the 46-day ensemble (SCOPING §5.2).

### `source_ecmwf()`

`source_ecmwf(stream = "eefo", resolution = "0p25", source_id = "ecmwf")`:

- **`stream = "eefo"`** is the 46-day **extended-range ensemble (101 members)** —
  the corrected identifier (the medium-range `enfo` stream stops at step 360 h,
  so it cannot serve the 46-day horizon). `resolution` defaults to `0p25` but is
  a parameter, not hard-coded: ECMWF has flagged a future move to 0.125°/9 km.
- `fetch_forecast(site, variables, issue_window, now)`:
  1. Resolve the issue cycles in `issue_window` (ECMWF issues on a fixed schedule).
  2. For each cycle: fetch the `.index` (JSON-lines, one record per message with
     `_offset`/`_length`), select messages for the requested
     variables × leads × members, range-download, read via the seam, extract at
     the site point.
  3. Map GRIB `paramId`/`shortName` → dictionary variables (a documented lookup
     in `ecmwf-index.R`), `to_canonical()` units (GRIB is SI — e.g. wind m/s,
     temp K → degC; assert the conversions).
  4. Build canonical forecast rows: `model = "ifs_eefo"` (the extended-range
     ensemble id), integer `member` = the demuxed `perturbationNumber` (0 =
     control, 1–100 = perturbed), `issue_time` = cycle, `valid_time` = cycle + step.
- `met_attribution()` returns the required CC-BY credit.
- Degradation clause (documented in roxygen and enforced by the guard): if terra
  is absent or the driver can’t read the ensemble files, `fetch_forecast` aborts
  `"terra_required"` / `"grib_unreadable"` pointing to the Open-Meteo seasonal
  splice — the pipeline (Plan 16) treats ECMWF as optional and continues.

## Test requirements

### `test-grib-read.R` (the spike; `skip_if_not_installed("terra")`)
- `grib_open()` on the committed `small.grib2` returns a `SpatRaster` with the
  expected number of bands. **Because the fixture is genuinely CCSDS-compressed,
  this simultaneously exercises the CCSDS/libaec path** — a GDAL built without
  libaec fails here, which is exactly the real-world failure we want caught (the
  guard should turn that into `"grib_ccsds_unsupported"`, not an opaque error).
- `grib_extract_point()` at a coordinate inside the fixture grid returns finite,
  physically plausible values.
- `grib_field_table()` decodes param/level/step and **demuxes `member` from
  `perturbationNumber`** for the fixture’s bands (assert a control + ≥1 perturbed
  member are distinguished).
- **This is the acceptance gate for the §14 GRIB spike** — if it can’t pass on a
  real (if tiny) CCSDS ECMWF GRIB2, escalate before building more on it.

### `test-source-ecmwf.R`
- `ecmwf_index_parse()` on the committed `small.index` yields the right messages
  with byte offsets/lengths; message selection picks only requested fields.
- With terra installed: an end-to-end `fetch_forecast()` over a mocked
  range-download of the fixture yields canonical forecast rows —
  `expect_canonical_forecast()`, integer `member`, units converted (K→degC,
  correct `valid_time = issue_time + step`).
- With terra **not** installed (simulate via `local_mocked_bindings` on the guard
  or `skip`), the adapter aborts `"terra_required"` with the Open-Meteo pointer.
- No test performs a live download (range requests hit the mocked seam).

## Definition of done

Shared skeleton plus:
- `source_ecmwf()`, `met_attribution()` method exported/documented; the terra/
  GDAL requirement and the seasonal-splice degradation path documented.
- The GRIB spike test passes on a real committed fixture (or the plan is flagged
  blocked and the degradation path is the shipped behaviour — record which).
- `terra` is in Suggests; absence is a clean, guided error, never a crash.
- `adapters_for_site()` resolves `"ecmwf"`.
- New condition classes registered in `meteo_conditions()`.
