# Plan 08 — Acquisition: ECMWF Open Data (GRIB2)

## Objective

Implement `source_ecmwf()` over ECMWF Open Data (CC-BY): the **medium-range
`enfo` ensemble** at 0.25° (see the "Deviation from SCOPING §5.2" note at the
end — the 46-day `eefo` stream this plan originally targeted is **not** in the
free open-data catalogue as of 2026-07-06), read from GRIB2 via terra/GDAL’s
GRIB driver, extracted at the site point, returned as canonical forecast rows.
This is the channel whose issue-time archive the deployment fully controls; the
46-day long-range need is met by the Open-Meteo seasonal splice (SCOPING §5.2).
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

> **As implemented, `stream` defaults to `"enfo"`, not `"eefo"` — see
> "Deviation from SCOPING §5.2" at the end of this file.** `"eefo"` does not
> exist in ECMWF's real open-data catalogue as of 2026-07-06; `"enfo"` (the
> real medium-range ensemble, ~360h/15-day horizon, 50 perturbed members, no
> control member) is what the shipped code actually targets.

`source_ecmwf(stream = "enfo", resolution = "0p25", source_id = "ecmwf")`
(original design below, retained for context; superseded per the deviation
note):

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

## Deviation from SCOPING §5.2 (recorded 2026-07-06)

Live verification against `https://data.ecmwf.int/forecasts/` (and its S3
mirror, `ecmwf-forecasts` on `eu-central-1`) on 2026-07-06 found two things
this plan and SCOPING §5.2 got wrong, per the README's "stop and flag it"
rule for a plan/scoping disagreement:

1. **`"eefo"` (the 46-day extended-range ensemble) is not present in the real
   open-data catalogue.** The only streams that exist under `ifs/0p25/` are
   `oper`, `enfo`, `waef`, `wave` (plus the separate `aifs-ens`/`aifs-single`
   model families) — checked across every issue cycle for the preceding 30
   days. SCOPING §5.2's premise ("fully open since 2025-10-01... the `eefo`
   stream") does not hold as of this verification date. **Decision (user
   confirmed):** `source_ecmwf()` now defaults to `stream = "enfo"` — real,
   open today, ~360h/15-day medium-range horizon, 50 perturbed members
   (`1..50`, **no separate control/member-0** in the open feed — also
   verified live, contrary to this plan's original test assumptions).
   `stream = "eefo"` remains an accepted value (untested; will 404 until/
   unless ECMWF opens it) so the adapter picks it up with no code change if
   it ever ships. **Net effect: `source_ecmwf()` currently provides no
   long-range (46-day) coverage.** `source_openmeteo(product = "seasonal")`
   (Plan 05) is the only long-range channel in practice, contradicting
   SCOPING §5.2's "both ship in v1" — flagged here rather than silently
   resolved; a future plan should revisit long-range coverage if/when ECMWF
   opens an extended-range stream, or scope the seasonal splice as the sole
   v1 long-range channel in SCOPING itself.
2. **File layout is per-step, not per-cycle.** ECMWF Open Data ships one
   GRIB2 + `.index` pair *per forecast step* (e.g.
   `20260702000000-24h-enfo-ef.grib2` under
   `<date>/<HH>z/ifs/<res>/<stream>/`), not one pair covering the whole issue
   cycle. `R/source-ecmwf.R`'s URL building was corrected accordingly.

Additional bugs the real fixture caught (all fixed in the same pass, see
`R/grib-read.R`'s and `R/source-ecmwf.R`'s header notes for detail):
`terra::metags()` does not expose GDAL's native GRIB band metadata (the real
accessor is `terra::meta(rast, layers = TRUE)`); GDAL auto-converts ECMWF's
Kelvin temperature fields to Celsius on read, so the unit must be read off
the decoded band, not assumed to be Kelvin; the CCSDS/libaec guard
(`.grib_check_ccsds_support()`) only tested `grib_open()`, which succeeds
regardless of libaec support since it never touches pixel data — it now
attempts a real point extraction; `.http_get()` (Plan 04) was JSON-only and
could not have served the JSON-*lines* `.index` sidecar or raw `.grib2`
bytes at all — extended with a `parse = c("json", "lines", "raw")` argument.

**Environment note:** this dev machine's terra/GDAL (terra 1.8.70, bundled
GDAL) cannot decode CCSDS/AEC-compressed pixel data (confirmed:
`g2_unpack7: ... requires building against libaec`) — exactly the risk
SCOPING §13 flagged. The guard now correctly turns this into
`"grib_ccsds_unsupported"` rather than a raw GDAL error; the test suite
detects this live (`ecmwf_ccsds_supported()`, `tests/testthat/helper-ecmwf.R`)
and asserts whichever of the two documented outcomes is actually true, so it
is meaningful on both a libaec-enabled and a libaec-less GDAL build. Getting a
libaec-enabled GDAL working with `terra` in this environment (a from-source
`terra` rebuild against a system GDAL) was deferred as a separate, larger
piece of work (user confirmed) rather than blocking this plan.

## Post-audit follow-up: the eccodes CLI fallback (2026-07-07)

The deferred libaec item above was picked back up and investigated properly.
A from-source `terra` rebuilt against Homebrew's current `gdal` (which does
bundle libaec) **does** fix the CCSDS decode -- verified live: a real
CCSDS-compressed message decoded to plausible values via `terra::extract()`.
But it is not a clean substitute for the CRAN binary: GDAL's own band-metadata
exposure drifted between 3.8.5 (the CRAN binary) and 3.13.1 (current
Homebrew), and `terra::meta(rast, layers = TRUE)` -- the call
`.grib_band_tag()` depends on -- stopped surfacing `GRIB_UNIT`/
`GRIB_FORECAST_SECONDS`/`GRIB_PDS_TEMPLATE_ASSEMBLED_VALUES` against the newer
GDAL, even though `gdalinfo -json` (the same GDAL library, no terra involved)
reports them correctly. This reproduced identically with both terra's GitHub
HEAD and the latest CRAN-released source build, so it looks like a genuine
terra/newer-GDAL incompatibility, not a fluke of unreleased code -- chasing a
"right" GDAL version is not a stable foundation to build on.

Since the CCSDS/AEC compression only affects a GRIB2 message's *pixel data*
section -- its metadata (PDS/GDS) is always plain and already read correctly
by the CRAN binary's terra -- the decode step is separable from everything
else this plan needs. `R/ecmwf-eccodes.R` adds a narrow, decode-only fallback
using eccodes (ECMWF's own GRIB library, which handles every packing
template reliably): `grib_ls -l lat,lon,1`, a first-class eccodes CLI feature
purpose-built for "value nearest this point" (not `grib_get_data`, which
dumps every one of ECMWF's ~1.04M global gridpoints per message -- wildly
wasteful for a single-site query). CLI, not eccodes' Python bindings: this
package already treats one heavy optional binary (terra/GDAL) as
Suggests-gated; a second optional *binary* is a smaller footprint than also
managing a Python interpreter + numpy + `python-eccodes`.

eccodes is an external system binary, not an R package, so it cannot be
declared as a normal dependency (`SystemRequirements` documents it, but that
is informational only -- nothing auto-installs it for a user). Rather than
brew/apt/choco (three divergent code paths, each needing a system package
manager that may need elevated privileges, installing into unpinned *shared*
system state -- exactly the GDAL-version-drift problem above, not a fix for
it), `ecmwf_install_eccodes()` downloads `micromamba` (a tiny, dependency-free
package manager, ~7-14MB, downloaded once) and uses it to install the plain
`eccodes` conda-forge package into one self-contained, deletable folder under
`tools::R_user_dir("meteoTidy", "cache")` -- pinned, reproducible, and
identical across macOS/Linux/Windows. This is never triggered automatically;
`fetch_forecast()` only reaches for it when GDAL's own decode fails at value-
read time and a usable `grib_ls` happens to already be cached or on `PATH`.

Verified live end-to-end (2026-07-07), the exact mechanism now shipped: a
real `micromamba create -c conda-forge eccodes`, followed by `grib_ls -j -l
lat,lon,1` against the committed CCSDS fixture, decoded all 3 members to the
same values (Kelvin, converted to Celsius) as a from-source terra/Homebrew-GDAL
decode of the identical file -- two independent decoders agreeing. eccodes
reports the *native* GRIB2 unit (Kelvin for temperature), unlike GDAL's GRIB
driver, which auto-converts to Celsius on read -- `fetch_forecast()` uses
eccodes' own reported unit for `to_canonical()` when the fallback path is
taken, not GDAL's.
