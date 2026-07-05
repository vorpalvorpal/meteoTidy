# Plan 09 — Curation: QC engine

## Objective

Implement the quality-control engine: ~10 WMO-style rules dispatched per
statistical class, **including spatial/buddy checks against the configured donor
stations** (the review’s highest-value QC addition — the strongest test for slow
sensor drift) and a **solar QC** rule with a named clear-sky model and BSRN-style
limits. QC sets the `qc_flag` and writes an auditable per-rule log. It is
incremental and idempotent.

## Scope

**In:**
- A rule registry; each rule declares the statistical classes it applies to.
- Rules: range, step, persistence (flat-line), internal consistency, climatological
  bounds, spatial/buddy, solar clear-sky.
- The `qc_log` companion table (which rule fired, per row).
- Incremental application over the window since the QC watermark; idempotent.

**Out:**
- Gap-fill (Plan 10) — QC only *flags*; it never imputes.
- Correction (Plans 11–13).
- The physical-consistency *enforcement* pass that clips corrected values
  (Plan 12) — this plan’s internal-consistency rule *flags* raw observations;
  Plan 12 *enforces* on corrected output. They **share the constraint module**
  (`R/physics-constraints.R`, created here, reused there).

## Prerequisites

Plans 00–03, 06 (donor obs for spatial checks come from GHCNh/BOM adapters).

## Background

SCOPING §6 (QC rules list; custom-built; dispatched per statistical class;
**spatial/buddy checks**; **solar clear-sky (McClear or Ineichen–Perez) + BSRN
limits**; incremental + idempotent), §3 (`qc_flag` closed enum; statistical
classes), §7.3 (model-only variables have no site truth — QC of them is limited
to range/step/consistency, never spatial-to-a-site).

## File layout

```
R/qc.R                    # qc_run(): orchestration, dispatch, watermark, idempotency
R/qc-rules.R              # individual rule functions + the registry
R/qc-spatial.R            # buddy check against donor stations
R/qc-solar.R              # clear-sky model + BSRN limits
R/physics-constraints.R   # shared physical constraints (reused by Plan 12)
R/qc-log.R                # qc_log companion table schema + IO
tests/testthat/test-qc-rules.R
tests/testthat/test-qc-spatial.R
tests/testthat/test-qc-solar.R
tests/testthat/test-qc-consistency.R
tests/testthat/test-qc-run.R
```

Possibly add a clear-sky helper dependency; prefer a self-contained Ineichen–Perez
implementation (no heavy dep) with McClear as an optional `Suggests` path.

## Detailed design

### Rule contract (`R/qc-rules.R`)

Every rule is a pure function:

```r
rule_fn(obs, dict, context) -> obs   # with qc_flag possibly downgraded + qc_log rows emitted
```

- `obs` is a canonical long table (one variable or many).
- `context` carries what the rule needs: `history_daily` (climatology bounds),
  donor obs (spatial), site metadata, the current window.
- A rule may only **downgrade** `qc_flag` (`ok`→`suspect`→`fail`) — never upgrade.
  Multiple rules compose; the final flag is the worst any rule assigned.
- Each rule appends `qc_log` rows `(site_id, datetime_utc, variable, rule,
  outcome, detail)` so the decision is auditable (SCOPING §3.2 audit spirit).

The registry maps `rule_id → list(fn, applies_to = <statistical classes>)`.
`qc_run` dispatches each variable’s rows only to rules whose `applies_to`
includes that variable’s `statistical_class` (from the dictionary, Plan 01).

### The rules

1. **range** — value outside dictionary `[min, max]` → `fail`. All classes.
2. **step** — |Δvalue/Δt| above a per-class limit → `suspect`. Linear/bounded/
   clear-sky classes. **Circular variables wrap**: the step for `wind_direction`
   is the angular difference (min of `|d|`, `360−|d|`), not the raw subtraction.
3. **persistence (flat-line)** — value unchanged for longer than a per-variable
   window (a stuck sensor) → `suspect`. Skip for genuinely constant-capable
   variables and for `intermittent` (zero rain is legitimately flat).
4. **internal consistency** — physical relations across variables at the same
   `(site_id, datetime_utc)`: `dewpoint ≤ temperature`; `relative_humidity ≤ 100`;
   `wind_gusts ≥ wind_speed`; `direct + diffuse ≤ clear-sky ceiling`. Violations
   → `suspect` on the implicated variables. **Implemented in
   `R/physics-constraints.R`** so Plan 12 reuses the exact same relations.
5. **climatological bounds** — value outside a seasonal envelope derived from
   `history_daily` (e.g. beyond climatological p0.1/p99.9 for that day-of-year) →
   `suspect`. Requires `history_daily` in context; skipped (with a logged note) at
   day 0 when no climatology exists yet.
6. **spatial/buddy** (`R/qc-spatial.R`) — **the review’s key addition.** Compare
   the site value to the same variable at nearby donor stations (Plan 06),
   bias-adjusted for elevation/known offset. When the site deviates from the
   spatially-consistent neighbour consensus by more than a robust threshold (e.g.
   > k·MAD of neighbour spread), flag `suspect`. This is the strongest detector
   of slow **sensor drift**, which range/step/persistence all miss. Only applies
   to variables observable at donors (`site_measurable` / `donor_observable`);
   **never** to `model_only` variables (no site truth — SCOPING §7.3). Requires
   ≥ 2 usable donors; logs “insufficient donors” and skips otherwise.
7. **solar clear-sky** (`R/qc-solar.R`) — for `clear_sky_indexed` variables
   (`direct_radiation`, `diffuse_radiation`, and GHI if present): compute the
   clear-sky irradiance from a named model (**Ineichen–Perez** built-in; McClear
   optional) for the site/time/solar-geometry, then apply **BSRN-style physically-
   possible and extremely-rare limits** (e.g. GHI ≤ 1.5·E0·cosθ^1.2 + 100). Values
   above the physically-possible limit → `fail`; above the rare limit → `suspect`;
   nonzero radiation at night → `fail`. Name the model and cite BSRN in comments.

### `qc_run` (`R/qc.R`)

`qc_run(store_root, site, variables = NULL, now = .now(), donors = NULL,
history_daily = NULL)`:

- **Incremental** (SCOPING §6): read the QC watermark (Plan 03); process only rows
  from `watermark` forward (with a small re-look-back so a late-arriving neighbour
  can re-buddy-check the recent tail). Advance the watermark on success.
- Load the target window, assemble `context`, run the applicable rules per
  variable, combine flags (worst wins), write the updated `qc_flag` back via
  `store_write_obs(mode = "supersede")` (a QC flag change supersedes the prior
  row — Plan 03), and append `qc_log`.
- **Idempotent** (SCOPING §6): running twice over the same window yields identical
  flags and no duplicate `qc_log` rows (dedupe the log on
  `(site_id, datetime_utc, variable, rule)` keeping the latest run).

## Test requirements

### `test-qc-rules.R`
- Each rule flags a hand-built violating series with the documented flag and
  leaves clean data `ok`.
- **step wraps for circular**: 350°→10° is a 20° step (clean), not 340° (flagged)
  — a direct circular-handling test.
- persistence ignores legitimately-flat zero rain (`intermittent`).
- Dispatch: a `model_only` variable is **not** sent to the spatial rule.

### `test-qc-consistency.R`
- `physics-constraints` flags `dewpoint > temperature`, `RH > 100`,
  `gusts < wind`, `direct + diffuse > clear-sky ceiling`. The **same** module,
  called in “enforce” mode (a flag Plan 12 uses), returns clipped values — proving
  the shared module supports both flag and enforce (Plan 12 depends on this).

### `test-qc-spatial.R`
- A site series with a slow additive drift against three steady donors gets
  flagged `suspect` once it exceeds the MAD threshold; the pre-drift portion stays
  `ok`. (Directly tests the drift-detection rationale.)
- With < 2 donors, the rule skips and logs, does not error.

### `test-qc-solar.R`
- A physically-impossible irradiance (above the clear-sky-possible limit) → `fail`;
  nighttime nonzero radiation → `fail`; a normal clear-day value → `ok`.
- The clear-sky computation is deterministic for a fixed site/time (snapshot a few
  values).

### `test-qc-run.R`
- **Idempotency:** `qc_run` twice over the same window yields identical flags and
  no duplicate `qc_log` rows.
- **Incrementality:** a second run over a later window does not re-scan (or re-flag
  differently) already-finalised earlier rows beyond the look-back.
- QC flag changes are persisted via supersede (the old flag is retrievable with
  `include_superseded = TRUE`, Plan 03).

## Definition of done

Shared skeleton plus:
- `qc_run()` exported/documented; rules and the registry internal but documented;
  `physics-constraints` supports both `flag` and `enforce` modes (Plan 12 reuse).
- Solar QC names its clear-sky model and cites BSRN in code.
- `qc_log` schema defined and written; idempotency proven.
- New condition classes registered in `meteo_conditions()`.
