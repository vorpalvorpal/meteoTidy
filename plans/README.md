# meteoTidy — implementation plan series

This directory holds the staged implementation plans for `meteoTidy`. They are
written to be executed **one at a time, in order**, by an implementer who has
*not* read the whole design. Each plan is self-contained: it states its
prerequisites, the exact files to create, the function signatures and
algorithms, and the tests that must pass before the plan is considered done.

Read this file first. It defines conventions that every plan assumes and does
not repeat.

> **Source of truth.** The design rationale lives in [`../SCOPING.md`](../SCOPING.md).
> Plans cite it as “§7.1”, “§3.2”, etc. When a plan and the scoping document
> disagree, **stop and flag it** — do not silently pick one. The scoping
> document is frozen; plans may refine it but must call out where they do.

---

## How to execute a plan

1. Read the plan top to bottom before writing any code.
2. Confirm every plan in **Prerequisites** is merged. If not, stop.
3. Implement only what is in **Scope (in)**. If you find yourself needing
   something in another plan’s scope, stop and say so — do not reach ahead.
4. Write the code and the tests described in **Test requirements**.
5. Run the quality gate (below). It must be green.
6. The plan is done only when **Definition of done** is fully satisfied. Do not
   mark a plan complete with skipped tests, `@examples` that error, or `R CMD
   check` NOTEs you introduced.

If a step is ambiguous, prefer the choice that is (a) consistent with an
already-merged plan, then (b) consistent with SCOPING.md, then (c) the simplest
thing that satisfies the tests. Record the choice in a code comment only if a
future reader could not infer it.

---

## Plan roadmap (execute top-to-bottom)

Dependencies flow downward; a plan may depend only on plans above it.

| #  | Plan | Depends on | One-line scope |
|----|------|-----------|----------------|
| 00 | Package scaffolding & shared infrastructure | — | DESCRIPTION, testthat setup, house-style helpers, CI, error/message helpers |
| 01 | Canonical data model | 00 | Table schemas, variable dictionary, QC/method enums, units, member/stat rule |
| 02 | Site registry & station-ID resolution | 00, 01 | S7 `met_site` class, YAML (de)serialisation, resolved-ID cache |
| 03 | Storage layer | 00, 01, 02 | Parquet hive datasets, read/write, watermarks, revision handling, calibration store |
| 04 | Acquisition — adapter contract + generic adapters | 00–03 | S7 adapter contract, `source_rest()`, `source_file()`, mapping spec, unit enforcement |
| 05 | Acquisition — Open-Meteo | 00–04 | `source_openmeteo()`: forecast, ensemble, historical, previous/single runs, seasonal |
| 06 | Acquisition — SILO & GHCNh | 00–04 | `source_silo()` (weatherOz), `source_ghcnh()` (worldmet) |
| 07 | Acquisition — BOM + vendored weatherBOM | 00–04 | `source_bom_forecast()`, `source_bom_obs()`, transport ladder + circuit breaker |
| 08 | Acquisition — ECMWF Open Data (GRIB2) | 00–04 | `source_ecmwf()`, terra/GDAL GRIB read, graceful absence |
| 09 | Curation — QC engine | 00–03, 06 | WMO-style rules by statistical class, spatial/buddy, solar clear-sky |
| 10 | Curation — gap-fill, transfer engine & curated products | 00–03, 06, 09 | Tiered fill, donor dedup, shared transform machinery, hourly/daily aggregation, history products, SILO disaggregation |
| 11 | Correction — physical adjustments & tier framework | 00–03, 10 | Day-0 physics, tier selection, calibration manifest lifecycle |
| 12 | Correction — fitted tiers | 11 | mean-bias, `qmap`, EMOS (`crch`), MBC, lead-shrinkage, consistency pass |
| 13 | Verification engine | 03, 11, 12 | Rolling-origin, `scoringRules`, PIT/rank/Brier, block bootstrap, skill-gated promotion |
| 14 | Read API + config & secrets | 03 | `met_history()` etc. tibble API, `met_connect()`, YAML config loader, secret resolution |
| 15 | Classed-tibble contract + wide emitter | 01, 03, 14 | Classed `tbl_df`, `dplyr_reconstruct` methods, wide §3.1 emitter, content hashing |
| 16 | Pipeline verbs | 04–15 | `met_sync_live/daily()`, `met_refit()`, `met_backfill()`; incremental, idempotent |
| 17 | Correction serve-wiring, verification enrichment, refactors | 00–16 | Post-review: apply corrections at serve time, honest `met_wide` provenance, SILO daily QM in `history_daily`, forecast-source refits, consistency pass, baselines/diagnostics in `verify_run`, `as_of` history, BOM transport provenance, and three refactors |

**Status:** all plans (00–16) implemented; plan 17 written (post-implementation
review follow-ups), its BDD acceptance specs committed as skipped
`tests/testthat/test-plan17-*.R` blocks to un-skip per item.

---

## Shared conventions (all plans inherit these)

### House style

- **Package name is `meteoTidy`** (camelCase). Function and argument names are
  `snake_case`. User-facing verbs are prefixed `met_` (`met_history()`,
  `met_sync_daily()`); adapter constructors are prefixed `source_`
  (`source_rest()`). Internal (unexported) helpers need no prefix but must not
  collide with exported names.
- **Functional by default.** Functions take inputs and return values; they do
  not mutate their arguments or rely on hidden global state. The only sanctioned
  state is on disk (the Parquet store, manifests, watermarks) and is always
  passed in via an explicit `store`/`site` argument, never read from a global.
- **Correctness over performance.** Write the clear version first. Optimise only
  with a benchmark showing it matters, and keep the clear version tested.
- **Object system: S7.** Domain objects (site registry, adapters, and any other
  structured value with methods) are S7 classes with validators, matching
  meteoHazard’s “S7-plus-`units`” idiom. **Tabular data is never an S7 object** —
  it is a tibble (plain until Plan 15, classed thereafter). Do not reach for R6;
  there is no mutable-object requirement anywhere in this package.
- **Units.** Physical quantities carry units via the `units` package at
  construction and at every external boundary. Canonical units are defined once
  in the variable dictionary (Plan 01) and enforced there; downstream code
  assumes canonical units and does not re-check. Never pass a bare numeric across
  an adapter boundary for a dimensioned quantity.
- **Time.** All stored timestamps are UTC `POSIXct` with `tz = "UTC"`. Local
  time appears only for display and at the daily-aggregate boundary, where it is
  **local clock time** — the site's IANA timezone, DST-inclusive, matching the
  BOM/SILO 9am-day convention (SCOPING §3; DST transitions give 23/25-hour days).
  Never use `Sys.time()` / `Sys.Date()` inside package logic: take the current
  time as an injectable argument (`now = Sys.time()`) so tests can freeze it.
- **Messaging: `cli`.** All user-facing messages, warnings, and errors go through
  `cli::cli_inform()` / `cli::cli_warn()` / `cli::cli_abort()`. Never use bare
  `message()`, `warning()`, `stop()`, `print()`, or `cat()`. Give every
  condition a class (`class = "meteoTidy_error_<kind>"`) so callers and tests can
  catch it precisely (Plan 00 provides the helpers).
- **No side effects on load.** No `options()`, no `Sys.setenv()`, no directory
  creation at package load. Configuration is read explicitly (Plan 14).
- **Documentation: roxygen2.** Every exported function has a roxygen block with
  `@param`, `@return`, a runnable `@examples` (use `\dontrun{}` only for calls
  that truly need network or credentials — prefer `@examplesIf` guarded by a
  helper that returns `FALSE` in checks), and `@family` grouping.

### Dependencies

Declare dependencies in the plan that first needs them. Prefer `Imports` for
anything used in the core path; `Suggests` for optional backends
(`duckdb`), heavy/optional readers (`terra`), and everything used only in tests.
When code uses a `Suggests` package, guard with
`rlang::check_installed("pkg", reason = "...")` and give an informative error —
never a bare “could not find function”. The heavy statistics and IO packages
(`arrow`, `qmap`, `MBC`, `crch`, `imputeTS`, `scoringRules`, `circular`,
`worldmet`, `weatherOz`) attach lazily through `::`; do not `library()` anything.

### Testing conventions (testthat edition 3)

Every plan lists concrete **Test requirements**. These conventions apply to all
of them.

- **Edition 3.** `DESCRIPTION` has `Config/testthat/edition: 3`. No `context()`.
  Each test is self-sufficient: it creates what it needs and cleans up after
  itself with `withr::local_*` / `withr::defer()`. No test depends on another
  test’s side effects or ordering.
- **No network, ever, in tests.** Adapters talk HTTP through `httr2`; mock it
  with **`httptest2`** (record fixtures once against the live API, then replay
  from `tests/testthat/_fixtures/`). Tests must pass with networking disabled.
  CI sets an env var that makes any accidental live request an error.
- **Determinism.** Seed every stochastic path (`withr::local_seed()`). Never call
  the real clock — pass a fixed `now`. Never depend on the machine timezone —
  set it explicitly with `withr::local_timezone()` where it could matter.
- **Filesystem isolation.** Anything touching the store writes under
  `withr::local_tempdir()`. Never write into the package or the user’s home.
- **What to assert.** Prefer asserting on *behaviour and invariants*, not on
  incidental representation:
  - **Schema/contract tests** — column names, types, units, and key uniqueness
    of every table a function returns (helper `expect_canonical_obs()` etc. from
    Plan 01). These are the load-bearing tests of this package.
  - **Round-trip / property tests** — e.g. widen→narrow is identity on the value
    columns; write→read a Parquet dataset returns the same rows; unit conversion
    round-trips within tolerance; the pipeline is idempotent on re-run.
  - **Error-path tests** — every documented failure mode has a test asserting the
    specific condition class via `expect_error(..., class = "meteoTidy_error_x")`.
  - **Snapshot tests** (`expect_snapshot()`) for multi-line `cli` output and for
    human-readable reports. Keep snapshots small and reviewed; never snapshot
    volatile content (timestamps, paths, RNG) — inject stable values instead.
- **Mocking.** Mock package-internal functions with
  `testthat::local_mocked_bindings()` (edition-3 API). Do not reassign functions
  by hand. Mock *seams you own* (an internal `.http_get()`), not other packages’
  internals.
- **Fixtures.** Small canonical fixtures live in `tests/testthat/helper-*.R` as
  builder functions (`make_test_site()`, `make_obs(...)`), so a test reads as
  data-in → assertion, not 40 lines of setup. Large recorded API payloads live
  in `tests/testthat/_fixtures/` and are loaded, never inlined.

### The quality gate (run before declaring a plan done)

The r-science `verify` gate. A plan is **NOT READY** unless all of:

1. `devtools::load_all()` clean (no errors, no new warnings).
2. `devtools::test()` — all tests pass, none skipped except network/credential
   tests explicitly skipped via `skip_on_ci()` / `skip_if_offline()`, and those
   have a recorded-fixture replay path that *does* run.
3. `devtools::document()` produces no diff beyond the current plan’s exports
   (i.e. you committed the regenerated `man/` and `NAMESPACE`).
4. `lintr::lint_package()` clean against the project `.lintr` (Plan 00).
5. `R CMD check` (`devtools::check()`) adds **no new** ERRORs/WARNINGs/NOTEs.

Gate on those, not on a coverage percentage — but every public function and
every documented error path must have at least one test exercising it.

### Definition of done (shared skeleton)

Each plan’s **Definition of done** extends this:

- All files in **File layout** exist and are the only files the plan added.
- All **Test requirements** are implemented and green under the quality gate.
- Exported functions are documented; `NAMESPACE`/`man/` regenerated and committed.
- No TODO left in code without a linked follow-up note in the plan file.
- `NEWS.md` has a bullet describing what this plan added (user-visible items only).
