# meteoTidy test suite (BDD specifications)

These are **behaviour-driven, test-first specifications** for the plan series in
[`../plans/`](../plans). Every `describe()` block names a unit or feature; every
`it()` block is one behaviour taken directly from a plan's **Test requirements**
section. A trailing comment on each `it()` cites the plan whose contract it pins
(e.g. `# Plan 01`).

## Status: red until implemented

The suite is written **before** the implementation (BDD). Until a plan's `R/`
code exists, that plan's tests are expected to fail (or error at collection when
the package itself is not yet built). Executing a plan means turning its
`describe()` blocks green under the quality gate in
[`../plans/README.md`](../plans/README.md) — no test deleted, none skipped except
the network/credential paths that the plans mark `skip_if_*`.

## Conventions (inherited from `plans/README.md`)

- **testthat edition 3**, BDD style (`describe()` / `it()`).
- **No network, ever.** `setup.R` sets `METEOTIDY_NO_NET=1`; adapter tests replay
  fixtures from `testthat/_fixtures/` through the `.http_get()` seam.
- **Determinism.** Frozen clock via `local_frozen_clock()`; seeded RNG via
  `withr::local_seed()`; explicit timezone via `withr::local_timezone()`.
- **Filesystem isolation.** Anything touching the store writes under
  `withr::local_tempdir()` (`local_store()`).
- **Shared vocabulary.** Builders (`make_obs()`, `make_forecast()`,
  `make_test_site()`, …) and custom expectations (`expect_canonical_obs()`, …)
  live in `helper-*.R` so each `it()` reads as *data-in → assertion*.

Some `_fixtures/` payloads that must be **recorded** against a live API (or are
genuinely binary, e.g. the CCSDS ECMWF GRIB2) are described in a sibling
`README` inside their fixture directory and recorded during implementation.
