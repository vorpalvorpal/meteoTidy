# Test fixtures

Small, canonical inputs live in `helper-*.R` as builder functions. This
directory holds the payloads that are too large to inline or that must be
captured from a real source.

## Hand-authored (checked in as-is)

- `sites/*.yaml` — site-registry config (Plan 02).
- `config/*.yaml` — deployment config (Plan 14): one valid `deployment.yaml`,
  plus `bad-inline-secret.yaml` and `bad-unknown-key.yaml` for the fail-loud
  guards.
- `rest/*.json`, `file/*.csv` — synthetic adapter bodies shaped like the real
  APIs (Plan 04), enough to exercise mapping + unit conversion.
- `openmeteo/*.json` — synthetic Open-Meteo bodies (Plan 05) covering each
  product shape (obs / forecast / ensemble / seasonal / previous-runs /
  historical-forecast). Replace with **recorded** bodies via `httptest2` when
  tightening against the live schema.
- `bom/*.xml|*.json` — synthetic BOM précis XML, 72-h obs JSON, web-API obs and
  geohash-search bodies (Plan 07), shaped like the real feeds. Replace with
  recorded bodies when tightening against the live schema.
- `ecmwf/small.index` — a hand-authored JSON-lines index sidecar (Plan 08) with
  a control + perturbed members across two params/steps, enough to test
  `ecmwf_index_parse()` / message selection without the binary.

SILO / GHCNh (Plan 06) need **no** on-disk fixtures: `helper-station.R` builds
`weatherOz` / `worldmet` return frames in memory and the adapters replay them
through the package-owned `.weatheroz_get()` / `.worldmet_get()` seams. Record
real frames only when tightening against the live library schemas.

## Recorded during implementation

These are captured once against the live API/library (networking on) and then
replayed offline. They are listed here so the implementer knows what to record:

- `http/retry-500-200/` — an `httptest2` mock dir with a 500 then a 200.
- `ecmwf/small.grib2` — a **genuine, CCSDS-compressed** ECMWF Open-Data GRIB2
  (few fields, tiny grid), paired with the hand-authored `small.index`. Must be
  a real CCSDS file: a synthetic round-trip does not prove the libaec/CCSDS
  path. Until it is recorded, `test-grib-read.R` and the end-to-end ECMWF fetch
  test skip via `skip_unless_grib_ready()`.

No fixture may contain a secret, token, or the SILO email PII.
