# meteoTidy (development version)

- Initial scaffolding: package infrastructure, `testthat` 3 harness, CI, the
  classed condition system (`abort_meteo()`, `warn_meteo()`, `inform_meteo()`,
  `meteo_conditions()`), and the injectable clock seam (`.now()`).
- Canonical data model: the variable dictionary (`met_variables()`,
  `met_variable()`, `met_register_variable()`), the closed enumerations
  (`qc_flag`, `method`, `tier`, `statistical_class`, `measurability_class`),
  canonical-unit conversion (`to_canonical()`, `canonical_unit()`, including
  the km/h-to-m/s wind conversion), and the canonical observation and
  forecast/`forecast_aux` table validators (`widen_obs()`/`narrow_obs()`
  round-trip, the member/stat mutual-exclusion rule).
- Site registry: the `met_site`/`met_instrument` S7 classes and their
  validators, the `met_sites` collection wrapper, and YAML (de)serialisation
  (`read_sites_yaml()`/`write_sites_yaml()`) for version-controlled site
  configuration, including the resolved-external-ID cache accessors
  (`site_resolved()`/`site_set_resolved()`).
- Storage layer (internal): hive-partitioned Parquet observation and
  forecast/`forecast_aux` archives with per-source watermarks, the
  observation revision/supersede policy with point-in-time (`as_of`) reads,
  partition compaction, and a calibration store that persists fitted
  coefficients as versioned Parquet tables (never `.rds`).
- Acquisition — the adapter contract: the `met_adapter` S7 base class with
  `fetch()`/`fetch_forecast()`/`resolve_station()` generics and a
  `check_fetch_result()` contract check, so third parties can write their own
  sources; the declarative `met_mapping()`/`apply_mapping()` response-mapping
  spec (JSON path or CSV column, with unit and sensor-height metadata) that
  enforces canonical units at the acquisition boundary; two source-agnostic
  built-in adapters, `source_rest()` (single-page JSON/CSV REST APIs, with
  `none`/`header`/`basic` auth reading secrets from named environment
  variables at fetch time, never stored or printed) and `source_file()`
  (local logger CSV/TSV drops, glob-matched and concatenated in time order);
  the internal `.http_get()` HTTP seam (built on `httr2`) with a no-network
  test guard and retry/error classification (persistent `404`/`410` never
  retried, transient `429`/`5xx` retried with backoff); and
  `adapters_for_site()`, which builds a site's configured adapters and stubs
  the not-yet-implemented provider adapters (Open-Meteo, SILO, GHCNh, BOM,
  ECMWF) with a tested placeholder error for later plans to replace.
