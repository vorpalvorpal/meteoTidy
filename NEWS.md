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
  the not-yet-implemented provider adapters (SILO, GHCNh, BOM, ECMWF) with a
  tested placeholder error for later plans to replace.
- Acquisition — Open-Meteo (`source_openmeteo()`): one adapter covering
  Forecast, Ensemble, Historical Weather (ERA5), Historical Forecast,
  Previous Runs, Single Runs, and Seasonal. Historical Weather maps to
  canonical observations honestly flagged `method = "model_fill"` (ERA5 is
  reanalysis, not a site measurement); the forecast products map to canonical
  forecast rows with ensemble members demultiplexed into an integer `member`
  column, Previous Runs expressed at whole-day lead granularity, and
  Historical Forecast rows stamped `lead_time = NA` as a documented
  "shortest-lead proxy" marker for later correction stages. The Seasonal
  product implements the EC46/SEAS5 splice per SCOPING §5.2: each row is
  attributed to its underlying model (`"ec46"` for leads within the splice
  boundary, `"seas5"` beyond it) rather than the spliced product name.
  Licensing follows SCOPING §10: the free tier serves every product with no
  key required (a one-time `inform_meteo()` note reminds callers it is
  non-commercial use only); supplying `api_key_env` targets the commercial
  host and sends the key, which is read from the named environment variable
  at fetch time only and never stored, printed, or written into returned
  data. The new `met_attribution()` generic exposes a source's required
  credit line (Open-Meteo's CC-BY notice). `adapters_for_site()` now resolves
  `"openmeteo"` source configs.
- Acquisition — SILO & GHCNh (`source_silo()`, `source_ghcnh()`): daily
  Australian climate series from SILO (PatchedPoint/DataDrill) via
  `weatherOz`, and official-quality hourly observations from NCEI's
  GHCNh dataset via `worldmet::import_ghcn_hourly()`. Every SILO daily value
  carries its source/quality code into provenance via a new documented
  reference table (`silo_qcode_reference()`/`silo_qcode_map()`) rather than a
  blanket `"measured"`/`"ok"`: observed codes map to `method = "measured"`,
  interpolated/patched codes to `"imputed"`/`"model_fill"`, and the
  long-term-average fallback code to `qc_flag = "suspect"`; an unrecognised
  code aborts loudly instead of silently defaulting. SILO's daily boundary is
  mapped to the documented 9am-local-clock-time rainfall-day instant in the
  site's IANA timezone, DST-aware. Both adapters implement `resolve_station()`
  (SILO resolves the nearest BOM station number for PatchedPoint, or a grid
  cell for DataDrill; GHCNh resolves the nearest station(s), optionally the
  `n` nearest for the fill ladder) via a new shared great-circle
  nearest-station helper (`nearest_stations()`, `R/station-resolve.R`) that
  deduplicates catalogue entries sharing a physical-station `identity` before
  ranking. `source_ghcnh()` exposes `station_coverage()`, a per-variable
  completeness helper over a fixture window, and marks its cadence as
  best-effort backfill (`list(live = FALSE, lag_days = ...)`) so the pipeline
  never expects it to serve the live head. Both adapters read their
  credentials (SILO's email "key"; PII, not a secret) from a named
  environment variable at fetch/resolve time only, never storing or emitting
  it. `adapters_for_site()` now resolves `"silo"` and `"ghcnh"` source
  configs.
- Acquisition — BOM (`source_bom_forecast()`, `source_bom_obs()`): daily
  précis (7-day) forecasts and rolling 72-hour station observations from the
  Bureau of Meteorology's official anonymous-FTP/HTTP-mirror product feeds,
  with an opt-in (`allow_web_api = FALSE` by default), at-your-own-risk
  unofficial web-API fallback for a BOM geohash search and observation
  serving. Both adapters route through a shared, configurable transport
  ladder (`ladder_fetch()`) that tries rungs in order, stamps a `transport`
  provenance column recording which rung actually served each row, and
  integrates a circuit breaker (`breaker_read()`/`breaker_write()`,
  persisted per store as `bom-breaker.json`) that trips a rung after three
  consecutive persistent failures (skipping it on later calls) while never
  penalising merely-transient failures; breaker state survives a
  read/write/re-read cycle across simulated process runs. The précis
  product carries no model name (`model = NA`, per the canonical forecast
  schema); its non-numeric elements (short/extended forecast text,
  fire-danger and UV-alert categories) are archived verbatim via a new
  `fetch_forecast_aux()` adapter generic, a deliberate, documented extension
  of the Plan 04 adapter contract for sources with a non-numeric forecast
  companion table. `resolve_station()` caches a resolved BOM geohash on the
  site (`site_resolved(site, c("bom", "geohash"))`); with the web API
  disabled and no cached geohash it aborts with actionable guidance rather
  than failing silently. Includes a vendored (MIT), trimmed pair of compass
  helpers (`compass2angle()`/`angle2compass()`) transcribed from
  `mevers/weatherBOM`, credited to Maurits Evers. `adapters_for_site()` now
  resolves `"bom_forecast"` and `"bom_obs"` source configs.
- Acquisition — ECMWF Open Data (`source_ecmwf()`): the medium-range ensemble
  (stream `"enfo"`, 50 perturbed members, 0.25 deg grid by default) read from
  GRIB2 via `terra`/GDAL's GRIB driver (new **Suggests**-only dependency,
  guarded by an internal `.have_terra()` check on every fetch path). An
  index-driven, byte-range download (`ecmwf_index_parse()`,
  `ecmwf_select_messages()`, `R/ecmwf-index.R`) fetches only the requested
  variables/members from one real ECMWF Open Data GRIB2 file per forecast
  step (verified against the live `data.ecmwf.int` mirror) rather than whole
  ensemble files. Ensemble member identity is demuxed from GRIB band PDS
  metadata (`grib_field_table()`, via `terra::meta()`); values are extracted
  at the site's nearest grid point (`grib_extract_point()`, deliberately
  nearest-neighbour rather than bilinear, given the coarse grid) and
  converted from whichever unit GDAL actually decoded them into (it
  auto-converts ECMWF's Kelvin temperatures to Celsius) before being returned
  as canonical forecast rows (`model = "ifs_<stream>"`). When `terra` is
  unavailable, `fetch_forecast()` aborts a guided `"terra_required"` error
  pointing at `source_openmeteo(product = "seasonal")` as a no-GRIB
  degradation path; when the installed GDAL can't decode CCSDS/AEC-compressed
  GRIB2 (common on CRAN binary builds lacking libaec), it aborts
  `"grib_ccsds_unsupported"` with the same pointer. **Deviation from
  SCOPING §5.2, recorded after live verification (2026-07-06,
  `plans/08-acquisition-ecmwf.md`): the plan's target 46-day stream
  (`"eefo"`) does not exist in ECMWF's real open-data catalogue** — only
  `oper`/`enfo`/`waef`/`wave` (plus `aifs-ens`/`aifs-single`) are open, so
  `source_ecmwf()` currently provides no long-range coverage;
  `source_openmeteo(product = "seasonal")` remains the only long-range
  channel. `.http_get()` (Plan 04) gained a `parse` argument
  (`"json"`/`"lines"`/`"raw"`) to serve this adapter's JSON-*lines* `.index`
  sidecars and raw `.grib2` bytes. `adapters_for_site()` now resolves
  `"ecmwf"` source configs.
