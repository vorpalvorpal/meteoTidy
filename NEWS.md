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
- Curation -- the QC engine (`qc_run()`): ~10 WMO-style rules dispatched by a
  variable's statistical class (range, step -- with correct wraparound for
  circular variables like wind direction, persistence/flat-line, and a
  climatological-bounds check), an internal-consistency rule enforcing
  physical relations across variables at a timestamp (dewpoint must not
  exceed temperature, relative humidity must not exceed 100%, gusts must be
  at least the mean wind speed, direct + diffuse radiation must not exceed a
  clear-sky ceiling), a spatial/buddy check against configured donor
  stations (a robust MAD-based drift detector -- the strongest available
  signal for slow sensor drift, which range/step/persistence all miss), and
  a solar clear-sky rule applying BSRN-style physically-possible/
  extremely-rare irradiance limits against a self-contained Ineichen-Perez
  clear-sky model (`clear_sky_irradiance()`). The internal-consistency
  relations live in a new shared module (`physics_constraints()`,
  `R/physics-constraints.R`) that supports both a `"flag"` mode (used here)
  and an `"enforce"`/clip mode reserved for Plan 12's post-correction
  consistency pass, so the two plans share exactly one set of relations.
  Every rule may only downgrade `qc_flag` (`ok` -> `suspect` -> `fail`),
  never upgrade it, and every decision is appended to a new auditable
  `qc_log` companion table (`site_id`, `datetime_utc`, `variable`, `rule`,
  `outcome`, `detail`; `qc_log_read()`), deduplicated so repeated runs never
  accumulate duplicate audit rows. `qc_run()` is incremental (it reads the
  QC watermark, with a short look-back so a late-arriving donor observation
  can still retrigger the spatial check on the recent tail) and idempotent
  (re-running over the same window reproduces identical flags with no
  duplicate log rows); QC flag changes are written back via the observation
  store's supersede path, so the pre-QC flag stays retrievable for audit
  (`store_read_obs(..., include_superseded = TRUE)`). A `model_only`
  variable (no site truth to compare against a neighbour) is never routed
  to the spatial rule and aborts loudly if called on directly.
- Curation -- gap-fill, the shared transfer engine, and the curated products
  (`fill_run()`): a tiered gap-fill (micro/medium/macro, SCOPING section 6)
  built on a new shared bias-correction primitive, `fit_transfer()`/
  `apply_transfer()` (exported: Plan 12's forecast correction wraps the same
  engine to add lead-dependent shrinkage, but the primitive itself is
  deliberately **skill-decay-free** -- no lead/weight/shrink argument, ever;
  applying a fitted transfer to an early or late row of the same series
  yields an identical correction). Both `"mean_bias"` (a constant offset)
  and `"qmap"` (hand-rolled empirical quantile mapping via `stats::approx()`,
  no new heavy dependency) fitting methods are supported. Each variable is
  filled in its own statistical space (`R/fill-treatments.R`): relative
  humidity via Magnus-Tetens dewpoint conversion (never overshoots
  0-100%), wind direction via unit-vector interpolation (never crosses the
  0/360 wrap through ~180 degrees), precipitation via an occurrence+amount
  treatment (never a linear "drizzle ramp" through a dry spell), solar
  radiation via a clear-sky index (reusing Plan 09's clear-sky model, forced
  to zero at night), and wind speed via a log-wind-profile height
  correction (`height_correct()`, with a documented neutral-stability
  caveat) before any cross-station transfer. The donor ladder
  (`rank_donors()`, BOM -> GHCNh -> ERA5 -> SILO) deduplicates candidates by
  physical station identity (reusing Plan 06's `.dedup_by_identity()`) so a
  station reached by two transports is never double-counted, keeping the
  higher-priority transport on a tie; `model_only` variables (upper-level
  winds, boundary-layer height) always skip the donor ladder entirely and
  take the raw model series. Every filled row is stamped `qc_flag = "ok"`
  (never the gap's inherited `missing`/`fail`) and a `method` identifying it
  as filled (`imputed`/`donor_fill`/`model_fill`), never `measured`.
  `fill_run()` mirrors `qc_run()`'s incremental/idempotent watermark shape
  and writes via the store's supersede path, so a better donor arriving
  later can supersede an earlier fill while the earlier one stays
  retrievable for audit. This plan also assembles the curated products
  (added to this plan's scope 2026-07-05, previously homeless in the
  series): `aggregate_hourly()` (native resolution -> hourly, dispatched
  per statistical class -- mean, sum for rain, vector mean for direction --
  with a documented 75% completeness threshold), `aggregate_daily()`
  (hourly -> daily on the local-day boundary, DST-aware, with a
  table-driven per-variable day-window convention matching SILO's
  documented rain-day and calendar-day windows -- a transition day is a
  correctly-computed 23-/25-hour day), `build_history_hourly()`/
  `build_history_daily()` (the `history_hourly`/`history_daily` products,
  SILO as the daily base with the site's own AWS aggregate winning wherever
  present and QC-clean, provenance always recording which leg served each
  value -- documented as fit for operational bounds but **not** a
  homogenized record, unfit for trend analysis across the AWS install
  date), and `disaggregate_silo()` (the donor ladder's last rung: scales a
  caller-supplied diurnal shape so the 24 disaggregated hours exactly
  reproduce the daily total/min/max, never inventing sub-daily structure
  beyond the shape, with the shape's provenance recorded).
- Correction -- the physical adjustments and tier framework (internal): the
  always-on, never-fitted day-0 corrections (SCOPING section 7.1) --
  log-wind-profile height correction (reusing `height_correct()` from
  Plan 10, so gap-fill and correction share the identical physics) and a
  fixed-lapse-rate temperature adjustment, both documented with their
  stability caveats (neutral-stability wind, fixed-lapse-rate-wrong-under-
  inversions temperature, the latter overridable including a `0` rate for
  sites in a persistent inversion regime). A new `tier_select()` chooses
  the correction tier from two gates that **both** must pass to promote:
  a data-availability gate on training overlap/pair counts (with a
  documented special case letting Open-Meteo daily-lead forecasts reach the
  top tier from day 0 via Previous-Runs pseudo-truth pairs, SCOPING
  section 7.2), and a skill gate (Plan 13's out-of-sample verdict) that
  blocks promotion outright when the data volume alone would otherwise
  justify it -- "data volume never proves the complex method stopped
  overfitting". The correction lifecycle (`correct_apply()`) reads the
  Plan 03 calibration manifest to decide what tier is currently active,
  applies the physical tier at day 0, passes `model_only` variables through
  unchanged at tier `raw`, and *enforces* (not merely advises) that the tier
  actually applied matches the tier-selection process's answer, aborting
  `tier_mismatch` on disagreement. `correct_apply(target = "forecast")`
  routes corrected values through a new shrinkage hook
  (`shrink_to_climatology()`), currently an identity placeholder for Plan
  12's lead-dependent shrinkage; `target = "record"` never shrinks, the
  same forecast/record distinction Plan 10 draws for gap-fill. The monthly
  `correct_refit()` job is scaffolded as a documented skeleton awaiting
  Plan 12's fitting functions and Plan 13's skill verdict.
- Correction -- the fitted tiers (internal): `mean_bias` fits day-of-year and
  hour-of-day bias as a harmonic (sin/cos) regression rather than raw
  hour-of-day bins, so a bias that flips sign summer/winter is recovered and
  removed across the whole year; when the training overlap covers less than
  a full annual cycle, the annual-harmonic amplitudes are damped toward zero
  in proportion to seasonal coverage, so a partial-year fit contributes only
  a mild tilt in the untrained opposite season instead of extrapolating a
  larger, wrong-signed correction there (hour-of-day harmonics are never
  shrunk). `qmap` (hand-rolled empirical quantile mapping) always fits an
  unconditional pooled map alongside any per-group (e.g. per-season) maps,
  so a group with too little or no training data still gets corrected via
  the pooled base rather than left unmapped; beyond the training quantile
  support it extrapolates by the constant shift implied at the nearest
  trained quantile, never an unbounded or clamped-output correction. `emos`
  fits a `crch`-based (falling back to a plain location/scale regression on
  a degenerate fixture) predictive mean+spread per lead bucket, and refuses
  to fit on Historical-Forecast `lead_time = NA` proxy rows
  (`"lead_unresolved"`), so those rows can never contaminate lead-aware
  training. `R/correct.R`'s Plan 11 shrinkage placeholder is now the real
  `shrink_to_climatology(corrected, climatology, weight)` blend primitive
  (`R/shrinkage.R`, plus a target-aware `apply_correction_shrinkage()`
  wrapper); `correct_apply(target = "forecast")` calls it with a
  placeholder `weight = 1` ("trust the correction fully") until Plan 13
  supplies a real verified-skill-derived weight per lead bucket -- a
  documented, known gap. Wind direction is corrected as a joint u/v vector
  rotation (`dir_to_uv()`/`uv_to_dir()`/`correct_wind_direction()`), never
  quantile-mapped as a raw angle, so a bias straddling north is corrected
  without a spurious ~180-degree artefact. A new post-correction
  consistency pass (`consistency_pass()`) reuses Plan 09's shared
  `physics_constraints()` module in its `"enforce"` mode to clip any
  remaining cross-variable violation (gusts, dewpoint, RH, radiation
  ceiling) and counts how many relations needed clipping. The model-only
  experiments (`profile_rescale()`, `diagnostic_blh()`,
  `radiation_resplit()`, `model_only_correct()`) are all opt-in and
  default off: `profile_rescale()` rescales upper-level winds by the
  corrected/raw 10 m wind ratio, damped with height and capped, and
  suppressed entirely under stable stratification; `diagnostic_blh()`
  serves a simple recomputed boundary-layer height alongside (never
  instead of) the raw model BLH; `radiation_resplit()` passes raw
  direct/diffuse through unchanged (tier `"raw"`) without a pyranometer.
- Verification engine (internal): `rolling_origin_score()` walks forward
  through archived `(forecast, observation)` pairs, fitting a calibration
  only on data strictly before a rolling origin (with a documented buffer)
  and scoring it only on the window issued after that origin -- the central
  review fix (never score a calibration on its own training window, which
  would inflate its reported skill and corrupt Plan 11's promotion gate).
  Deterministic (`score_deterministic()`: MAE/RMSE) and probabilistic
  (`score_crps()`, via `scoringRules`, now an `Imports` dependency) scores,
  a `skill_score()` against a baseline, and per-member cumulative
  accumulation (`cumulative_by_member()`, so multi-day totals are summed
  per ensemble member rather than by invalidly summing daily percentiles)
  round out the scoring primitives. Baselines (`baseline_persistence()`,
  `baseline_climatology()`, day-of-year windowed) let "correction helps" be
  judged against climatology, not just the raw model -- climatology can and
  does beat a low-skill long-lead forecast. Calibration diagnostics
  (`rank_histogram()`/`histogram_flatness()`, `spread_error_ratio()`,
  `brier_score()`/`reliability_table()`) catch sharpness/reliability issues
  CRPS alone conflates. A moving-block bootstrap (`block_bootstrap_ci()`)
  gives a significance call on score differences that correctly widens
  (versus a naive i.i.d. bootstrap) on autocorrelated series.
  `skill_verdict_compute()` -- deliberately not named `skill_verdict()`, to
  avoid colliding with the identically-named test-helper builder Plans
  11/12 already depend on -- turns scores and a bootstrap result into the
  `promote`/`shrink_weight`/`consistency_violation_rate` verdict those
  plans consume, requiring both a real out-of-sample improvement *and*
  that improvement surviving the bootstrap before promoting a tier.
  `verify_run()` assembles pairs (`assemble_verification_pairs()`), scores
  the raw tier out-of-sample per `(source, variable, lead_bucket)`, and
  writes a report retrievable via `read_verification_report()`; wiring in
  Plan 11/12's actual fitted tiers side by side is left to Plan 16's
  pipeline orchestration.
- The stable, public read surface: `met_history()`, `met_record()`,
  `met_forecast_archive()`, and `met_verification()` are the first exported,
  versioned tibble-returning functions over the store -- every other verb so
  far has stayed internal pipeline machinery. Each accepts a single
  `met_site` or a `met_sites` collection and row-binds across sites, and
  validates its return value with the Plan 01 canonical helper before
  returning. `met_forecast_archive(members = TRUE)` (the default) retrieves
  per-member ensemble trajectories; `met_record(as_of = ...)` reproduces a
  historical point-in-time view via Plan 03's revision policy. The four
  signatures are snapshot-guarded (`formals()`) so an accidental breaking
  change to this contract is caught in review. `met_connect()` exposes the
  same store over `duckdb`/`arrow` for ad-hoc SQL/dplyr access, documented
  experimental (only the tibble functions are a stability promise -- this
  one exposes the raw physical schema, including bookkeeping columns like
  `superseded`, and may change without a deprecation cycle). Deployment
  configuration (`read_deployment_config()`, non-secret YAML layered over
  the Plan 02 per-site registry: store roots, per-source refetch windows,
  adapter defaults) reuses Plan 02's inline-secret guard so a literal secret
  value anywhere under `sources` still fails loud. Secrets are resolved by
  name only, never inlined: `resolve_secret()` reads an env var or (via the
  optional `keyring` package) a keyring entry at use time and caches
  nothing; `redact()` hides a secret in any print/format output; and
  `assert_no_secrets_in()` is a belt-and-braces guard callers can run before
  any store write to abort loudly if a resolved secret value would
  otherwise leak into Parquet/manifests/provenance.
- Fixed two real bugs caught while implementing the read API: `cli::cli_abort()`
  cannot interpolate a glue expression that starts with a literal dot
  (`{.deployment_top_level_keys}` was misparsed as a cli inline style, not a
  variable reference) -- fixed by binding to a plain local name first. And a
  classic R partial-argument-matching footgun: a mockable bridge function's
  `site_id` parameter was silently overwritten by an unrelated `site = `
  argument at the call site, because `"site"` is an unambiguous prefix of
  `"site_id"` and R's partial matching resolved it there instead of falling
  through to `...` -- fixed by renaming the parameter to remove the prefix
  relationship entirely, rather than relying on argument order.
- The meteoHazard interface: `met_table`, a classed tibble (S3, extending
  `tbl_df` -- the sf/tsibble pattern, not an opaque S7 wrapper) carrying
  per-variable provenance (`tier`, `train_overlap`, `source`), site/window
  keys, and schema/calibration-manifest versions (`new_met_table()`,
  `met_provenance()`/`met_keys()`/`met_versions()`). `dplyr` verbs
  (`filter()`, `arrange()`, `mutate()`, `select()`) keep the class and
  metadata alive through ordinary wrangling; an operation that genuinely
  invalidates the metadata -- dropping a tracked value column, or
  `bind_rows()` combining two `met_table`s whose provenance disagrees for
  the same variable -- downgrades **visibly** to a plain tibble with a
  `met_table_downgraded` warning, rather than silently carrying stale
  attributes. Because `dplyr`'s reconstruction hook cannot see an in-place
  value mutation (`mutate(x = x + 1)` leaves the class and provenance
  untouched while the data silently changes), every value column is
  content-hashed (via `digest`, a new `Imports` dependency) at construction
  time; `met_validate_boundary()` recomputes and compares those hashes,
  downgrading just the affected column's `tier` to `"unverified"` on a
  mismatch -- the honest-scoping guarantee that metadata is authoritative
  only *at a validated boundary*, made enforceable rather than aspirational
  (the review fix). `met_wide()` is the one-call section-3.1 wide emitter:
  `kind = "record"`/`"forecast"` route through `met_record()`/
  `met_forecast_archive()` (Plan 14), widen to one row per timestamp with
  the `datetime_utc`/`valid_time` index renamed to `time` at this outer
  boundary only, and keep a variable absent from the underlying data as a
  stable all-`NA` column when explicitly requested via `variables =`.
  `met_ingest()` is the dual-accept boundary validator meteoHazard calls
  once: a classed `met_table` is hash-validated and trusted thereafter; a
  plain tibble is schema-checked on entry (a `time` column is the minimum
  section-3.1 contract) and wrapped with its provenance marked entirely
  `"unverified"`. `met_assert_single_tier()` implements the
  single-provenance-class rule, warning when a derived index would mix
  inputs from more than one correction tier (e.g. a corrected 10 m wind
  with a raw 80 m wind feeding one shear calculation).
- Fixed a real, reproducible bug found while implementing the classed
  tibble: reconstructing a `met_table`'s class from the *intermediate*
  object's own class (rather than always rebuilding from
  `tibble::as_tibble()`) silently dropped `tbl_df`/`tbl` when that
  intermediate happened to be a bare `data.frame` (as `dplyr::mutate()`'s
  internal machinery produces) -- invisible until a keep-all `mutate()`
  (which still runs its result through a column-selection step) then
  dispatched to `` `[.data.frame` `` instead of `` `[.tbl_df` ``, silently
  losing the provenance/keys/versions/content-hash attributes.
  Also: an unrelated, package-wide `S7`/base-generic interaction -- any
  `S7::method(print, ...)`/`S7::method(format, ...)` registration anywhere
  in the package (several earlier plans' adapter classes have one) was
  found to break ordinary S3 dispatch of `print()`/`format()` for *every
  other* plain-S3 class in the package when called from outside it, even
  though the S3 method stayed correctly listed in the package's own methods
  table throughout -- worked around with an explicit re-registration in a
  new `.onLoad()` (which also now calls the S7-recommended
  `methods_register()`).
