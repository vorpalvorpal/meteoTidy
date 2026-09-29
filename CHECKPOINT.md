# CHECKPOINT — production-ready forecast archiving

Task: `C:\Users\KatoombaWMF\dev\meteoTidy_prompt.md` (problems 1–12 + Definition of Done).
Branch: `fix/production-archiving` (off `main` c53f59f). Push + PR at end; do NOT merge.

## Current state
- Baseline: 954 pass / 0 fail / 3 skip (`devtools::test()`, ~60 s).
- Working on: see "Pending" — first unchecked item.

## Pending (in order; tick when committed)
- [ ] 1  lead_time whole seconds (write+read tolerance) — parquet round-trip test
- [ ] 9  filelock on store_root + idempotent writes
- [ ] 2/3/6 Open-Meteo: forecast_days horizon, issue_time from model meta.json (fallback floor 6 h),
        ensemble default models, skip "undefined"-unit vars w/ warning, YAML `provides`, 429 backoff
- [ ] 4  BOM forecast: web API daily+hourly (6-char geohash) + précis XML (AAC) rung; dict extension
- [ ] 5  BOM obs: IDN60901.<wmo>.json URL, web-API single-object parser, 6-char geohash
- [ ] 7/10 per-source isolation + per-source status, `fail_on`, stderr log line
- [ ] 8  met_wide source/model selection, no pooling, gusts>=wind, shortwave_radiation
- [ ] 11 eagleio adapter (JTS JSON, X-Api-Key from EAGLE_API_KEY, stale station handling)
- [ ] 12 SILO HTML "Request Rejected" detection
- [ ] inst/acceptance/live_sync.R ; README + vignette ; NEWS ; devtools::check()
- [ ] live acceptance run → PR with output, issues for deferred items

## Blockers
- Auto-mode classifier denies any command that reads/uses the eagle.io or SILO key
  (even the key the user pasted). Live eagle.io + SILO checks need the user to allow
  it or run `inst/acceptance/live_sync.R` themselves with EAGLE_API_KEY/SILO_API_KEY set.
  The eagle.io fixture is built from the documented JTS format, not recorded — re-record when possible.

## Key facts / decisions (verified live 2026-09-29)
- Open-Meteo: `/data/<model>/static/meta.json` → `last_run_initialisation_time` (unix).
  Ensemble meta path `ecmwf_ifs025_ensemble`. `bom_access_global` on Open-Meteo is stale (2025).
  best_match gives all §3.1 vars (soil moisture only ~7.5 d); ecmwf_ifs025 lacks 80–180 m wind,
  BLH, soil moisture, uv (unit "undefined").
- BOM web API: `/v1/locations/<geohash6>/forecasts/{daily,hourly}`, `/observations` (single object).
  `metadata.issue_time` present. Daily: temp_max/min, rain.chance, rain.precipitation_amount_{25,50,75}_percent_chance,
  uv.max_index, fire_danger, short_text, extended_text. Hourly: temp, temp_feels_like, dew_point,
  relative_humidity, wind.{speed,gust_speed}_kilometre, wind.direction (compass), uv, rain.chance,
  rain.precipitation_amount_{10,25,50}_percent_chance.
- BOM précis XML: `https://reg.bom.gov.au/fwo/IDN11060.xml` (needs browser UA; ftp returned empty).
  Katoomba AAC NSW_PT072; Blaxland nearest Springwood NSW_PT129 / Penrith NSW_PT114.
  precipitation_range "a to b mm" = 50 % / 25 % chance amounts.
- BOM 72-h obs: `https://reg.bom.gov.au/fwo/IDN60901/IDN60901.<wmo>.json`; Mount Boyce 94743 (kat),
  Penrith 94763 (blax). Half-hourly, gust_kmh, wind_dir compass.
- "X % chance of at least A mm" is stored as quantile stat p(100-X).
- meteoHazard needs: wind_speed_10m, wind_gusts_10m (>= wind), wind_direction_10m, precipitation,
  soil_moisture_0_to_1cm, temperature_2m, relative_humidity_2m, shortwave_radiation, direct/diffuse_radiation,
  cloud_cover, boundary_layer_height, surface_pressure; no NA for dust/litter; hourly & ordered.

## Process notes
- Run R: `cd /c && "/c/Program Files/R/R-4.5.1/bin/Rscript.exe" --vanilla ...`
- Probe responses: scratchpad `probe/` (session scratch, not in repo).
- Every fix: failing regression test on realistic input FIRST, then fix, then commit.
