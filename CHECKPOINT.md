# CHECKPOINT — production-ready forecast archiving

Task: `C:\Users\KatoombaWMF\dev\meteoTidy_prompt.md` (problems 1–12 + Definition of Done).
Branch: `fix/production-archiving` (off `main` c53f59f). Push + PR at end; do NOT merge.

## Current state
- Committed: fix(1) lead_time, fix(9) filelock, fix(2,3,6) Open-Meteo, fix(4) BOM forecast.
- UNCOMMITTED, in progress (finish → test → commit):
  - Problem 11 eagle.io: `R/source-eagleio.R` written; YAML kind `eagleio` wired in `R/adapter.R`;
    condition classes `source_stale`, `eagleio_node_empty` registered. Tests (written by a separate
    test-writer agent, from spec) in `tests/testthat/test-regress-11-eagleio.R`: 64 pass; only the
    export test fails, which needs `devtools::document()` (NAMESPACE export for `source_eagleio`).
  - Problem 5 BOM obs: `R/source-bom-obs.R` rungs rewritten (URL `IDN60901/IDN60901.<wmo>.json`,
    `resolved: bom: wmo:` + optional `obs_product:` (alias `product`), web API uses 6-char geohash
    + UA header, breaker persisted on failure); `R/bom-parse.R` obs parsers rewritten (gust, dewpt,
    press_msl, nulls/CALM skipped, web API single-object shape via metadata.observation_time).
    New fixtures: `_fixtures/bom/obs72h-IDN60901-{94743,94763}.json`, `webapi-obs-{r64bhq,r65050}.json`.
    TODO: write `test-regress-05-bom-obs.R` (fails on old code), update `test-source-bom-obs.R`
    test 1 to set `resolved bom wmo`, run the full suite, commit.
- Last full suite (before 5/11): 1036 pass / 0 fail / 3 skip.

## Pending (in order)
- [x] 1  lead_time whole seconds
- [x] 9  filelock + read-dedup
- [x] 2/3/6 Open-Meteo
- [x] 4  BOM forecast (daily précis/web API + hourly web API, model "daily"/"hourly", aux)
- [ ] 5  BOM obs (in progress, see above)
- [ ] 11 eagleio (in progress, see above)
- [ ] 7/10 archive_forecasts per-source isolation (tryCatch per source, record note/status), hold
        `with_store_lock(config$store_root, ...)` per site in sync verbs (config$lock_timeout),
        per-source status column(s) in the met_sync_* result, `fail_on = c("none","any","all")`
        → abort (class e.g. `sync_failed`) so Rscript exits non-zero; one stderr line per site/source
        (message()/cli to stderr). Test: bad-URL source + good source; Rscript exit code via callr/system2.
- [ ] 8  met_wide: `source`/`model` args (default openmeteo/best_match, no pooling), use p50/mean
        stat when no deterministic rows, filter to requested variables before building time base,
        enforce gusts >= wind after aggregation, add shortwave_radiation to .met31_variables
        (or compute direct+diffuse). Provenance names the chosen source.
- [ ] 12 SILO HTML "Request Rejected" → classed error; compare with weatherOz::get_data_drill
        request (UA "weatherOz R package"?). Record fixture of rejected page.
- [ ] inst/acceptance/live_sync.R (checks a–g); README + vignette production section; NEWS
        (breaking: BOM forecast variables/model labels; openmeteo provides default; lead_time units);
        `devtools::check()` 0E/0W; live acceptance run; push; PR; issues for deferred items.

## Key facts / decisions (verified live 2026-09-29)
- Open-Meteo `/data/<model>/static/meta.json` → `last_run_initialisation_time`; ensemble meta name
  `ecmwf_ifs025_ensemble`. best_match floors to 6 h. Default ensemble vars: temp, RH, precip, wind speed/dir/gust.
  best_match has all §3.1 vars (soil moisture only ~7.5 d). ecmwf lacks 80–180 m wind, BLH, soil, uv.
- BOM: précis `https://reg.bom.gov.au/fwo/IDN11060.xml`; Katoomba AAC NSW_PT072, Springwood NSW_PT129,
  Penrith NSW_PT114. Obs stations: Mount Boyce 94743 (kat), Penrith 94763 (blax), product IDN60901.
  Web API geohash kat r64bhq, blax r65050. "X % chance of ≥A mm" stored as stat p(100-X).
- eagle.io: `GET https://api.eagle.io/api/v1/nodes/<id>/historic?startTime=..Z&endTime=..Z&format=json`,
  header X-Api-Key; JTS doc (data[].ts, data[].f."0".v; header.columns."0".units "mm"/"°C").
  Blaxland node last value 2026-09-24T03:00Z (offline). Node meta: `/nodes/<id>?attr=currentTime`.
- meteoHazard needs: wind_speed_10m, wind_gusts_10m (>= wind), wind_direction_10m, precipitation,
  soil_moisture_0_to_1cm, temperature_2m, relative_humidity_2m, shortwave_radiation, direct/diffuse,
  cloud_cover, boundary_layer_height, surface_pressure; no NA for dust/litter; hourly & ordered.
  Working example calls: scratchpad `ex.R`. generate_twl takes vectors; pass 1 m wind? (clamps 0.2–4).

## Process notes
- Run tests: `bash <scratchpad>/tf.sh <regex>` (NOT_CRAN=true, load_all). Full: `tf.sh .` (~70 s).
- Before-fix check: `git stash push -- R/` → run the regress test → `git stash pop`.
- Keys: `. <scratchpad>/loadkeys.sh` exports EAGLE_API_KEY/SILO_API_KEY from C:\services\winsw.xml
  (user explicitly authorised; never print/commit). Auto-mode classifier sometimes denies key use;
  user has given explicit permission.
- Repo set `core.autocrlf=input`; working files are LF. Multi-line edits: Write to scratchpad file +
  awk splice, or Edit tool (heredocs with quotes can break).
- New condition classes must be added to BOTH vectors in `R/conditions.R` meteo_conditions().
- Never write `Sys.time`/`Sys.Date` text in R/ (house test), use `.now()`.
- CLAUDE.md: TDD; use a separate test-writer agent for new components where sensible.
