# meteoTidy

> A project of [`tidyWaste`](https://github.com/vorpalvorpal/tidyWaste) — an ecosystem of R packages for waste management facilities.

Ingest, clean, and calibrate site weather data. Part of the tidyWaste family.

meteoTidy is the site-weather data layer for
[meteoHazard](https://github.com/vorpalvorpal/meteoHazard) and related
dashboards. It has four jobs:

1. **Acquire** — adapters for a site's own AWS (generic REST or file drops),
   BOM observations and forecasts, SILO, Open-Meteo (forecast, ensemble,
   historical, seasonal), GHCNh, and ECMWF Open Data. The adapter contract is
   exported (`met_adapter()`, `fetch()`), so any other source can be wired in
   without touching package internals.
2. **Curate** — QC-flag (`qc_run()`), gap-fill (`fill_run()`), and
   provenance-track observations into a continuous "best available truth" per
   site (`met_record()`, `met_history()`).
3. **Correct** — fit and apply site-specific bias corrections, tiered by the
   training overlap available (physical → mean-bias → quantile mapping →
   EMOS), with skill-gated promotion and out-of-sample verification
   (`met_refit()`, `met_verification()`).
4. **Serve** — tidy, keyed tables from a hive-partitioned Parquet store,
   including a locally maintained forecast archive
   (`met_forecast_archive()`) and the wide hourly table meteoHazard consumes
   (`met_wide()`).

meteoTidy is **not** another weather-API client. It *uses*
[weatherOz](https://cran.r-project.org/package=weatherOz) for SILO transport
and [worldmet](https://cran.r-project.org/package=worldmet) for GHCNh, and
differentiates on everything downstream: QC, gap-fill, bias correction,
forecast archiving, verification, and storage.

## Installation

meteoTidy is distributed from GitHub (no CRAN submission is planned):

```r
pak::pak("vorpalvorpal/meteoTidy")
```

## Quick tour

```r
library(meteoTidy)

# Sites are configured in version-controlled YAML (secrets stay in env vars,
# referenced by NAME -- never inlined in the YAML).
sites <- read_sites_yaml("sites.yml")

config <- list(
  store_root = "/var/meteoTidy/store",
  obs_sources = c("site_aws", "silo"),
  forecast_sources = c("openmeteo", "bom_forecast")
)

# Day-0 bootstrap: full history pulls, AWS export ingestion, initial fits,
# and a per-site donor-coverage audit.
met_backfill(sites, config = config)

# Then on a schedule (cron / GitHub Actions / taskscheduleR -- see
# vignette("scheduling")):
met_sync_daily(sites, config = config)   # daily
met_sync_live(sites, config = config)    # hourly, optional
met_refit(sites, config = config)        # monthly

# The one-call meteoHazard interface: the wide, Open-Meteo-named hourly
# table with per-variable provenance attached (a classed tibble).
wide <- met_wide(sites@sites[[1]],
                 window = list(from = Sys.time(), to = Sys.time() + 7 * 86400),
                 kind = "forecast")
met_provenance(wide)
```

Every stored value carries provenance `(source, method, qc_flag)`; forecasts
are archived on every sync, deduplicated, because BOM keeps no public archive
of its edited forecast — anything not fetched is lost.

**Note:** `history_daily` (SILO base, AWS overlay) is *not* a homogenized
climate record — the AWS installation date introduces a step change. It is
fit for operational bounds and calibration priors, unfit for trend analysis.

## Licensing and attribution of upstream data

- **Open-Meteo** — the free tier is **non-commercial use only** (fewer than
  10,000 calls/day). Commercial deployments (which working livestock
  operations plausibly are) need a paid plan and API key; the historical,
  ensemble, and seasonal APIs sit on paid tiers. Data is CC-BY 4.0:
  attribute Open-Meteo in anything published. `source_openmeteo()` accepts
  an API key and `met_attribution()` returns the attribution text per
  adapter.
- **BOM** — official anonymous product feeds are used wherever they suffice.
  The undocumented `api.weather.bom.gov.au` web API is **opt-in and
  at-your-own-risk** (`allow_web_api = TRUE`); BOM has stated it is not for
  direct access, and it may disappear without notice. Never promise
  real-time BOM data: if every BOM web channel dies, live-head gap-fill
  degrades for up to ~a week until GHCNh catches up.
- **SILO** — the API key is your email address: PII rather than a secret,
  but keep it out of public repos (env var suffices).
- **Vendored weatherBOM code** — trimmed functions from
  [mevers/weatherBOM](https://github.com/mevers/weatherBOM) (MIT) are
  vendored in `R/vendor-weatherbom.R`; the MIT notice is retained and
  Maurits Evers is credited as a contributor.

## Documentation

- `vignette("scheduling")` — recommended cadence for the four pipeline verbs
  and cron / GitHub Actions / taskscheduleR recipes, including the BOM
  cannot-backfill trade-off.
