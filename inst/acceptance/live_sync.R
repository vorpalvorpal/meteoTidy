# Live acceptance checks for production archiving (hourly met_sync_live(),
# daily met_sync_daily()) against the real upstream APIs.
#
#   Rscript inst/acceptance/live_sync.R [scratch_dir]
#
# Needs SILO_API_KEY and EAGLE_API_KEY in the environment (never printed).
# Set METEOTIDY_SOURCE=<path to a meteoTidy source tree> to test an
# uninstalled checkout (loaded with pkgload); otherwise the installed
# package is used. Writes ONLY to a scratch store (default: a new directory
# under the system temp directory). Prints PASS/FAIL per check and exits
# non-zero if any check fails.
#
# Checks:
#   a. a second live run adds no duplicate rows
#   b. archive contents: Open-Meteo deterministic >= 7 days ahead, ensemble
#      with >= 2 members, BOM daily (7 days) + hourly, per-site values differ
#   c. met_wide() (72 h, one source) runs through meteoHazard's
#      dust_hazard(), litter_risk(), odour_risk() and generate_twl()
#   d. met_sync_daily() collects eagle.io (kat ok, blax stale), SILO, BOM obs
#   e. a forced source failure is isolated and named; fail_on = "any" gives
#      a non-zero Rscript exit status
#   f. two concurrent met_sync_live() processes: no duplicates, no lost rows
#   h. (follow-up 1) every row a sync reports is readable back from this
#      scratch path (run once with a long path, once short); a store root
#      too long for Windows is refused with status "error", nothing written
#   i. (follow-up 2) fail_on = "failed": exit 0 while blax's eagle.io is
#      merely stale; non-zero when a source fails
#   j. (follow-up 3) BOM daily forecast from the site geohash web API (model
#      "daily", location names the geohash), not the town précis
#   k. (follow-up 4) re-fetching unchanged observations supersedes nothing
#   l. (follow-up 5) met_compact() cuts the file count, reads unchanged
#   m. (follow-up 7) the second live run downloads no Open-Meteo run that is
#      already archived ("already archived" in the status)
#   n. (follow-up 9) the email variables, both ensembles and BOM's fire
#      danger category are archived
#   o. (follow-up 11) every source reads back (met_forecast_archive(),
#      met_record()) with lead_time == valid_time - issue_time
#   p. (follow-up 10) met_wide() per-variable models and stat = "p95"
#   g. store size and estimated growth per day

`%||%` <- function(x, y) if (is.null(x)) y else x

load_meteotidy <- function() {
  src <- Sys.getenv("METEOTIDY_SOURCE")
  if (nzchar(src)) {
    suppressMessages(pkgload::load_all(src, quiet = TRUE, export_all = FALSE))
  } else {
    suppressPackageStartupMessages(library(meteoTidy))
  }
}
load_meteotidy()
options(width = 160, meteoTidy.sync_log = TRUE)

for (key in c("SILO_API_KEY", "EAGLE_API_KEY")) {
  if (!nzchar(Sys.getenv(key))) stop(key, " is not set.", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
scratch <- normalizePath(
  if (length(args)) args[1] else file.path(Sys.getenv("TEMP", tempdir()), paste0("meteoTidy-acceptance-", format(Sys.time(), "%Y%m%d-%H%M%S"))),
  winslash = "/", mustWork = FALSE
)
if (grepl("^[A-Za-z]:/services|Dropbox", scratch, ignore.case = TRUE)) {
  stop("Refusing to write to ", scratch, ": use a scratch directory.", call. = FALSE)
}
dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
cat("Scratch directory:", scratch, "\n")

# ---- helpers ----------------------------------------------------------------

results <- data.frame(check = character(0), ok = logical(0), detail = character(0))
check <- function(id, ok, detail) {
  ok <- isTRUE(ok)
  results[nrow(results) + 1, ] <<- list(id, ok, detail)
  cat(sprintf("%s %s: %s\n", if (ok) "PASS" else "FAIL", id, detail))
  invisible(ok)
}
section <- function(title) cat("\n==", title, strrep("=", max(0, 70 - nchar(title))), "\n")

yaml_template <- system.file("acceptance", "sites.yaml", package = "meteoTidy")
write_sites <- function(root, path, extra_sources = NULL) {
  spec <- yaml::read_yaml(yaml_template)
  for (i in seq_along(spec$sites)) {
    spec$sites[[i]]$store_root <- root
    for (nm in names(extra_sources)) {
      spec$sites[[i]]$sources[[nm]] <- extra_sources[[nm]](spec$sites[[i]])
    }
  }
  yaml::write_yaml(spec, path)
  path
}

now_utc <- function() {
  t <- as.POSIXct(trunc(Sys.time(), "secs"))
  attr(t, "tzone") <- "UTC"
  t
}

forecast_key <- c("site_id", "source", "model", "issue_time", "valid_time", "member", "stat", "variable")
obs_key <- c("site_id", "datetime_utc", "variable", "source")

read_table <- function(root, table) {
  dir <- file.path(root, table)
  if (!dir.exists(dir)) return(NULL)
  as.data.frame(dplyr::collect(arrow::open_dataset(dir, format = "parquet",
                                                   partitioning = arrow::hive_partition())))
}

# Raw duplicate count, straight from the parquet files (store_read_* drop
# duplicate keys on read, which would hide them).
n_dup <- function(df, key) if (is.null(df) || nrow(df) == 0) 0L else sum(duplicated(df[key]))
raw_dups <- function(root) {
  fc <- read_table(root, "forecasts")
  obs <- read_table(root, "observations")
  obs <- obs[!obs$superseded, , drop = FALSE]
  c(forecasts = n_dup(fc, forecast_key), observations = n_dup(obs, obs_key))
}
row_counts <- function(root) {
  fc <- read_table(root, "forecasts")
  obs <- read_table(root, "observations")
  c(forecasts = nrow(fc %||% data.frame()), observations = sum(!(obs$superseded %||% logical(0))))
}

src_status <- function(res, site, source) {
  s <- res$sources[[match(site, res$site_id)]]
  s[s$source == source, , drop = FALSE]
}

dir_bytes <- function(path) {
  f <- list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  sum(file.size(f), na.rm = TRUE)
}
mb <- function(b) sprintf("%.1f MB", b / 1024^2)

timed <- function(expr) {
  t0 <- Sys.time()
  value <- force(expr)
  list(value = value, secs = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

# ---- setup -------------------------------------------------------------------

root <- file.path(scratch, "store")
dir.create(root, showWarnings = FALSE)
sites_path <- write_sites(root, file.path(scratch, "sites.yaml"))
sites <- read_sites_yaml(sites_path)

live_cfg <- list(
  store_root = root,
  obs_sources = c("bom_obs", "eagleio"),
  forecast_sources = c("openmeteo", "om_ens", "bom_forecast")
)
daily_cfg <- list(
  store_root = root,
  obs_sources = c("bom_obs", "eagleio", "silo"),
  forecast_sources = c("openmeteo", "om_ens", "bom_forecast"),
  refetch_windows = list(
    silo = as.difftime(30, units = "days"),
    eagleio = as.difftime(7, units = "days"),
    bom_obs = as.difftime(3, units = "days")
  )
)

# ---- a. second live run adds no duplicates -------------------------------------

section("a. live sync twice")
run1 <- timed(met_sync_live(sites, now = now_utc(), config = live_cfg))
bytes_live1 <- dir_bytes(root)
rows1 <- row_counts(root)
fc_after1 <- read_table(root, "forecasts")
print(run1$value[c("site_id", "status", "message")])
run2 <- timed(met_sync_live(sites, now = now_utc(), config = live_cfg))
rows2 <- row_counts(root)
bytes_live2 <- dir_bytes(root)
dups <- raw_dups(root)
cat(sprintf("run 1: %.0f s, run 2: %.0f s; rows after run 1: %s; after run 2: %s\n",
            run1$secs, run2$secs, paste(names(rows1), rows1, collapse = ", "),
            paste(names(rows2), rows2, collapse = ", ")))
check("a", all(dups == 0) && all(run1$value$status != "failed"),
      sprintf("duplicate keys after two live runs: forecasts %d, observations %d; run 2 added %d forecast rows",
              dups[["forecasts"]], dups[["observations"]], rows2[["forecasts"]] - rows1[["forecasts"]]))

# ---- b. archive contents ------------------------------------------------------

section("b. archive contents")
t_now <- now_utc()
b_ok <- TRUE
temps <- list()
fc_b <- read_table(root, "forecasts")
fc_src <- function(sid, src) fc_b[fc_b$site_id == sid & fc_b$source == src, , drop = FALSE]
for (sid in c("kat", "blax")) {
  om <- fc_src(sid, "openmeteo")
  ens <- fc_src(sid, "om_ens")
  bom <- fc_src(sid, "bom_forecast")
  om_days <- as.numeric(difftime(max(om$valid_time), t_now, units = "days"))
  n_members <- length(unique(stats::na.omit(ens$member)))
  bom_daily_days <- length(unique(as.Date(bom$valid_time[bom$model == "daily"], tz = "Australia/Sydney")))
  bom_hourly <- sum(bom$model == "hourly")
  ok <- om_days >= 7 && n_members >= 2 && bom_daily_days >= 7 && bom_hourly > 0
  b_ok <- b_ok && ok
  cat(sprintf("%s: openmeteo reaches +%.1f d (%d vars); om_ens %d members, +%.1f d; bom daily %d days, hourly %d rows (+%.1f h)\n",
              sid, om_days, length(unique(om$variable)), n_members,
              as.numeric(difftime(max(ens$valid_time), t_now, units = "days")),
              bom_daily_days, bom_hourly,
              as.numeric(difftime(max(bom$valid_time[bom$model == "hourly"]), t_now, units = "hours"))))
  t2m <- om[om$variable == "temperature_2m" & om$issue_time == max(om$issue_time), ]
  temps[[sid]] <- t2m$value[order(t2m$valid_time)]
}
n_cmp <- min(lengths(temps))
differs <- !isTRUE(all.equal(temps$kat[seq_len(n_cmp)], temps$blax[seq_len(n_cmp)]))
cat(sprintf("mean forecast temperature_2m: kat %.1f degC, blax %.1f degC\n", mean(temps$kat), mean(temps$blax)))
check("b", b_ok && differs,
      "Open-Meteo >= 7 d ahead, ensemble >= 2 members, BOM daily 7 d + hourly, sites differ")

# ---- c. met_wide -> meteoHazard -------------------------------------------------

section("c. met_wide() through meteoHazard")
c_ok <- tryCatch({
  suppressPackageStartupMessages({ library(meteoHazard); library(sf) })
  win <- list(from = as.POSIXct(ceiling(as.numeric(now_utc()) / 3600) * 3600, origin = "1970-01-01", tz = "UTC"))
  win$to <- win$from + 71 * 3600
  all_ok <- TRUE
  for (site in sites@sites) {
    met <- met_wide(site, win, source = "openmeteo")
    prov <- met_provenance(met)
    stopifnot(nrow(met) == 72, all(diff(as.numeric(met$time)) == 3600),
              all(met$wind_gusts_10m >= met$wind_speed_10m, na.rm = TRUE))
    need <- c("temperature_2m", "relative_humidity_2m", "precipitation", "wind_speed_10m",
              "wind_gusts_10m", "wind_direction_10m", "soil_moisture_0_to_1cm", "shortwave_radiation",
              "direct_radiation", "diffuse_radiation", "cloud_cover", "boundary_layer_height",
              "surface_pressure")
    nas <- vapply(need, function(v) sum(is.na(met[[v]])), integer(1))
    if (any(nas > 0)) stop("NA values in ", paste(names(nas)[nas > 0], collapse = ", "))
    met_df <- as.data.frame(met)

    ctr <- sf::st_transform(sf::st_sfc(sf::st_point(c(as.numeric(site@longitude), as.numeric(site@latitude))), crs = 4326), 28356)
    xy <- sf::st_coordinates(ctr)
    dust <- dust_hazard(met_df)
    sectors <- data.frame(arc_start = c("N", "S"), arc_end = c("S", "N"),
                          permeability = c(0.8, 0.4), sensitive = c(TRUE, FALSE))
    lsite <- site_from_sectors(sectors, sf::st_sf(geometry = ctr), ring_radius = 500, epsg = 28356)
    litter <- litter_risk(met_df, lsite, use_wetness_state = TRUE)
    feats <- sf::st_sf(id = c("src", "rcv"), geometry = sf::st_sfc(
      sf::st_point(xy[1, 1:2]), sf::st_point(xy[1, 1:2] + c(800, 500)), crs = 28356))
    roles <- data.frame(feature_id = c("src", "rcv"), hazard = "odour", role = c("source", "receptor"))
    odour <- odour_risk(met_df, mh_site(features = feats, roles = roles, epsg = 28356L), datetime = met_df$time)
    twl <- generate_twl(met_df$time, latitude = as.numeric(site@latitude), longitude = as.numeric(site@longitude),
                        temp = met_df$temperature_2m, wind_speed = met_df$wind_speed_10m,
                        RH = met_df$relative_humidity_2m, direct_solar = met_df$direct_radiation,
                        diffuse_solar = met_df$diffuse_radiation, pressure = met_df$surface_pressure,
                        convert_pressure = TRUE, verbose = FALSE)
    cat(sprintf("%s: 72 rows from %s (%s); dust %d rows, litter %d rows, odour %s, TWL range %.0f-%.0f\n",
                site_id(site), paste(unique(stats::na.omit(prov$source)), collapse = "/"),
                paste(unique(stats::na.omit(prov$model)), collapse = ", "),
                NROW(dust), NROW(litter), paste(dim(odour), collapse = "x"),
                min(as.numeric(twl), na.rm = TRUE), max(as.numeric(twl), na.rm = TRUE)))
    all_ok <- all_ok && NROW(dust) > 0 && NROW(litter) > 0 && length(odour) > 0 && any(is.finite(as.numeric(twl)))
  }
  all_ok
}, error = function(e) { cat("error:", conditionMessage(e), "\n"); FALSE })
check("c", c_ok, "72 h single-source met_wide() runs through dust_hazard/litter_risk/odour_risk/generate_twl")

# ---- d. daily sync -------------------------------------------------------------

section("d. daily sync")
daily <- timed(met_sync_daily(sites, now = now_utc(), config = daily_cfg))
bytes_daily <- dir_bytes(root) - bytes_live2
print(daily$value[c("site_id", "status", "message")])
d_expect <- list(
  c("kat", "eagleio", "ok"), c("blax", "eagleio", "stale"),
  c("kat", "silo", "ok"), c("blax", "silo", "ok"),
  c("kat", "bom_obs", "ok"), c("blax", "bom_obs", "ok")
)
d_ok <- TRUE
for (e in d_expect) {
  s <- src_status(daily$value, e[1], e[2])
  got <- if (nrow(s)) s$status else "missing"
  n <- if (nrow(s)) s$n else 0L
  cat(sprintf("%s/%s: %s (%d rows)%s\n", e[1], e[2], got, n,
              if (nrow(s) && !is.na(s$message)) paste0(" - ", substr(s$message, 1, 120)) else ""))
  d_ok <- d_ok && identical(got, e[3]) && (e[3] != "ok" || n > 0)
}
blax_fc_ok <- all(src_status(daily$value, "blax", "openmeteo")$status == "ok")
cat(sprintf("daily run: %.0f s\n", daily$secs))
check("d", d_ok && blax_fc_ok,
      "eagle.io kat ok / blax stale (other blax sources unaffected); SILO and BOM obs ok for both sites")

# ---- e. forced failure ----------------------------------------------------------

section("e. forced failure")
broken <- list(broken_obs = function(s) list(
  adapter = "eagleio", api_key_env = "EAGLE_API_KEY",
  base_url = "https://api.eagle.io/api/v1/no-such-endpoint",
  nodes = s$sources$eagleio$nodes
))
broken_path <- write_sites(root, file.path(scratch, "sites-broken.yaml"), broken)
broken_cfg <- live_cfg
broken_cfg$obs_sources <- c(live_cfg$obs_sources, "broken_obs")
log_lines <- character(0)
res_e <- withCallingHandlers(
  met_sync_live(read_sites_yaml(broken_path), now = now_utc(), config = broken_cfg),
  message = function(m) {
    log_lines <<- c(log_lines, conditionMessage(m))
    invokeRestart("muffleMessage")
  }
)
cat(log_lines, sep = "")
e_isolated <- all(vapply(c("kat", "blax"), function(sid) {
  b <- src_status(res_e, sid, "broken_obs")
  others <- src_status(res_e, sid, "openmeteo")
  # A zero-row "ok" is right when the run was skipped as already archived
  # (item 7): the earlier sections stored it.
  archived <- others$n > 0 | grepl("already archived", others$message, fixed = TRUE)
  nrow(b) == 1 && b$status == "failed" && all(others$status == "ok") && all(archived)
}, logical(1)))
e_logged <- any(grepl("broken_obs FAILED", log_lines, fixed = TRUE))

child <- file.path(scratch, "fail_on_child.R")
writeLines(c(
  paste("load_meteotidy <-", paste(deparse(load_meteotidy), collapse = "\n")),
  "load_meteotidy()",
  sprintf("cfg <- list(store_root = %s, obs_sources = c('bom_obs', 'broken_obs'), forecast_sources = character(0))", deparse(root)),
  sprintf("invisible(met_sync_live(read_sites_yaml(%s), config = cfg, fail_on = 'any'))", deparse(broken_path))
), child)
status <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(child)),
                                   stdout = FALSE, stderr = file.path(scratch, "fail_on_child.log")))
cat("fail_on = 'any' child exit status:", status, "\n")
cat(utils::tail(readLines(file.path(scratch, "fail_on_child.log")), 4), sep = "\n")
check("e", e_isolated && e_logged && status != 0,
      sprintf("broken source failed alone (others archived), log names it: %s, Rscript exit status %s",
              e_logged, status))

# ---- f. concurrent processes ------------------------------------------------------

section("f. two concurrent live syncs")
root_f <- file.path(scratch, "store-concurrent")
dir.create(root_f, showWarnings = FALSE)
sites_f <- write_sites(root_f, file.path(scratch, "sites-concurrent.yaml"))
go <- file.path(scratch, "GO")
cfg_f <- live_cfg
cfg_f$store_root <- root_f
worker <- function(loader, sites_path, cfg, go) {
  loader()
  while (!file.exists(go)) Sys.sleep(0.05)
  t <- as.POSIXct(trunc(Sys.time(), "secs"))
  attr(t, "tzone") <- "UTC"
  res <- met_sync_live(read_sites_yaml(sites_path), now = t, config = cfg)
  do.call(rbind, lapply(seq_len(nrow(res)), function(i) cbind(site_id = res$site_id[i], res$sources[[i]])))
}
procs <- lapply(1:2, function(i) callr::r_bg(worker, list(load_meteotidy, sites_f, cfg_f, go),
                                             env = c(callr::rcmd_safe_env(),
                                                     SILO_API_KEY = Sys.getenv("SILO_API_KEY"),
                                                     EAGLE_API_KEY = Sys.getenv("EAGLE_API_KEY"),
                                                     METEOTIDY_SOURCE = Sys.getenv("METEOTIDY_SOURCE"))))
Sys.sleep(15) # let both finish loading before releasing them together
file.create(go)
for (p in procs) p$wait(timeout = 30 * 60 * 1000)
outs <- lapply(procs, function(p) tryCatch(p$get_result(), error = function(e) { cat("worker error:", conditionMessage(e), "\n"); NULL }))
f_ok <- !any(vapply(outs, is.null, logical(1)))
if (f_ok) {
  dups_f <- raw_dups(root_f)
  fc <- read_table(root_f, "forecasts")
  obs <- read_table(root_f, "observations")
  obs <- obs[!obs$superseded, , drop = FALSE]
  lost <- character(0)
  for (sid in c("kat", "blax")) {
    for (src in unique(c(outs[[1]]$source, outs[[2]]$source))) {
      n <- vapply(outs, function(o) sum(o$n[o$site_id == sid & o$source == src & o$status == "ok"]), numeric(1))
      tbl <- if (src %in% live_cfg$forecast_sources) fc else obs
      stored <- sum(tbl$site_id == sid & tbl$source == src)
      cat(sprintf("%s/%s: process rows %d + %d, stored %d\n", sid, src, n[1], n[2], stored))
      if (stored < max(n) || stored > sum(n)) lost <- c(lost, paste(sid, src))
    }
  }
  cat(sprintf("duplicate keys: forecasts %d, observations %d\n", dups_f[["forecasts"]], dups_f[["observations"]]))
  f_ok <- all(dups_f == 0) && length(lost) == 0
}
check("f", f_ok, "concurrent met_sync_live(): no duplicate keys, every process's rows present")

# ---- h. long paths (follow-up 1) --------------------------------------------------------

section("h. store paths")
long_form <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
longest <- max(nchar(long_form(list.files(root, recursive = TRUE, full.names = TRUE))))
cat(sprintf("scratch root: %d chars (long form %d); longest stored path: %d chars\n",
            nchar(root), nchar(long_form(root)), longest))
h_rows <- TRUE
for (sid in c("kat", "blax")) {
  for (src in live_cfg$forecast_sources) {
    s <- src_status(run1$value, sid, src)
    stored <- sum(fc_after1$site_id == sid & fc_after1$source == src)
    cat(sprintf("run 1 %s/%s: reported %d rows, read back %d\n", sid, src, s$n, stored))
    h_rows <- h_rows && nrow(s) == 1 && s$status == "ok" && stored == s$n
  }
}
too_long <- file.path(scratch, strrep("x", max(1, 262 - nchar(long_form(scratch)))))
res_h <- met_sync_live(read_sites_yaml(write_sites(too_long, file.path(scratch, "sites-too-long.yaml"))),
                       now = now_utc(), config = modifyList(live_cfg, list(store_root = too_long)))
h_refused <- all(res_h$status == "error") && all(grepl("too long", res_h$message)) &&
  length(list.files(too_long, pattern = "[.]parquet$", recursive = TRUE)) == 0
cat(sprintf("root of %d chars: statuses %s; %s\n", nchar(too_long), paste(res_h$status, collapse = "/"),
            substr(res_h$message[1], 1, 140)))
check("h", h_rows && h_refused && longest <= 259,
      sprintf("all run-1 rows read back (longest path %d chars); a %d-char root is refused, nothing written",
              longest, nchar(too_long)))

# ---- i. fail_on = "failed" (follow-up 2) --------------------------------------------------

section("i. fail_on = 'failed'")
child_i <- function(name, sites_path, obs_sources) {
  path <- file.path(scratch, paste0(name, ".R"))
  writeLines(c(
    paste("load_meteotidy <-", paste(deparse(load_meteotidy), collapse = "\n")),
    "load_meteotidy()",
    sprintf("cfg <- list(store_root = %s, obs_sources = %s, forecast_sources = character(0))",
            deparse(root), deparse(obs_sources)),
    sprintf("invisible(met_sync_live(read_sites_yaml(%s), config = cfg, fail_on = 'failed'))", deparse(sites_path))
  ), path)
  suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(path)),
                           stdout = FALSE, stderr = file.path(scratch, paste0(name, ".log"))))
}
st_stale <- child_i("fail_on_failed_stale", sites_path, "eagleio")
st_broken <- child_i("fail_on_failed_broken", broken_path, c("eagleio", "broken_obs"))
cat(sprintf("eagle.io only (blax stale): exit %s; with a broken source: exit %s\n", st_stale, st_broken))
cat(utils::tail(readLines(file.path(scratch, "fail_on_failed_broken.log")), 3), sep = "\n")
check("i", st_stale == 0 && st_broken != 0,
      sprintf("fail_on = 'failed': stale-only run exits %s, failed source exits %s", st_stale, st_broken))

# ---- j. BOM daily source (follow-up 3) ----------------------------------------------------

section("j. BOM daily forecast source")
aux_all <- read_table(root, "forecast_aux")
j_ok <- TRUE
for (sid in c("kat", "blax")) {
  bom <- fc_b[fc_b$site_id == sid & fc_b$source == "bom_forecast", ]
  loc <- unique(aux_all$value_text[aux_all$site_id == sid & aux_all$source == "bom_forecast" &
                                     aux_all$field == "location"])
  gh <- sites@sites[[match(sid, vapply(sites@sites, site_id, ""))]]@resolved$bom$geohash
  cat(sprintf("%s: models %s; location %s\n", sid, paste(sort(unique(bom$model)), collapse = ", "),
              paste(loc, collapse = " | ")))
  j_ok <- j_ok && "daily" %in% bom$model && !"daily_precis" %in% bom$model &&
    any(grepl(gh, loc, fixed = TRUE))
}
check("j", j_ok, "BOM daily rows come from the site geohash web API, labelled with the geohash")

# ---- k. no observation churn (follow-up 4) ------------------------------------------------

section("k. observation churn")
obs_k <- read_table(root, "observations")
cur <- obs_k[!obs_k$superseded, ]
old <- obs_k[obs_k$superseded, ]
m <- match(do.call(paste, old[obs_key]), do.call(paste, cur[obs_key]))
same <- !is.na(m) & mapply(function(a, b) isTRUE(all.equal(a, b)), old$value, cur$value[m]) &
  old$method == cur$method[m]
cat(sprintf("%d current rows, %d superseded, of which %d only differ in qc_flag (churn)\n",
            nrow(cur), nrow(old), sum(same)))
check("k", sum(same) == 0, sprintf("%d superseded rows with unchanged value and method", sum(same)))

# ---- l. compaction (follow-up 5) --------------------------------------------------------------

section("l. met_compact()")
n_files <- function(r) length(list.files(r, pattern = "[.]parquet$", recursive = TRUE))
reads <- function(r) {
  out <- list()
  for (s in sites@sites) {
    sid <- site_id(s)
    out[[paste0(sid, "_fc")]] <- nrow(met_forecast_archive(s))
    out[[paste0(sid, "_obs")]] <- nrow(meteoTidy:::store_read_obs(r, sid))
    out[[paste0(sid, "_qc")]] <- nrow(meteoTidy:::qc_log_read(r, sid))
  }
  unlist(out)
}
before_files <- n_files(root)
before_reads <- reads(root)
comp <- timed(met_compact(root))
print(as.data.frame(comp$value), row.names = FALSE)
after_reads <- reads(root)
cat(sprintf("parquet files %d -> %d in %.0f s; reads %s\n", before_files, n_files(root), comp$secs,
            if (identical(before_reads, after_reads)) "unchanged" else "CHANGED"))
check("l", n_files(root) < before_files && identical(before_reads, after_reads),
      sprintf("files %d -> %d, every read unchanged", before_files, n_files(root)))

# ---- m. no re-download of an archived run (follow-up 7) ----------------------------------------

section("m. skip archived runs")
m_ok <- TRUE
for (sid in c("kat", "blax")) {
  for (src in c("openmeteo", "om_ens")) {
    s <- src_status(run2$value, sid, src)
    cat(sprintf("run 2 %s/%s: %d new rows; %s\n", sid, src, s$n, s$message))
    # A new run published between the two syncs is legitimately downloaded.
    m_ok <- m_ok && nrow(s) == 1 && s$status == "ok" && grepl("already archived", s$message)
  }
}
check("m", m_ok, "second live run skipped every Open-Meteo run already archived")

# ---- n. email variables (follow-up 9) ---------------------------------------------------------

section("n. email variables")
email_vars <- c("weather_code", "is_day", "uv_index", "soil_moisture_0_to_1cm", "soil_moisture_1_to_3cm",
                "boundary_layer_height", "cape", "cloud_cover", "wind_direction_10m", "wind_gusts_10m",
                "wind_speed_80m", "direct_radiation", "diffuse_radiation", "shortwave_radiation",
                "surface_pressure")
n_ok <- TRUE
for (sid in c("kat", "blax")) {
  om <- fc_b[fc_b$site_id == sid & fc_b$source == "openmeteo" & !is.na(fc_b$value), ]
  missing <- setdiff(email_vars, om$variable)
  ens_models <- sort(unique(fc_b$model[fc_b$site_id == sid & fc_b$source == "om_ens"]))
  fdc <- aux_all[aux_all$site_id == sid & aux_all$field == "fire_danger_category", ]
  cat(sprintf("%s: missing %s; ensembles %s; fire_danger_category %s\n", sid,
              if (length(missing)) paste(missing, collapse = ", ") else "none",
              paste(ens_models, collapse = " + "),
              paste(utils::head(unique(fdc$value_text), 3), collapse = "/")))
  n_ok <- n_ok && length(missing) == 0 && all(c("ecmwf_ifs025", "icon_seamless") %in% ens_models) &&
    nrow(fdc) > 0
}
check("n", n_ok, "all email variables, ECMWF + ICON ensembles and BOM fire danger category archived")

# ---- o. read-back of every source (follow-up 11) ------------------------------------------------

section("o. read-back")
o_ok <- TRUE
for (s in sites@sites) {
  for (src in live_cfg$forecast_sources) {
    r <- tryCatch(met_forecast_archive(s, source = src), error = function(e) e)
    ok <- !inherits(r, "error") && nrow(r) > 0 &&
      all(abs(as.numeric(r$lead_time, units = "secs") -
                as.numeric(difftime(r$valid_time, r$issue_time, units = "secs"))) < 1, na.rm = TRUE)
    cat(sprintf("%s/%s: %s\n", site_id(s), src,
                if (inherits(r, "error")) conditionMessage(r) else sprintf("%d rows", nrow(r))))
    o_ok <- o_ok && ok
  }
  for (src in daily_cfg$obs_sources) {
    r <- tryCatch(met_record(s, from = now_utc() - 30 * 86400, to = now_utc()), error = function(e) e)
    ok <- !inherits(r, "error")
    cat(sprintf("%s/record (%s): %s\n", site_id(s), src,
                if (inherits(r, "error")) conditionMessage(r) else sprintf("%d rows", sum(r$source == src))))
    o_ok <- o_ok && ok
  }
}
check("o", o_ok, "every forecast source reads back with exact lead times; the record reads")

# ---- p. met_wide models and stat (follow-up 10) ---------------------------------------------------

section("p. met_wide() models and stat")
p_ok <- tryCatch({
  s <- sites@sites[[1]]
  w_mean <- met_wide(s, win, source = "om_ens", variables = "temperature_2m")
  w_p95 <- met_wide(s, win, source = "om_ens", variables = "temperature_2m", stat = "p95")
  w_om <- met_wide(s, win, source = "openmeteo")
  prov <- met_provenance(w_om)
  cat("openmeteo models per variable:", paste(prov$variable, prov$model, sep = "=", collapse = ", "), "\n")
  cat(sprintf("om_ens temperature_2m: mean %.1f, p95 %.1f degC (72 h average)\n",
              mean(w_mean$temperature_2m), mean(w_p95$temperature_2m)))
  all(w_p95$temperature_2m >= w_mean$temperature_2m) && met_provenance(w_p95)$stat == "p95" &&
    length(unique(stats::na.omit(prov$model))) >= 2
}, error = function(e) { cat("error:", conditionMessage(e), "\n"); FALSE })
check("p", p_ok, "per-variable models in met_wide(); ensemble p95 >= mean")

# ---- g. store size and growth ---------------------------------------------------------

section("g. store size and growth")
tbl_bytes <- function(r, ...) dir_bytes(file.path(r, ...))
fc_all <- read_table(root, "forecasts")
per_source <- do.call(rbind, lapply(c("openmeteo", "om_ens", "bom_forecast"), function(src) {
  d <- fc_all[fc_all$source == src, ]
  data.frame(source = src,
             bytes = tbl_bytes(root, "forecasts", paste0("source=", src)),
             issues = nrow(unique(d[c("site_id", "model", "issue_time")])),
             rows = nrow(d))
}))
# Issuances per day per site/model captured by an hourly sync: every
# Open-Meteo model here (ECMWF IFS, GFS, ICON; ECMWF and ICON ensembles)
# runs every 6 h (4/day) and is downloaded once per run; BOM re-issues its
# web-API forecasts many times a day, so an hourly sync captures up to
# 24/day for each of daily and hourly.
per_day <- c(openmeteo = 4, om_ens = 4, bom_forecast = 24)
per_source$bytes_per_issue <- per_source$bytes / per_source$issues
per_source$issues_per_day <- per_day[per_source$source] * c(openmeteo = 6, om_ens = 4, bom_forecast = 4)[per_source$source] # sites x models
per_source$mb_per_day <- per_source$bytes_per_issue * per_source$issues_per_day / 1024^2
print(per_source, row.names = FALSE)
other_bytes <- dir_bytes(root) - sum(per_source$bytes)
growth <- sum(per_source$mb_per_day) + (other_bytes / 1024^2) / 2 # obs/aux/history: ~ one daily run's worth per day
cat(sprintf("store after all runs: %s (forecasts %s, other tables %s)\n",
            mb(dir_bytes(root)), mb(sum(per_source$bytes)), mb(other_bytes)))
cat(sprintf("live run 1 wrote %s; live run 2 wrote %s; daily run wrote %s\n",
            mb(bytes_live1), mb(bytes_live2 - bytes_live1), mb(bytes_daily)))
cat(sprintf("estimated growth: ~%.0f MB/day (~%.1f GB/year) for hourly live + daily syncs of both sites, after met_compact()\n",
            growth, growth * 365 / 1024))
check("g", is.finite(growth) && growth > 0, sprintf("store %s; ~%.0f MB/day", mb(dir_bytes(root)), growth))

# ---- summary ---------------------------------------------------------------------------

section("summary")
print(results, row.names = FALSE, right = FALSE)
if (!all(results$ok)) quit(status = 1)
cat("ALL CHECKS PASSED\n")
