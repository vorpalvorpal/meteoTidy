# Follow-up review, item 9 (metadata on S3): Open-Meteo stopped serving model metadata.
#
# `https://api.open-meteo.com/data/<name>/static/meta.json` (and the
# ensemble-api host) now return 404 for every model (checked 2026-11-15), so
# every named-model sync failed with `openmeteo_run_unknown`. The same JSON
# is served by the S3 mirror, https://openmeteo.s3.amazonaws.com/data/...;
# a commercial key still uses its customer- host.
#
# S3 publishes a run up to ~30 min before the API hosts serve it. A sync in
# that gap would stamp the previous run's data with the new run's time (and
# the run-already-stored check would then skip the real run). So when the
# metadata came from S3 and `last_run_availability_time` is less than the
# grace period old (default 3600 s; option meteoTidy.openmeteo_s3_grace_secs,
# 0 disables) the model is not downloaded this time: not an error, the
# adapter reports it in attr(, "run_pending") and the archive status notes
# "waiting for Open-Meteo API: ...".

s3m_s3_ifs <- "openmeteo\\.s3\\.amazonaws\\.com/data/ecmwf_ifs025/static/meta\\.json"
s3m_s3_ens <- "openmeteo\\.s3\\.amazonaws\\.com/data/ecmwf_ifs025_ensemble/static/meta\\.json"
s3m_s3_icon <- "openmeteo\\.s3\\.amazonaws\\.com/data/dwd_icon/static/meta\\.json"

# Metadata for a run initialised at 06:00 UTC on prod_now()'s day, made
# available `age` seconds before `now` (avail = FALSE drops the field).
s3m_meta <- function(age, init = 1790661600, avail = TRUE, valid_hours = 360,
                     now = prod_now()) {
  meta <- list(
    data_end_time = init + valid_hours * 3600,
    last_run_initialisation_time = init,
    last_run_modification_time = as.numeric(prod_now()) - 100,
    temporal_resolution_seconds = 3600,
    update_interval_seconds = 21600
  )
  if (avail) {
    meta$last_run_availability_time <- as.numeric(now) - age
  }
  meta
}

s3m_fetch <- function(adapter, routes, capture = new.env(), now = prod_now(),
                       site = make_prod_site("kat")) {
  with_routed_http(routes, {
    fetch_forecast(adapter, site, "temperature_2m",
                   list(from = now - 3 * 86400, to = now), now = now)
  }, capture = capture)
}

# with_routed_http() takes the FIRST matching route, so an override must go
# in front of openmeteo_routes(), whose S3 metadata routes would shadow it.
s3m_override <- function(routes, rx, target) {
  c(stats::setNames(list(target), rx), routes)
}

s3m_routes <- function(rx, meta) {
  s3m_override(openmeteo_routes(), rx, function(url) meta)
}

data_calls <- function(cap, what = "/v1/forecast") sum(grepl(what, cap$urls, fixed = TRUE))

describe("item 9 (metadata on S3): metadata URL", {
  it("uses the S3 mirror without a key, for forecast and ensemble", {
    expect_equal(.openmeteo_meta_url("forecast", "ecmwf_ifs025"),
                 "https://openmeteo.s3.amazonaws.com/data/ecmwf_ifs025/static/meta.json")
    expect_equal(.openmeteo_meta_url("forecast", "gfs_global"),
                 "https://openmeteo.s3.amazonaws.com/data/ncep_gfs025/static/meta.json")
    expect_equal(.openmeteo_meta_url("forecast", "icon_global"),
                 "https://openmeteo.s3.amazonaws.com/data/dwd_icon/static/meta.json")
    expect_equal(.openmeteo_meta_url("ensemble", "ecmwf_ifs025"),
                 "https://openmeteo.s3.amazonaws.com/data/ecmwf_ifs025_ensemble/static/meta.json")
    expect_equal(.openmeteo_meta_url("ensemble", "icon_seamless"),
                 "https://openmeteo.s3.amazonaws.com/data/dwd_icon_eps/static/meta.json")
  })

  it("keeps the customer host and key when a key is set", {
    expect_equal(
      .openmeteo_meta_url("forecast", "ecmwf_ifs025", api_key = "k-1"),
      "https://customer-api.open-meteo.com/data/ecmwf_ifs025/static/meta.json?apikey=k-1"
    )
    expect_equal(
      .openmeteo_meta_url("ensemble", "ecmwf_ifs025", api_key = "k-1"),
      "https://customer-ensemble-api.open-meteo.com/data/ecmwf_ifs025_ensemble/static/meta.json?apikey=k-1"
    )
  })

  it("never asks the free API hosts for metadata when fetching", {
    cap <- new.env()
    fc <- s3m_fetch(source_openmeteo("forecast", models = c("ecmwf_ifs025", "icon_global")),
                     openmeteo_routes(), capture = cap)
    fce <- s3m_fetch(source_openmeteo("ensemble", models = "ecmwf_ifs025"),
                      openmeteo_routes(), capture = cap)
    meta_urls <- grep("/static/meta.json", cap$urls, value = TRUE)
    expect_gt(length(meta_urls), 0)
    expect_true(all(grepl("^https://openmeteo\\.s3\\.amazonaws\\.com/", meta_urls)))
    expect_gt(nrow(fc), 0)
    expect_gt(nrow(fce), 0)
  })
})

describe("item 9 (metadata on S3): grace period for S3 metadata", {
  adapter <- function() source_openmeteo("forecast", models = "ecmwf_ifs025")

  it("does not download a run published to S3 less than an hour ago", {
    cap <- new.env()
    fc <- s3m_fetch(adapter(), s3m_routes(s3m_s3_ifs, s3m_meta(age = 600)), capture = cap)
    expect_equal(data_calls(cap), 0)
    expect_equal(nrow(fc), 0)
    expect_equal(attr(fc, "run_pending"), "ecmwf_ifs025 run 2026-09-29 06:00 UTC")
    expect_null(attr(fc, "already_stored"))
  })

  it("is not an error and raises no model-failed warning", {
    expect_no_condition(
      s3m_fetch(adapter(), s3m_routes(s3m_s3_ifs, s3m_meta(age = 600))),
      class = "meteoTidy_warning_openmeteo_model_failed"
    )
    expect_no_error(
      s3m_fetch(adapter(), s3m_routes(s3m_s3_ifs, s3m_meta(age = 600)))
    )
  })

  it("applies per model from .openmeteo_fetch_one_model()", {
    out <- with_routed_http(s3m_routes(s3m_s3_ifs, s3m_meta(age = 600)), {
      .openmeteo_fetch_one_model(adapter(), make_prod_site("kat"), "temperature_2m",
                                 list(from = prod_now() - 86400, to = prod_now()),
                                 prod_now(), "forecast", "ecmwf_ifs025", NULL, 16)
    })
    expect_equal(nrow(out), 0)
    expect_equal(attr(out, "run_pending"), "ecmwf_ifs025 run 2026-09-29 06:00 UTC")
  })

  it("fetches and stamps the run once it is at least an hour old", {
    cap <- new.env()
    fc <- s3m_fetch(adapter(), s3m_routes(s3m_s3_ifs, s3m_meta(age = 3601)), capture = cap)
    expect_equal(data_calls(cap), 1)
    expect_gt(nrow(fc), 0)
    expect_equal(unique(fc$issue_time), as.POSIXct(1790661600, origin = "1970-01-01", tz = "UTC"))
    expect_null(attr(fc, "run_pending"))
  })

  it("applies the grace period to ensembles too", {
    cap <- new.env()
    fc <- s3m_fetch(source_openmeteo("ensemble", models = "ecmwf_ifs025"),
                     s3m_routes(s3m_s3_ens, s3m_meta(age = 1200)), capture = cap)
    expect_equal(data_calls(cap, "/v1/ensemble"), 0)
    expect_equal(nrow(fc), 0)
    expect_match(attr(fc, "run_pending"), "^ecmwf_ifs025 run 2026-09-29 06:00 UTC$")
  })

  it("can be lengthened with the meteoTidy.openmeteo_s3_grace_secs option", {
    withr::local_options(meteoTidy.openmeteo_s3_grace_secs = 6 * 3600)
    cap <- new.env()
    fc <- s3m_fetch(adapter(), s3m_routes(s3m_s3_ifs, s3m_meta(age = 2 * 3600)), capture = cap)
    expect_equal(data_calls(cap), 0)
    expect_false(is.null(attr(fc, "run_pending")))
  })

  it("can be shortened with the option", {
    withr::local_options(meteoTidy.openmeteo_s3_grace_secs = 300)
    cap <- new.env()
    fc <- s3m_fetch(adapter(), s3m_routes(s3m_s3_ifs, s3m_meta(age = 600)), capture = cap)
    expect_equal(data_calls(cap), 1)
    expect_gt(nrow(fc), 0)
  })

  it("is disabled by a grace period of 0", {
    withr::local_options(meteoTidy.openmeteo_s3_grace_secs = 0)
    cap <- new.env()
    fc <- s3m_fetch(adapter(), s3m_routes(s3m_s3_ifs, s3m_meta(age = 10)), capture = cap)
    expect_equal(data_calls(cap), 1)
    expect_gt(nrow(fc), 0)
    expect_null(attr(fc, "run_pending"))
  })

  it("applies no grace when last_run_availability_time is missing", {
    cap <- new.env()
    fc <- s3m_fetch(adapter(), s3m_routes(s3m_s3_ifs, s3m_meta(age = 0, avail = FALSE)),
                     capture = cap)
    expect_equal(data_calls(cap), 1)
    expect_gt(nrow(fc), 0)
  })

  it("applies no grace when last_run_availability_time is not numeric", {
    meta <- s3m_meta(age = 0)
    meta$last_run_availability_time <- "soon"
    cap <- new.env()
    fc <- s3m_fetch(adapter(), s3m_routes(s3m_s3_ifs, meta), capture = cap)
    expect_equal(data_calls(cap), 1)
    expect_gt(nrow(fc), 0)
  })

  it("applies no grace to a commercial key's customer host", {
    withr::local_envvar(FU09_S3M_OM_KEY = "k-9f2e1d")
    routes <- c(list("customer-api.*/data/ecmwf_ifs025/static/meta\\.json" =
                       function(url) s3m_meta(age = 10),
                     "customer-api\\.open-meteo\\.com/v1/forecast" =
                       "openmeteo/forecast-ecmwf-ifs025-undefined.json"),
                openmeteo_routes())
    cap <- new.env()
    fc <- s3m_fetch(source_openmeteo("forecast", models = "ecmwf_ifs025",
                                      api_key_env = "FU09_S3M_OM_KEY"), routes, capture = cap)
    expect_equal(data_calls(cap), 1)
    expect_gt(nrow(fc), 0)
    expect_null(attr(fc, "run_pending"))
    expect_false(any(grepl("openmeteo.s3.amazonaws.com", cap$urls, fixed = TRUE)))
  })

  it("still fetches the other models of the same adapter", {
    routes <- s3m_routes(s3m_s3_ifs, s3m_meta(age = 600))  # ifs fresh; icon recorded (old)
    cap <- new.env()
    fc <- s3m_fetch(source_openmeteo("forecast", models = c("ecmwf_ifs025", "icon_global")),
                     routes, capture = cap)
    expect_equal(data_calls(cap), 1)
    expect_true(any(grepl("models=icon_global", cap$urls)))
    expect_false(any(grepl("models=ecmwf_ifs025", cap$urls[grepl("/v1/forecast", cap$urls)])))
    expect_equal(unique(fc$model), "icon_global")
    expect_equal(attr(fc, "run_pending"), "ecmwf_ifs025 run 2026-09-29 06:00 UTC")
  })

  it("collects one run_pending string per pending model", {
    routes <- s3m_routes(s3m_s3_ifs, s3m_meta(age = 600))
    routes <- s3m_override(routes, s3m_s3_icon, function(url) s3m_meta(age = 900))
    cap <- new.env()
    fc <- s3m_fetch(source_openmeteo("forecast", models = c("ecmwf_ifs025", "icon_global")),
                     routes, capture = cap)
    expect_equal(data_calls(cap), 0)
    expect_setequal(attr(fc, "run_pending"),
                    c("ecmwf_ifs025 run 2026-09-29 06:00 UTC", "icon_global run 2026-09-29 06:00 UTC"))
  })

  it("still aborts with openmeteo_run_unknown when S3 metadata is unavailable", {
    routes <- s3m_override(openmeteo_routes(), s3m_s3_ifs, 404)
    err <- tryCatch(s3m_fetch(adapter(), routes), error = identity)
    expect_s3_class(err, "meteoTidy_error_openmeteo_run_unknown")
  })

  it("requests no metadata for best_match and is unaffected by the grace period", {
    cap <- new.env()
    fc <- s3m_fetch(source_openmeteo("forecast", models = "best_match"), openmeteo_routes(),
                     capture = cap)
    expect_false(any(grepl("static/meta.json", cap$urls)))
    expect_equal(data_calls(cap), 1)
    expect_gt(nrow(fc), 0)
  })
})

describe("item 9 (metadata on S3): archive status", {
  s3m_sync <- function(root, routes, now = prod_now()) {
    site <- make_prod_site("kat", store_root = root, sources = list(
      om = list(adapter = "openmeteo", product = "forecast", models = c("ecmwf_ifs025", "icon_global"),
                provides = "temperature_2m")
    ))
    cfg <- list(store_root = root, obs_sources = character(0), forecast_sources = "om")
    suppressMessages(with_routed_http(routes, met_sync_live(site, now = now, config = cfg)))
  }

  it("notes the pending run, does not fail, and archives the other model", {
    root <- withr::local_tempdir()
    st <- s3m_sync(root, s3m_routes(s3m_s3_ifs, s3m_meta(age = 600)))
    expect_equal(st$status, "ok")
    msg <- st$sources[[1]]$message
    expect_match(msg, "waiting for Open-Meteo API: ecmwf_ifs025 run 2026-09-29 06:00 UTC", fixed = TRUE)
    expect_equal(unique(store_read_forecast(root, "kat")$model), "icon_global")
  })

  it("reports pending and already-archived runs side by side", {
    root <- withr::local_tempdir()
    s3m_sync(root, openmeteo_routes())  # both models archived
    routes <- s3m_routes(s3m_s3_ifs, s3m_meta(age = 600, init = 1790661600 + 6 * 3600,
                                                    now = prod_now() + 6 * 3600))
    st <- s3m_sync(root, routes, now = prod_now() + 6 * 3600)
    expect_equal(st$status, "ok")
    msg <- st$sources[[1]]$message
    expect_match(msg, "already archived: icon_global", fixed = TRUE)
    expect_match(msg, "waiting for Open-Meteo API: ecmwf_ifs025 run 2026-09-29 12:00 UTC", fixed = TRUE)
  })

  it("archives the run on the next sync once it is past the grace period", {
    root <- withr::local_tempdir()
    s3m_sync(root, s3m_routes(s3m_s3_ifs, s3m_meta(age = 600)))
    later <- prod_now() + 3600
    # The same run, published 600 s before the first sync, seen an hour later.
    meta <- s3m_meta(age = 600 + 3600, now = later)
    st <- s3m_sync(root, s3m_routes(s3m_s3_ifs, meta), now = later)
    expect_equal(st$status, "ok")
    fc <- store_read_forecast(root, "kat")
    expect_setequal(unique(fc$model), c("ecmwf_ifs025", "icon_global"))
    expect_equal(unique(fc$issue_time[fc$model == "ecmwf_ifs025"]),
                 as.POSIXct(1790661600, origin = "1970-01-01", tz = "UTC"))
  })
})
