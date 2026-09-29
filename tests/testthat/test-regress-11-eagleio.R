# Regression (production review, problem 11): there was no eagle.io source, so
# the sites' own gauges (Katoomba, Blaxland) could not be ingested.
# Replays eagle.io responses recorded 2026-09-29. Blaxland's rain gauge has been
# offline since 24 Sep 2026 (0 records), which must surface as a visible
# `source_stale` failure -- not a silent empty success, and not a crash of the
# whole site sync.

kat_temp_node <- "64642e52fbaed638fdb04100"
kat_rain_node <- "64642e52fbaed638fdb04110"
blax_rain_node <- "647e9492dac9bf46aa1ffd0f"

eagle_window <- function() {
  list(from = as.POSIXct("2026-09-28 21:00:00", tz = "UTC"),
       to = as.POSIXct("2026-09-29 06:00:00", tz = "UTC"))
}

kat_nodes <- function() {
  list(precipitation = kat_rain_node, temperature_2m = kat_temp_node)
}

kat_routes <- function() {
  list(
    "nodes/64642e52fbaed638fdb04100/historic" = "eagleio/historic-kat-temperature.json",
    "nodes/64642e52fbaed638fdb04110/historic" = "eagleio/historic-kat-precipitation.json"
  )
}

blax_routes <- function(meta = "eagleio/node-blax-precipitation.json") {
  list(
    "nodes/647e9492dac9bf46aa1ffd0f/historic" = "eagleio/historic-blax-precipitation-offline.json",
    "nodes/647e9492dac9bf46aa1ffd0f" = meta
  )
}

describe("problem 11: source_eagleio constructor", {
  it("provides exactly the variables named in `nodes`", {
    a <- source_eagleio(kat_nodes())
    expect_true(S7::S7_inherits(a, met_adapter))
    expect_setequal(a@provides, c("precipitation", "temperature_2m"))
    b <- source_eagleio(c(precipitation = kat_rain_node))
    expect_equal(b@provides, "precipitation")
  })

  it("is exported", {
    expect_true("source_eagleio" %in% getNamespaceExports("meteoTidy"))
  })

  it("never shows the API key in format()/print() output", {
    withr::local_envvar(EAGLE_API_KEY = "test-key-not-real")
    a <- source_eagleio(kat_nodes())
    out <- paste(c(format(a), utils::capture.output(print(a))), collapse = "\n")
    expect_false(grepl("test-key-not-real", out, fixed = TRUE))
  })
})

describe("problem 11: fetching observations", {
  it("requests <base>/nodes/<id>/historic with ISO-8601 Z times and format=json", {
    withr::local_envvar(EAGLE_API_KEY = "test-key-not-real")
    site <- make_prod_site("kat")
    a <- source_eagleio(kat_nodes())
    cap <- new.env()
    with_routed_http(kat_routes(), {
      fetch(a, site, c("precipitation", "temperature_2m"), eagle_window(), now = prod_now())
    }, capture = cap)
    rain <- grep("nodes/64642e52fbaed638fdb04110/historic", cap$urls, value = TRUE)
    temp <- grep("nodes/64642e52fbaed638fdb04100/historic", cap$urls, value = TRUE)
    expect_length(rain, 1)
    expect_length(temp, 1)
    expect_true(startsWith(rain, "https://api.eagle.io/api/v1/nodes/"))
    expect_match(rain, "startTime=2026-09-28T21:00:00Z", fixed = TRUE)
    expect_match(rain, "endTime=2026-09-29T06:00:00Z", fixed = TRUE)
    expect_match(rain, "format=json", fixed = TRUE)
  })

  it("sends the key in X-Api-Key (read at fetch time) and never in the URL", {
    site <- make_prod_site("kat")
    a <- source_eagleio(kat_nodes()) # constructed BEFORE the key exists
    withr::local_envvar(EAGLE_API_KEY = "test-key-not-real")
    cap <- new.env()
    with_routed_http(kat_routes(), {
      fetch(a, site, "precipitation", eagle_window(), now = prod_now())
    }, capture = cap)
    expect_false(any(grepl("test-key-not-real", cap$urls, fixed = TRUE)))
    expect_gt(length(cap$headers), 0)
    for (h in cap$headers) {
      expect_equal(unlist(h)[["X-Api-Key"]], "test-key-not-real")
    }
  })

  it("returns all 37 Katoomba precipitation records with the key header on every request", {
    withr::local_envvar(EAGLE_API_KEY = "test-key-not-real")
    site <- make_prod_site("kat")
    a <- source_eagleio(kat_nodes())
    cap <- new.env()
    obs <- with_routed_http(kat_routes(), {
      fetch(a, site, "precipitation", eagle_window(), now = prod_now())
    }, capture = cap)
    expect_equal(sum(obs$variable == "precipitation"), 37)
    expect_equal(nrow(obs), 37)
    for (h in cap$headers) {
      expect_equal(unlist(h)[["X-Api-Key"]], "test-key-not-real")
    }
  })

  it("returns the canonical long observation tibble at native resolution, UTC times", {
    withr::local_envvar(EAGLE_API_KEY = "test-key-not-real")
    site <- make_prod_site("kat")
    a <- source_eagleio(kat_nodes())
    obs <- with_routed_http(kat_routes(), {
      fetch(a, site, c("precipitation", "temperature_2m"), eagle_window(), now = prod_now())
    })
    expect_s3_class(obs, "tbl_df")
    expect_true(all(c("site_id", "datetime_utc", "variable", "value", "source",
                      "method", "qc_flag") %in% names(obs)))
    expect_s3_class(obs$datetime_utc, "POSIXct")
    expect_equal(attr(obs$datetime_utc, "tzone"), "UTC")
    expect_type(obs$value, "double")
    expect_true(all(obs$site_id == "kat"))
    expect_true(all(obs$source == "eagleio"))
    expect_true(all(obs$method == "measured"))
    expect_true(all(obs$qc_flag == "ok"))
    expect_equal(sum(obs$variable == "precipitation"), 37)
    expect_equal(sum(obs$variable == "temperature_2m"), 19)
    expect_false(anyNA(obs$value))

    # Values equal the recorded ones; timestamps are true UTC (no tz shift).
    raw <- jsonlite::read_json(fixture_path("eagleio/historic-kat-temperature.json"))
    want_ts <- as.POSIXct(vapply(raw$data, function(d) d$ts, ""),
                          format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    want_v <- vapply(raw$data, function(d) as.numeric(d$f[["0"]]$v), 0)
    temp <- obs[obs$variable == "temperature_2m", ]
    temp <- temp[order(temp$datetime_utc), ]
    expect_equal(as.numeric(temp$datetime_utc), as.numeric(sort(want_ts)))
    expect_equal(temp$value, want_v[order(want_ts)])
    first <- temp[temp$datetime_utc == as.POSIXct("2026-09-29 00:00:00", tz = "UTC"), ]
    expect_equal(first$value, 11.5)

    # 10-minute native resolution: no aggregation.
    expect_equal(min(diff(as.numeric(temp$datetime_utc))), 600)
  })

  it("drops null values", {
    withr::local_envvar(EAGLE_API_KEY = "test-key-not-real")
    site <- make_prod_site("kat")
    a <- source_eagleio(c(precipitation = kat_rain_node))
    tmp <- withr::local_tempfile(fileext = ".json")
    writeLines(paste0(
      '{"header":{"columns":{"0":{"units":"mm"}}},"data":[',
      '{"ts":"2026-09-29T00:10:00.000Z","f":{"0":{"v":0.2}}},',
      '{"ts":"2026-09-29T00:20:00.000Z","f":{"0":{"v":null}}},',
      '{"ts":"2026-09-29T00:30:00.000Z","f":{"0":{"v":0}}}]}'
    ), tmp)
    obs <- with_routed_http(list("nodes/64642e52fbaed638fdb04110/historic" = function(url) tmp), {
      fetch(a, site, "precipitation", eagle_window(), now = prod_now())
    })
    expect_equal(nrow(obs), 2)
    expect_false(anyNA(obs$value))
  })

  it("ignores requested variables the adapter does not provide", {
    withr::local_envvar(EAGLE_API_KEY = "test-key-not-real")
    site <- make_prod_site("kat")
    a <- source_eagleio(c(precipitation = kat_rain_node))
    cap <- new.env()
    obs <- with_routed_http(kat_routes(), {
      fetch(a, site, c("precipitation", "temperature_2m"), eagle_window(), now = prod_now())
    }, capture = cap)
    expect_false(any(grepl(kat_temp_node, cap$urls, fixed = TRUE)))
    expect_setequal(unique(obs$variable), "precipitation")
  })
})

describe("problem 11: missing API key", {
  it("aborts secret_unresolved when the env var is unset, with no HTTP request", {
    withr::local_envvar(EAGLE_API_KEY = NA)
    site <- make_prod_site("kat")
    a <- source_eagleio(kat_nodes())
    cap <- new.env()
    expect_error(
      with_routed_http(kat_routes(), {
        fetch(a, site, "precipitation", eagle_window(), now = prod_now())
      }, capture = cap),
      class = "meteoTidy_error_secret_unresolved"
    )
    expect_length(cap$urls, 0)
  })

  it("aborts secret_unresolved when the env var is empty", {
    withr::local_envvar(EAGLE_API_KEY = "")
    site <- make_prod_site("kat")
    a <- source_eagleio(kat_nodes())
    cap <- new.env()
    expect_error(
      with_routed_http(kat_routes(), {
        fetch(a, site, "precipitation", eagle_window(), now = prod_now())
      }, capture = cap),
      class = "meteoTidy_error_secret_unresolved"
    )
    expect_length(cap$urls, 0)
  })

  it("reads the env var named by api_key_env", {
    withr::local_envvar(EAGLE_API_KEY = NA, MY_EAGLE_KEY = "other-key")
    site <- make_prod_site("kat")
    a <- source_eagleio(kat_nodes(), api_key_env = "MY_EAGLE_KEY")
    cap <- new.env()
    obs <- with_routed_http(kat_routes(), {
      fetch(a, site, "precipitation", eagle_window(), now = prod_now())
    }, capture = cap)
    expect_equal(nrow(obs), 37)
    expect_equal(unlist(cap$headers[[1]])[["X-Api-Key"]], "other-key")
  })
})

describe("problem 11: stale / offline station", {
  it("aborts source_stale naming the last-report date when every node is empty", {
    withr::local_envvar(EAGLE_API_KEY = "test-key-not-real")
    site <- make_prod_site("blax")
    a <- source_eagleio(c(precipitation = blax_rain_node))
    cap <- new.env()
    err <- expect_error(
      with_routed_http(blax_routes(), {
        fetch(a, site, "precipitation", eagle_window(), now = prod_now())
      }, capture = cap),
      class = "meteoTidy_error_source_stale"
    )
    expect_match(conditionMessage(err), "2026-09-24", fixed = TRUE)
    meta <- grep("nodes/647e9492dac9bf46aa1ffd0f(\\?|$)", cap$urls, value = TRUE)
    expect_gte(length(meta), 1)
    expect_false(any(grepl("test-key-not-real", cap$urls, fixed = TRUE)))
    for (h in cap$headers) {
      expect_equal(unlist(h)[["X-Api-Key"]], "test-key-not-real")
    }
  })

  it("still aborts source_stale when the metadata lookup itself fails", {
    withr::local_envvar(EAGLE_API_KEY = "test-key-not-real")
    site <- make_prod_site("blax")
    a <- source_eagleio(c(precipitation = blax_rain_node))
    expect_error(
      with_routed_http(blax_routes(meta = 500L), {
        fetch(a, site, "precipitation", eagle_window(), now = prod_now())
      }),
      class = "meteoTidy_error_source_stale"
    )
  })

  it("returns the data it has, with an eagleio_node_empty warning, when only some nodes are empty", {
    withr::local_envvar(EAGLE_API_KEY = "test-key-not-real")
    site <- make_prod_site("blax")
    a <- source_eagleio(list(precipitation = blax_rain_node, temperature_2m = kat_temp_node))
    routes <- c(blax_routes(), list(
      "nodes/64642e52fbaed638fdb04100/historic" = "eagleio/historic-kat-temperature.json"
    ))
    w <- NULL
    obs <- withCallingHandlers(
      with_routed_http(routes, {
        fetch(a, site, c("precipitation", "temperature_2m"), eagle_window(), now = prod_now())
      }),
      meteoTidy_warning_eagleio_node_empty = function(cnd) {
        w <<- c(w, conditionMessage(cnd))
        invokeRestart("muffleWarning")
      }
    )
    expect_equal(sum(obs$variable == "temperature_2m"), 19)
    expect_false("precipitation" %in% obs$variable)
    expect_true(any(grepl("precipitation", w, fixed = TRUE)))
  })
})

describe("problem 11: site configuration", {
  it("builds a source_eagleio adapter from a site's eagleio source block", {
    site <- make_prod_site("kat", sources = list(
      eagleio = list(adapter = "eagleio", nodes = kat_nodes(), api_key_env = "EAGLE_API_KEY")
    ))
    adapters <- adapters_for_site(site)
    a <- adapters[["eagleio"]]
    expect_true(S7::S7_inherits(a, source_eagleio))
    expect_setequal(a@provides, c("precipitation", "temperature_2m"))
  })

  it(".acquire_obs() fetches through the configured adapter", {
    withr::local_envvar(EAGLE_API_KEY = "test-key-not-real")
    site <- make_prod_site("kat", sources = list(
      eagleio = list(adapter = "eagleio", nodes = kat_nodes(), api_key_env = "EAGLE_API_KEY")
    ))
    obs <- with_routed_http(kat_routes(), {
      .acquire_obs("eagleio", site, eagle_window(), now = prod_now())
    })
    expect_equal(sum(obs$variable == "precipitation"), 37)
    expect_equal(sum(obs$variable == "temperature_2m"), 19)
  })
})

describe("problem 11: met_sync_daily integration", {
  eagle_site <- function(id, nodes, root) {
    make_prod_site(id, store_root = root, sources = list(
      eagleio = list(adapter = "eagleio", nodes = nodes, api_key_env = "EAGLE_API_KEY")
    ))
  }
  eagle_cfg <- function(root) {
    list(store_root = root, obs_sources = c("eagleio"), forecast_sources = character(0))
  }

  it("does not throw for the offline Blaxland site and reports the stale source", {
    withr::local_envvar(EAGLE_API_KEY = "test-key-not-real")
    root <- withr::local_tempdir()
    site <- eagle_site("blax", list(precipitation = blax_rain_node), root)
    res <- NULL
    expect_no_error(
      res <- with_routed_http(blax_routes(), {
        met_sync_daily(site, now = prod_now(), config = eagle_cfg(root))
      })
    )
    expect_s3_class(res, "tbl_df")
    expect_true(all(c("site_id", "status", "message") %in% names(res)))
    bad <- res[res$status != "ok" & grepl("eagleio", res$message, ignore.case = TRUE), ]
    expect_gte(nrow(bad), 1)
    expect_true(any(grepl("stale", bad$message, ignore.case = TRUE)))
  })

  it("stores Katoomba's eagle.io observations, idempotently", {
    withr::local_envvar(EAGLE_API_KEY = "test-key-not-real")
    root <- withr::local_tempdir()
    site <- eagle_site("kat", kat_nodes(), root)
    with_routed_http(kat_routes(), {
      met_sync_daily(site, now = prod_now(), config = eagle_cfg(root))
    })
    stored <- store_read_obs(root, "kat")
    ev <- stored[stored$source == "eagleio", ]
    expect_gte(sum(ev$variable == "precipitation"), 37)
    expect_gte(sum(ev$variable == "temperature_2m"), 19)
    n1 <- nrow(stored)

    with_routed_http(kat_routes(), {
      met_sync_daily(site, now = prod_now(), config = eagle_cfg(root))
    })
    expect_equal(nrow(store_read_obs(root, "kat")), n1)
  })
})
