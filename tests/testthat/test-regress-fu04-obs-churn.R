# Follow-up review, item 4: BOM observation churn.
#
# Every hourly sync re-fetches the same BOM observations with the adapter's
# raw qc_flag "ok"; QC had since flagged some of them "suspect". The store
# compared value, method AND qc_flag, so the identical readings were
# "superseded" on every run: 100+ junk rows per hour and a rewrite of the
# whole year partition. A re-fetch must compare value and method only.
# Replayed with the recorded BOM 72-h observation JSON for Mount Boyce.

fu04_routes <- function() {
  list("reg\\.bom\\.gov\\.au/fwo/IDN60901/IDN60901\\.94743\\.json" = "bom/obs72h-IDN60901-94743.json")
}

fu04_site <- function(root) {
  site <- make_prod_site("kat", store_root = root, sources = list(
    bom_obs = list(adapter = "bom_obs")
  ))
  site_set_resolved(site, c("bom", "wmo"), "94743")
}

raw_obs <- function(root) {
  as.data.frame(dplyr::collect(arrow::open_dataset(file.path(root, "observations"),
                                                   partitioning = arrow::hive_partition())))
}

describe("item 4: re-fetching unchanged observations", {
  it("does not supersede rows whose only difference is the QC flag", {
    root <- withr::local_tempdir()
    cfg <- list(store_root = root, obs_sources = "bom_obs", forecast_sources = character(0))
    site <- fu04_site(root)
    now1 <- prod_now()
    with_routed_http(fu04_routes(), met_sync_live(site, now = now1, config = cfg))

    # QC has flagged some of the stored readings (as the spatial/climatology
    # rules do in production).
    cur <- store_read_obs(root, "kat")
    flag <- cur[cur$variable == "temperature_2m", , drop = FALSE][1:5, ]
    flag$qc_flag <- "suspect"
    store_write_obs(root, flag, now = now1 + 60, mode = "supersede")
    n_before <- nrow(raw_obs(root))
    files_before <- list.files(file.path(root, "observations"), recursive = TRUE)

    # The next hourly sync fetches the very same readings, flagged "ok".
    status <- with_routed_http(fu04_routes(),
                               met_sync_live(site, now = now1 + 3600, config = cfg))
    expect_equal(status$status, "ok")

    raw <- raw_obs(root)
    expect_equal(nrow(raw), n_before)
    expect_equal(list.files(file.path(root, "observations"), recursive = TRUE), files_before)
    # QC's verdict is kept.
    kept <- store_read_obs(root, "kat")
    kept <- kept[match(paste(flag$datetime_utc, flag$variable), paste(kept$datetime_utc, kept$variable)), ]
    expect_true(all(kept$qc_flag == "suspect"))
  })

  it("still records a genuine revision of the value", {
    root <- withr::local_tempdir()
    t <- as.POSIXct("2026-09-29 06:00:00", tz = "UTC")
    obs <- tibble::tibble(site_id = "kat", datetime_utc = t, variable = "temperature_2m",
                          value = 12.3, source = "bom_obs", method = "measured", qc_flag = "ok")
    .sync_write_obs(root, obs, now = t)
    obs$value <- 12.4
    .sync_write_obs(root, obs, now = t + 3600)
    all_rows <- store_read_obs(root, "kat", include_superseded = TRUE)
    expect_equal(nrow(all_rows), 2L)
    expect_equal(store_read_obs(root, "kat")$value, 12.4)
  })

  it("QC flag changes are still written by qc_run()'s path", {
    root <- withr::local_tempdir()
    t <- as.POSIXct("2026-09-29 06:00:00", tz = "UTC")
    obs <- tibble::tibble(site_id = "kat", datetime_utc = t, variable = "temperature_2m",
                          value = 12.3, source = "bom_obs", method = "measured", qc_flag = "ok")
    store_write_obs(root, obs, now = t, mode = "supersede")
    obs$qc_flag <- "suspect"
    store_write_obs(root, obs, now = t + 60, mode = "supersede")
    expect_equal(store_read_obs(root, "kat")$qc_flag, "suspect")
  })
})
