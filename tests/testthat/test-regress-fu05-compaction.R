# Follow-up review, item 5: unbounded small files.
#
# qc_log and obs_transport gain one parquet file per site per sync run and
# were never compacted; store_compact() was internal and only knew the three
# hive-partitioned tables. met_compact() is exported, covers every
# append-only table, keeps what readers see identical, and holds the store
# lock (a real second process holding the lock makes it wait or time out).

qc_rows <- function(t0, n = 24, outcome = "pass") {
  tibble::tibble(site_id = "kat", datetime_utc = t0 + (seq_len(n) - 1) * 3600,
                 variable = "temperature_2m", rule = "range", outcome = outcome, detail = NA_character_)
}

transport_rows <- function(t0, n = 24) {
  tibble::tibble(site_id = "kat", datetime_utc = t0 + (seq_len(n) - 1) * 3600,
                 variable = "temperature_2m", source = "bom_obs", transport = "ftp_feeds")
}

n_files <- function(root, table) {
  length(list.files(file.path(root, table), pattern = "\\.parquet$", recursive = TRUE))
}

describe("item 5: compaction of append-only tables", {
  it("is exported", {
    expect_true("met_compact" %in% getNamespaceExports("meteoTidy"))
  })

  it("compacts qc_log and obs_transport to one file per site, reads unchanged", {
    root <- withr::local_tempdir()
    t0 <- as.POSIXct("2026-09-28 00:00:00", tz = "UTC")
    # 24 hourly syncs, each re-logging an overlapping 6 h window.
    for (h in 0:23) {
      now <- t0 + h * 3600
      qc_log_write(root, qc_rows(now - 5 * 3600, 6, if (h %% 2) "pass" else "suspect"), now = now)
      obs_transport_write(root, transport_rows(now - 5 * 3600, 6), now = now)
    }
    expect_equal(n_files(root, "qc_log"), 24)
    expect_equal(n_files(root, "obs_transport"), 24)
    qc_before <- qc_log_read(root, "kat")
    tr_before <- obs_transport_read(root, "kat", t0 - 86400, t0 + 86400)

    met_compact(root)

    expect_equal(n_files(root, "qc_log"), 1)
    expect_equal(n_files(root, "obs_transport"), 1)
    expect_equal(qc_log_read(root, "kat"), qc_before)
    expect_equal(obs_transport_read(root, "kat", t0 - 86400, t0 + 86400), tr_before)
    # Superseded log rows (older verdicts for the same key) are dropped.
    raw <- arrow::read_parquet(list.files(file.path(root, "qc_log"), "\\.parquet$",
                                          recursive = TRUE, full.names = TRUE))
    expect_equal(nrow(raw), nrow(qc_before))
  })

  it("still compacts the observation and forecast tables", {
    root <- withr::local_tempdir()
    base <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
    for (i in 0:3) store_write_obs(root, new_obs(make_obs(n = 1, start = base + i * 3600)))
    expect_gt(n_files(root, "observations"), 1)
    met_compact(root, tables = "observations")
    expect_equal(n_files(root, "observations"), 1)
  })

  it("waits for, and respects, another process's store lock", {
    skip_on_cran()
    skip_if_not_installed("callr")
    root <- normalizePath(withr::local_tempdir(), winslash = "/")
    t0 <- as.POSIXct("2026-09-28 00:00:00", tz = "UTC")
    for (h in 0:2) qc_log_write(root, qc_rows(t0 + h * 3600, 2), now = t0 + h * 3600)
    lock <- .store_lock_path(root)
    ready <- file.path(root, "holding")
    holder <- callr::r_bg(function(lock, ready) {
      l <- filelock::lock(lock, exclusive = TRUE)
      file.create(ready)
      Sys.sleep(4)
      filelock::unlock(l)
    }, list(lock, ready))
    on.exit(holder$kill(), add = TRUE)
    for (i in 1:100) if (!file.exists(ready)) Sys.sleep(0.1)
    expect_true(file.exists(ready))

    expect_error(met_compact(root, lock_timeout = 0.5), class = "meteoTidy_error_store_locked")
    expect_equal(n_files(root, "qc_log"), 3)
    met_compact(root, lock_timeout = 30)
    expect_equal(n_files(root, "qc_log"), 1)
  })
})
