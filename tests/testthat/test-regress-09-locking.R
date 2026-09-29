# Regression (production review, problem 9): two concurrent syncs on one
# store_root wrote 2,208 duplicate forecast rows -- the dedup in
# .write_forecast_like() is read-then-write with no lock, so two writers both
# see "not yet stored" and both append. .atomic_rewrite_partition() can also
# drop rows between writers. Reproduced here with two real R processes
# hammering the same partition, released together by a start barrier.

# Load meteoTidy in a child process: the source tree under devtools::test(),
# the installed package under R CMD check.
.child_load_expr <- function() {
  pkg <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(pkg, "DESCRIPTION"))) {
    bquote(suppressMessages(pkgload::load_all(.(pkg), quiet = TRUE, helpers = FALSE)))
  } else {
    quote(suppressMessages(library(meteoTidy)))
  }
}

.spawn_writer <- function(root, go_file, worker, n_batches) {
  load_expr <- .child_load_expr()
  callr::r_bg(function(root, go_file, worker, n_batches, load_expr) {
    eval(load_expr)
    ns <- asNamespace("meteoTidy")
    issue <- as.POSIXct("2026-09-29 06:00:00", tz = "UTC")
    while (!file.exists(go_file)) Sys.sleep(0.01)
    for (b in seq_len(n_batches)) {
      # Every batch overlaps the other worker's batches (shared issuance) and
      # adds one row unique to this worker + batch.
      valid <- issue + 3600 * seq_len(24)
      shared <- tibble::tibble(
        site_id = "kat", source = "openmeteo", model = "best_match",
        issue_time = issue, valid_time = valid,
        lead_time = as.difftime(as.numeric(valid - issue, units = "hours"), units = "hours"),
        member = NA_integer_, stat = NA_character_,
        variable = "temperature_2m", value = 1
      )
      own_valid <- issue + 3600 * (1000 + worker * 100 + b)
      own <- shared[1, ]
      own$valid_time <- own_valid
      own$lead_time <- as.difftime(as.numeric(own_valid - issue, units = "hours"), units = "hours")
      ns$store_write_forecast(root, rbind(shared, own))
      ns$store_write_obs(root, tibble::tibble(
        site_id = "kat", datetime_utc = issue + 600 * (worker * 100 + b),
        variable = "temperature_2m", value = as.double(b), source = "eagleio",
        method = "measured", qc_flag = "ok"
      ), mode = "supersede")
    }
    TRUE
  }, args = list(root = root, go_file = go_file, worker = worker,
                 n_batches = n_batches, load_expr = load_expr))
}

describe("concurrent writers on one store_root", {
  it("store no duplicate and lose no rows", {
    skip_on_cran()
    skip_if_not_installed("callr")
    skip_if_not_installed("pkgload")
    root <- local_store()
    go <- file.path(root, "GO")
    n <- 15
    p1 <- .spawn_writer(root, go, 1L, n)
    p2 <- .spawn_writer(root, go, 2L, n)
    # Give both children time to load, then release them together.
    Sys.sleep(8)
    file.create(go)
    p1$wait(120000)
    p2$wait(120000)
    expect_true(p1$get_result())
    expect_true(p2$get_result())

    fc <- store_read_forecast(root, "kat")
    key <- paste(fc$issue_time, fc$valid_time, fc$variable)
    expect_equal(anyDuplicated(key), 0L)
    # 24 shared + one per (worker, batch)
    expect_equal(nrow(fc), 24L + 2L * n)

    obs <- store_read_obs(root, "kat")
    expect_equal(anyDuplicated(paste(obs$datetime_utc, obs$variable)), 0L)
    expect_equal(nrow(obs), 2L * n)
  })
})

describe("store lock", {
  it("times out with a classed error when another process holds it", {
    skip_on_cran()
    skip_if_not_installed("callr")
    root <- local_store()
    lock_path <- .store_lock_path(root)
    holder <- callr::r_bg(function(path) {
      l <- filelock::lock(path, exclusive = TRUE)
      Sys.sleep(20)
      filelock::unlock(l)
    }, args = list(path = lock_path))
    on.exit(holder$kill(), add = TRUE)
    # Wait until the child actually holds the lock.
    deadline <- Sys.time() + 15
    repeat {
      probe <- filelock::lock(lock_path, exclusive = TRUE, timeout = 0)
      if (is.null(probe) || Sys.time() > deadline) break
      filelock::unlock(probe)
      Sys.sleep(0.1)
    }
    expect_error(
      with_store_lock(root, 1, timeout = 0.5),
      class = "meteoTidy_error_store_locked"
    )
  })

  it("is re-entrant within one process", {
    root <- local_store()
    out <- with_store_lock(root, with_store_lock(root, 42, timeout = 1), timeout = 1)
    expect_equal(out, 42)
  })
})

describe("reading a store that already holds duplicates", {
  it("returns each forecast key once instead of aborting", {
    root <- local_store()
    issue <- as.POSIXct("2026-09-29 06:00:00", tz = "UTC")
    fc <- tibble::tibble(
      site_id = "kat", source = "openmeteo", model = "best_match",
      issue_time = issue, valid_time = issue + 3600,
      lead_time = as.difftime(1, units = "hours"),
      member = NA_integer_, stat = NA_character_,
      variable = "temperature_2m", value = 1
    )
    # Simulate the pre-lock race: the same rows appended twice as raw parts.
    dir <- dataset_path(root, "forecasts",
                        list(source = "openmeteo", site_id = "kat", issue_date = "2026-09-29"))
    .write_part(dir, new_forecast(fc))
    .write_part(dir, new_forecast(fc))
    expect_equal(nrow(store_read_forecast(root, "kat")), 1L)
  })
})
