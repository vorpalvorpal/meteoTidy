# Follow-up review, item 1: long Windows paths lost data silently.
#
# Windows (without the LongPathsEnabled policy) cannot open a path longer
# than 259 characters. A store_root given in 8.3 short form (e.g. %TEMP% =
# C:/Users/KATOOM~1/...) let arrow WRITE part files whose full long-form path
# was 260-266 characters; the write "succeeded", the sync reported "ok", and
# every later read silently skipped the file (arrow expands the short name
# before opening it). These tests use real directories and real parquet
# files on this machine.

# A real directory whose normalised (long-form) path is exactly `total`
# characters. Padding uses 8-character segments, which are already valid 8.3
# names, so a short-form spelling of the root differs from the long form
# only in the temp-directory prefix -- as with a real %TEMP% store.
long_root <- function(total, env = parent.frame()) {
  base <- normalizePath(withr::local_tempdir(.local_envir = env), winslash = "/")
  root <- base
  left <- total - nchar(base)
  while (left > 0) {
    seg <- if (left == 10L) 7L else min(8L, left - 1L)
    root <- file.path(root, strrep("r", seg))
    left <- left - seg - 1L
  }
  stopifnot(nchar(root) == total)
  dir.create(root, recursive = TRUE)
  root
}

aux_row <- function() {
  t <- as.POSIXct("2026-09-29 07:16:20", tz = "UTC")
  tibble::tibble(site_id = "blax", source = "bom_forecast", issue_time = t,
                 valid_time = t, field = "fire_danger_category", value_text = "Moderate")
}

describe("item 1: long store paths", {
  it("generates short part-file names", {
    expect_lte(nchar(.part_file_name()), 20)
    expect_lte(nchar(.part_file_name(tmp = TRUE)), 20)
    expect_match(.part_file_name(), "\\.parquet$")
    # Temp files must stay invisible to arrow datasets (leading dot).
    expect_match(.part_file_name(tmp = TRUE), "^\\.")
  })

  it("never reports success for a write Windows cannot read back (8.3 root)", {
    skip_if_not(.Platform$OS.type == "windows")
    # Root whose LONG form puts the aux part file at ~262 characters.
    root_long <- long_root(262 - 70 - 34)
    root_short <- utils::shortPathName(root_long)
    root_short <- gsub("\\\\", "/", root_short)
    skip_if(nchar(root_short) >= nchar(root_long), "no 8.3 short names on this volume")

    # Either the write fails loudly, or what it wrote is readable -- never a
    # silent "success" that loses the row (the behaviour before the fix).
    err <- tryCatch(store_write_forecast_aux(root_short, aux_row()), error = identity)
    if (inherits(err, "error")) {
      expect_s3_class(err, "meteoTidy_error_store_path_too_long")
    } else {
      back <- suppressWarnings(store_read_forecast_aux(root_long, "blax"))
      expect_equal(nrow(back), 1L)
    }

    # Deeper still, the write must fail loudly with a classed error.
    root_long2 <- long_root(262 - 69 - 20)
    root_short2 <- gsub("\\\\", "/", utils::shortPathName(root_long2))
    err2 <- tryCatch(store_write_forecast_aux(root_short2, aux_row()), error = identity)
    if (!inherits(err2, "error")) {
      back <- suppressWarnings(store_read_forecast_aux(root_long2, "blax"))
      expect_equal(nrow(back), 1L)
    } else {
      expect_s3_class(err2, "meteoTidy_error_store_path_too_long")
      expect_match(conditionMessage(err2), "259")
    }
  })

  it("verifies each part file after writing it", {
    root <- withr::local_tempdir()
    dir <- file.path(root, "t")
    path <- .write_part(dir, data.frame(x = 1:3))
    expect_true(file.exists(path))
    # A truncated (corrupt) part file is detected, not trusted.
    bytes <- readBin(path, "raw", file.size(path))
    writeBin(bytes[seq_len(length(bytes) %/% 2)], path)
    expect_error(.verify_part(path, 3L), class = "meteoTidy_error_store_write_unverified")
  })

  it("keeps the old partition file when the rewrite cannot be verified", {
    root <- withr::local_tempdir()
    dir <- file.path(root, "p")
    .atomic_rewrite_partition(dir, data.frame(x = 1:3))
    before <- list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
    local_mocked_bindings(.verify_part = function(path, n) {
      abort_meteo("simulated unreadable file", class = "store_write_unverified")
    })
    expect_error(.atomic_rewrite_partition(dir, data.frame(x = 1:5)),
                 class = "meteoTidy_error_store_write_unverified")
    after <- list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
    expect_equal(after, before)
    expect_equal(nrow(arrow::read_parquet(after)), 3L)
  })

  it("a sync on a store_root that is too long fails the site loudly, before writing", {
    root <- long_root(200)
    site <- make_prod_site("blax", store_root = root, sources = list(
      bom_forecast = list(adapter = "bom_forecast", allow_web_api = TRUE)
    ))
    cfg <- list(store_root = root, obs_sources = character(0),
                forecast_sources = "bom_forecast")
    routes <- list(
      "forecasts/daily" = "bom/webapi-daily-r65050.json",
      "forecasts/hourly" = "bom/webapi-hourly-r65050.json"
    )
    status <- with_routed_http(routes, met_sync_live(site, now = prod_now(), config = cfg))
    expect_equal(status$status, "error")
    expect_match(status$message, "too long")
    expect_length(list.files(file.path(root, "forecasts"), recursive = TRUE), 0)
  })

  it("accepts a store_root whose planned paths fit", {
    root <- long_root(120)
    expect_silent(.check_store_paths(root, "blax", c("bom_forecast", "openmeteo")))
    expect_lte(.planned_max_path(root, "blax", "bom_forecast"), 250)
  })
})
