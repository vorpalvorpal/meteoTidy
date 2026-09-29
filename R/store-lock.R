#' @include conditions.R
NULL

# Store-level write lock (production review, problem 9).
#
# Every store write is read-check-write (forecast dedup, observation
# supersede, watermark/calibration JSON) and so is not safe against a second
# process writing the same store_root at the same time: two concurrent syncs
# both saw "not yet stored" and both appended (2,208 duplicate rows in the
# trial), and .atomic_rewrite_partition() could drop rows another writer had
# just added. One exclusive OS file lock per store_root (the `filelock`
# package; works on Windows and POSIX) serialises writers across processes.
#
# The lock is RE-ENTRANT within a process: the pipeline verbs hold it for a
# whole per-site sync (so QC/fill/history/watermark read-modify-write cycles
# are atomic as a unit), and every low-level writer also takes it so direct
# callers are protected too -- a nested acquire only bumps a counter.

.store_lock_state <- new.env(parent = emptyenv())

.store_lock_path <- function(store_root) {
  dir.create(store_root, recursive = TRUE, showWarnings = FALSE)
  file.path(normalizePath(store_root, winslash = "/", mustWork = FALSE), ".meteoTidy.lock")
}

# Default seconds to wait for another writer: a full per-site sync (fetch +
# QC + fill + history) can hold the lock for a couple of minutes.
.store_lock_default_timeout <- function() {
  getOption("meteoTidy.lock_timeout", 600)
}

#' Evaluate code while holding the store_root write lock
#'
#' @param store_root Root directory of the store.
#' @param code Code to evaluate with the lock held.
#' @param timeout Seconds to wait for the lock before aborting with class
#'   `"store_locked"`. Defaults to `getOption("meteoTidy.lock_timeout", 600)`.
#' @return The value of `code`.
#' @keywords internal
#' @noRd
with_store_lock <- function(store_root, code, timeout = NULL) {
  path <- .store_lock_path(store_root)
  held <- .store_lock_state[[path]]
  if (!is.null(held)) {
    held$depth <- held$depth + 1L
    on.exit(held$depth <- held$depth - 1L, add = TRUE)
    return(force(code))
  }

  timeout <- timeout %||% .store_lock_default_timeout()
  lck <- filelock::lock(path, exclusive = TRUE, timeout = as.integer(ceiling(timeout * 1000)))
  if (is.null(lck)) {
    abort_meteo(
      c(
        "Timed out after {timeout}s waiting for the store lock.",
        "i" = "Another meteoTidy process is writing to {.path {store_root}}.",
        "i" = "Raise {.code options(meteoTidy.lock_timeout = )} or {.code config$lock_timeout} if syncs overlap routinely."
      ),
      class = "store_locked"
    )
  }
  state <- new.env(parent = emptyenv())
  state$depth <- 1L
  state$lock <- lck
  .store_lock_state[[path]] <- state
  on.exit({
    rm(list = path, envir = .store_lock_state)
    filelock::unlock(lck)
  }, add = TRUE)
  force(code)
}
