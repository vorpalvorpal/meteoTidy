#' @include conditions.R
NULL

# Per-source isolation, status, logging and failure signalling for the sync
# verbs (problems 7 and 10 of the production review).
#
# Every acquisition step (one obs source, one forecast source) runs through
# .run_source(): an error becomes that source's "failed" (or "stale") row in
# a per-source status table instead of unwinding the whole site, so a dead
# forecast feed can no longer skip the site's observations, QC and history.
# The verbs then log one line per site to stderr and, with `fail_on`, turn a
# failure into an error so `Rscript` exits non-zero for the scheduler.

.source_status_row <- function(kind, source, status, n = 0L, message = NA_character_) {
  tibble::tibble(kind = kind, source = source, status = status,
                 n = as.integer(n), message = as.character(message))
}

.empty_source_status <- function() {
  .source_status_row(character(0), character(0), character(0), integer(0), character(0))
}

.one_line <- function(x) {
  gsub("\\s+", " ", trimws(paste(x, collapse = " ")))
}

# Run one source's work. `expr` must return the number of rows written.
# Errors are caught and recorded; warnings are recorded and muffled (they are
# reported in the status table and the log line instead).
.run_source <- function(kind, source, expr) {
  warnings <- character(0)
  stale <- FALSE
  result <- withCallingHandlers(
    tryCatch(
      list(n = force(expr)),
      error = function(cnd) list(error = cnd)
    ),
    warning = function(w) {
      stale <<- stale || inherits(w, "meteoTidy_warning_source_stale")
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  if (!is.null(result$error)) {
    status <- if (inherits(result$error, "meteoTidy_error_source_stale")) "stale" else "failed"
    return(.source_status_row(kind, source, status, 0L,
                              .one_line(conditionMessage(result$error))))
  }
  .source_status_row(kind, source, if (stale) "stale" else "ok", result$n %||% 0L,
                     if (length(warnings)) .one_line(warnings) else NA_character_)
}

# Summarise a site's per-source table: "ok" when every source succeeded,
# "failed" when every source failed (nothing was acquired), else "degraded".
.site_status <- function(sources, step_errors = character(0)) {
  bad <- sources$status != "ok"
  status <- if (nrow(sources) > 0 && all(bad)) {
    "failed"
  } else if (any(bad) || length(step_errors) > 0) {
    "degraded"
  } else {
    "ok"
  }
  msgs <- c(
    sprintf("%s: %s", sources$source[bad], sources$message[bad]),
    step_errors
  )
  list(
    status = status,
    message = if (length(msgs)) paste(msgs, collapse = "; ") else NA_character_,
    sources = sources
  )
}

# One concise stderr line per site, e.g.
#   meteoTidy met_sync_live kat: degraded | bom_obs ok 144 | openmeteo FAILED (HTTP 404) | ...
.sync_log_line <- function(verb, site_id, status, message, sources) {
  parts <- if (is.null(sources) || nrow(sources) == 0) {
    character(0)
  } else {
    vapply(seq_len(nrow(sources)), function(i) {
      s <- sources[i, ]
      if (s$status == "ok") {
        sprintf("%s ok %d", s$source, s$n)
      } else {
        sprintf("%s %s (%s)", s$source, toupper(s$status), substr(s$message, 1, 160))
      }
    }, character(1))
  }
  if (status == "error") {
    parts <- c(parts, sprintf("error: %s", substr(.one_line(message), 1, 200)))
  }
  paste(c(sprintf("meteoTidy %s %s: %s", verb, site_id, status), parts), collapse = " | ")
}

.sync_log <- function(verb, status_tbl) {
  if (!isTRUE(getOption("meteoTidy.sync_log", TRUE))) {
    return(invisible())
  }
  for (i in seq_len(nrow(status_tbl))) {
    message(.sync_log_line(verb, status_tbl$site_id[[i]], status_tbl$status[[i]],
                           status_tbl$message[[i]], status_tbl$sources[[i]]))
  }
  invisible()
}

# The sources of one site's status row that FAILED (not merely stale).
.failed_sources <- function(sources) {
  if (is.null(sources) || nrow(sources) == 0) {
    return(character(0))
  }
  sources$source[sources$status == "failed"]
}

# Turn the per-site status table into an error when the scheduler asked for
# one: "any" -- any site not fully ok (a stale source counts); "failed" --
# any source "failed" or any site "error", ignoring "stale" sources (the
# production setting: a silent station is logged, a dead feed fails the
# run); "all" -- every site failed outright (status "failed" or "error");
# "none" -- never.
.apply_fail_on <- function(verb, status_tbl, fail_on) {
  failed_by_site <- lapply(status_tbl$sources %||% vector("list", nrow(status_tbl)), .failed_sources)
  site_failed <- status_tbl$status %in% c("failed", "error") | lengths(failed_by_site) > 0
  bad <- switch(fail_on,
    none = FALSE,
    any = any(status_tbl$status != "ok"),
    failed = any(site_failed),
    all = nrow(status_tbl) > 0 && all(status_tbl$status %in% c("failed", "error"))
  )
  if (isTRUE(bad)) {
    rows <- if (identical(fail_on, "failed")) which(site_failed) else which(status_tbl$status != "ok")
    lines <- vapply(rows, function(i) {
      failed <- failed_by_site[[i]]
      sprintf("%s: %s%s", status_tbl$site_id[[i]], status_tbl$status[[i]],
              if (length(failed)) sprintf(" (failed: %s)", paste(failed, collapse = ", ")) else "")
    }, character(1))
    names(lines) <- rep("x", length(lines))
    lines <- gsub("([{}])", "\\1\\1", lines)
    abort_meteo(
      c("{verb}() failed (fail_on = {.val {fail_on}}).", lines),
      class = "sync_failed",
      status = status_tbl
    )
  }
  invisible(status_tbl)
}

# Shared driver for the sync verbs: per-site isolation, the store lock held
# for the whole site, the per-site status table, logging, and fail_on.
.run_sync_verb <- function(verb, sites, config, fail_on, site_fn) {
  status <- for_each_site(sites, function(site) {
    store_root <- config$store_root %||% site_store_root(site)
    # Fail the site up front (status "error") rather than half-writing it
    # when its paths could exceed the Windows limit (follow-up item 1).
    .check_store_paths(store_root, site_id(site),
                       c(config$obs_sources, config$forecast_sources))
    with_store_lock(store_root, site_fn(site), timeout = config$lock_timeout)
  }, on_error = "isolate")

  ok <- status$status == "ok"
  status$sources <- lapply(seq_len(nrow(status)), function(i) {
    if (ok[[i]]) status$result[[i]]$sources %||% .empty_source_status() else .empty_source_status()
  })
  status$message <- ifelse(
    ok,
    vapply(status$result, function(r) r$message %||% NA_character_, character(1)),
    status$message
  )
  status$status <- ifelse(
    ok,
    vapply(status$result, function(r) r$status %||% NA_character_, character(1)),
    status$status
  )
  status$result <- NULL
  status <- status[c("site_id", "status", "message", "sources")]

  .sync_log(verb, status)
  .apply_fail_on(verb, status, fail_on)
  status
}

# Coerce a step's summary into the per-source status shape, filling any
# missing column (e.g. a minimal summary from a caller-supplied stand-in).
.as_source_status <- function(x, kind) {
  if (is.null(x) || nrow(x) == 0) {
    return(.empty_source_status())
  }
  n <- nrow(x)
  .source_status_row(
    kind = x$kind %||% rep(kind, n),
    source = x$source %||% rep(NA_character_, n),
    status = x$status %||% rep("ok", n),
    n = x$n %||% rep(0L, n),
    message = x$message %||% rep(NA_character_, n)
  )
}
