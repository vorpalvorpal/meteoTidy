# Plan 13 -- verify_run(): assemble (forecast, observation) pairs, run the
# rolling-origin evaluation (SCOPING section 7.4's central review fix: every
# score is computed out-of-sample, never on a calibration's own training
# window), and persist a renderable report. Mirrors R/qc-log.R's companion-
# table pattern (a bespoke Parquet dataset under `<store_root>/verification/`,
# not one of the three canonical tables `R/store.R` manages).

.verification_dir <- function(store_root, site_id) {
  file.path(store_root, "verification", paste0("site_id=", site_id))
}

.verification_report_empty <- function() {
  tibble::tibble(
    site_id = character(0), source = character(0), variable = character(0),
    lead_bucket = character(0), tier = character(0),
    n_pairs = integer(0), mae = double(0), rmse = double(0)
  )
}

# Parse a "<number> <unit>" period string (e.g. "30 days", "1 day") into a
# difftime. Singular units are accepted (as.difftime() requires the plural
# form for its `units` argument).
.parse_period <- function(period) {
  parts <- strsplit(trimws(period), "\\s+")[[1]]
  n <- as.numeric(parts[[1]])
  unit <- parts[[2]]
  if (!grepl("s$", unit)) {
    unit <- paste0(unit, "s")
  }
  as.difftime(n, units = unit)
}

# Bucket a difftime lead time into a coarse "d<N>" label (N = whole days,
# rounded up so a same-day lead still gets its own bucket, e.g. 6h -> "d1").
.verify_lead_bucket <- function(lead_time) {
  days <- as.numeric(lead_time, units = "days")
  paste0("d", pmax(1L, ceiling(days)))
}

#' Assemble (forecast, observation) verification pairs from the store
#'
#' Reads the site's archived forecasts for `sources` (`store_read_forecast()`,
#' Plan 03; deterministic/`stat`-summary rows only -- per-member ensemble
#' trajectories are not paired here) and its current, QC-clean
#' (`qc_flag == "ok"`) observations for the same variables
#' (`store_read_obs()`), and joins them on `(site_id, variable, valid_time ==
#' datetime_utc)` into a `forecast_obs_pairs()`-shaped tibble (see
#' `tests/testthat/helper-correct.R`): `site_id`, `source`, `model`,
#' `issue_time`, `valid_time`, `lead_time`, `variable`, `forecast`,
#' `observation`.
#'
#' This is the real (non-mocked) implementation; `test-verify-run.R` mocks
#' this function directly to supply a synthetic pairs tibble without needing
#' a populated store, so its internals are not exercised by any pinned test
#' beyond "the function exists and is callable" -- kept deliberately simple.
#'
#' @param store_root Root directory of the store.
#' @param site A [met_site()] object.
#' @param sources Character vector of source names to include.
#' @param variables Optional character vector to restrict which variables are
#'   paired; `NULL` (default) pairs every variable present in the archived
#'   forecasts.
#' @return A tibble in `forecast_obs_pairs()`'s shape; zero rows (typed) if
#'   no forecast/observation overlap exists.
#' @keywords internal
#' @noRd
assemble_verification_pairs <- function(store_root, site, sources, variables = NULL) {
  sid <- site_id(site)
  fc <- store_read_forecast(store_root, sid, source = sources, members = FALSE)

  empty <- tibble::tibble(
    site_id = character(0), source = character(0), model = character(0),
    issue_time = as.POSIXct(character(0), tz = "UTC"),
    valid_time = as.POSIXct(character(0), tz = "UTC"),
    lead_time = as.difftime(numeric(0), units = "hours"),
    variable = character(0), forecast = double(0), observation = double(0)
  )
  if (nrow(fc) == 0) {
    return(empty)
  }
  if (!is.null(variables)) {
    fc <- fc[fc$variable %in% variables, , drop = FALSE]
  }
  # Deterministic (non-ensemble) rows only: a single value per
  # (site_id, source, model, issue_time, valid_time, variable).
  fc <- fc[is.na(fc$member), , drop = FALSE]
  if (nrow(fc) == 0) {
    return(empty)
  }

  obs <- store_read_obs(store_root, sid, variables = unique(fc$variable))
  obs <- obs[obs$qc_flag == "ok", , drop = FALSE]
  if (nrow(obs) == 0) {
    return(empty)
  }

  fc_time <- format(fc$valid_time, "%Y-%m-%dT%H:%M:%OS6", tz = "UTC")
  obs_time <- format(obs$datetime_utc, "%Y-%m-%dT%H:%M:%OS6", tz = "UTC")
  fc_key <- paste(fc$variable, fc_time, sep = "\r")
  obs_key <- paste(obs$variable, obs_time, sep = "\r")
  m <- match(fc_key, obs_key)
  matched <- !is.na(m)
  if (!any(matched)) {
    return(empty)
  }

  tibble::tibble(
    site_id = fc$site_id[matched],
    source = fc$source[matched],
    model = fc$model[matched],
    issue_time = fc$issue_time[matched],
    valid_time = fc$valid_time[matched],
    lead_time = fc$lead_time[matched],
    variable = fc$variable[matched],
    forecast = fc$value[matched],
    observation = obs$value[m[matched]]
  )
}

# Identity fit/apply: the "raw" tier, no correction applied. verify_run()'s
# rolling-origin baseline for whichever tier's calibration Plan 16 has not
# yet wired in -- scoring the raw forecast out-of-sample is still a real,
# useful report row (SCOPING section 7.4's raw-model baseline), not a stub.
.verify_identity_fit <- function(train) {
  NULL
}
.verify_identity_apply <- function(fit, newdata) {
  newdata$forecast
}

#' Rolling-origin, out-of-sample scoring
#'
#' Walks forward through `pairs$issue_time` in `step`-sized windows. At each
#' origin `t`: fits `fit_fn()` on only the pairs with
#' `issue_time < t - buffer` (never on the window being scored -- this is the
#' load-bearing correctness property, SCOPING section 7.4: a calibration must
#' never be scored on its own training data, or the reported skill is
#' inflated and would corrupt Plan 11's promotion gate), then scores
#' `apply_fn()`'s predictions on the pairs issued in `[t, t + step)`.
#' Aggregates every origin's scoring-window errors into one overall
#' `mae`/`rmse`.
#'
#' @param pairs A `forecast_obs_pairs()`-shaped tibble (`issue_time`,
#'   `forecast`, `observation`).
#' @param fit_fn `function(train_pairs) -> fit` (a fitted object, any shape
#'   the paired `apply_fn` understands).
#' @param apply_fn `function(fit, score_pairs) -> numeric` (corrected forecast
#'   values for `score_pairs`, same length/order as `score_pairs`).
#' @param step A `"<number> <unit>"` string (e.g. `"30 days"`), the width of
#'   each scoring window.
#' @param buffer A `"<number> <unit>"` string (e.g. `"1 day"`), the gap kept
#'   between the training cutoff and the scoring window's start.
#' @return A list with `mae`, `rmse` (overall, across every scored row) and
#'   `n_scored` (how many rows were actually scored out-of-sample).
#' @keywords internal
#' @noRd
rolling_origin_score <- function(pairs, fit_fn, apply_fn, step, buffer) {
  step_dt <- .parse_period(step)
  buffer_dt <- .parse_period(buffer)

  pairs <- pairs[order(pairs$issue_time), , drop = FALSE]
  if (nrow(pairs) == 0) {
    return(list(mae = NA_real_, rmse = NA_real_, n_scored = 0L))
  }

  origins <- seq(min(pairs$issue_time), max(pairs$issue_time), by = step_dt)

  corrected_all <- numeric(0)
  observed_all <- numeric(0)

  for (origin in origins) {
    origin <- as.POSIXct(origin, tz = "UTC", origin = "1970-01-01")
    train <- pairs[pairs$issue_time < origin - buffer_dt, , drop = FALSE]
    in_window <- pairs$issue_time >= origin & pairs$issue_time < origin + step_dt
    score_set <- pairs[in_window, , drop = FALSE]
    if (nrow(train) == 0 || nrow(score_set) == 0) {
      next
    }

    fit <- fit_fn(train)
    corrected <- apply_fn(fit, score_set)
    corrected_all <- c(corrected_all, corrected)
    observed_all <- c(observed_all, score_set$observation)
  }

  if (length(corrected_all) == 0) {
    return(list(mae = NA_real_, rmse = NA_real_, n_scored = 0L))
  }

  scores <- score_deterministic(corrected_all, observed_all)
  list(mae = scores$mae, rmse = scores$rmse, n_scored = length(corrected_all))
}

#' Run the verification engine and persist a report
#'
#' Assembles verification pairs (`assemble_verification_pairs()`), scores the
#' raw (uncorrected) forecast out-of-sample via `rolling_origin_score()` per
#' `(source, variable, lead_bucket)`, and writes the resulting report to the
#' store (`read_verification_report()` reads it back). This plan's tested
#' scope is the rolling-origin correctness property and report persistence;
#' wiring in Plan 11/12's actual fitted calibrations (so the report compares
#' `raw` against `physical`/`mean_bias`/`qmap`/`emos` tiers side by side) is
#' Plan 16's pipeline-orchestration concern -- every reported row here is
#' honestly tier `"raw"` until that wiring exists.
#'
#' @param store_root Root directory of the store.
#' @param site A [met_site()] object.
#' @param sources Character vector of source names to verify.
#' @param now Injectable current time; see `.now()`.
#' @return Invisibly, the report tibble just written.
#' @keywords internal
#' @noRd
verify_run <- function(store_root, site, sources, now = .now()) {
  sid <- site_id(site)
  pairs <- assemble_verification_pairs(store_root, site, sources)

  if (nrow(pairs) == 0) {
    report <- .verification_report_empty()
    .write_part(.verification_dir(store_root, sid), report)
    return(invisible(report))
  }

  pairs$lead_bucket <- .verify_lead_bucket(pairs$lead_time)
  groups <- unique(pairs[c("source", "variable", "lead_bucket")])

  rows <- lapply(seq_len(nrow(groups)), function(i) {
    grp <- groups[i, ]
    sub <- pairs[pairs$source == grp$source & pairs$variable == grp$variable &
                   pairs$lead_bucket == grp$lead_bucket, , drop = FALSE]
    ro <- rolling_origin_score(sub, fit_fn = .verify_identity_fit,
                               apply_fn = .verify_identity_apply,
                               step = "30 days", buffer = "1 day")
    tibble::tibble(
      site_id = sid, source = grp$source, variable = grp$variable,
      lead_bucket = grp$lead_bucket, tier = "raw",
      n_pairs = ro$n_scored, mae = ro$mae, rmse = ro$rmse
    )
  })

  report <- vctrs::vec_rbind(!!!rows)
  .write_part(.verification_dir(store_root, sid), report)
  invisible(report)
}

#' Read a site's stored verification report
#'
#' @param store_root Root directory of the store.
#' @param site_id Site identifier.
#' @return A tibble (see `verify_run()`); zero rows (typed) if no report has
#'   been written yet.
#' @keywords internal
#' @noRd
read_verification_report <- function(store_root, site_id) {
  dir <- .verification_dir(store_root, site_id)
  if (!dir.exists(dir)) {
    return(.verification_report_empty())
  }
  files <- list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
  if (length(files) == 0) {
    return(.verification_report_empty())
  }
  tibble::as_tibble(do.call(rbind, lapply(files, arrow::read_parquet)))
}
