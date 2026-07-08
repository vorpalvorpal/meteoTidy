# Plan 13 -- verify_run(): assemble (forecast, observation) pairs, run the
# rolling-origin evaluation (SCOPING section 7.4's central review fix: every
# score is computed out-of-sample, never on a calibration's own training
# window), and persist a renderable report. Mirrors R/qc-log.R's companion-
# table pattern (a bespoke Parquet dataset under `<store_root>/verification/`,
# not one of the three canonical tables `R/store.R` manages).

.verification_dir <- function(store_root, site_id) {
  file.path(store_root, "verification", paste0("site_id=", site_id))
}

.verification_diagnostics_dir <- function(store_root, site_id) {
  file.path(store_root, "verification_diagnostics", paste0("site_id=", site_id))
}

.verification_report_empty <- function() {
  tibble::tibble(
    site_id = character(0), source = character(0), variable = character(0),
    lead_bucket = character(0), tier = character(0),
    n_pairs = integer(0), mae = double(0), rmse = double(0)
  )
}

.verification_diagnostics_empty <- function() {
  tibble::tibble(
    site_id = character(0), source = character(0), variable = character(0),
    lead_bucket = character(0), n_members = integer(0), n_cases = integer(0),
    histogram_flatness = double(0), spread_error_ratio = double(0), brier_score = double(0)
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

# The baselines/tier methods scored per group in verify_run() (Plan 17 item
# 5, SCOPING section 7.4): every method is scored out-of-sample the same way
# (rolling_origin_score()'s fit_fn/apply_fn contract), so "raw"/"persistence"/
# "climatology" are just three more fit_fn/apply_fn pairs alongside the
# fitted-tier one, not a special case.
#
# `climatology_apply` closes over `hist` (`build_history_daily()`, built ONCE
# per group by the caller, not once per rolling-origin window -- expensive to
# recompute and does not depend on the training window at all, unlike a real
# fit) and `variable`.
.verify_baseline_methods <- function(hist, variable) {
  list(
    raw = list(fit = .verify_identity_fit, apply = .verify_identity_apply),
    persistence = list(
      fit = function(train) NULL,
      apply = function(fit, score_set) baseline_persistence(score_set$observation)
    ),
    climatology = list(
      fit = function(train) NULL,
      apply = function(fit, score_set) {
        vapply(seq_len(nrow(score_set)), function(j) {
          if (nrow(hist) == 0) {
            return(NA_real_)
          }
          baseline_climatology(hist, score_set$valid_time[[j]], variable)$mean
        }, numeric(1))
      }
    )
  )
}

# Score one (source, variable, lead_bucket) group's `sub` pairs against every
# baseline method plus, when a calibration is on file for (variable, source),
# the incumbent fitted tier -- one report row per method (Plan 17 item 5).
.verify_score_group <- function(store_root, site, sid, grp, sub) {
  window <- list(from = min(sub$valid_time) - as.difftime(1095, units = "days"),
                 to = max(sub$valid_time))
  hist <- build_history_daily(store_root, site, window)
  methods <- .verify_baseline_methods(hist, grp$variable)

  calib <- tryCatch(calib_read(store_root, sid, grp$variable, grp$source, version = "current"),
                    error = function(e) NULL)
  if (!is.null(calib)) {
    tier <- calib$manifest$tier[[1]]
    methods[[tier]] <- .correct_refit_fit_apply(tier)
  }

  rows <- lapply(names(methods), function(m) {
    ro <- rolling_origin_score(sub, fit_fn = methods[[m]]$fit, apply_fn = methods[[m]]$apply,
                               step = "30 days", buffer = "1 day")
    tibble::tibble(
      site_id = sid, source = grp$source, variable = grp$variable,
      lead_bucket = grp$lead_bucket, tier = m,
      n_pairs = ro$n_scored, mae = ro$mae, rmse = ro$rmse
    )
  })
  vctrs::vec_rbind(!!!rows)
}

.verify_report <- function(store_root, site, sid, sources) {
  pairs <- assemble_verification_pairs(store_root, site, sources)
  if (nrow(pairs) == 0) {
    return(.verification_report_empty())
  }

  pairs$lead_bucket <- .verify_lead_bucket(pairs$lead_time)
  groups <- unique(pairs[c("source", "variable", "lead_bucket")])

  rows <- lapply(seq_len(nrow(groups)), function(i) {
    grp <- groups[i, ]
    sub <- pairs[pairs$source == grp$source & pairs$variable == grp$variable &
                   pairs$lead_bucket == grp$lead_bucket, , drop = FALSE]
    .verify_score_group(store_root, site, sid, grp, sub)
  })
  vctrs::vec_rbind(!!!rows)
}

# Ensemble calibration diagnostics (Plan 17 item 5, SCOPING section 7.4):
# rank/PIT histogram flatness, spread-error ratio, and (for precipitation)
# Brier score, per (source, variable, lead_bucket) group that has archived
# member rows. Reads the forecast archive directly (not `assemble_verifi-
# cation_pairs()`, which drops member rows entirely) and pivots to one
# ensemble-matrix row per `valid_time`, matched against the site's QC-clean
# observation at that instant.
.verify_diagnostics <- function(store_root, site, sources, sid) {
  fc <- store_read_forecast(store_root, sid, source = sources, members = TRUE)
  fc <- fc[!is.na(fc$member), , drop = FALSE]
  if (nrow(fc) == 0) {
    return(.verification_diagnostics_empty())
  }

  fc$lead_bucket <- .verify_lead_bucket(fc$lead_time)
  groups <- unique(fc[c("source", "variable", "lead_bucket")])

  rows <- lapply(seq_len(nrow(groups)), function(i) {
    grp <- groups[i, ]
    sub <- fc[fc$source == grp$source & fc$variable == grp$variable &
                fc$lead_bucket == grp$lead_bucket, , drop = FALSE]

    obs <- store_read_obs(store_root, sid, variables = grp$variable)
    obs <- obs[obs$qc_flag == "ok", , drop = FALSE]
    if (nrow(obs) == 0) {
      return(NULL)
    }

    valid_times <- sort(unique(sub$valid_time))
    members <- sort(unique(sub$member))
    mat <- matrix(NA_real_, nrow = length(valid_times), ncol = length(members))
    for (mi in seq_along(members)) {
      msub <- sub[sub$member == members[[mi]], , drop = FALSE]
      mat[, mi] <- msub$value[match(valid_times, msub$valid_time)]
    }
    truth <- obs$value[match(valid_times, obs$datetime_utc)]

    complete <- stats::complete.cases(mat) & !is.na(truth)
    mat <- mat[complete, , drop = FALSE]
    truth <- truth[complete]
    if (nrow(mat) == 0 || ncol(mat) < 2) {
      return(NULL)
    }

    brier <- NA_real_
    if (identical(grp$variable, "precipitation")) {
      brier <- brier_score(rowMeans(mat > 0), as.numeric(truth > 0))
    }

    tibble::tibble(
      site_id = sid, source = grp$source, variable = grp$variable,
      lead_bucket = grp$lead_bucket, n_members = ncol(mat), n_cases = nrow(mat),
      histogram_flatness = histogram_flatness(rank_histogram(mat, truth)),
      spread_error_ratio = spread_error_ratio(mat, truth), brier_score = brier
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(.verification_diagnostics_empty())
  }
  vctrs::vec_rbind(!!!rows)
}

#' Run the verification engine and persist a report
#'
#' Assembles verification pairs (`assemble_verification_pairs()`) and, per
#' `(source, variable, lead_bucket)` group, scores every applicable method
#' out-of-sample via `rolling_origin_score()` (SCOPING section 7.4): the raw
#' (uncorrected) forecast, the persistence baseline (`baseline_persistence()`),
#' the climatology baseline (`baseline_climatology()` over
#' `build_history_daily()`), and, when a calibration is on file for
#' `(variable, source)`, the incumbent fitted tier -- one report row per
#' method, all scored out-of-sample, never merely before/after. Also writes a
#' companion `verification_diagnostics` dataset (rank/PIT histogram flatness,
#' spread-error ratio, and Brier score for precipitation) for any group with
#' archived ensemble member rows (`read_verification_diagnostics()` reads it
#' back).
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

  report <- .verify_report(store_root, site, sid, sources)
  # REPLACE the stored report rather than appending a new part-file:
  # read_verification_report() rbinds every part-file in the directory, so an
  # append here would duplicate rows on every met_refit() run -- the report
  # is a "current state" product, not an accumulating log.
  .atomic_rewrite_partition(.verification_dir(store_root, sid), report)

  diagnostics <- .verify_diagnostics(store_root, site, sources, sid)
  .atomic_rewrite_partition(.verification_diagnostics_dir(store_root, sid), diagnostics)

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

#' Read a site's stored verification diagnostics
#'
#' @param store_root Root directory of the store.
#' @param site_id Site identifier.
#' @return A tibble (see `verify_run()`); zero rows (typed) if no diagnostics
#'   have been written yet.
#' @keywords internal
#' @noRd
read_verification_diagnostics <- function(store_root, site_id) {
  dir <- .verification_diagnostics_dir(store_root, site_id)
  if (!dir.exists(dir)) {
    return(.verification_diagnostics_empty())
  }
  files <- list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
  if (length(files) == 0) {
    return(.verification_diagnostics_empty())
  }
  tibble::as_tibble(do.call(rbind, lapply(files, arrow::read_parquet)))
}
