# Plan 09 -- qc_run(): orchestration, dispatch, watermark, idempotency.
#
# qc_run() is the only QC entry point most callers need: given a site and a
# store, it reads the QC watermark (a normal store watermark, keyed by
# `source = "qc"` so it shares the exact same mechanism Plan 03 built for
# adapter sync watermarks -- no new bookkeeping concept needed), applies a
# small look-back before it so a late-arriving donor observation can
# retrigger the spatial rule on the recent tail, reads the target window,
# dispatches every requested variable's rows to the rules
# `qc_rules_for_variable()` says apply, combines flags (worst wins), writes
# back only the rows whose flag actually changed via
# `store_write_obs(mode = "supersede")`, appends to `qc_log`, and advances
# the watermark.

# How far before the watermark to re-scan on every run, so a donor
# observation that arrives late (e.g. a neighbour's revision lands a day
# after the fact) can still retrigger the spatial/buddy rule on the recent
# tail. Not pinned precisely by the plan ("a specific look-back duration
# isn't tested precisely, so pick something reasonable" -- plans/09); one day
# comfortably covers typical adapter revision lag (SCOPING section 9)
# without re-scanning so much history that every run becomes expensive.
.qc_lookback <- function() {
  as.difftime(1, units = "days")
}

# Which rules in the registry need context beyond (obs, dict): each context
# provider is a function(obs_for_variable, variable, site, donors,
# history_daily) -> obs (the rule applied). Kept as a small dispatch table
# rather than special-casing rule ids inline in `.qc_run_dispatch()`.
.qc_rule_dispatch <- function(rule_id, obs, dict, variable, site, donors, history_daily) {
  switch(rule_id,
    range = qc_range(obs, dict = dict),
    step = qc_step(obs, dict = dict),
    persistence = qc_persistence(obs, dict = dict),
    climatology = qc_climatology(obs, dict = dict, history_daily = history_daily),
    spatial = .qc_run_spatial(obs, variable, site, donors),
    solar = qc_solar(obs, site = site),
    # "consistency" is cross-variable (physics_constraints on the WIDE
    # frame), not dispatched per single-variable series here; qc_run()
    # applies it separately across the whole window (see
    # `.qc_run_consistency()`).
    obs
  )
}

# Apply the spatial rule for one variable's series against whichever donor
# series (if any) were supplied for that variable. `donors` is a named list
# keyed by variable, each element a list of single-variable long donor
# series (see qc_run()'s `donors` argument). No donors supplied at all (the
# common case when a caller has not wired up donor stations yet) degrades
# to "insufficient donors" rather than an error.
.qc_run_spatial <- function(obs, variable, site, donors) {
  row <- met_variable(variable)
  if (isTRUE(row$measurability_class == "model_only")) {
    return(obs) # spatial never dispatches here anyway (qc_rules_for_variable), defensive no-op
  }
  donor_series <- donors[[variable]] %||% list()
  qc_spatial(obs, donors = donor_series, site = site)
}

# Run the internal-consistency (physics_constraints) rule across the whole
# window: widen to one row per (site_id, datetime_utc), flag each relation,
# and downgrade qc_flag on every implicated variable's long-form row at that
# timestamp where its relation was violated.
.qc_run_consistency <- function(obs, site) {
  if (nrow(obs) == 0) {
    return(obs)
  }
  wide <- widen_obs(obs, variables = unique(obs$variable))
  wide$site_id <- site_id(site)

  flag_cols <- character(0)
  violated_at <- rep(FALSE, nrow(wide))
  for (i in seq_len(nrow(wide))) {
    flagged <- physics_constraints(wide[i, , drop = FALSE], mode = "flag")
    violated_at[i] <- isTRUE(flagged$violated)
    new_flag_cols <- grep("^flag_", names(flagged), value = TRUE)
    flag_cols <- union(flag_cols, new_flag_cols)
    for (fc in new_flag_cols) {
      wide[[fc]][i] <- flagged[[fc]]
    }
  }

  if (length(flag_cols) == 0) {
    return(obs)
  }

  bad <- integer(0)
  for (fc in flag_cols) {
    variable <- sub("^flag_", "", fc)
    suspect_times <- wide$datetime_utc[wide[[fc]] == "suspect"]
    if (length(suspect_times) == 0) next
    hit <- which(obs$variable == variable & obs$datetime_utc %in% suspect_times)
    bad <- c(bad, hit)
  }
  bad <- sort(unique(bad))

  out <- .qc_downgrade(obs, bad, "suspect")
  log_rows <- .qc_log_rows(
    obs, bad, "consistency", "suspect", "violates a physics_constraints relation"
  )
  .qc_log_attach(out, log_rows)
}

# Dispatch every variable present in `obs` to the rules that apply to it
# (per `qc_rules_for_variable()`), combining qc_log rows as we go. Returns a
# list(obs = <updated obs>, qc_log = <combined qc_log tibble>).
.qc_run_dispatch <- function(obs, site, donors, history_daily) {
  dict <- met_variables()
  qc_log_acc <- .qc_log_empty()

  variables <- unique(obs$variable)
  for (variable in variables) {
    idx <- which(obs$variable == variable)
    var_obs <- obs[idx, , drop = FALSE]
    attr(var_obs, "qc_log") <- NULL

    rules <- qc_rules_for_variable(variable)
    for (rule_id in setdiff(rules, "consistency")) {
      var_obs <- .qc_rule_dispatch(rule_id, var_obs, dict, variable, site, donors, history_daily)
      log_rows <- attr(var_obs, "qc_log")
      if (!is.null(log_rows) && nrow(log_rows) > 0) {
        qc_log_acc <- vctrs::vec_rbind(qc_log_acc, log_rows)
      }
      attr(var_obs, "qc_log") <- NULL
    }

    obs$qc_flag[idx] <- .qc_worse_flag(obs$qc_flag[idx], var_obs$qc_flag)
  }

  consistency_obs <- .qc_run_consistency(obs, site)
  log_rows <- attr(consistency_obs, "qc_log")
  if (!is.null(log_rows) && nrow(log_rows) > 0) {
    qc_log_acc <- vctrs::vec_rbind(qc_log_acc, log_rows)
  }
  attr(consistency_obs, "qc_log") <- NULL

  list(obs = consistency_obs, qc_log = qc_log_acc)
}

#' Run the QC engine over a site's observation window
#'
#' Reads the QC watermark for `site` (a store watermark keyed by `source =
#' "qc"`), applies a small look-back before it (`.qc_lookback()`) so a
#' late-arriving donor observation can retrigger the spatial rule on the
#' recent tail, reads the resulting window's observations, dispatches every
#' variable present to the rules `qc_rules_for_variable()` says apply
#' (combining flags worst-wins across rules), writes back via
#' `store_write_obs(mode = "supersede")` only the rows whose flag actually
#' changed, appends an auditable record to `qc_log`, and advances the
#' watermark to `now`.
#'
#' Incremental: only the window from `watermark - lookback` to `now` is
#' scanned. Idempotent: re-running over the same window reproduces identical
#' flags and does not create duplicate `qc_log` rows (the log is
#' deduplicated on `(site_id, datetime_utc, variable, rule)`, keeping the
#' latest run's verdict; see `qc_log_read()`).
#'
#' @param store_root Root directory of the store.
#' @param site A `met_site` object.
#' @param variables Character vector of variables to QC; `NULL` (default)
#'   means every variable present in the window.
#' @param now Injectable current time; see `.now()`.
#' @param donors A named list keyed by variable name, each element a list of
#'   single-variable long donor series tibbles (see `qc_spatial()`). `NULL`
#'   (default) means no donors are available, so the spatial rule logs
#'   "insufficient donors" for every variable rather than erroring.
#' @param history_daily A climatology tibble for the climatological-bounds
#'   rule (see `qc_climatology()`), or `NULL` (default) if none exists yet.
#' @return Invisibly, a list `(n_flagged, n_log_rows)` summarising the run.
#' @family qc
#' @export
#' @examples
#' \dontrun{
#' root <- withr::local_tempdir()
#' site <- met_site(
#'   site_id = "example",
#'   latitude = units::set_units(-34.75, "degree"),
#'   longitude = units::set_units(148.20, "degree"),
#'   elevation = units::set_units(220, "m"),
#'   timezone = "Australia/Sydney",
#'   instruments = list(),
#'   sources = list(),
#'   store_root = root
#' )
#' qc_run(root, site)
#' }
qc_run <- function(store_root, site, variables = NULL, now = .now(),
                   donors = NULL, history_daily = NULL) {
  sid <- site_id(site)
  watermark <- store_get_watermark(store_root, sid, "observations", "qc")
  from <- if (is.na(watermark)) NULL else watermark - .qc_lookback()

  window <- store_read_obs(store_root, sid, variables = variables, from = from, to = now)
  if (nrow(window) == 0) {
    store_set_watermark(store_root, sid, "observations", "qc", now)
    return(invisible(list(n_flagged = 0L, n_log_rows = 0L)))
  }

  result <- .qc_run_dispatch(window, site = site, donors = donors, history_daily = history_daily)
  updated <- result$obs

  changed <- updated$qc_flag != window$qc_flag
  n_flagged <- sum(changed)
  if (n_flagged > 0) {
    store_write_obs(store_root, updated[changed, , drop = FALSE], now = now, mode = "supersede")
  }

  qc_log_write(store_root, result$qc_log, now = now)
  store_set_watermark(store_root, sid, "observations", "qc", now)

  invisible(list(n_flagged = n_flagged, n_log_rows = nrow(result$qc_log)))
}
