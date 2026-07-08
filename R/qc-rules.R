# Plan 09 -- individual QC rules (range/step/persistence/climatology) plus the
# rule registry and per-variable dispatch.
#
# Every rule is a pure function `rule_fn(obs, dict, ...) -> obs`: it may only
# DOWNGRADE `qc_flag` (ok -> suspect -> fail), never upgrade it, and it never
# mutates its input in place (house style: functional by default). Rules
# operate on a canonical long-format obs tibble (site_id, datetime_utc,
# variable, value, source, method, qc_flag) for one or more variables.
#
# The registry (`qc_registry()`) maps rule id -> list(fn, applies_to =
# <statistical classes>); `qc_rules_for_variable()` looks a variable's
# statistical_class/measurability_class up in the dictionary and returns the
# rule ids that dispatch to it (SCOPING section 7.3: the spatial rule never
# applies to a `model_only` variable, since a model-only field has no site
# truth to buddy-check against).

# Worst-of ordering helper: combine an existing qc_flag with a candidate flag,
# keeping whichever is worse on the QC_FLAG_LEVELS ladder (ok < suspect <
# fail < missing). Never upgrades: if `candidate` is better than `current`,
# `current` wins.
.qc_worse_flag <- function(current, candidate) {
  levels <- QC_FLAG_LEVELS
  # Recycle `candidate` up to `current`'s length first (it is very often a
  # scalar, e.g. "suspect", applied against a whole vector of current
  # flags) so the later logical subsetting indexes same-length vectors.
  candidate <- rep_len(candidate, length(current))
  cur_rank <- match(current, levels)
  cand_rank <- match(candidate, levels)
  out <- current
  worse <- !is.na(cand_rank) & (is.na(cur_rank) | cand_rank > cur_rank)
  out[worse] <- candidate[worse]
  out
}

# Downgrade qc_flag at `idx` to `flag` (only where it is actually worse),
# returning the updated obs tibble.
.qc_downgrade <- function(obs, idx, flag) {
  if (length(idx) == 0) {
    return(obs)
  }
  obs$qc_flag[idx] <- .qc_worse_flag(obs$qc_flag[idx], flag)
  obs
}

# Build a qc_log row set for the rows at `idx` (or all rows, when `idx`
# selects everything) that a rule flagged, one row per (site_id,
# datetime_utc, variable). `outcome` is the qc_flag value assigned;
# `detail` is a short human-readable reason.
.qc_log_rows <- function(obs, idx, rule, outcome, detail) {
  if (length(idx) == 0) {
    return(.qc_log_empty())
  }
  tibble::tibble(
    site_id = obs$site_id[idx],
    datetime_utc = obs$datetime_utc[idx],
    variable = obs$variable[idx],
    rule = rule,
    outcome = outcome,
    detail = detail
  )
}

# Attach (accumulating) qc_log rows to `obs` as an attribute, so a chain of
# rule calls can build up a full audit trail without a separate return value.
.qc_log_attach <- function(obs, new_rows) {
  prior <- attr(obs, "qc_log")
  combined <- if (is.null(prior)) new_rows else vctrs::vec_rbind(prior, new_rows)
  attr(obs, "qc_log") <- combined
  obs
}

#' Range rule: flag values outside the dictionary's plausible range
#'
#' Universal rule (applies to every `statistical_class`): a value outside its
#' variable's dictionary `[min, max]` is physically impossible, so it is
#' downgraded to `"fail"`.
#'
#' @param obs A canonical long obs tibble, one or more variables.
#' @param dict The variable dictionary (see `met_variables()`).
#' @return `obs` with `qc_flag` downgraded (never upgraded) where the value is
#'   out of range, and `qc_log` rows attached via `attr(obs, "qc_log")`.
#' @keywords internal
#' @noRd
qc_range <- function(obs, dict = met_variables()) {
  ranges <- dict[match(obs$variable, dict$variable), c("min", "max")]
  below <- !is.na(ranges$min) & !is.na(obs$value) & obs$value < ranges$min
  above <- !is.na(ranges$max) & !is.na(obs$value) & obs$value > ranges$max
  bad <- which(below | above)

  out <- .qc_downgrade(obs, bad, "fail")
  log_rows <- .qc_log_rows(obs, bad, "range", "fail", "value outside dictionary [min, max]")
  .qc_log_attach(out, log_rows)
}

# Per-statistical-class step limits (canonical units), i.e. the largest
# plausible change between two CONSECUTIVE hourly samples. These are
# deliberately generous starting points (WMO-style QC guidance uses
# variable-specific step tests; refine with a cited authority as later plans
# need tighter bounds -- see plans/09-curation-qc.md).
.qc_step_limits <- function() {
  c(
    temperature_2m = 8, dewpoint_2m = 8, surface_pressure = 15, pressure_msl = 15,
    relative_humidity_2m = 40, wind_speed_10m = 25, wind_gusts_10m = 40,
    wind_speed_80m = 25, wind_speed_120m = 25, wind_speed_180m = 25,
    wind_direction_10m = 150, wind_direction_80m = 150,
    wind_direction_120m = 150, wind_direction_180m = 150,
    cloud_cover = 100, boundary_layer_height = 3000,
    direct_radiation = 1200, diffuse_radiation = 800
  )
}

# The default step limit for a variable not listed in `.qc_step_limits()`:
# a generous fraction of its dictionary range.
.qc_default_step_limit <- function(variable, dict) {
  row <- dict[match(variable, dict$variable), , drop = FALSE]
  span <- row$max - row$min
  ifelse(is.na(span), Inf, span * 0.5)
}

# Per-row step (|delta|) between consecutive samples of the SAME
# (site_id, source, variable) series, ordered by datetime_utc. Circular
# variables (statistical_class == "circular") wrap: the step is the angular
# difference min(|d|, period - |d|), not the raw subtraction (a 350 -> 10
# step is a 20-degree turn, not 340 degrees).
.qc_step_values <- function(obs, dict) {
  n <- nrow(obs)
  step <- rep(NA_real_, n)
  key <- paste(obs$site_id, obs$source, obs$variable, sep = "\r")
  for (k in unique(key)) {
    idx <- which(key == k)
    idx <- idx[order(obs$datetime_utc[idx])]
    if (length(idx) < 2) next
    v <- obs$value[idx]
    d <- diff(v)
    variable <- obs$variable[idx[1]]
    row <- dict[match(variable, dict$variable), , drop = FALSE]
    if (isTRUE(row$statistical_class == "circular") && !is.na(row$circular_period)) {
      period <- row$circular_period
      d <- pmin(abs(d) %% period, period - (abs(d) %% period))
    } else {
      d <- abs(d)
    }
    step[idx[-1]] <- d
  }
  step
}

#' Step rule: flag implausible jumps between consecutive samples
#'
#' Applies to `linear`, `bounded`, `circular`, and `clear_sky_indexed`
#' statistical classes (not `intermittent`, where legitimate step changes -
#' e.g. rain starting - are the point). Circular variables (wind direction)
#' use the angular difference, not the raw numeric subtraction, so a
#' 350 degree -> 10 degree turn is correctly seen as a 20 degree step.
#'
#' @inheritParams qc_range
#' @return `obs` with `qc_flag` downgraded to `"suspect"` where the
#'   per-variable step limit is exceeded, and `qc_log` rows attached.
#' @keywords internal
#' @noRd
qc_step <- function(obs, dict = met_variables()) {
  applicable_classes <- c("linear", "bounded", "circular", "clear_sky_indexed")
  dict_class <- dict$statistical_class[match(obs$variable, dict$variable)]
  eligible <- dict_class %in% applicable_classes

  step <- rep(NA_real_, nrow(obs))
  if (any(eligible)) {
    step[eligible] <- .qc_step_values(obs[eligible, , drop = FALSE], dict)
  }

  limits <- .qc_step_limits()
  limit <- limits[obs$variable]
  limit[is.na(limit)] <- .qc_default_step_limit(obs$variable[is.na(limit)], dict)

  bad <- which(eligible & !is.na(step) & step > limit)
  out <- .qc_downgrade(obs, bad, "suspect")
  log_rows <- .qc_log_rows(obs, bad, "step", "suspect", "implausible step between samples")
  .qc_log_attach(out, log_rows)
}

# Per-statistical-class persistence (flat-line) window, in HOURS: how long a
# value may stay exactly unchanged before it looks like a stuck sensor rather
# than a genuinely steady reading.
.qc_persistence_window <- function() {
  6L
}

# Which statistical classes are eligible for the persistence rule at all.
# `intermittent` (precipitation) is excluded outright: zero rain for hours is
# a legitimate dry spell, not a stuck sensor (plans/09-curation-qc.md).
.qc_persistence_eligible_classes <- function() {
  setdiff(STAT_CLASS_LEVELS, "intermittent")
}

#' Persistence (flat-line) rule: flag a sensor stuck for too long
#'
#' A value that is exactly unchanged for longer than a per-class window is
#' downgraded to `"suspect"` (a classic stuck-sensor signature). Skipped
#' entirely for the `intermittent` statistical class, where a run of
#' identical (zero) values is a legitimate dry spell, not a fault.
#'
#' @inheritParams qc_range
#' @return `obs` with `qc_flag` downgraded to `"suspect"` on runs longer than
#'   the persistence window, and `qc_log` rows attached.
#' @keywords internal
#' @noRd
qc_persistence <- function(obs, dict = met_variables()) {
  dict_class <- dict$statistical_class[match(obs$variable, dict$variable)]
  eligible_rows <- dict_class %in% .qc_persistence_eligible_classes()
  window <- .qc_persistence_window()

  bad <- integer(0)
  key <- paste(obs$site_id, obs$source, obs$variable, sep = "\r")
  for (k in unique(key)) {
    idx <- which(key == k & eligible_rows)
    if (length(idx) < window + 1) next
    idx <- idx[order(obs$datetime_utc[idx])]
    v <- obs$value[idx]
    run_id <- cumsum(c(TRUE, diff(v) != 0 | is.na(diff(v))))
    run_len <- stats::ave(seq_along(run_id), run_id, FUN = length)
    flagged <- idx[run_len > window]
    bad <- c(bad, flagged)
  }
  bad <- sort(unique(bad))

  out <- .qc_downgrade(obs, bad, "suspect")
  log_rows <- .qc_log_rows(
    obs, bad, "persistence", "suspect",
    sprintf("value unchanged for more than %d consecutive samples", window)
  )
  .qc_log_attach(out, log_rows)
}

#' Climatological-bounds rule: flag values outside a seasonal envelope
#'
#' Compares each value to a climatological envelope (e.g. day-of-year
#' p0.1/p99.9) derived from `history_daily`. Requires `history_daily`;
#' when it is `NULL` (e.g. day 0, before any climatology exists), the rule is
#' a documented no-op and logs a note rather than erroring.
#'
#' `history_daily` is expected to have columns `variable`, `day_of_year`,
#' `p_low`, `p_high` (one row per variable/day-of-year); this shape is
#' produced by the Plan 10 curated-products machinery and is only consumed
#' here.
#'
#' @inheritParams qc_range
#' @param history_daily A climatology tibble (see Details), or `NULL`.
#' @return `obs` with `qc_flag` downgraded to `"suspect"` outside the
#'   envelope, and `qc_log` rows attached (including a no-op note when
#'   `history_daily` is `NULL`).
#' @keywords internal
#' @noRd
qc_climatology <- function(obs, dict = met_variables(), history_daily = NULL) {
  if (is.null(history_daily) || nrow(history_daily) == 0) {
    log_rows <- .qc_log_rows(
      obs, seq_len(nrow(obs)), "climatology", "ok",
      "skipped: no history_daily climatology available yet"
    )
    return(.qc_log_attach(obs, log_rows))
  }

  doy <- as.integer(format(obs$datetime_utc, "%j"))
  key <- paste(obs$variable, doy, sep = "\r")
  clim_key <- paste(history_daily$variable, history_daily$day_of_year, sep = "\r")
  m <- match(key, clim_key)

  p_low <- history_daily$p_low[m]
  p_high <- history_daily$p_high[m]
  bad <- which(!is.na(m) & !is.na(obs$value) &
                 ((!is.na(p_low) & obs$value < p_low) | (!is.na(p_high) & obs$value > p_high)))

  out <- .qc_downgrade(obs, bad, "suspect")
  log_rows <- .qc_log_rows(
    obs, bad, "climatology", "suspect", "value outside climatological day-of-year envelope"
  )
  .qc_log_attach(out, log_rows)
}

#' The QC rule registry
#'
#' Maps each rule id to the function that implements it and the
#' `statistical_class` values it dispatches to. `qc_run()` and
#' `qc_rules_for_variable()` are the only consumers; the registry itself is
#' pure data plus function references, kept in one place so the dispatch
#' contract (plans/09-curation-qc.md) is visible at a glance.
#'
#' @return A named list, one element per rule id, each `list(fn =
#'   <function>, applies_to = <character vector of STAT_CLASS_LEVELS>)`.
#' @keywords internal
#' @noRd
qc_registry <- function() {
  list(
    range = list(fn = qc_range, applies_to = STAT_CLASS_LEVELS),
    step = list(fn = qc_step, applies_to = c("linear", "bounded", "circular", "clear_sky_indexed")),
    persistence = list(fn = qc_persistence, applies_to = .qc_persistence_eligible_classes()),
    consistency = list(fn = NULL, applies_to = STAT_CLASS_LEVELS),
    climatology = list(fn = qc_climatology, applies_to = STAT_CLASS_LEVELS),
    spatial = list(fn = qc_spatial, applies_to = STAT_CLASS_LEVELS),
    solar = list(fn = qc_solar, applies_to = "clear_sky_indexed")
  )
}

#' Which QC rules dispatch to a given variable
#'
#' Looks `variable` up in the dictionary and returns the ids (from
#' `qc_registry()`) of every rule whose `applies_to` includes the variable's
#' `statistical_class`, additionally excluding the `"spatial"` rule for any
#' variable whose `measurability_class` is `"model_only"` (SCOPING section
#' 7.3: a model-only field has no site truth to buddy-check against, so the
#' spatial rule can never fire for it regardless of statistical class) and
#' excluding `"solar"` for anything but a variable actually named as
#' radiation.
#'
#' @param variable A single dictionary variable name.
#' @return A character vector of rule ids.
#' @keywords internal
#' @noRd
qc_rules_for_variable <- function(variable) {
  row <- met_variable(variable)
  reg <- qc_registry()

  ids <- vapply(names(reg), function(id) {
    isTRUE(row$statistical_class %in% reg[[id]]$applies_to)
  }, logical(1))
  rules <- names(reg)[ids]

  if (isTRUE(row$measurability_class == "model_only")) {
    rules <- setdiff(rules, "spatial")
  }
  if (!(row$statistical_class == "clear_sky_indexed")) {
    rules <- setdiff(rules, "solar")
  }

  rules
}
