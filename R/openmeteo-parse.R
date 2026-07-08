# Plan 05 — Open-Meteo response parsing: response -> canonical obs/forecast.
# Pure functions (no IO), testable independently of HTTP.

# Parse Open-Meteo's naive ISO timestamps ("2020-01-01T00:00" for hourly,
# "2026-01-02" for daily/bare dates) as UTC. Open-Meteo returns local-ish
# naive strings; per the frozen tests we treat every timestamp as UTC
# directly (no further tz conversion).
.openmeteo_parse_time <- function(x) {
  x <- as.character(unlist(x, use.names = FALSE))
  # Try datetime first, then bare date; per-element so a vector's shape never
  # forces one format onto every element (mirrors the .parse_one_timestamp
  # caution in R/adapter-mapping.R).
  out <- as.POSIXct(rep(NA_character_, length(x)), tz = "UTC")
  has_time <- grepl("T", x, fixed = TRUE)
  if (any(has_time)) {
    out[has_time] <- as.POSIXct(x[has_time], format = "%Y-%m-%dT%H:%M", tz = "UTC")
  }
  if (any(!has_time)) {
    out[!has_time] <- as.POSIXct(x[!has_time], format = "%Y-%m-%d", tz = "UTC")
  }
  out
}

# Pull the named block ("hourly" or "daily") out of a parsed Open-Meteo body.
.openmeteo_get_block <- function(body, block) {
  out <- body[[block]]
  if (is.null(out)) {
    abort_meteo(
      "Open-Meteo response has no {.field {block}} block.",
      class = "openmeteo_bad_response"
    )
  }
  out
}

# Column names in a block other than "time".
.openmeteo_value_columns <- function(block_data) {
  setdiff(names(block_data), "time")
}

# ---- observation products (fetch()) ----------------------------------------

#' Parse an Open-Meteo observation-shaped response (Historical Weather/ERA5)
#'
#' @param body Parsed JSON body (a list).
#' @param site A `met_site`.
#' @param variables Requested dictionary variable names.
#' @param source_id Stamped into `source`.
#' @return A canonical long observation tibble (validated via `new_obs()`).
#' @keywords internal
#' @noRd
.openmeteo_parse_obs <- function(body, site, variables, source_id) {
  block <- .openmeteo_get_block(body, "hourly")
  time_utc <- .openmeteo_parse_time(block$time)

  rows <- lapply(variables, function(v) {
    raw <- block[[v]]
    if (is.null(raw)) {
      return(tibble::tibble(
        site_id = character(0),
        datetime_utc = as.POSIXct(character(0), tz = "UTC"),
        variable = character(0),
        value = double(0),
        source = character(0),
        method = character(0),
        qc_flag = character(0)
      ))
    }
    raw_values <- as.double(unlist(raw, use.names = FALSE))
    source_unit <- body$hourly_units[[v]] %||% canonical_unit(v)
    canonical <- to_canonical(raw_values, from = source_unit, variable = v)
    tibble::tibble(
      site_id = site_id(site),
      datetime_utc = time_utc,
      variable = v,
      value = as.double(units::drop_units(canonical)),
      source = source_id,
      method = "model_fill",
      qc_flag = "ok"
    )
  })

  out <- vctrs::vec_rbind(!!!rows)
  new_obs(out)
}

# ---- forecast products (fetch_forecast()) ----------------------------------

# Split a member-suffixed column name into its base variable and integer
# member id, e.g. "temperature_2m_member01" -> list(variable="temperature_2m",
# member=1L). Returns NULL if `col` does not match the member-suffix pattern.
.openmeteo_split_member_col <- function(col) {
  m <- regmatches(col, regexec("^(.*)_member([0-9]+)$", col))[[1]]
  if (length(m) == 0) {
    return(NULL)
  }
  list(variable = m[2], member = as.integer(m[3]))
}

# Split a previous-runs-suffixed column name, e.g.
# "temperature_2m_previous_day1" -> list(variable="temperature_2m", day=1L).
.openmeteo_split_previous_day_col <- function(col) {
  m <- regmatches(col, regexec("^(.*)_previous_day([0-9]+)$", col))[[1]]
  if (length(m) == 0) {
    return(NULL)
  }
  list(variable = m[2], day = as.integer(m[3]))
}

# Split a seasonal summary-suffixed column name, e.g.
# "temperature_2m_mean" -> list(variable="temperature_2m", stat="mean").
.openmeteo_seasonal_stat_suffixes <- function() c("mean", "p10", "p50", "p90")

.openmeteo_split_stat_col <- function(col) {
  for (stat in .openmeteo_seasonal_stat_suffixes()) {
    suffix <- paste0("_", stat)
    if (endsWith(col, suffix)) {
      return(list(variable = substring(col, 1, nchar(col) - nchar(suffix)), stat = stat))
    }
  }
  NULL
}

# The seasonal EC46/SEAS5 splice boundary (SCOPING §5.2): rows with
# lead_days <= this constant are attributed to EC46, rows beyond it to SEAS5.
# Never the literal spliced product name "seasonal".
.OPENMETEO_SEASONAL_SPLICE_DAYS <- 46

.openmeteo_seasonal_model_for_lead <- function(lead_days) {
  ifelse(lead_days <= .OPENMETEO_SEASONAL_SPLICE_DAYS, "ec46", "seas5")
}

# Build one forecast tibble for a base variable's plain (unsuffixed) column:
# deterministic single-valued forecast -- member/stat both NA.
.openmeteo_forecast_rows <- function(site, source_id, model, issue_time,
                                     valid_time, variable, value, lead_time,
                                     member = NA_integer_, stat = NA_character_) {
  n <- length(value)
  tibble::tibble(
    site_id = site_id(site),
    source = source_id,
    model = model,
    issue_time = issue_time,
    valid_time = valid_time,
    lead_time = lead_time,
    member = as.integer(rep(member, length.out = n)),
    stat = as.character(rep(stat, length.out = n)),
    variable = variable,
    value = as.double(value)
  )
}

# Convert raw values to canonical units for one variable, given the source
# unit reported in the block's `_units` companion (or the block's own name if
# it's a suffixed column whose base unit is reported under the base name).
.openmeteo_to_canonical_values <- function(raw, variable, units_block) {
  source_unit <- units_block[[variable]] %||% canonical_unit(variable)
  canonical <- to_canonical(as.double(unlist(raw, use.names = FALSE)),
                            from = source_unit, variable = variable)
  as.double(units::drop_units(canonical))
}

# **forecast** (deterministic) and **historical_forecast** (shortest-lead
# proxy) share the same plain-column shape; they differ only in how
# issue_time/lead_time are set (the caller passes lead_time = NA for
# historical_forecast).
.openmeteo_parse_plain_forecast <- function(body, site, variables, source_id, model,
                                            issue_time, now, lead_is_na) {
  block <- .openmeteo_get_block(body, "hourly")
  units_block <- body$hourly_units %||% list()
  time_utc <- .openmeteo_parse_time(block$time)

  rows <- lapply(variables, function(v) {
    raw <- block[[v]]
    if (is.null(raw)) {
      return(NULL)
    }
    value <- .openmeteo_to_canonical_values(raw, v, units_block)
    lead_time <- if (lead_is_na) {
      as.difftime(rep(NA_real_, length(value)), units = "hours")
    } else {
      as.difftime(as.numeric(difftime(time_utc, issue_time, units = "hours")), units = "hours")
    }
    .openmeteo_forecast_rows(
      site, source_id, model,
      issue_time = rep(issue_time, length(value)),
      valid_time = time_utc,
      variable = v, value = value, lead_time = lead_time
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  vctrs::vec_rbind(!!!rows)
}

#' Parse an Open-Meteo deterministic forecast response
#' @keywords internal
#' @noRd
.openmeteo_parse_forecast <- function(body, site, variables, source_id, model, now) {
  new_forecast(.openmeteo_parse_plain_forecast(
    body, site, variables, source_id, model,
    issue_time = now, now = now, lead_is_na = FALSE
  ))
}

#' Parse an Open-Meteo Historical Forecast (shortest-lead proxy) response
#' @keywords internal
#' @noRd
.openmeteo_parse_historical_forecast <- function(body, site, variables, source_id, model, now) {
  new_forecast(.openmeteo_parse_plain_forecast(
    body, site, variables, source_id, model,
    issue_time = now, now = now, lead_is_na = TRUE
  ))
}

#' Parse an Open-Meteo Ensemble response
#' @keywords internal
#' @noRd
.openmeteo_parse_ensemble <- function(body, site, variables, source_id, model, now) {
  block <- .openmeteo_get_block(body, "hourly")
  units_block <- body$hourly_units %||% list()
  time_utc <- .openmeteo_parse_time(block$time)
  value_cols <- .openmeteo_value_columns(block)

  rows <- lapply(value_cols, function(col) {
    split <- .openmeteo_split_member_col(col)
    if (is.null(split) || !(split$variable %in% variables)) {
      return(NULL)
    }
    value <- .openmeteo_to_canonical_values(block[[col]], split$variable, units_block)
    lead_time <- as.difftime(as.numeric(difftime(time_utc, now, units = "hours")), units = "hours")
    .openmeteo_forecast_rows(
      site, source_id, model,
      issue_time = rep(now, length(value)),
      valid_time = time_utc,
      variable = split$variable, value = value, lead_time = lead_time,
      member = split$member, stat = NA_character_
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  new_forecast(vctrs::vec_rbind(!!!rows))
}

#' Parse an Open-Meteo Previous Runs response (daily-lead training pairs)
#' @keywords internal
#' @noRd
.openmeteo_parse_previous_runs <- function(body, site, variables, source_id, model, now) {
  block <- .openmeteo_get_block(body, "hourly")
  units_block <- body$hourly_units %||% list()
  time_utc <- .openmeteo_parse_time(block$time)
  value_cols <- .openmeteo_value_columns(block)

  rows <- lapply(value_cols, function(col) {
    split <- .openmeteo_split_previous_day_col(col)
    if (is.null(split) || !(split$variable %in% variables)) {
      return(NULL)
    }
    value <- .openmeteo_to_canonical_values(block[[col]], split$variable, units_block)
    # Whole-day lead granularity (SCOPING §7.2): the issue time is `day`
    # whole days before each valid_time, so lead_time is exactly `day` days.
    issue_time <- time_utc - as.difftime(split$day, units = "days")
    lead_time <- as.difftime(rep(as.numeric(split$day), length(value)), units = "days")
    .openmeteo_forecast_rows(
      site, source_id, model,
      issue_time = issue_time,
      valid_time = time_utc,
      variable = split$variable, value = value, lead_time = lead_time
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  new_forecast(vctrs::vec_rbind(!!!rows))
}

#' Parse an Open-Meteo Seasonal response (EC46 + SEAS5 splice)
#' @keywords internal
#' @noRd
.openmeteo_parse_seasonal <- function(body, site, variables, source_id, now) {
  block <- .openmeteo_get_block(body, "daily")
  units_block <- body$daily_units %||% list()
  time_utc <- .openmeteo_parse_time(block$time)
  value_cols <- .openmeteo_value_columns(block)
  lead_days <- as.numeric(difftime(time_utc, now, units = "days"))
  model_for_row <- .openmeteo_seasonal_model_for_lead(lead_days)
  lead_time <- as.difftime(lead_days, units = "days")

  rows <- lapply(value_cols, function(col) {
    member_split <- .openmeteo_split_member_col(col)
    stat_split <- if (is.null(member_split)) .openmeteo_split_stat_col(col) else NULL

    if (!is.null(member_split) && member_split$variable %in% variables) {
      value <- .openmeteo_to_canonical_values(block[[col]], member_split$variable, units_block)
      return(.openmeteo_forecast_rows(
        site, source_id, model_for_row,
        issue_time = rep(now, length(value)),
        valid_time = time_utc,
        variable = member_split$variable, value = value, lead_time = lead_time,
        member = member_split$member, stat = NA_character_
      ))
    }
    if (!is.null(stat_split) && stat_split$variable %in% variables) {
      value <- .openmeteo_to_canonical_values(block[[col]], stat_split$variable, units_block)
      return(.openmeteo_forecast_rows(
        site, source_id, model_for_row,
        issue_time = rep(now, length(value)),
        valid_time = time_utc,
        variable = stat_split$variable, value = value, lead_time = lead_time,
        member = NA_integer_, stat = stat_split$stat
      ))
    }
    NULL
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  new_forecast(vctrs::vec_rbind(!!!rows))
}
