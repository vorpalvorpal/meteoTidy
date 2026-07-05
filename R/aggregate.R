# Plan 10 -- native-resolution -> hourly, and hourly -> local-day daily
# aggregation (SCOPING sections 3 and 6).

# The minimum fraction of expected native-cadence records an hour must have
# (among qc_flag == "ok" rows) to be aggregated at all; below this the hour
# is left missing for the fill tiers to handle rather than aggregated from a
# too-sparse sample. SCOPING section 6 documents "a documented completeness
# threshold (default >= 75%)"; not pinned more precisely by any test beyond
# "6 of 6 aggregates, 2 of 6 does not", so 75% is used as written.
.hourly_completeness_threshold <- function() {
  0.75
}

# The expected number of native-cadence samples per hour, inferred from the
# input's own modal sampling interval (the smallest common gap between
# consecutive timestamps of the same variable) rather than a hard-coded
# constant, so this works for both 10-minute logger data and any other
# native cadence.
.expected_per_hour <- function(datetime_utc) {
  ord <- sort(unique(datetime_utc))
  if (length(ord) < 2) {
    return(1L)
  }
  gaps <- as.numeric(diff(ord), units = "secs")
  modal_gap <- as.numeric(names(sort(table(gaps), decreasing = TRUE))[1])
  if (is.na(modal_gap) || modal_gap <= 0) {
    return(1L)
  }
  max(1L, round(3600 / modal_gap))
}

# Vector (circular) mean of a set of angles (degrees): convert to unit
# vectors, average the components, recombine via atan2(), wrap to [0, 360).
.circular_mean <- function(angles_deg) {
  if (length(angles_deg) == 0 || all(is.na(angles_deg))) {
    return(NA_real_)
  }
  rad <- angles_deg * pi / 180
  s <- mean(sin(rad), na.rm = TRUE)
  co <- mean(cos(rad), na.rm = TRUE)
  (atan2(s, co) * 180 / pi + 360) %% 360
}

# Aggregate one variable's native-resolution rows (already filtered to
# qc_flag == "ok") within one (site_id, hour) bucket to a single value,
# dispatched by statistical_class.
.aggregate_value <- function(values, stat_class) {
  if (isTRUE(stat_class == "intermittent")) {
    sum(values, na.rm = TRUE)
  } else if (isTRUE(stat_class == "circular")) {
    .circular_mean(values)
  } else {
    mean(values, na.rm = TRUE)
  }
}

#' Aggregate native-resolution observations to hourly
#'
#' Aggregates `obs` (native-resolution rows, e.g. 10-minute logger data) up
#' to one row per `(site_id, variable, hour)`, dispatched by the variable's
#' `statistical_class` (SCOPING section 6): mean for `linear`/`bounded`/
#' `clear_sky_indexed` (radiation, already a rate in W/m^2), a **vector
#' (sin/cos) mean** for `circular` (wind direction -- never a plain numeric
#' mean, which would fail across the 0/360 wrap), and sum for `intermittent`
#' (rainfall). Only `qc_flag == "ok"` rows contribute. An hour with fewer
#' than `.hourly_completeness_threshold()` (default 75%) of its expected
#' native-cadence records (inferred from the input's own modal sampling
#' interval) is left out of the aggregated output entirely, so the fill
#' tiers can treat it as a gap rather than aggregating from a too-sparse
#' sample.
#'
#' @param obs A canonical long obs tibble at native resolution.
#' @param dict The variable dictionary.
#' @return A canonical long obs tibble, one row per `(site_id, variable,
#'   hour)` that met the completeness threshold, `method = "aggregated"`.
#' @keywords internal
#' @noRd
aggregate_hourly <- function(obs, dict = met_variables()) {
  if (nrow(obs) == 0) {
    return(obs)
  }

  hour <- as.POSIXct(trunc(obs$datetime_utc, "hours"), tz = "UTC")
  variables <- unique(obs$variable)

  out_rows <- list()
  for (variable in variables) {
    var_obs <- obs[obs$variable == variable, , drop = FALSE]
    var_hour <- hour[obs$variable == variable]
    expected <- .expected_per_hour(var_obs$datetime_utc)
    stat_class <- dict$statistical_class[match(variable, dict$variable)]

    for (site_id in unique(var_obs$site_id)) {
      site_mask <- var_obs$site_id == site_id
      for (h in unique(var_hour[site_mask])) {
        bucket_mask <- site_mask & var_hour == h
        bucket <- var_obs[bucket_mask, , drop = FALSE]
        ok <- bucket[bucket$qc_flag == "ok" & !is.na(bucket$value), , drop = FALSE]

        completeness <- nrow(ok) / expected
        if (completeness < .hourly_completeness_threshold()) {
          next
        }

        value <- .aggregate_value(ok$value, stat_class)
        out_rows[[length(out_rows) + 1]] <- tibble::tibble(
          site_id = site_id,
          datetime_utc = h,
          variable = variable,
          value = value,
          source = ok$source[1],
          method = "aggregated",
          qc_flag = "ok"
        )
      }
    }
  }

  if (length(out_rows) == 0) {
    return(obs[0, , drop = FALSE])
  }
  tibble::as_tibble(do.call(rbind, out_rows))
}

# ---- hourly -> daily --------------------------------------------------------

# Table-driven day-window convention per variable (SCOPING section 3: "do not
# hard-code one convention across variables"). `"rain_day"` is SILO's
# documented 24h-to-9am convention (rainfall accumulated in the 24 h ending
# at 9am local clock time is assigned to the day of observation -- see SILO's
# climate-variables documentation, https://www.longpaddock.qld.gov.au/silo/
# about/climate-variables/, and `.silo_day_to_utc_instant()` in
# R/source-silo.R, which maps the same convention at ingest). Every other
# variable defaults to `"calendar_day"` (local midnight to midnight) -- SILO
# reports temperature min/max on essentially the same station-day, but absent
# a test pinning a different (e.g. 9am-to-9am) convention for temperature,
# the plain local calendar day is the simplest defensible default (plans/
# README's ambiguity order, (c)).
.daily_window_convention <- function(variable) {
  if (identical(variable, "precipitation")) "rain_day" else "calendar_day"
}

# The "day" (a Date) each `datetime_utc` belongs to, for a given convention,
# in the site's local timezone. `"rain_day"` shifts local time back 9 hours
# before taking the calendar date, so the 9am boundary becomes the day
# change -- this correctly inherits the DST offset AT EACH TIMESTAMP's own
# local conversion (rather than computing a single static UTC window), so a
# DST-transition day naturally becomes a 23-/25-hour day by construction
# (SCOPING section 3).
.assign_day <- function(datetime_utc, timezone, convention) {
  local <- as.POSIXct(format(datetime_utc, tz = timezone), tz = timezone)
  shifted <- if (identical(convention, "rain_day")) local - 9 * 3600 else local
  as.Date(format(shifted, tz = timezone))
}

#' Aggregate hourly observations to daily on the local-day boundary
#'
#' Aggregates `obs_hourly` up to one row per `(site_id, variable, day)`,
#' where "day" is assigned per a **table-driven, per-variable** local-clock-
#' time convention (`.daily_window_convention()`) matching SILO's documented
#' day windows (SCOPING section 3): `precipitation` uses the 24h-to-9am
#' rain-day (summed), matching SILO's convention exactly, including at DST
#' transitions (a 23-/25-hour day by construction, since the boundary is
#' computed from each timestamp's own local-clock conversion, not a fixed UTC
#' span); every other variable defaults to the local calendar day (mean,
#' except `intermittent` classes which still sum).
#'
#' @param obs_hourly A canonical long obs tibble at hourly resolution (e.g.
#'   `aggregate_hourly()`'s output).
#' @param site A `met_site` object (supplies the IANA timezone).
#' @return A canonical long obs tibble, one row per `(site_id, variable,
#'   day)`, `datetime_utc` set to local midnight (UTC-labelled instant) of
#'   the assigned day, `method = "aggregated"`.
#' @keywords internal
#' @noRd
aggregate_daily <- function(obs_hourly, site) {
  if (nrow(obs_hourly) == 0) {
    return(obs_hourly)
  }
  dict <- met_variables()
  tz <- site@timezone

  variables <- unique(obs_hourly$variable)
  out_rows <- list()

  for (variable in variables) {
    var_obs <- obs_hourly[obs_hourly$variable == variable, , drop = FALSE]
    convention <- .daily_window_convention(variable)
    day <- .assign_day(var_obs$datetime_utc, tz, convention)
    stat_class <- dict$statistical_class[match(variable, dict$variable)]
    if (is.na(stat_class)) stat_class <- "linear"

    for (site_id in unique(var_obs$site_id)) {
      site_mask <- var_obs$site_id == site_id
      for (d in unique(day[site_mask])) {
        bucket <- var_obs[site_mask & day == d & var_obs$qc_flag == "ok" &
                            !is.na(var_obs$value), , drop = FALSE]
        if (nrow(bucket) == 0) next

        value <- .aggregate_value(bucket$value, stat_class)
        day_label <- as.character(as.Date(d, origin = "1970-01-01"))
        day_instant <- as.POSIXct(paste(day_label, "00:00:00"), tz = tz)
        attr(day_instant, "tzone") <- "UTC"

        out_rows[[length(out_rows) + 1]] <- tibble::tibble(
          site_id = site_id,
          datetime_utc = day_instant,
          variable = variable,
          value = value,
          source = bucket$source[1],
          method = "aggregated",
          qc_flag = "ok"
        )
      }
    }
  }

  if (length(out_rows) == 0) {
    return(obs_hourly[0, , drop = FALSE])
  }
  tibble::as_tibble(do.call(rbind, out_rows))
}
