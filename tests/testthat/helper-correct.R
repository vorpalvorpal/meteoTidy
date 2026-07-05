# Helpers for Plans 11–13 — training summaries, skill verdicts, forecast pairs.

# A training-summary row as tier_select() consumes it: overlap length + pair
# counts per (source, variable, lead_bucket).
training_summary <- function(overlap_months = 12, n_pairs = 8760,
                             source = "openmeteo", variable = "temperature_2m",
                             lead_bucket = NA, has_archive = TRUE,
                             truth_source = "observed") {
  tibble::tibble(
    source = source, variable = variable, lead_bucket = lead_bucket,
    overlap_months = overlap_months, n_pairs = n_pairs,
    has_archive = has_archive, truth_source = truth_source
  )
}

# A skill verdict as Plan 13's skill_verdict() returns it and Plans 11/12 read.
skill_verdict <- function(promote = TRUE, shrink_weight = 1,
                          variable = "temperature_2m", lead_bucket = NA) {
  tibble::tibble(
    variable = variable, lead_bucket = lead_bucket,
    promote = promote, shrink_weight = shrink_weight,
    consistency_violation_rate = 0
  )
}

# Aligned (forecast, observation) pairs for fitting/verification. `bias_fun`
# adds a deterministic, optionally seasonal/lead-dependent bias to the forecast.
forecast_obs_pairs <- function(n = 365, seed = 1,
                               bias_fun = function(doy, lead) 0,
                               lead_hours = 24,
                               issue0 = as.POSIXct("2025-01-01", tz = "UTC")) {
  withr::local_seed(seed)
  issue <- issue0 + (seq_len(n) - 1L) * 86400
  doy <- as.integer(format(issue, "%j"))
  truth <- 15 + 10 * sin(2 * pi * doy / 365.25) + rnorm(n, 0, 1)
  fc <- truth + bias_fun(doy, lead_hours)
  tibble::tibble(
    site_id = "test", source = "openmeteo", model = "ecmwf_ifs025",
    issue_time = issue,
    valid_time = issue + lead_hours * 3600,
    lead_time = as.difftime(rep(lead_hours, n), units = "hours"),
    variable = "temperature_2m",
    forecast = fc, observation = truth
  )
}
