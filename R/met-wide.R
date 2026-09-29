#' @include met-table.R met-table-hash.R read-api.R correct-forecast.R
NULL

# Plan 15 -- the one-call meteoHazard interface (SCOPING section 10): the
# wide, Open-Meteo-named section 3.1 hourly table, wrapped as a `met_table`.
# `kind = "record"` reads the curated observation record (`met_record()`,
# Plan 14) for hindcast; `kind = "forecast"` reads the archived/corrected
# forecast (`met_forecast_archive()`, Plan 14) for prediction. Both paths
# widen to one row per timestamp with one column per variable, rename the
# time index to `time` at this outer boundary only (the canonical stores
# keep `datetime_utc`/`valid_time`), and wrap the result with provenance,
# keys, and versions.

.met_wide_schema_version <- "1.0.0"

# The exact SCOPING section 3.1 wide-contract variable set (the stable shape
# meteoHazard consumes), in contract order. Plan 15: when `variables` is not
# supplied, met_wide() emits exactly this set -- absent variables appear as
# all-NA columns rather than silently narrowing the table to whatever the
# window happened to contain.
.met31_variables <- function() {
  c(
    "temperature_2m", "relative_humidity_2m", "surface_pressure",
    "pressure_msl", "precipitation", "cloud_cover", "direct_radiation",
    "diffuse_radiation", "shortwave_radiation", "wind_speed_10m", "wind_direction_10m",
    "wind_gusts_10m", "wind_speed_80m", "wind_direction_80m",
    "wind_speed_120m", "wind_direction_120m", "wind_speed_180m",
    "wind_direction_180m", "boundary_layer_height",
    "soil_moisture_0_to_1cm", "soil_moisture_1_to_3cm"
  )
}

# The manifest has no single global "version" concept (Plan 03's calibration
# store versions each (variable, source) pair independently) -- for a
# multi-variable wide table, the most defensible single number is the
# highest version any calibration at this site has reached, or 0L if none
# exists yet (no calibration has ever been fit).
.met_wide_calibration_manifest_version <- function(store_root, site_id) {
  manifest <- tryCatch(calib_manifest(store_root, site_id), error = function(e) NULL)
  if (is.null(manifest) || nrow(manifest) == 0) {
    return(0L)
  }
  as.integer(max(manifest$version))
}

# Widen a canonical forecast tibble to one row per `valid_time`, one column
# per variable -- the forecast analogue of `widen_obs()`.
#
# DECIDED ensemble contract (post-implementation audit, see
# IMPLEMENTER_PROMPT.md item 6): the section 3.1 wide shape is one row per
# timestamp, one column per variable (SCOPING section 3.1) -- a per-member
# trajectory table already exists (`met_forecast_archive(members = TRUE)`),
# so the wide emitter does not attempt to also be one. Per (valid_time,
# variable), this takes the **ensemble mean across member rows** when
# members are present (a deterministic single row is the mean of one, so
# this is a no-op for non-ensemble sources) -- never a first-row-wins pick,
# which would silently and arbitrarily drop every member but one. Named
# `stat` summary rows (e.g. a source's own precomputed "min"/"max") are
# excluded from the mean: mixing heterogeneous stats into an unweighted
# average would be meaningless.
#
# A product that publishes a variable ONLY as distribution summaries (BOM's
# hourly rain: 10/25/50 % chance amounts, stored as p90/p75/p50 rows) has no
# deterministic row to widen; the wide column then takes the "mean" summary
# if present, else the median ("p50").
#
# `stat` (item 10): "mean" as above; "pNN" serves the NN-th percentile
# across ensemble members, else the product's own published "pNN" rows (BOM:
# p90 = the 10 % chance amount), else a deterministic value as is (a single
# run has no spread), else NA. Attribute "served" names, per variable, what
# the column holds: "mean", "pNN", "deterministic", or NA when empty.
.widen_forecast <- function(fc, variables, stat = "mean") {
  fc <- fc[fc$variable %in% variables, , drop = FALSE]
  base <- unique(fc["valid_time"])
  base <- base[order(base$valid_time), , drop = FALSE]
  prob <- if (identical(stat, "mean")) NA_real_ else as.numeric(sub("^p", "", stat)) / 100

  wide <- base
  served <- stats::setNames(rep(NA_character_, length(variables)), variables)
  for (v in variables) {
    plain <- fc[fc$variable == v & is.na(fc$stat), , drop = FALSE]
    has_members <- any(!is.na(plain$member))
    fun <- mean
    if (nrow(plain) > 0 && (is.na(prob) || has_members)) {
      sub <- plain[c("valid_time", "value")]
      if (has_members && !is.na(prob)) {
        fun <- function(x, na.rm = TRUE) unname(stats::quantile(x, prob, na.rm = na.rm))
        served[[v]] <- stat
      } else {
        served[[v]] <- if (has_members) "mean" else "deterministic"
      }
    } else {
      wanted <- if (is.na(prob)) c("mean", "p50") else stat
      sub <- plain[0, c("valid_time", "value")]
      for (st in wanted) {
        sub <- fc[fc$variable == v & fc$stat %in% st, c("valid_time", "value")]
        if (nrow(sub) > 0) {
          served[[v]] <- st
          break
        }
      }
      if (nrow(sub) == 0 && nrow(plain) > 0) {
        sub <- plain[c("valid_time", "value")]
        served[[v]] <- "deterministic"
      }
    }
    # A requested variable absent from the archive window (or an entirely
    # empty window) must yield an all-NA column, not an error --
    # stats::aggregate() aborts on zero rows ("no rows to aggregate").
    sub <- sub[!is.na(sub$value), , drop = FALSE]
    if (nrow(sub) == 0) {
      wide[[v]] <- rep(NA_real_, nrow(wide))
      served[[v]] <- NA_character_
      next
    }
    agg <- stats::aggregate(value ~ valid_time, data = sub, FUN = fun, na.rm = TRUE)
    matched <- agg$value[match(wide$valid_time, agg$valid_time)]
    wide[[v]] <- if (length(matched) == 0) rep(NA_real_, nrow(wide)) else matched
  }

  wide <- tibble::as_tibble(wide)
  attr(wide, "served") <- served
  wide
}

# Restrict an archived-forecast read to the LATEST issuance per (source,
# model). met_wide(kind = "forecast") serves "the corrected forecast for
# prediction" (SCOPING section 10): with the archive-on-every-sync policy the
# store holds every past issuance overlapping a valid window, and pooling
# them into one mean would average today's forecast with progressively
# staler ones. Older issuances remain fully retrievable via
# met_forecast_archive() -- this filter is only about what the one-call wide
# table means.
.latest_issuance <- function(fc) {
  if (nrow(fc) == 0) {
    return(fc)
  }
  grp_key <- paste(fc$source, fc$model, sep = "\r")
  keep <- rep(FALSE, nrow(fc))
  for (g in unique(grp_key)) {
    in_grp <- grp_key == g
    keep[in_grp] <- fc$issue_time[in_grp] == max(fc$issue_time[in_grp])
  }
  fc[keep, , drop = FALSE]
}

# Build `met_wide()`'s output provenance, with a real per-variable
# correction tier. Plan 17 item 1: `long` (for `kind = "forecast"`) has
# already been through `correct_forecast()`, which stamps a per-row `tier`
# column recording what was actually APPLIED -- that is the honest source of
# truth here, not a fresh manifest re-derivation (which only records what
# tier the manifest *claims*, and previously could disagree with what a
# consumer actually received). `kind = "record"` has no such `tier` column
# (`met_record()` is not itself serve-time corrected by this plan), so it
# keeps the manifest-lookup fallback:
#
#  - model-only variables (SCOPING section 7.3) have no site truth to
#    correct against and are always `"raw"`;
#  - a variable with no calibration on file yet is `"physical"` (the day-0
#    default `correct_apply()`/`correct_forecast()` itself falls back to);
#  - otherwise, `long$tier` when present (the applied tier), else the
#    highest-version manifest row's `tier` for `(variable, source)`.
.met_wide_provenance <- function(store_root, site_id, value_cols, long) {
  src <- if ("source" %in% names(long) && nrow(long) > 0) {
    long$source[match(value_cols, long$variable)]
  } else {
    rep(NA_character_, length(value_cols))
  }

  manifest <- tryCatch(calib_manifest(store_root, site_id), error = function(e) NULL)
  has_applied_tier <- "tier" %in% names(long) && nrow(long) > 0

  tier <- character(length(value_cols))
  train_overlap <- numeric(length(value_cols))
  for (i in seq_along(value_cols)) {
    v <- value_cols[[i]]
    s <- src[[i]]
    if (isTRUE(met_variable(v)$measurability_class == "model_only")) {
      tier[[i]] <- "raw"
      next
    }

    if (has_applied_tier) {
      applied <- long$tier[long$variable == v]
      tier[[i]] <- if (length(applied) == 0) "physical" else applied[[1]]
    }

    rows <- if (is.null(manifest) || nrow(manifest) == 0 || is.na(s)) {
      NULL
    } else {
      manifest[manifest$variable == v & manifest$source == s, , drop = FALSE]
    }
    if (is.null(rows) || nrow(rows) == 0) {
      if (!has_applied_tier) tier[[i]] <- "physical"
      next
    }
    current <- rows[which.max(rows$version), , drop = FALSE]
    if (!has_applied_tier) tier[[i]] <- current$tier[[1]]
    # SCOPING section 3.2: provenance carries the training-overlap length.
    # The manifest records the fit's training window; report it in hours.
    train_overlap[[i]] <- as.numeric(difftime(current$train_end[[1]],
                                              current$train_start[[1]],
                                              units = "hours"))
  }

  tibble::tibble(
    variable = value_cols,
    tier = tier,
    train_overlap = train_overlap,
    source = src
  )
}

#' The section 3.1 wide emitter -- the one-call meteoHazard interface
#'
#' Returns the wide, Open-Meteo-named hourly table (SCOPING section 3.1):
#' one row per timestamp, one column per variable, canonical units, `time`
#' in UTC -- wrapped as a [new_met_table()] carrying provenance, keys, and
#' versions. `kind = "record"` reads the curated observation record
#' ([met_record()]) for hindcast; `kind = "forecast"` reads the archived
#' forecast ([met_forecast_archive()]) for prediction.
#'
#' For `kind = "forecast"`, **one source** is served, and **each variable
#' comes from one model** -- never a mean across sources or models (which
#' would blend, say, an Open-Meteo run, an ECMWF ensemble and BOM's edited
#' forecast into a product nobody issued). `source` chooses the source: by
#' default the archive's only source, or `"openmeteo"` when there are
#' several. `model` is a precedence list: each variable is served from the
#' first listed model that has any value for it in the window (no single
#' Open-Meteo model has every variable -- ECMWF IFS has no boundary-layer
#' height or soil moisture). By default it is the source's only model, or
#' else `"ecmwf_ifs025"`, `"gfs_global"`, `"icon_global"`, `"best_match"`,
#' `"icon_seamless"`, `"hourly"` in that order. A model whose latest
#' archived run is more than a day (option `meteoTidy.wide_stale_hours`,
#' default 24) behind the newest run of the other listed models -- one no
#' longer fetched, such as `best_match` after the move to named models --
#' drops behind every current model, so it only serves variables no current
#' model has; named alone it is served as is. [met_provenance()] records
#' the `model` and `stat` behind each column. Of each model, only the
#' **latest archived issuance** is served: the archive holds every past
#' issuance overlapping the window (SCOPING section 9's archive-on-every-sync
#' policy); per-member trajectories and older issuances remain available via
#' [met_forecast_archive()].
#'
#' `stat` picks the statistic. `"mean"` (default): the ensemble mean across
#' members, a deterministic run's value as is, and for a variable published
#' only as quantiles (BOM's hourly rain) its median (`p50`). `"pNN"` (e.g.
#' `"p95"`; `"median"` = `"p50"`): the NN-th percentile across ensemble
#' members, or the product's own published percentile (BOM publishes rain
#' `p50`, `p75` and `p90`, i.e. the 50/25/10 % chance amounts), or a
#' deterministic value as is (provenance `stat = "deterministic"`); `NA` if
#' the product has none of those.
#'
#' meteoHazard contract: `wind_gusts_10m` is never below `wind_speed_10m`
#' in any row (gusts are raised to the mean wind where a product's own
#' values disagree), and `shortwave_radiation` is derived as
#' `direct_radiation + diffuse_radiation` when not archived itself.
#'
#' @param site A single `met_site` (or a `met_sites` of length one) -- the
#'   wide table is a per-site product.
#' @param window A list with `from`/`to` UTC POSIXct bounds.
#' @param kind Either `"forecast"` or `"record"` (default `"forecast"`).
#' @param variables Optional character vector of variable names. Every named
#'   variable appears as a column even if absent from the underlying data
#'   (an all-`NA` column) -- the stable section 3.1 shape. Defaults to the
#'   full section 3.1 contract set (see SCOPING section 3.1).
#' @param source,model For `kind = "forecast"`: the archived source, and
#'   the model precedence list (a single model, or several: each variable
#'   comes from the first that has it). `NULL` picks the default (see
#'   Details).
#' @param stat For `kind = "forecast"`: `"mean"` (default), `"median"`, or
#'   a percentile `"p1"`..`"p99"` (see Details).
#' @param now Injectable current time; see `.now()`.
#' @return A `met_table`.
#' @family met-table
#' @export
#' @examples
#' \dontrun{
#' met_wide(site, window = list(from = as.POSIXct("2026-01-01", tz = "UTC"),
#'                              to = as.POSIXct("2026-01-02", tz = "UTC")),
#'         kind = "record")
#' }
met_wide <- function(site, window, kind = c("forecast", "record"), variables = NULL,
                     now = .now(), source = NULL, model = NULL, stat = "mean") {
  kind <- rlang::arg_match(kind)
  stat <- .wide_stat(stat)
  sites <- as_met_sites(site)
  if (length(sites@sites) != 1) {
    abort_meteo(
      c(
        "met_wide() builds a per-site table; {.arg site} has {length(sites@sites)} sites.",
        "i" = "Call it once per site (the wide table has no site_id column)."
      ),
      class = "multi_site_wide"
    )
  }
  s <- sites@sites[[1]]
  value_cols <- variables %||% .met31_variables()

  if (kind == "record") {
    long <- met_record(site, variables = variables, from = window$from, to = window$to)
    wide <- widen_obs(long, variables = value_cols)
    names(wide)[names(wide) == "datetime_utc"] <- "time"
    wide$site_id <- NULL
  } else {
    long <- met_forecast_archive(site, valid_from = window$from, valid_to = window$to)
    long <- .select_source_model(long, source, model)
    precedence <- attr(long, "precedence") %||% character(0)
    long <- .latest_issuance(long)
    long <- correct_forecast(site_store_root(s), s, long, now = now)
    wide <- .widen_forecast_by_precedence(long, value_cols, precedence, stat)
    served <- attr(wide, "served")
    # Provenance describes the rows actually served: per variable, its model.
    used <- paste(long$variable, long$model, sep = "\r") %in%
      paste(served$variable, served$model, sep = "\r")
    long <- long[used, , drop = FALSE]
    names(wide)[names(wide) == "valid_time"] <- "time"
  }

  wide <- .wide_hazard_contract(wide)
  attr(wide$time, "tzone") <- "UTC"

  provenance <- .met_wide_provenance(site_store_root(s), site_id(s), value_cols, long)
  if (kind == "forecast") {
    provenance$model <- served$model[match(provenance$variable, served$variable)]
    provenance$stat <- served$stat[match(provenance$variable, served$variable)]
  }
  keys <- list(site_id = site_id(s), from = window$from, to = window$to)
  versions <- list(
    schema_version = .met_wide_schema_version,
    calibration_manifest_version = .met_wide_calibration_manifest_version(
      site_store_root(s), site_id(s)
    )
  )

  new_met_table(wide, provenance = provenance, keys = keys, versions = versions)
}

# Restrict an archive read to ONE (source, model) -- problem 8 of the
# production review: met_wide() used to average every source and model in
# the window together. Defaults are deterministic (see met_wide() docs).
.select_source_model <- function(fc, source, model) {
  if (nrow(fc) == 0) {
    return(fc)
  }
  pick <- function(have, wanted, preferred, what) {
    if (!is.null(wanted)) {
      if (!wanted %in% have) {
        abort_meteo(
          c(
            "No archived forecast from {what} {.val {wanted}} in this window.",
            "i" = "Available: {.val {have}}."
          ),
          class = "wide_source_unavailable"
        )
      }
      return(wanted)
    }
    if (length(have) == 1) {
      return(have)
    }
    hit <- intersect(preferred, have)
    if (length(hit) > 0) {
      return(hit[[1]])
    }
    abort_meteo(
      c(
        "Several {what}s are archived for this window; choose one.",
        "i" = "Available: {.val {have}}. Pass {.arg {what}} to {.fn met_wide}."
      ),
      class = "wide_source_unavailable"
    )
  }
  src <- pick(sort(unique(fc$source)), source, "openmeteo", "source")
  fc <- fc[fc$source == src, , drop = FALSE]
  fc$model <- ifelse(is.na(fc$model), "", fc$model)
  mdl_have <- sort(unique(fc$model))
  precedence <- if (!is.null(model)) {
    hit <- intersect(model, mdl_have)
    if (length(hit) == 0) {
      abort_meteo(
        c(
          "No archived forecast from model{?s} {.val {model}} in this window.",
          "i" = "Available: {.val {mdl_have}}."
        ),
        class = "wide_source_unavailable"
      )
    }
    hit
  } else if (length(mdl_have) == 1) {
    mdl_have
  } else {
    hit <- intersect(.wide_default_model_precedence(), mdl_have)
    if (length(hit) == 0) {
      abort_meteo(
        c(
          "Several models are archived for this window; choose one.",
          "i" = "Available: {.val {mdl_have}}. Pass {.arg model} to {.fn met_wide}."
        ),
        class = "wide_source_unavailable"
      )
    }
    hit
  }
  fc <- fc[fc$model %in% precedence, , drop = FALSE]
  attr(fc, "precedence") <- .demote_stale_models(fc, precedence)
  fc
}

# A model whose latest archived run is more than `stale_hours` behind the
# newest run of any model in the precedence has stopped being fetched (e.g.
# best_match after the move to named models, a retired model, a failing
# feed): it moves behind every current model, so it only serves a variable
# no current model has. Models run every 6-12 h and publish at different
# delays, so a gap under a day is normal and does not reorder anything.
.demote_stale_models <- function(fc, precedence,
                                 stale_hours = getOption("meteoTidy.wide_stale_hours", 24)) {
  if (length(precedence) < 2 || nrow(fc) == 0) {
    return(precedence)
  }
  latest <- vapply(precedence, function(m) {
    it <- fc$issue_time[fc$model == m]
    if (length(it)) as.numeric(max(it)) else NA_real_
  }, numeric(1))
  stale <- !is.na(latest) & latest < max(latest, na.rm = TRUE) - stale_hours * 3600
  c(precedence[!stale], precedence[stale])
}

# met_wide()'s default model precedence when a source has several:
# Open-Meteo's default named deterministic models (ECMWF IFS, GFS, ICON --
# item 6), then best_match (no verifiable run time; a store upgraded from
# before item 6 still holds its last runs, which must not outrank the named
# models even while less than a day old), the default ensembles, then BOM's
# hourly forecast. Each variable is served from the first of these that has
# it (item 10).
.wide_default_model_precedence <- function() {
  unique(c(.openmeteo_default_models("forecast"), "best_match",
           .openmeteo_default_models("ensemble"), "hourly"))
}

# Validate met_wide()'s `stat`: "mean", "median" (= "p50") or "p1".."p99".
.wide_stat <- function(stat) {
  if (!is.character(stat) || length(stat) != 1 || is.na(stat)) {
    stat <- "?"
  }
  if (identical(stat, "median")) {
    stat <- "p50"
  }
  if (!identical(stat, "mean") && !grepl("^p([1-9]|[1-9][0-9])$", stat)) {
    abort_meteo(
      c(
        "{.arg stat} must be {.val mean}, {.val median} or a percentile {.val p1}..{.val p99}.",
        "x" = "Got {.val {stat}}."
      ),
      class = "bad_wide_stat"
    )
  }
  stat
}

# Widen with per-variable model precedence (item 10): each variable comes
# from the first model in `precedence` with any value in the window -- one
# model per variable, never a blend across models. Returns the wide tibble
# with attribute "served": a tibble (variable, model, stat) recording what
# each column actually holds.
.widen_forecast_by_precedence <- function(fc, variables, precedence, stat = "mean") {
  base <- sort(unique(fc$valid_time))
  wide <- tibble::tibble(valid_time = base)
  served <- tibble::tibble(variable = variables, model = NA_character_, stat = NA_character_)
  for (i in seq_along(variables)) {
    v <- variables[[i]]
    wide[[v]] <- rep(NA_real_, length(base))
    for (m in precedence) {
      w <- .widen_forecast(fc[fc$model == m & fc$variable == v, , drop = FALSE], v, stat)
      vals <- w[[v]][match(base, w$valid_time)]
      if (length(vals) > 0 && any(!is.na(vals))) {
        wide[[v]] <- vals
        served$model[[i]] <- m
        served$stat[[i]] <- attr(w, "served")[[v]]
        break
      }
    }
  }
  attr(wide, "served") <- served
  wide
}

# Post-widening guarantees meteoHazard relies on.
.wide_hazard_contract <- function(wide) {
  # Gusts can never be below the mean wind; a product (or the member mean of
  # an ensemble, or a correction) can still produce gust < wind, which makes
  # meteoHazard::dust_hazard() abort.
  if (all(c("wind_gusts_10m", "wind_speed_10m") %in% names(wide))) {
    low <- !is.na(wide$wind_gusts_10m) & !is.na(wide$wind_speed_10m) &
      wide$wind_gusts_10m < wide$wind_speed_10m
    wide$wind_gusts_10m[low] <- wide$wind_speed_10m[low]
  }
  # Global horizontal = direct + diffuse, for litter_risk(use_wetness_state = TRUE).
  if (all(c("shortwave_radiation", "direct_radiation", "diffuse_radiation") %in% names(wide))) {
    gap <- is.na(wide$shortwave_radiation)
    wide$shortwave_radiation[gap] <- wide$direct_radiation[gap] + wide$diffuse_radiation[gap]
  }
  wide
}
