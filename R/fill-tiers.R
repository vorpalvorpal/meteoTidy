# Plan 10 -- micro/medium/macro fill tiers (SCOPING section 6).
#
# `fill_micro()` is the low-level, per-variable-treatment function: given a
# canonical long obs tibble (one or more variables, one or more gaps already
# present as NA/`missing`-flagged rows), it dispatches each variable to the
# correct statistical-space treatment (R/fill-treatments.R) and fills EVERY
# gap it can, regardless of length. `fill_tier()` is the tier ROUTER: given a
# gap's length and the variable's statistical/measurability class, it decides
# whether micro is even the right tier, or whether the gap should instead be
# routed to the medium (donor) or macro (model) tier.
#
# This split matters because test-fill-treatments.R exercises `fill_micro()`
# directly on every statistical class (including rain and direction) and
# expects each to be handled in its correct space, while test-fill-tiers.R
# exercises `fill_tier()`'s ROUTING decision and expects rain to never be
# micro-routed regardless of gap length (intermittent variables have no
# smooth "short gap" concept -- a dry vs wet transition can happen at any
# timescale, so length-based interpolation eligibility does not apply to
# them at all; they always go to donor/model).

# Micro-eligible tier routing: which statistical classes are considered for
# the micro (interpolation) tier at all, by ROUTING (not by what
# fill_micro() itself can technically handle). `intermittent` (rain) is
# excluded per SCOPING section 6 / test-fill-tiers.R: rain is never
# micro-interpolated, no matter how short the gap, since even a short gap can
# span a genuine occurrence transition. Circular (direction) IS included:
# fill_micro()'s vector (sin/cos) interpolation is wrap-safe and well-behaved
# at any short length, consistent with test-fill-treatments.R exercising it
# directly via fill_micro().
.micro_eligible_classes <- function() {
  c("linear", "bounded", "circular", "clear_sky_indexed")
}

# The micro-tier gap-length ceiling: SCOPING section 6 says "2-3 h"; not
# pinned more precisely by any test, so 3 hours is chosen as the generous end
# of that documented range (a 2 h gap in the tests must qualify; nothing
# tests the boundary itself).
.micro_max_gap_hours <- function() {
  3
}

# ---- fill_micro(): per-variable treatment dispatch --------------------------

#' Fill gaps using the per-variable statistical treatment (micro tier)
#'
#' The low-level per-variable treatment dispatcher: for every variable
#' present in `obs`, fills any NA/gap rows using the treatment appropriate to
#' its dictionary `statistical_class` (dewpoint-space for relative humidity,
#' vector interpolation for circular/direction, occurrence+amount for
#' intermittent/rain, clear-sky-index for solar, plain interpolation for
#' linear/bounded). Unlike `fill_tier()`, this function does not consider gap
#' length or make a tier-routing decision -- it fills every eligible gap it
#' is given, in the correct space. Callers that want the length/class-aware
#' ROUTING decision (e.g. "should this gap even go to micro") should use
#' `fill_tier()`.
#'
#' @param obs A canonical long obs tibble, one or more variables, one or more
#'   sites.
#' @param dict The variable dictionary (see [met_variables()]).
#' @param site A `met_site` object, required only if `obs` contains a
#'   `clear_sky_indexed` variable (needed to compute clear-sky irradiance).
#'   `NULL` (default) otherwise.
#' @return `obs` with every fillable gap filled: `value` no longer `NA`,
#'   `qc_flag` set to `"ok"`, `method` set to `"imputed"` on filled rows.
#' @keywords internal
#' @noRd
fill_micro <- function(obs, dict = met_variables(), site = NULL) {
  variables <- unique(obs$variable)
  temp_obs <- obs[obs$variable == "temperature_2m", , drop = FALSE]

  out <- obs[0, , drop = FALSE]
  for (variable in variables) {
    var_obs <- obs[obs$variable == variable, , drop = FALSE]
    var_obs <- var_obs[order(var_obs$datetime_utc), , drop = FALSE]

    row <- dict[match(variable, dict$variable), , drop = FALSE]
    stat_class <- row$statistical_class

    filled <- if (isTRUE(stat_class == "circular")) {
      .fill_circular(var_obs)
    } else if (isTRUE(stat_class == "intermittent")) {
      .fill_rain(var_obs)
    } else if (isTRUE(stat_class == "clear_sky_indexed")) {
      .fill_clear_sky(var_obs, site = site)
    } else if (identical(variable, "relative_humidity_2m")) {
      .fill_rh_dewpoint(var_obs, temp_obs)
    } else {
      .linear_interpolate(var_obs)
    }

    out <- vctrs::vec_rbind(out, filled)
  }
  out
}

# ---- fill_medium(): donor fill ----------------------------------------------

# Pick the single best-available donor from a named list of already-selected
# donor series (test-fill-tiers.R's shape: `donors = list(bom = <series>)`).
# For this plan's tested scope (no explicit ranking metadata attached to
# these already-resolved series), the first entry is used; `rank_donors()`
# is what determines ORDER before donors reach this point (fill_run()'s job),
# so by the time fill_medium() sees `donors` they are already in priority
# order.
.pick_donor <- function(donors) {
  if (length(donors) == 0) {
    return(NULL)
  }
  donors[[1]]
}

#' Fill gaps from the best available donor, transfer-corrected (medium tier)
#'
#' For each variable present in `obs`, fills its gaps using the best
#' available donor series in `donors` (the first element, since `donors` is
#' expected to already be in priority order -- see `rank_donors()`):
#' fits a transfer ([fit_transfer()]) between the donor and the site's own
#' non-gap values over their overlap, applies it to the donor's values at the
#' gap timestamps, and fills. The filled rows carry `method = "donor_fill"`
#' and `source` equal to the donor's own `source` value (so provenance
#' records which donor actually served the fill), never the site's own
#' `source`.
#'
#' @param obs A canonical long obs tibble with gaps (NA value rows).
#' @param donors A named list, each element a single-variable long donor obs
#'   tibble (already resolved/selected, in priority order).
#' @param site A `met_site` object (currently unused directly here beyond
#'   being part of the shared tier-routing signature; kept for symmetry with
#'   `fill_macro()`/`fill_tier()` and future donor-specific treatment, e.g.
#'   height-correcting a wind donor before transfer).
#' @param dict The variable dictionary.
#' @return `obs` with donor-fillable gaps filled.
#' @keywords internal
#' @noRd
fill_medium <- function(obs, donors, site, dict = met_variables()) {
  variables <- unique(obs$variable)
  out <- obs[0, , drop = FALSE]

  for (variable in variables) {
    var_obs <- obs[obs$variable == variable, , drop = FALSE]
    var_obs <- var_obs[order(var_obs$datetime_utc), , drop = FALSE]
    gap_idx <- which(is.na(var_obs$value))

    donor <- .pick_donor(donors)
    if (!is.null(donor) && nrow(donor) > 0) {
      donor <- donor[donor$variable == variable, , drop = FALSE]
    }
    if (length(gap_idx) == 0 || is.null(donor) || nrow(donor) == 0) {
      out <- vctrs::vec_rbind(out, var_obs)
      next
    }

    non_gap <- var_obs[-gap_idx, , drop = FALSE]
    transfer <- tryCatch(
      fit_transfer(donor, non_gap, method = "mean_bias"),
      meteoTidy_error_transfer_no_overlap = function(cnd) NULL
    )

    donor_at_gap <- donor[match(var_obs$datetime_utc[gap_idx], donor$datetime_utc), , drop = FALSE]
    has_donor <- !is.na(donor_at_gap$datetime_utc)

    if (!is.null(transfer) && any(has_donor)) {
      corrected <- apply_transfer(transfer, donor_at_gap[has_donor, , drop = FALSE])
      fill_at <- gap_idx[has_donor]
      var_obs$value[fill_at] <- corrected$value
      var_obs$source[fill_at] <- corrected$source
      var_obs$method[fill_at] <- "donor_fill"
      var_obs$qc_flag[fill_at] <- "ok"
    } else if (any(has_donor)) {
      # No overlap to fit a transfer against: use the donor's raw values
      # directly rather than leaving the gap unfilled.
      fill_at <- gap_idx[has_donor]
      var_obs$value[fill_at] <- donor_at_gap$value[has_donor]
      var_obs$source[fill_at] <- donor_at_gap$source[has_donor]
      var_obs$method[fill_at] <- "donor_fill"
      var_obs$qc_flag[fill_at] <- "ok"
    }

    out <- vctrs::vec_rbind(out, var_obs)
  }
  out
}

# ---- fill_macro(): model fill -----------------------------------------------

#' Fill gaps from a model series (macro / pre-installation tier)
#'
#' For each variable present in `obs`, fills its gaps directly from `model`
#' (a single long obs tibble covering the gap, e.g. `source = "openmeteo"`),
#' matched on `datetime_utc`. Filled rows carry `method = "model_fill"`. This
#' is also the entire treatment for `measurability_class == "model_only"`
#' variables (SCOPING section 7.3): they always use the raw model value here,
#' never a donor.
#'
#' @param obs A canonical long obs tibble with gaps.
#' @param model A single long obs tibble (the model series) covering the gap.
#' @param dict The variable dictionary.
#' @return `obs` with model-fillable gaps filled.
#' @keywords internal
#' @noRd
fill_macro <- function(obs, model, dict = met_variables()) {
  variables <- unique(obs$variable)
  out <- obs[0, , drop = FALSE]

  for (variable in variables) {
    var_obs <- obs[obs$variable == variable, , drop = FALSE]
    var_obs <- var_obs[order(var_obs$datetime_utc), , drop = FALSE]
    gap_idx <- which(is.na(var_obs$value))

    model_var <- if (!is.null(model)) model[model$variable == variable, , drop = FALSE] else NULL

    if (length(gap_idx) == 0 || is.null(model_var) || nrow(model_var) == 0) {
      out <- vctrs::vec_rbind(out, var_obs)
      next
    }

    model_match <- match(var_obs$datetime_utc[gap_idx], model_var$datetime_utc)
    model_at_gap <- model_var[model_match, , drop = FALSE]
    has_model <- !is.na(model_at_gap$datetime_utc)
    fill_at <- gap_idx[has_model]

    var_obs$value[fill_at] <- model_at_gap$value[has_model]
    var_obs$method[fill_at] <- "model_fill"
    var_obs$qc_flag[fill_at] <- "ok"

    out <- vctrs::vec_rbind(out, var_obs)
  }
  out
}

# ---- fill_tier(): the top-level router --------------------------------------

# Contiguous runs of NA `value` within a single (already time-ordered)
# variable series. Returns a list of integer index vectors (positions into
# the ordered series), one per run.
.gap_runs <- function(is_na) {
  if (!any(is_na)) {
    return(list())
  }
  run_id <- cumsum(c(TRUE, diff(is_na) != 0))
  run_id[!is_na] <- NA
  idx <- which(is_na)
  split(idx, run_id[idx])
}

# For a model-only variable, EVERY value is the raw model value (SCOPING
# section 7.3: "model-only variables ... always the raw model value here") --
# there is no site-measured truth for these variables at all, so unlike the
# other tiers (which only touch gap rows), this replaces every row's value
# with the model series and stamps `method = "model_fill"` throughout, not
# just at gaps.
.fill_model_only <- function(var_obs, model) {
  if (is.null(model) || nrow(model) == 0) {
    return(var_obs)
  }
  m <- match(var_obs$datetime_utc, model$datetime_utc)
  has_model <- !is.na(m)
  var_obs$value[has_model] <- model$value[m[has_model]]
  var_obs$method[has_model] <- "model_fill"
  var_obs$qc_flag[has_model] <- "ok"
  var_obs
}

# Route one variable's (already time-ordered) gaps into the micro-eligible
# set (short gaps in a smooth class -> interpolation tier) versus everything
# else (`other_idx`: long gaps, and any gap in a non-micro-eligible class),
# which is handled by the derive/donor/model tiers. Returns integer index
# vectors into the ordered series.
.route_variable_gaps <- function(var_obs, stat_class) {
  runs <- .gap_runs(is.na(var_obs$value))
  micro_eligible <- stat_class %in% .micro_eligible_classes()
  max_gap_h <- .micro_max_gap_hours()

  micro_idx <- integer(0)
  other_idx <- integer(0)
  for (run in runs) {
    span_hours <- as.numeric(
      difftime(var_obs$datetime_utc[max(run)], var_obs$datetime_utc[min(run)], units = "hours")
    )
    if (micro_eligible && span_hours <= max_gap_h) {
      micro_idx <- c(micro_idx, run)
    } else {
      other_idx <- c(other_idx, run)
    }
  }
  list(micro_idx = micro_idx, other_idx = other_idx)
}

# The donor (medium) then model (macro) tiers for one variable's series,
# targeting exactly the `other_idx` gaps routed away from micro (so a short
# micro-eligible gap that micro could not fill never leaks into a donor
# fetch). Any of those gaps already resolved by an earlier tier (micro or the
# derivation tier) is skipped via the `still_na` re-check.
.fill_variable_donor_model <- function(var_obs, other_idx, dict, model, donors, site) {
  if (length(other_idx) == 0) {
    return(var_obs)
  }

  still_na <- other_idx[is.na(var_obs$value[other_idx])]
  if (length(still_na) > 0 && !is.null(donors) && length(donors) > 0) {
    medium_result <- fill_medium(var_obs, donors = donors, site = site, dict = dict)
    var_obs[still_na, ] <- medium_result[still_na, ]
  }
  still_na <- other_idx[is.na(var_obs$value[other_idx])]
  if (length(still_na) > 0 && !is.null(model)) {
    macro_result <- fill_macro(var_obs, model = model, dict = dict)
    var_obs[still_na, ] <- macro_result[still_na, ]
  }
  var_obs
}

#' Route each gap to the right fill tier (micro/derive/medium/macro)
#'
#' The top-level fill router. It runs the fill ladder in tier order so that a
#' cheaper, less-biased tier always pre-empts a more expensive one. Per gap the
#' order is **micro -> derive -> donor -> model**:
#'
#' - **model-only** variables (`measurability_class == "model_only"`) always
#'   skip straight to the macro/model tier -- the donor ladder
#'   (`rank_donors()`) is never consulted for them, regardless of gap length,
#'   and they are never derivable from surface obs.
#' - **micro (interpolation)** takes short gaps (<= ~3 h) in smooth classes
#'   (`linear`/`bounded`/`circular`/`clear_sky_indexed`). `intermittent` (rain)
#'   gaps are never routed to micro, at any length.
#' - **derive (exact physics)** then fills any remaining gap in a coupled
#'   variable (RH <-> dewpoint <-> temperature) wherever every input is
#'   co-observed and QC-clean at that site + timestamp (see `fill_derive()`),
#'   stamping `method = "derived"`. Running before the donor tier means an
#'   exact, unbiased derivation pre-empts a donor fetch.
#' - **donor (medium)** then fills remaining long gaps from the best donor if
#'   `donors` is supplied, and finally **model (macro)** from `model`.
#'
#' Every filled row's `qc_flag` becomes `"ok"` (never the gap's inherited
#' `"missing"`/`"fail"`) and `method` becomes one of `"imputed"`/`"derived"`/
#' `"donor_fill"`/`"model_fill"` (never `"measured"`).
#'
#' @param obs A canonical long obs tibble, one or more variables.
#' @param dict The variable dictionary.
#' @param model A single long obs tibble (the model series) for the macro
#'   tier, or `NULL` if none is available.
#' @param donors A named list of single-variable long donor obs tibbles (see
#'   `fill_medium()`), or `NULL` if none is available.
#' @param site A `met_site` object, needed only for `clear_sky_indexed`
#'   variables.
#' @return `obs` with every routable gap filled.
#' @keywords internal
#' @noRd
fill_tier <- function(obs, dict = met_variables(), model = NULL, donors = NULL, site = NULL) {
  variables <- unique(obs$variable)
  if (length(variables) == 0) {
    return(obs)
  }

  # Phase 1 -- micro tier (short smooth gaps), per variable. Also records, per
  # variable, whether it is model-only and which gaps were routed AWAY from
  # micro (`other_idx`), so the later donor/model phase targets exactly those.
  micro_parts <- vector("list", length(variables))
  other_idx_by_var <- vector("list", length(variables))
  is_model_only <- stats::setNames(logical(length(variables)), variables)

  for (i in seq_along(variables)) {
    variable <- variables[[i]]
    var_obs <- obs[obs$variable == variable, , drop = FALSE]
    var_obs <- var_obs[order(var_obs$datetime_utc), , drop = FALSE]
    row <- dict[match(variable, dict$variable), , drop = FALSE]
    model_only <- isTRUE(row$measurability_class == "model_only")
    is_model_only[[variable]] <- model_only

    if (!model_only) {
      route <- .route_variable_gaps(var_obs, row$statistical_class)
      if (length(route$micro_idx) > 0) {
        micro_result <- fill_micro(var_obs, dict = dict, site = site)
        var_obs[route$micro_idx, ] <- micro_result[route$micro_idx, ]
      }
      other_idx_by_var[[i]] <- route$other_idx
    }
    micro_parts[[i]] <- var_obs
  }
  micro_frame <- do.call(vctrs::vec_rbind, unname(micro_parts))

  # Phase 2 -- derive tier (exact physics), cross-variable, on the full frame.
  derived_frame <- fill_derive(micro_frame, dict = dict)

  # Phase 3 -- donor (medium) then model (macro) tiers, per variable.
  out <- obs[0, , drop = FALSE]
  for (i in seq_along(variables)) {
    variable <- variables[[i]]
    var_obs <- derived_frame[derived_frame$variable == variable, , drop = FALSE]
    var_obs <- var_obs[order(var_obs$datetime_utc), , drop = FALSE]
    model_var <- if (!is.null(model)) model[model$variable == variable, , drop = FALSE] else model

    var_obs <- if (is_model_only[[variable]]) {
      .fill_model_only(var_obs, model_var)
    } else {
      .fill_variable_donor_model(
        var_obs, other_idx_by_var[[i]],
        dict = dict, model = model_var, donors = donors, site = site
      )
    }
    out <- vctrs::vec_rbind(out, var_obs)
  }
  out
}
