# Plan 12 -- post-correction physical-consistency pass (SCOPING section 6
# review fix). After univariate fitted-tier corrections, cross-variable
# physical relations (gusts >= wind, dewpoint <= temperature, RH <= 100,
# direct+diffuse <= clear-sky ceiling) can be violated even though each
# variable was individually well-corrected. This is a thin wrapper around
# Plan 09's shared `physics_constraints()` module in `mode = "enforce"`:
# violations are clipped to the constraint boundary and counted, so Plan 13
# can surface a rising violation rate as a red flag.

#' Run the post-correction physical-consistency pass
#'
#' Enforces the shared physical-consistency relations (`R/physics-constraints.R`,
#' the same module Plan 09's QC engine uses in `mode = "flag"`) on a wide,
#' single-row-per-timestamp corrected frame, clipping any violation to its
#' constraint boundary.
#'
#' @param wide A wide tibble (one row per `(site_id, datetime_utc)`, columns
#'   named after dictionary variables) of corrected values.
#' @return A list with `result` (the clipped wide tibble) and `n_violations`
#'   (integer count of relations that needed clipping, `0` when nothing did).
#' @keywords internal
#' @noRd
consistency_pass <- function(wide) {
  enforced <- physics_constraints(wide, mode = "enforce")
  n_violations <- attr(enforced, "n_violations") %||% 0L
  attr(enforced, "n_violations") <- NULL
  list(result = enforced, n_violations = n_violations)
}

# Run consistency_pass() over a long (variable, value) tibble by widening
# each `key_cols`-identified timestamp to one row (columns = variable names),
# clipping via consistency_pass(), and narrowing the clipped values back into
# `long`'s own row order. Shared by correct_forecast() (Plan 17 item 1/4) and
# build_history_daily() (item 2/4) -- physics_constraints() only understands
# a single-row wide frame, so every caller needs this same widen/narrow
# shuttle; kept here rather than duplicated per caller.
.consistency_pass_long <- function(long, key_cols) {
  if (nrow(long) == 0) {
    attr(long, "n_violations") <- 0L
    return(long)
  }

  key <- do.call(paste, c(lapply(key_cols, function(cn) {
    col <- long[[cn]]
    if (inherits(col, "POSIXct")) {
      format(col, "%Y-%m-%dT%H:%M:%OS6", tz = "UTC")
    } else {
      as.character(col)
    }
  }), sep = "\r"))

  out <- long
  n_violations <- 0L
  for (k in unique(key)) {
    idx <- which(key == k)
    wide <- tibble::as_tibble(stats::setNames(as.list(long$value[idx]), long$variable[idx]))
    enforced <- consistency_pass(wide)
    n_violations <- n_violations + enforced$n_violations
    clipped <- enforced$result
    for (j in idx) {
      v <- long$variable[[j]]
      if (v %in% names(clipped)) {
        out$value[[j]] <- clipped[[v]][[1]]
      }
    }
  }

  attr(out, "n_violations") <- n_violations
  out
}
