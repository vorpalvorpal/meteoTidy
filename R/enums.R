# Plan 01 — closed enumerations.
#
# Each enum is a character vector constant plus a `validate_<enum>()` that
# returns `x` invisibly when every value is legal, or aborts (via
# `abort_meteo()`) with a class `"invalid_<enum>"` listing the offending
# values. A tiny factory (`.enum_validator()`) avoids repeating the same
# check/abort logic five times.

#' Quality-control flag levels
#'
#' The closed set of QC states a canonical observation row may carry. Note
#' the deliberate absence of `"estimated"`: production method (see
#' [METHOD_LEVELS]) is not a QC state (SCOPING §3).
#'
#' @family enums
#' @export
QC_FLAG_LEVELS <- c("ok", "suspect", "fail", "missing")

#' Production-method levels
#'
#' How a canonical value was produced.
#'
#' @family enums
#' @export
METHOD_LEVELS <- c(
  "measured", "aggregated", "donor_fill", "model_fill",
  "imputed", "disaggregated", "derived"
)

#' Correction-tier levels
#'
#' Ordered from least to most corrected. Use [tier_rank()] to compare tiers.
#'
#' @family enums
#' @export
TIER_LEVELS <- c("raw", "physical", "mean_bias", "qmap", "emos")

#' Statistical-class levels
#'
#' Drives QC and correction dispatch (SCOPING §3, §6).
#'
#' @family enums
#' @export
STAT_CLASS_LEVELS <- c(
  "linear", "circular", "bounded", "intermittent", "clear_sky_indexed"
)

#' Measurability-class levels
#'
#' Drives the gap-fill ladder (SCOPING §6).
#'
#' @family enums
#' @export
MEASURABILITY_LEVELS <- c(
  "site_measurable", "derived_measurable", "donor_observable", "model_only"
)

# Build a validator for a closed enum. `label` is used in the message and to
# build the short abort class (`"invalid_<label>"`).
.enum_validator <- function(levels, label) {
  force(levels)
  force(label)
  function(x) {
    bad <- setdiff(unique(x), levels)
    if (length(bad) > 0) {
      abort_meteo(
        c(
          "{.arg x} contains values outside the {.val {label}} enum.",
          "x" = "Offending value{?s}: {.val {bad}}",
          "i" = "Legal levels: {.val {levels}}"
        ),
        class = paste0("invalid_", label)
      )
    }
    invisible(x)
  }
}

#' Validate a qc_flag vector
#'
#' @param x Character vector to validate against [QC_FLAG_LEVELS].
#' @return `x`, invisibly, if every value is legal.
#' @family enums
#' @export
#' @examples
#' validate_qc_flag(c("ok", "missing"))
validate_qc_flag <- .enum_validator(QC_FLAG_LEVELS, "qc_flag")

#' Validate a method vector
#'
#' @param x Character vector to validate against [METHOD_LEVELS].
#' @return `x`, invisibly, if every value is legal.
#' @family enums
#' @export
#' @examples
#' validate_method(c("measured", "derived"))
validate_method <- .enum_validator(METHOD_LEVELS, "method")

#' Validate a tier vector
#'
#' @param x Character vector to validate against [TIER_LEVELS].
#' @return `x`, invisibly, if every value is legal.
#' @family enums
#' @export
#' @examples
#' validate_tier(c("raw", "emos"))
validate_tier <- .enum_validator(TIER_LEVELS, "tier")

#' Validate a statistical_class vector
#'
#' @param x Character vector to validate against [STAT_CLASS_LEVELS].
#' @return `x`, invisibly, if every value is legal.
#' @family enums
#' @export
#' @examples
#' validate_statistical_class(c("linear", "circular"))
validate_statistical_class <- .enum_validator(STAT_CLASS_LEVELS, "statistical_class")

#' Validate a measurability_class vector
#'
#' @param x Character vector to validate against [MEASURABILITY_LEVELS].
#' @return `x`, invisibly, if every value is legal.
#' @family enums
#' @export
#' @examples
#' validate_measurability_class(c("site_measurable", "model_only"))
validate_measurability_class <- .enum_validator(MEASURABILITY_LEVELS, "measurability_class")

#' Rank of a correction tier
#'
#' Returns the integer rank of `tier` within [TIER_LEVELS], so "higher tier"
#' comparisons are unambiguous (`tier_rank("raw") < tier_rank("emos")`).
#'
#' @param tier A single string, one of [TIER_LEVELS].
#' @return An integer scalar rank (1 = lowest tier).
#' @family enums
#' @export
#' @examples
#' tier_rank("raw")
#' tier_rank("emos")
tier_rank <- function(tier) {
  validate_tier(tier)
  match(tier, TIER_LEVELS)
}
