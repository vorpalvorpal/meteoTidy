#' @include met-table.R met-table-hash.R
NULL

# Plan 15 -- the meteoHazard dual-accept boundary validator (SCOPING section
# 3.2): a consumer calls `met_ingest()` once at its boundary. A `met_table`
# input is hash-validated once and trusted thereafter (no defensive
# re-checking downstream); a plain tibble is schema-validated on entry and
# its provenance is marked entirely `"unverified"`, since nothing is known
# about how it was produced.

.met_wide_required_columns <- function() "time"

#' Validate a plain tibble against the section 3.1 wide schema
#'
#' The minimal schema check `met_ingest()` applies to a plain-tibble input:
#' a `time` column must be present. (The full section 3.1 column set is not
#' enforced here -- a subset of variables is a legitimate, stable shape;
#' Plan 01's `widen_obs()` is what produces the full contract on the way in.)
#'
#' @param x A data frame.
#' @return `x`, invisibly, if it has a `time` column.
#' @keywords internal
#' @noRd
.assert_met_wide_schema <- function(x) {
  missing <- setdiff(.met_wide_required_columns(), names(x))
  if (length(missing) > 0) {
    abort_meteo(
      c(
        "Input does not satisfy the {.pkg meteoHazard} wide (section 3.1) schema.",
        "x" = "Missing required column{?s}: {.val {missing}}."
      ),
      class = "schema_violation"
    )
  }
  invisible(x)
}

#' The meteoHazard dual-accept boundary validator
#'
#' A consumer (meteoHazard) calls `met_ingest()` once at its boundary
#' (SCOPING section 3.2):
#'
#' - If `x` is already a [new_met_table()]: [met_validate_boundary()] (the hash
#'   re-check) runs once, and the (possibly per-column-downgraded)
#'   provenance is trusted thereafter.
#' - If `x` is a plain tibble: the section 3.1 schema is validated on entry
#'   (a `time` column must be present) and, if valid, every value column's
#'   provenance is marked `tier = "unverified"` -- nothing is known about
#'   how a plain tibble was produced, so its metadata cannot be trusted.
#'
#' @param x A `met_table` or a plain tibble.
#' @return A `met_table`.
#' @family met-table
#' @export
#' @examples
#' plain <- tibble::tibble(time = as.POSIXct("2026-01-01", tz = "UTC"),
#'                         temperature_2m = 20)
#' met_ingest(plain)
met_ingest <- function(x) {
  if (inherits(x, "met_table")) {
    return(met_validate_boundary(x))
  }

  .assert_met_wide_schema(x)

  value_cols <- met_value_columns(x)
  provenance <- tibble::tibble(
    variable = value_cols,
    tier = "unverified",
    train_overlap = NA_real_,
    source = NA_character_
  )

  new_met_table(
    x,
    provenance = provenance,
    keys = list(site_id = NA_character_),
    versions = list(schema_version = .met_wide_schema_version,
                    calibration_manifest_version = 0L)
  )
}

#' Enforce the single-provenance-class rule for a derived index
#'
#' Implements SCOPING section 3.2's enforceable "each derived quantity is
#' computed within one provenance class" rule: warns when the `tier` values
#' of the requested `variables` are not all identical (e.g. mixing a
#' corrected 10 m wind with a raw 80 m wind into one derived shear index --
#' physically nonsensical, since one leg was bias-corrected and the other
#' was not).
#'
#' @param x A `met_table`.
#' @param variables Character vector of variable names feeding the derived
#'   index.
#' @return `x`, invisibly.
#' @family met-table
#' @export
#' @examples
#' mt <- new_met_table(
#'   tibble::tibble(time = as.POSIXct("2026-01-01", tz = "UTC"),
#'                  wind_speed_10m = 5, wind_speed_80m = 9),
#'   provenance = tibble::tibble(
#'     variable = c("wind_speed_10m", "wind_speed_80m"),
#'     tier = c("qmap", "raw"), train_overlap = c(24, 0), source = "openmeteo"
#'   ),
#'   keys = list(site_id = "test"),
#'   versions = list(schema_version = "1.0.0", calibration_manifest_version = 0L)
#' )
#' met_assert_single_tier(mt, c("wind_speed_10m", "wind_speed_80m"))
met_assert_single_tier <- function(x, variables) {
  prov <- met_provenance(x)
  tiers <- prov$tier[prov$variable %in% variables]
  if (length(unique(tiers)) > 1) {
    warn_meteo(
      c(
        "Derived index mixes inputs from more than one correction tier.",
        "x" = "Variables {.val {variables}} have tiers {.val {tiers}}."
      ),
      class = "mixed_tier"
    )
  }
  invisible(x)
}
