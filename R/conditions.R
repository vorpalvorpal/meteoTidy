#' The meteoTidy condition taxonomy
#'
#' @description
#' `meteoTidy` raises every user-facing error, warning, and message through a
#' small set of helpers (`abort_meteo()`, `warn_meteo()`, `inform_meteo()`) so
#' that conditions are always classed and discoverable. Each helper prepends a
#' package-specific prefix to the short `class` it is given, and attaches an
#' umbrella class shared by every condition of that kind:
#'
#' | Helper          | Short class   | Full class                     | Umbrella class        |
#' | --------------- | ------------- | ------------------------------- | ---------------------- |
#' | `abort_meteo()`  | `"bad_units"` | `"meteoTidy_error_bad_units"`   | `"meteoTidy_error"`   |
#' | `warn_meteo()`   | `"risky"`     | `"meteoTidy_warning_risky"`     | `"meteoTidy_warning"` |
#' | `inform_meteo()` | `"note"`      | `"meteoTidy_message_note"`      | `"meteoTidy_message"` |
#'
#' Use [meteo_conditions()] to list every class the installed package can
#' raise.
#'
#' @name meteoTidy-conditions
NULL

#' Signal a classed meteoTidy error
#'
#' Wraps [cli::cli_abort()], requiring a `class` so every error raised by the
#' package can be caught precisely. The short `class` supplied is prefixed
#' with `"meteoTidy_error_"`; the umbrella class `"meteoTidy_error"` is always
#' attached as well.
#'
#' @param message A `cli`-formatted character vector (supports inline markup
#'   such as `{.val }` and named bullets `"i"`, `"x"`, `"!"`).
#' @param ... Passed on to [cli::cli_abort()].
#' @param class Required. The short condition class, e.g. `"bad_units"`.
#' @param call The call to report as the origin of the error. Defaults to the
#'   caller of `abort_meteo()`, not `abort_meteo()` itself.
#' @param .envir Environment used for glue interpolation of `message`.
#'
#' @return Never returns; always signals an error.
#' @family conditions
#' @export
#' @examples
#' f <- function() abort_meteo("Something went wrong.", class = "demo")
#' tryCatch(f(), meteoTidy_error_demo = function(cnd) cnd$message)
abort_meteo <- function(message, ..., class, call = rlang::caller_env(), .envir = parent.frame()) {
  rlang::check_required(class)
  full_class <- c(paste0("meteoTidy_error_", class), "meteoTidy_error")
  cli::cli_abort(message, ..., class = full_class, call = call, .envir = .envir)
}

#' Signal a classed meteoTidy warning
#'
#' Wraps [cli::cli_warn()], requiring a `class` so every warning raised by the
#' package can be caught precisely. The short `class` supplied is prefixed
#' with `"meteoTidy_warning_"`; the umbrella class `"meteoTidy_warning"` is
#' always attached as well.
#'
#' @inheritParams abort_meteo
#' @return `NULL`, invisibly. Called for its side effect of signalling a warning.
#' @family conditions
#' @export
#' @examples
#' warn_meteo("Proceeding with a risky default.", class = "demo")
warn_meteo <- function(message, ..., class, .envir = parent.frame()) {
  rlang::check_required(class)
  full_class <- c(paste0("meteoTidy_warning_", class), "meteoTidy_warning")
  cli::cli_warn(message, ..., class = full_class, .envir = .envir)
}

#' Signal a meteoTidy informational message
#'
#' Wraps [cli::cli_inform()]. Unlike [abort_meteo()] and [warn_meteo()], the
#' `class` is optional: when supplied it is prefixed with
#' `"meteoTidy_message_"`; the umbrella class `"meteoTidy_message"` is always
#' attached.
#'
#' @inheritParams abort_meteo
#' @param class Optional. The short condition class, e.g. `"note"`.
#' @return `NULL`, invisibly. Called for its side effect of signalling a message.
#' @family conditions
#' @export
#' @examples
#' inform_meteo("Starting a long-running task.")
#' inform_meteo("Using a cached value.", class = "note")
inform_meteo <- function(message, ..., class = NULL, .envir = parent.frame()) {
  full_class <- c(
    if (!is.null(class)) paste0("meteoTidy_message_", class),
    "meteoTidy_message"
  )
  cli::cli_inform(message, ..., class = full_class, .envir = .envir)
}

# The registry backing `meteo_conditions()`. Each plan that introduces a new
# condition class appends a row here. Keep classes and meanings short.
.meteo_condition_registry <- function() {
  data.frame(
    class = c(
      "meteoTidy_error",
      "meteoTidy_warning",
      "meteoTidy_message",
      # Plan 01 — enums (R/enums.R)
      "meteoTidy_error_invalid_qc_flag",
      "meteoTidy_error_invalid_method",
      "meteoTidy_error_invalid_tier",
      "meteoTidy_error_invalid_statistical_class",
      "meteoTidy_error_invalid_measurability_class",
      # Plan 01 — units (R/units.R)
      "meteoTidy_error_bad_units",
      "meteoTidy_warning_units_conflict",
      # Plan 01 — dictionary (R/dict.R)
      "meteoTidy_error_bad_variable_name",
      "meteoTidy_error_bad_variable_range",
      "meteoTidy_error_duplicate_variable",
      "meteoTidy_error_unknown_variable",
      # Plan 01 — shared schema utilities (R/schema.R)
      "meteoTidy_error_schema_missing_column",
      "meteoTidy_error_schema_bad_type",
      # Plan 01 — observation schema (R/schema-obs.R)
      "meteoTidy_error_non_utc_time",
      "meteoTidy_error_range_violation",
      "meteoTidy_error_duplicate_key",
      # Plan 01 — forecast schema (R/schema-forecast.R)
      "meteoTidy_error_member_stat_conflict",
      "meteoTidy_error_lead_inconsistent",
      # Plan 02 — site registry (R/site.R)
      "meteoTidy_error_bad_site_id",
      "meteoTidy_error_bad_coordinates",
      "meteoTidy_error_bad_timezone",
      "meteoTidy_error_bad_store_root",
      "meteoTidy_error_missing_roughness",
      # Plan 02 — site list (R/site-list.R)
      "meteoTidy_error_bad_site_list",
      "meteoTidy_error_duplicate_site_id",
      # Plan 02 — site YAML (R/site-yaml.R)
      "meteoTidy_error_inline_secret",
      "meteoTidy_error_unknown_config_key",
      # Plan 03 — storage layer (R/store.R, R/store-calib.R)
      "meteoTidy_error_unknown_store_table",
      "meteoTidy_error_calib_not_found",
      # Plan 04 — HTTP seam (R/http.R)
      "meteoTidy_error_network_disabled",
      "meteoTidy_error_http_gone",
      "meteoTidy_error_http_client_error",
      # Plan 04 — adapter contract (R/adapter.R)
      "meteoTidy_error_source_not_uniform",
      "meteoTidy_error_unrequested_variable",
      "meteoTidy_error_no_forecast_support",
      "meteoTidy_error_unknown_adapter",
      "meteoTidy_error_adapter_not_yet_implemented",
      # Plan 04 — response mapping / source_rest (R/adapter-mapping.R, R/source-rest.R)
      "meteoTidy_error_bad_mapping",
      "meteoTidy_error_unsupported_response",
      # Plan 05 — Open-Meteo adapter (R/source-openmeteo.R, R/openmeteo-*.R)
      "meteoTidy_error_unknown_openmeteo_product",
      "meteoTidy_error_openmeteo_bad_response",
      "meteoTidy_message_openmeteo_free_tier",
      # Plan 06 — SILO/GHCNh adapters (R/silo-qcode.R, R/source-silo.R,
      # R/source-ghcnh.R, R/station-resolve.R)
      "meteoTidy_error_unknown_silo_code",
      "meteoTidy_error_unresolved_station",
      # Plan 07 — BOM adapters (R/bom-transport.R, R/source-bom-forecast.R,
      # R/source-bom-obs.R)
      "meteoTidy_error_bom_all_transports_failed",
      "meteoTidy_error_bom_geohash_unavailable",
      "meteoTidy_error_no_forecast_aux_support",
      # Plan 08 — ECMWF adapter (R/grib-read.R, R/source-ecmwf.R)
      "meteoTidy_error_terra_required",
      "meteoTidy_error_grib_ccsds_unsupported",
      # Plan 09 -- QC engine (R/qc-spatial.R)
      "meteoTidy_error_spatial_not_applicable",
      # Plan 10 -- transfer engine (R/transfer.R)
      "meteoTidy_error_transfer_no_overlap",
      # Plan 10 -- fill treatments (R/fill-treatments.R)
      "meteoTidy_error_fill_missing_site"
    ),
    meaning = c(
      "Umbrella class attached to every error raised via abort_meteo().",
      "Umbrella class attached to every warning raised via warn_meteo().",
      "Umbrella class attached to every message raised via inform_meteo().",
      "A qc_flag value outside QC_FLAG_LEVELS.",
      "A method value outside METHOD_LEVELS.",
      "A tier value outside TIER_LEVELS.",
      "A statistical_class value outside STAT_CLASS_LEVELS.",
      "A measurability_class value outside MEASURABILITY_LEVELS.",
      "A unit that cannot be converted to the target canonical unit.",
      "A units-carrying input whose unit disagrees with the `from` argument (the carried unit is trusted).", # nolint: line_length_linter.
      "met_register_variable() called with a non-scalar-string variable name.",
      "met_register_variable() called with min > max.",
      "met_register_variable() attempted to silently redefine an existing variable without overwrite = TRUE.", # nolint: line_length_linter.
      "met_variable() lookup for a variable not in the dictionary.",
      "A new_*() constructor input is missing a required column.",
      "A new_*() constructor input has a column of the wrong type.",
      "A datetime column is not tagged with tzone = \"UTC\".",
      "An ok-flagged observation value outside its variable's [min, max] range.",
      "A canonical table has duplicate key rows.",
      "A forecast row has both member and stat set (violates the member/stat rule).",
      "A forecast row's lead_time does not equal valid_time - issue_time.",
      "A met_site site_id is empty or contains characters outside [A-Za-z0-9_-].",
      "A met_site latitude/longitude/elevation is out of range or non-finite.",
      "A met_site timezone is not in OlsonNames().",
      "A met_site store_root is empty.",
      "A wind instrument on a met_site has no roughness_length (z0).",
      "A met_sites element is not a met_site object.",
      "A met_sites collection has duplicate site_id values.",
      "A site YAML sources entry has a literal secret value instead of a *_env/*_keyring reference.", # nolint: line_length_linter.
      "A site YAML file has an unrecognised top-level or site-level key.",
      "store_compact() was asked to compact a table name it does not recognise.",
      "calib_read() found no calibration manifest row for the requested key/version.",
      "The no-network test guard tripped: METEOTIDY_NO_NET=1 blocked a live HTTP request.",
      "An HTTP request received a persistent failure status (404/410); never retried.",
      "An HTTP request received a non-retryable client-error status, or exhausted its retries.", # nolint: line_length_linter.
      "check_fetch_result() found a non-uniform source column in a fetch() result.",
      "check_fetch_result() found a variable in a fetch() result that was not requested.",
      "fetch_forecast() was called on an adapter that does not support forecast retrieval.",
      "adapters_for_site() found a source config with an unrecognised adapter kind.",
      "adapters_for_site() found a source config for an adapter kind reserved for a later plan.", # nolint: line_length_linter.
      "met_mapping()/source_rest()/source_file() received a malformed mapping or adapter config.", # nolint: line_length_linter.
      "source_rest() received a response that looks paginated (unsupported; single-page only).",
      "An unrecognised Open-Meteo product name was requested.",
      "An Open-Meteo response was missing an expected hourly/daily block.",
      "Emitted once per fetch when an Open-Meteo request uses the free (non-commercial) tier.", # nolint: line_length_linter.
      "silo_qcode_map() received a SILO source/quality code outside the documented reference table.", # nolint: line_length_linter.
      "fetch() was called on a station-resolving adapter (silo/ghcnh) before resolve_station() populated the site's resolved-ID cache.", # nolint: line_length_linter.
      "ladder_fetch() exhausted every eligible BOM transport rung for the requested product.",
      "resolve_station() needs a BOM geohash but none is cached and allow_web_api = FALSE.",
      "fetch_forecast_aux() was called on an adapter that does not support forecast_aux retrieval.", # nolint: line_length_linter.
      "source_ecmwf()'s fetch_forecast() was called but the terra package is not installed.",
      "The installed GDAL build could not read a CCSDS/AEC-compressed ECMWF GRIB2 message (likely missing libaec support).", # nolint: line_length_linter.
      "qc_spatial() was called on a model_only variable, which has no site truth to buddy-check against (SCOPING section 7.3).", # nolint: line_length_linter.
      "fit_transfer() was called with source/target series that share no overlapping timestamp to fit against.", # nolint: line_length_linter.
      "fill_micro()/fill_tier() needs a met_site to fill a clear_sky_indexed variable but none was supplied." # nolint: line_length_linter.
    ),
    stringsAsFactors = FALSE
  )
}

#' List the meteoTidy condition taxonomy
#'
#' Returns every condition class the installed package can raise, so the
#' taxonomy is discoverable and testable. Later plans append their own
#' classes to this table as they introduce new condition kinds.
#'
#' @return A data frame with columns `class` (character, unique, always
#'   prefixed `"meteoTidy_"`) and `meaning` (character, a short description).
#' @family conditions
#' @export
#' @examples
#' meteo_conditions()
meteo_conditions <- function() {
  .meteo_condition_registry()
}
