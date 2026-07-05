# Plan 04 — source_file(): local logger CSV/TSV drop adapter.
#
# No network, no HTTP seam: reads only files under paths the caller supplies,
# matched by a glob. Applies the same apply_mapping() machinery as
# source_rest() (format = "csv"), so unit conversion and canonicalisation are
# identical between the two built-in adapters.

#' A local logger-file observation adapter
#'
#' `source_file()` builds a [met_adapter()] that reads one or more local
#' CSV/TSV logger exports matched by `glob`, concatenates them in
#' deterministic time order, and maps them to canonical observations via
#' [apply_mapping()]. Handles the common messiness of logger exports minimally
#' (configurable delimiter, header, `na` strings, and a `skip` count for
#' preamble lines); anything beyond that is a user-written adapter.
#'
#' @param source_id Single string, stamped into the `source` column of every
#'   returned row.
#' @param glob Single string, a `Sys.glob()` pattern matching one or more
#'   files to read.
#' @param mapping A [met_mapping()] (with `format = "csv"`) describing how to
#'   turn each parsed file into canonical rows.
#' @param provides Character vector of variables this adapter can return.
#'   Defaults to the variable names declared in `mapping`.
#' @param cadence Single string, a scheduling hint (default `"daily"`).
#' @param delim Single string, the field delimiter (default `","`).
#' @param has_header Logical, does each file have a header row naming
#'   columns matching `mapping`'s `column` entries (default `TRUE`)?
#' @param na Character vector of strings to treat as `NA` (default `"NA"`).
#' @param skip Integer, number of preamble lines to skip before the header
#'   (default `0`).
#'
#' @return A `source_file` (`met_adapter` subclass) S7 object.
#' @family adapter
#' @export
#' @examples
#' adapter <- source_file(
#'   "logger", glob = tempfile(fileext = ".csv"),
#'   met_mapping(
#'     format = "csv",
#'     time = list(column = "timestamp", tz = "UTC"),
#'     variables = list(
#'       list(variable = "temperature_2m", column = "temp_c", unit = "degC")
#'     )
#'   )
#' )
source_file <- S7::new_class(
  "source_file",
  package = "meteoTidy",
  parent = met_adapter,
  properties = list(
    glob = S7::class_character,
    mapping = S7::class_list,
    delim = S7::class_character,
    has_header = S7::class_logical,
    na = S7::class_character,
    skip = S7::class_double
  ),
  constructor = function(source_id, glob, mapping, provides = NULL, cadence = "daily",
                         delim = ",", has_header = TRUE, na = "NA", skip = 0) {
    if (is.null(provides)) {
      provides <- vapply(mapping$variables, function(v) v$variable, character(1))
    }
    S7::new_object(
      met_adapter(source_id = source_id, provides = provides, cadence = cadence),
      glob = glob,
      mapping = list(mapping),
      delim = delim,
      has_header = has_header,
      na = na,
      skip = as.double(skip)
    )
  }
)

# Read and concatenate every glob-matched file as a data frame, in
# deterministic (sorted-path) order, using readr so delim/skip/na/header are
# all handled uniformly.
.file_read_all <- function(adapter) {
  paths <- sort(Sys.glob(adapter@glob))
  if (length(paths) == 0) {
    return(NULL)
  }
  frames <- lapply(paths, function(p) {
    readr::read_delim(
      p,
      delim = adapter@delim,
      col_names = adapter@has_header,
      na = adapter@na,
      skip = adapter@skip,
      show_col_types = FALSE,
      progress = FALSE
    )
  })
  do.call(rbind, frames)
}

S7::method(fetch, source_file) <- function(adapter, site, variables, window, now = .now()) {
  parsed <- .file_read_all(adapter)
  mapping <- adapter@mapping[[1]]

  if (is.null(parsed)) {
    out <- new_obs(tibble::tibble(
      site_id = character(0),
      datetime_utc = as.POSIXct(character(0), tz = "UTC"),
      variable = character(0),
      value = double(0),
      source = character(0),
      method = character(0),
      qc_flag = character(0)
    ))
    return(check_fetch_result(out, adapter, variables))
  }

  out <- apply_mapping(parsed, mapping, site, source_id = adapter@source_id, now = now)
  out <- out[order(out$datetime_utc), , drop = FALSE]
  out <- out[out$variable %in% variables, , drop = FALSE]
  check_fetch_result(out, adapter, variables)
}
