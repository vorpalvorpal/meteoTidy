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
      "meteoTidy_message"
    ),
    meaning = c(
      "Umbrella class attached to every error raised via abort_meteo().",
      "Umbrella class attached to every warning raised via warn_meteo().",
      "Umbrella class attached to every message raised via inform_meteo()."
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
