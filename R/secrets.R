# Plan 14 -- secret resolution, redaction, and the store-write guard (SCOPING
# section 11): non-secret configuration lives in version-controlled YAML;
# secrets are referenced by name (`list(env = "VAR")` / `list(keyring =
# "service")`) and resolved from the environment or a keyring at USE TIME
# only -- never cached on an object, never logged, never written to disk.

#' Resolve a secret reference to its value
#'
#' `ref` names where to find a secret, never the secret itself: `list(env =
#' "VAR_NAME")` reads an environment variable, `list(keyring = "service")`
#' reads a `keyring` entry (an optional `Suggests` dependency, guarded here).
#' Resolution happens fresh on every call -- nothing is cached.
#'
#' @param ref A list with exactly one of `env` (an environment variable name)
#'   or `keyring` (a `keyring` service name).
#' @return A single string, the resolved secret value.
#' @keywords internal
#' @noRd
resolve_secret <- function(ref) {
  if (!is.null(ref$env)) {
    val <- Sys.getenv(ref$env, unset = NA_character_)
    if (is.na(val)) {
      abort_meteo(
        c(
          "Environment variable {.envvar {ref$env}} is not set.",
          "i" = "Set it (e.g. in {.file .Renviron}) before resolving this secret." # nolint: line_length_linter.
        ),
        class = "secret_unresolved"
      )
    }
    return(val)
  }

  if (!is.null(ref$keyring)) {
    rlang::check_installed("keyring", reason = "to resolve a keyring-referenced secret.")
    return(keyring::key_get(ref$keyring))
  }

  abort_meteo(
    c(
      "{.arg ref} must name a secret source.",
      "i" = "Use {.code list(env = \"VAR_NAME\")} or {.code list(keyring = \"service\")}."
    ),
    class = "bad_secret_ref"
  )
}

#' Redact a secret value out of a display string
#'
#' Used in every `print`/`format` method that could otherwise leak a
#' resolved secret through object display.
#'
#' @param text Single string, the text to redact within.
#' @param secret Single string, the secret value to hide.
#' @return `text` with every occurrence of `secret` replaced by
#'   `"<redacted>"`.
#' @keywords internal
#' @noRd
redact <- function(text, secret) {
  if (is.na(secret) || !nzchar(secret)) {
    return(text)
  }
  gsub(secret, "<redacted>", text, fixed = TRUE)
}

#' Guard: abort if any resolved secret appears in a store-bound data frame
#'
#' A belt-and-braces check (SCOPING section 11) that a resolved secret value
#' never reaches Parquet/manifests/provenance: scans every column of `df`
#' (coerced to character) for a literal occurrence of any value in
#' `secrets`. Intended to be called by store write paths before persisting.
#'
#' @param df A data frame/tibble about to be written to the store.
#' @param secrets A character vector of resolved secret values to check for.
#' @return `df`, invisibly, if no secret value appears anywhere in it.
#' @keywords internal
#' @noRd
assert_no_secrets_in <- function(df, secrets) {
  secrets <- secrets[!is.na(secrets) & nzchar(secrets)]
  if (length(secrets) == 0 || nrow(df) == 0) {
    return(invisible(df))
  }

  leaked <- vapply(df, function(col) {
    col_chr <- as.character(col)
    any(vapply(secrets, function(s) any(grepl(s, col_chr, fixed = TRUE)), logical(1)))
  }, logical(1))

  if (any(leaked)) {
    abort_meteo(
      c(
        "A resolved secret value appears in column{?s} {.val {names(df)[leaked]}}.", # nolint: line_length_linter.
        "i" = "Secrets must never reach the store (Parquet/manifests/provenance)."
      ),
      class = "secret_leak"
    )
  }
  invisible(df)
}
