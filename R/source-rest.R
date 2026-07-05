# Plan 04 — source_rest(): generic REST AWS adapter.
#
# Constrained by design (SCOPING §13): single-page responses, JSON or CSV, no
# OAuth, no pagination. Anything more exotic is a user-written adapter
# (proven implementable by the contract tests in test-adapter-contract.R).

#' A generic REST-API observation adapter
#'
#' `source_rest()` builds a [met_adapter()] that fetches a single-page JSON
#' response from a REST endpoint and maps it to canonical observations via
#' [apply_mapping()]. Intended for simple automatic-weather-station APIs that
#' return one page of hourly data per request; anything needing pagination,
#' OAuth, or multi-page assembly is out of scope (SCOPING §13) — write a
#' bespoke adapter instead.
#'
#' @param source_id Single string, stamped into the `source` column of every
#'   returned row.
#' @param endpoint Single string, a URL template. Supported placeholders:
#'   - `{site}` — replaced with `site_id(site)`.
#'   - `{from}`, `{to}` — replaced with the requested window bounds,
#'     formatted `"%Y-%m-%dT%H:%M:%SZ"` (an ISO instant; substring-matches a
#'     bare `"%Y-%m-%d"` date prefix too, since `format()` renders the date
#'     first).
#' @param mapping A [met_mapping()] describing how to turn the parsed JSON
#'   body into canonical rows.
#' @param auth One of `"none"` (default), `"header"`, or `"basic"`.
#'   - `"header"` sends `Authorization: <token>`, where `<token>` is read from
#'     the environment variable named by `token_env` **at fetch time**; the
#'     token is never stored on the adapter object and never appears in
#'     `print()`/`format()` output (SCOPING §11).
#'   - `"basic"` uses `httr2::req_auth_basic()` with the username and
#'     password read from `paste0(token_env, "_USER")` and
#'     `paste0(token_env, "_PASS")` at fetch time.
#' @param token_env Single string, the *name* of the environment variable
#'   holding the secret (never the secret itself). Required when
#'   `auth != "none"`.
#' @param provides Character vector of variables this adapter can return.
#'   Defaults to the variable names declared in `mapping`.
#' @param cadence Single string, a scheduling hint (default `"hourly"`).
#'
#' @return A `source_rest` (`met_adapter` subclass) S7 object.
#' @family adapter
#' @export
#' @examples
#' adapter <- source_rest(
#'   "site_aws", "https://aws.example/api?site={site}&from={from}&to={to}",
#'   met_mapping(
#'     format = "json",
#'     time = list(path = "hourly/time", tz = "UTC"),
#'     variables = list(
#'       list(variable = "temperature_2m", path = "hourly/temperature_2m",
#'            unit = "degC")
#'     )
#'   )
#' )
source_rest <- S7::new_class(
  "source_rest",
  package = "meteoTidy",
  parent = met_adapter,
  properties = list(
    endpoint = S7::class_character,
    mapping = S7::class_list,
    auth = S7::class_character,
    token_env = S7::class_character
  ),
  constructor = function(source_id, endpoint, mapping, auth = c("none", "header", "basic"),
                         token_env = NULL, provides = NULL, cadence = "hourly") {
    auth <- rlang::arg_match(auth, c("none", "header", "basic"))
    if (auth != "none" && is.null(token_env)) {
      abort_meteo(
        "{.arg token_env} is required when {.arg auth} = {.val {auth}}.",
        class = "bad_mapping"
      )
    }
    if (is.null(provides)) {
      provides <- vapply(mapping$variables, function(v) v$variable, character(1))
    }
    S7::new_object(
      met_adapter(source_id = source_id, provides = provides, cadence = cadence),
      endpoint = endpoint,
      mapping = list(mapping),
      auth = auth,
      token_env = token_env %||% NA_character_
    )
  }
)

# Interpolate {site}/{from}/{to} placeholders into the endpoint template.
.rest_build_url <- function(endpoint, site, window) {
  url <- endpoint
  url <- gsub("{site}", site_id(site), url, fixed = TRUE)
  url <- gsub("{from}", format(window$from, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), url, fixed = TRUE)
  url <- gsub("{to}", format(window$to, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), url, fixed = TRUE)
  url
}

# A response "looks paginated" if it carries a top-level next-page marker.
.looks_paginated <- function(parsed) {
  is.list(parsed) && (!is.null(parsed$next_cursor) || !is.null(parsed[["next"]]))
}

.rest_auth_headers <- function(adapter) {
  if (adapter@auth == "header") {
    token <- Sys.getenv(adapter@token_env)
    return(list(Authorization = token))
  }
  list()
}

S7::method(fetch, source_rest) <- function(adapter, site, variables, window, now = .now()) {
  url <- .rest_build_url(adapter@endpoint, site, window)
  headers <- .rest_auth_headers(adapter)

  request_fn <- .http_get
  if (adapter@auth == "basic") {
    user <- Sys.getenv(paste0(adapter@token_env, "_USER"))
    pass <- Sys.getenv(paste0(adapter@token_env, "_PASS"))
    headers$Authorization <- paste(
      "Basic", jsonlite::base64_enc(paste0(user, ":", pass))
    )
  }

  parsed <- request_fn(url, headers = headers, now = now)

  if (.looks_paginated(parsed)) {
    abort_meteo(
      c(
        "The response from {.url {url}} looks paginated.",
        "i" = "{.fn source_rest} supports single-page responses only (SCOPING §13).",
        "i" = "Write a bespoke adapter for paginated APIs."
      ),
      class = "unsupported_response"
    )
  }

  mapping <- adapter@mapping[[1]]
  out <- apply_mapping(parsed, mapping, site, source_id = adapter@source_id, now = now)
  out <- out[out$variable %in% variables, , drop = FALSE]
  check_fetch_result(out, adapter, variables)
}

S7::method(format, source_rest) <- function(x, ...) {
  c(
    sprintf("<source_rest> source_id: %s", x@source_id),
    sprintf("  endpoint: %s", x@endpoint),
    sprintf("  auth: %s", x@auth),
    if (x@auth != "none") sprintf("  token_env: %s (value not shown)", x@token_env)
  )
}

S7::method(print, source_rest) <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
