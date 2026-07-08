# Plan 14 -- the deployment config: global settings layered over the
# per-site YAML (Plan 02), reusing that plan's inline-secret guard directly
# (`.check_no_inline_secrets()`, `R/site-yaml.R`) rather than re-implementing
# it. Non-secret only: secrets are referenced by `*_env`/`*_keyring` name,
# never inlined (SCOPING section 11).

.deployment_top_level_keys <- c(
  "store_root", "sites_file", "refetch_windows", "adapter_defaults", "sources"
)

#' Read a deployment configuration from YAML
#'
#' Parses a version-controlled deployment config: the store root, the
#' per-site YAML file to load (`sites_file`), per-source **refetch windows**
#' (parsed into `difftime`s -- reuses `R/verify.R`'s `.parse_period()`
#' `"<number> <unit>"` parser), adapter defaults (e.g. `allow_web_api`), and
#' source configs. Unknown top-level keys abort `"unknown_config_key"` (a
#' typo fails loud, matching Plan 02's site-YAML convention); an inline
#' secret value anywhere under `sources` aborts `"inline_secret"` (Plan 02's
#' existing guard).
#'
#' @param path Single string, path to a deployment YAML file.
#' @return A list: `store_root`, `sites_file`, `refetch_windows` (a named
#'   list of `difftime`s), `adapter_defaults`, `sources`.
#' @keywords internal
#' @noRd
read_deployment_config <- function(path) {
  raw <- yaml::read_yaml(path)

  recognised <- .deployment_top_level_keys
  unknown <- setdiff(names(raw), recognised)
  if (length(unknown) > 0) {
    abort_meteo(
      c(
        "Unknown top-level key{?s} in deployment config: {.val {unknown}}.",
        "i" = "Recognised keys: {.val {recognised}}."
      ),
      class = "unknown_config_key"
    )
  }

  .check_no_inline_secrets(raw$sources %||% list())

  refetch_windows <- lapply(raw$refetch_windows %||% list(), .parse_period)

  list(
    store_root = raw$store_root,
    sites_file = raw$sites_file,
    refetch_windows = refetch_windows,
    adapter_defaults = raw$adapter_defaults %||% list(),
    sources = raw$sources %||% list()
  )
}
