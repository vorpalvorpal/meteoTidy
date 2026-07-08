#' @keywords internal
#' @importFrom rlang .data %||% abort caller_env
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# S7's documented package convention (vignette("packages", package = "S7")):
# always call methods_register() in .onLoad() so S7 methods for generics
# owned by other packages (several earlier plans' `S7::method(print, ...)`/
# `S7::method(format, ...)` on adapter classes, e.g. R/source-rest.R) are
# registered against the generic's *final* definition rather than a snapshot
# taken at package build time.
#
# Empirically verified (Plan 15, throwaway scripts under a bisected copy of
# this package): `S7::method(print, <any S7 class>) <-`/`S7::method(format,
# <any S7 class>) <-` anywhere in this package -- as several Plan 04-08
# adapters' `format`/`print` methods do -- corrupts *ordinary* S3 dispatch
# of `print()`/`format()` for every OTHER plain-S3 class in the same
# package (verified with a minimal, unrelated non-tbl_df reproduction), even
# though the S3 method stays correctly listed in the package's
# `.__S3MethodsTable__.` the whole time -- so `print.met_table`/
# `format.met_table` (R/met-table.R) silently stopped dispatching from a
# caller outside the package despite a correct `S3method()` NAMESPACE entry.
# Explicitly re-registering them here, with `envir` pointed at this
# package's own namespace, fixes dispatch; this must run from `.onLoad()`
# (after every top-level `S7::method()<-` call in the package has already
# executed), not from a `S3method()` NAMESPACE declaration alone.
.onLoad <- function(libname, pkgname) { # nolint: object_name_linter.
  S7::methods_register()
  registerS3method("print", "met_table", print.met_table, envir = asNamespace(pkgname))
  registerS3method("format", "met_table", format.met_table, envir = asNamespace(pkgname))
}
