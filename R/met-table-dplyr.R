#' @include met-table.R
NULL

# Plan 15 -- dplyr/vctrs integration for `met_table` (SCOPING section 3.2).
#
# Empirically verified against the installed dplyr (1.2.0)/vctrs (0.7.2) in
# this environment (throwaway scripts, not guessed -- see
# R/meteoTidy-package.R's `.onLoad()` comment for a related, genuinely
# external dispatch quirk found along the way):
#
# 1. `dplyr::filter()`/`arrange()` funnel through `dplyr_row_slice(data, i)`;
#    `dplyr::mutate()` funnels through `dplyr_col_modify(data, cols)`. Both
#    default (`.data.frame`) implementations end by calling
#    `dplyr_reconstruct(new_plain_object, data)`, where `data` is the
#    original `met_table` (`template`) and the first argument is a
#    freshly-built plain object. `dplyr_reconstruct.met_table()` is the
#    standard, documented extension point for this, registered via
#    `#' @exportS3Method dplyr::dplyr_reconstruct`.
#
#    A real, self-inflicted bug was caught here during testing (not a
#    dplyr/S7 dispatch quirk): the first draft rebuilt the class as
#    `c("met_table", setdiff(class(data), "met_table"))`, but `data` in
#    `dplyr_col_modify()`'s default implementation is sometimes a bare
#    `vctrs::new_data_frame()` result (class `"data.frame"` only, no
#    `tbl_df`/`tbl`) -- so the reconstructed object silently lost
#    `tbl_df`/`tbl` from its class. That was invisible until a *keep-all*
#    `mutate()` (which still runs its result through `dplyr_col_select()`,
#    i.e. `` `[.met_table` ``, even when no column is actually dropped): the
#    `` `[.met_table` ``'s `NextMethod()` then dispatched to `` `[.data.frame` ``
#    instead of `` `[.tbl_df` ``, and `` `[.data.frame` `` does not carry
#    arbitrary extra attributes (provenance/keys/versions/content_hash)
#    forward the way tibble's own restore does -- silently dropping them.
#    The fix: always reconstruct the class from `tibble::as_tibble(data)`,
#    never from `data`'s own (possibly bare) class.
#
#    Dedicated `dplyr_col_modify.met_table()`/`dplyr_row_slice.met_table()`
#    methods (reimplementing the default `.data.frame` logic, then calling
#    `dplyr_reconstruct.met_table()` directly) are defined below rather than
#    relying on the `.data.frame` default + generic re-dispatch, so every
#    verb is provably routed through the same downgrade-checking logic
#    instead of trusting an accidental vctrs attribute-copy-through for
#    `filter()`/`arrange()` (verified: without an explicit
#    `dplyr_row_slice.met_table()`, `filter()`/`arrange()` "looked" fine but
#    only because vctrs's default restore blindly copies attributes
#    forward -- the exact silent-stale-attribute failure mode the plan
#    warns about, not really a pass).
#
# 2. `dplyr::select()` does **not** call `dplyr_reconstruct()` at all in
#    this dplyr version when the object's class is not exactly
#    `"data.frame"` (see `dplyr:::dplyr_col_select()`, which only calls
#    `dplyr_reconstruct()` when `identical(class(.data), "data.frame")`).
#    Instead it subsets via `.data[loc]`, i.e. R's own `` `[` `` dispatch --
#    which, absent a custom method, falls through to `tibble:::`[.tbl_df``,
#    a C-level attribute-copying restore that carries the `met_table` class
#    and every attribute forward *unconditionally*, even when a value column
#    was dropped. That is the silent-stale-attribute failure mode the plan
#    warns about. The fix verified here is a custom `` `[.met_table` ``
#    method that calls `NextMethod()` then checks whether every
#    provenance-covered value column survived; this also transparently
#    covers direct bracket indexing (`mt[, cols]`), not just `select()`.
#
# 3. `dplyr::bind_rows()` combining two `met_table`s calls
#    `vec_ptype2.met_table.met_table()` (registered via vctrs double
#    dispatch) *and, separately*, `dplyr_reconstruct(combined_data,
#    first_input)` -- but `dplyr_reconstruct()` only ever sees the *first*
#    input as `template`; it has no direct view of the second input's
#    provenance. `vec_ptype2()` is the only hook that sees both objects, so
#    it is where the provenance-compatibility check must happen; it flags
#    the mismatch via a package-private pending-downgrade flag (there is no
#    other channel between the two generics -- verified empirically that
#    `vec_cast()` is not even called on this code path), and
#    `dplyr_reconstruct()` reads (and clears) that flag to perform the
#    actual downgrade + emit the warning exactly once, at the point the
#    class would otherwise be reattached.

# Package-private signal from `vec_ptype2.met_table.met_table()` to the next
# `dplyr_reconstruct.met_table()` call: sidesteps the fact that
# `dplyr_reconstruct()`'s `template` argument is only ever the *first* input
# to `bind_rows()`, so it alone cannot see a second input's provenance.
.met_table_state <- new.env(parent = emptyenv())
.met_table_state$downgrade_pending <- FALSE

.met_table_downgrade_to_plain <- function(x) {
  out <- x
  class(out) <- setdiff(class(out), "met_table")
  attr(out, "provenance") <- NULL
  attr(out, "keys") <- NULL
  attr(out, "versions") <- NULL
  attr(out, "content_hash") <- NULL
  out
}

#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.met_table <- function(data, template) {
  if (isTRUE(.met_table_state$downgrade_pending)) {
    .met_table_state$downgrade_pending <- FALSE
    warn_meteo(
      "Combining {.cls met_table} objects with incompatible provenance; downgrading to a plain tibble.", # nolint: line_length_linter.
      class = "met_table_downgraded"
    )
    return(tibble::as_tibble(data))
  }

  value_cols <- met_value_columns(template)
  if (!all(value_cols %in% names(data))) {
    warn_meteo(
      "A dplyr operation dropped a value column tracked in {.cls met_table} provenance; downgrading to a plain tibble.", # nolint: line_length_linter.
      class = "met_table_downgraded"
    )
    return(tibble::as_tibble(data))
  }

  # Reconstruct the class from `tibble::as_tibble()`'s output, not from
  # `data`'s own class, since `data` here is sometimes a bare `data.frame`
  # (e.g. `vctrs::new_data_frame()`'s output in `dplyr_col_modify.met_table()`
  # above) -- naively prepending `"met_table"` onto that would silently drop
  # `tbl_df`/`tbl` from the class vector, which in turn makes a later
  # `` `[.met_table` ``'s `NextMethod()` dispatch to `` `[.data.frame` `` (not
  # `` `[.tbl_df` ``), and `` `[.data.frame` `` does not carry arbitrary
  # extra attributes (provenance/keys/versions/content_hash) forward the way
  # tibble's restore does -- a real bug caught by testing `mutate()` after a
  # keep-all `dplyr_col_select()` pass, not a dispatch mystery.
  out <- tibble::as_tibble(data)
  class(out) <- c("met_table", class(out))
  attr(out, "provenance") <- attr(template, "provenance")
  attr(out, "keys") <- attr(template, "keys")
  attr(out, "versions") <- attr(template, "versions")
  attr(out, "content_hash") <- attr(template, "content_hash")
  out
}

#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.met_table <- function(data, cols) {
  # Reimplements `dplyr:::dplyr_col_modify.data.frame()`'s logic (rather than
  # `NextMethod()`) so the call into `dplyr_reconstruct.met_table()` below is
  # explicit and provably reached, instead of depending on the default
  # `.data.frame` method's own internal call to the *generic*
  # `dplyr_reconstruct()` (see the file-level note above for the real class-
  # construction bug this surfaced during testing).
  cols <- vctrs::vec_recycle_common(!!!cols, .size = nrow(data))
  out <- as.list(vctrs::vec_data(data))
  nms <- names(cols)
  names(out) <- names(out)
  for (i in seq_along(cols)) {
    out[[nms[[i]]]] <- cols[[i]]
  }
  row_names <- .row_names_info(data, type = 0L)
  out <- vctrs::new_data_frame(out, n = nrow(data), row.names = row_names)
  dplyr_reconstruct.met_table(out, data)
}

#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.met_table <- function(data, i, ...) {
  # Mirrors `dplyr_col_modify.met_table()` above: `filter()`/`arrange()`
  # (which route through `dplyr_row_slice()`) were observed to keep the
  # class/attributes even without this method, via vctrs's own
  # attribute-copying restore -- but that path doesn't run *this* class's
  # downgrade logic, so it is the "silent stale-attribute carry-through"
  # the plan warns about, not a real pass. Defining this method explicitly
  # makes row-slicing go through the same `dplyr_reconstruct.met_table()`
  # checks as every other verb, for the same robustness reason as
  # `dplyr_col_modify.met_table()`.
  out <- vctrs::vec_slice(tibble::as_tibble(data), i)
  dplyr_reconstruct.met_table(out, data)
}

#' @export
`[.met_table` <- function(x, ...) {
  out <- NextMethod()
  if (inherits(out, "met_table")) {
    value_cols <- met_value_columns(x)
    if (!all(value_cols %in% names(out))) {
      warn_meteo(
        "Dropping a value column tracked in {.cls met_table} provenance; downgrading to a plain tibble.", # nolint: line_length_linter.
        class = "met_table_downgraded"
      )
      out <- .met_table_downgrade_to_plain(out)
    }
  }
  out
}

#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.met_table.met_table <- function(x, y, ...) {
  # Clear any stale TRUE left by a prior combine that set the flag here but
  # then errored before the matching dplyr_reconstruct.met_table() call
  # could read (and clear) it -- otherwise that leak silently downgrades
  # this, unrelated, comparison's result.
  .met_table_state$downgrade_pending <- FALSE

  px <- met_provenance(x)
  py <- met_provenance(y)
  shared <- intersect(px$variable, py$variable)
  compatible <- vapply(shared, function(v) {
    identical(px$tier[px$variable == v], py$tier[py$variable == v])
  }, logical(1))

  if (!all(compatible)) {
    .met_table_state$downgrade_pending <- TRUE
  }

  vctrs::vec_ptype2(tibble::as_tibble(x), tibble::as_tibble(y), ...)
}

#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.met_table.tbl_df <- function(x, y, ...) {
  vctrs::vec_ptype2(tibble::as_tibble(x), y, ...)
}

#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.tbl_df.met_table <- function(x, y, ...) {
  vctrs::vec_ptype2(x, tibble::as_tibble(y), ...)
}

#' @exportS3Method vctrs::vec_cast
vec_cast.met_table.met_table <- function(x, to, ...) {
  tibble::as_tibble(x)
}
