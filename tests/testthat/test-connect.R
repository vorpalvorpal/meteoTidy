# Plan 14 — met_connect() experimental SQL path, parity with the tibble surface.

describe("met_connect() parity", {
  it("returns the same current rows as met_record() for a fixture", {
    skip_if_not_installed("duckdb")
    root <- local_store()
    site <- make_test_site(store_root = root)
    store_write_obs(root, new_obs(make_obs(n = 5)), mode = "supersede")

    tibble_rows <- met_record(site)
    con <- met_connect(site, backend = "duckdb")
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
    sql_rows <- dplyr::collect(dplyr::tbl(con, "observations"))
    sql_current <- sql_rows[sql_rows$superseded == FALSE, ]
    expect_equal(nrow(sql_current), nrow(tibble_rows))
    expect_setequal(sql_current$value, tibble_rows$value)
  })

  it("carries the experimental lifecycle badge in its documentation", {
    # The tibble surface is the only stability promise; met_connect exposes the
    # physical schema and is documented experimental.
    expect_true(exists("met_connect"))
    # a machine-readable marker the roxygen sets (badge helper)
    expect_match(met_connect_lifecycle(), "experimental")
  })
})
