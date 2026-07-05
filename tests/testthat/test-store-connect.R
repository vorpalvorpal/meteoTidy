# Plan 03 — optional DuckDB read path (parity with the arrow reader).

describe("store_connect()", {
  it("sees exactly the current rows store_read_obs() sees", {
    skip_if_not_installed("duckdb")
    root <- local_store()
    store_write_obs(root, new_obs(make_obs(n = 5)), mode = "supersede")

    arrow_rows <- store_read_obs(root, "test")
    con <- store_connect(root, backend = "duckdb")
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    duck_rows <- dplyr::collect(dplyr::tbl(con, "observations"))
    duck_current <- duck_rows[duck_rows$superseded == FALSE, ]
    expect_equal(nrow(duck_current), nrow(arrow_rows))
    expect_setequal(duck_current$value, arrow_rows$value)
  })
})
