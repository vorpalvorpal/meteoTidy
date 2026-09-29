# Post-review, medium: a failed write verification left the bad file behind.
#
# .write_part() wrote straight to the final part-file path and then
# verified it; when verification failed (a truncated or unreadable file, as
# on a path Windows cannot reopen) the error was raised but the bad file
# stayed in the partition, so every later read of the table could fail.
# Now the part is written to a temp name, verified, renamed into place and
# verified again; any failure removes what was written. The failure is
# injected by truncating the real Parquet file just before verification.

corrupting_verify <- function() {
  real <- .verify_part
  function(path, n) {
    writeBin(as.raw(1:16), path) # an unreadable, truncated "parquet" file
    real(path, n)
  }
}

describe(".write_part() after a failed verification", {
  it("leaves no file behind and the table still reads", {
    root <- withr::local_tempdir()
    store_write_obs(root, make_obs(n = 3, site_id = "kat"), mode = "append")
    before <- store_read_obs(root, "kat")

    expect_error(
      with_mocked_bindings(
        store_write_obs(root, make_obs(n = 2, site_id = "kat", variable = "wind_speed_10m"),
                        mode = "append"),
        .verify_part = corrupting_verify()
      ),
      class = "meteoTidy_error_store_write_unverified"
    )
    files <- list.files(root, recursive = TRUE, all.files = TRUE)
    files <- files[!grepl("[.]lock$", files)]
    expect_equal(length(grep("[.]parquet$", files)), 1L)

    expect_equal(store_read_obs(root, "kat"), before)
  })
})
