# Plan 07 — vendored weatherBOM licence notice (guards against a future edit
# stripping the MIT attribution). Reads the shipped source file, not a fixture.

describe("vendored weatherBOM source header", {
  it("retains the MIT notice and the Maurits Evers attribution", {
    # The vendored file lives in the installed/loaded package; locate its source.
    path <- NULL
    candidates <- c(
      testthat::test_path("..", "..", "R", "vendor-weatherbom.R"),
      system.file("R", "vendor-weatherbom.R", package = "meteoTidy")
    )
    for (p in candidates) if (nzchar(p) && file.exists(p)) { path <- p; break }
    skip_if(is.null(path), "vendored source not present until Plan 07 is built")

    src <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(src, "MIT")
    expect_match(src, "Maurits Evers")
    expect_match(src, "weatherBOM")
  })
})
