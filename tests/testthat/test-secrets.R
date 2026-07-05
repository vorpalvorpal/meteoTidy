# Plan 14 — secret resolution, redaction, and the store-write guard.

describe("resolve_secret()", {
  it("reads an env-var reference at use time and does not persist it", {
    withr::local_envvar(MY_SECRET = "top-secret-value")
    val <- resolve_secret(list(env = "MY_SECRET"))
    expect_equal(val, "top-secret-value")
    # nothing about the resolution is cached on a global/object we can see
    expect_false(any(grepl("top-secret-value",
                           utils::capture.output(str(resolve_secret)))))
  })

  it("aborts when the referenced env var is unset", {
    withr::local_envvar(MY_SECRET = NA)
    expect_error(resolve_secret(list(env = "MY_SECRET")),
                 class = "meteoTidy_error_secret_unresolved")
  })
})

describe("redact()", {
  it("hides a secret value in print output", {
    withr::local_envvar(MY_SECRET = "top-secret-value")
    printed <- redact("token is top-secret-value", "top-secret-value")
    expect_false(grepl("top-secret-value", printed))
    expect_match(printed, "\\*|REDACTED|<redacted>")
  })
})

describe("assert_no_secrets_in() — the belt-and-braces store-write guard", {
  it("aborts secret_leak when a resolved secret appears in a store-bound frame", {
    withr::local_envvar(MY_SECRET = "top-secret-value")
    secret <- resolve_secret(list(env = "MY_SECRET"))
    df <- tibble::tibble(site_id = "test", note = "token top-secret-value")
    expect_error(
      assert_no_secrets_in(df, secrets = secret),
      class = "meteoTidy_error_secret_leak"
    )
  })

  it("passes a clean frame carrying no secret", {
    df <- tibble::tibble(site_id = "test", value = 20)
    expect_no_error(assert_no_secrets_in(df, secrets = "top-secret-value"))
  })
})
