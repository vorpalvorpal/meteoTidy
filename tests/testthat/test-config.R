# Plan 14 — deployment config loader (global + per-site YAML).

describe("read_deployment_config()", {
  it("loads a valid config with sites, store roots, and refetch windows", {
    cfg <- read_deployment_config(test_path("_fixtures/config/deployment.yaml"))
    expect_equal(cfg$store_root, "/data/meteo")
    expect_equal(cfg$refetch_windows$silo, as.difftime(30, units = "days"))
    expect_false(cfg$adapter_defaults$bom$allow_web_api)
    expect_equal(cfg$adapter_defaults$openmeteo$api_key_env, "OPEN_METEO_KEY")
  })

  it("aborts unknown_config_key on a typo'd top-level key", {
    expect_error(
      read_deployment_config(test_path("_fixtures/config/bad-unknown-key.yaml")),
      class = "meteoTidy_error_unknown_config_key"
    )
  })

  it("aborts inline_secret when a secret value appears inline (Plan 02 guard)", {
    expect_error(
      read_deployment_config(test_path("_fixtures/config/bad-inline-secret.yaml")),
      class = "meteoTidy_error_inline_secret"
    )
  })
})
