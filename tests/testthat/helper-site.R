# Helpers for Plan 02 — site registry builders.

# A valid single site. `store_root` defaults to a tempdir tied to the caller's
# frame so it is cleaned up when the test finishes.
make_test_site <- function(site_id = "test",
                           with_wind = TRUE,
                           with_pyranometer = FALSE,
                           sources = list(),
                           store_root = NULL,
                           env = parent.frame()) {
  if (is.null(store_root)) {
    store_root <- withr::local_tempdir(.local_envir = env)
  }

  instruments <- list()
  if (with_wind) {
    instruments <- c(instruments, list(met_instrument(
      name = "anemometer",
      variable = c("wind_speed_10m", "wind_direction_10m", "wind_gusts_10m"),
      height = units::set_units(10, "m"),
      roughness_length = units::set_units(0.03, "m"),
      displacement_height = units::set_units(0, "m")
    )))
  }
  instruments <- c(instruments, list(met_instrument(
    name = "thermo",
    variable = c("temperature_2m", "relative_humidity_2m"),
    height = units::set_units(2, "m")
  )))
  if (with_pyranometer) {
    instruments <- c(instruments, list(met_instrument(
      name = "pyranometer",
      variable = c("direct_radiation", "diffuse_radiation"),
      height = units::set_units(2, "m")
    )))
  }

  met_site(
    site_id = site_id,
    latitude = units::set_units(-34.75, "degree"),
    longitude = units::set_units(148.20, "degree"),
    elevation = units::set_units(220, "m"),
    timezone = "Australia/Sydney",
    instruments = instruments,
    sources = sources,
    store_root = store_root
  )
}

make_test_sites <- function(n = 2, env = parent.frame()) {
  sites <- lapply(seq_len(n), function(i) {
    make_test_site(site_id = paste0("site_", i), env = env)
  })
  met_sites(sites)
}
