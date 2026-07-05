# meteoTidy (development version)

- Initial scaffolding: package infrastructure, `testthat` 3 harness, CI, the
  classed condition system (`abort_meteo()`, `warn_meteo()`, `inform_meteo()`,
  `meteo_conditions()`), and the injectable clock seam (`.now()`).
- Site registry: the `met_site`/`met_instrument` S7 classes and their
  validators, the `met_sites` collection wrapper, and YAML (de)serialisation
  (`read_sites_yaml()`/`write_sites_yaml()`) for version-controlled site
  configuration, including the resolved-external-ID cache accessors
  (`site_resolved()`/`site_set_resolved()`).
