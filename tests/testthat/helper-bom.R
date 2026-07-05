# Helpers for Plan 07 — BOM transport ladder / breaker / adapter fixtures.

# Load a BOM fixture (XML as text, JSON as parsed list).
read_bom_xml <- function(name) {
  xml2::read_xml(testthat::test_path(file.path("_fixtures/bom", name)))
}
read_bom_json <- function(name) {
  read_json_fixture(file.path("_fixtures/bom", name))
}

# Build a fake transport rung for ladder tests. `outcome` is one of:
#   - a tibble of canonical rows to return (success),
#   - "gone"      → abort class "http_gone" (persistent → a breaker strike),
#   - "transient" → abort class "http_client_error" (no persistent strike).
# Every call increments counter$<id> so tests can assert rung call counts.
fake_transport <- function(id, applies_to, outcome, counter = new.env()) {
  if (is.null(counter[[id]])) counter[[id]] <- 0L
  fetch_fn <- function(request, now = NULL) {
    counter[[id]] <- counter[[id]] + 1L
    if (is.character(outcome) && identical(outcome, "gone")) {
      abort_meteo("gone", class = "http_gone")
    } else if (is.character(outcome) && identical(outcome, "transient")) {
      abort_meteo("temporary", class = "http_client_error")
    }
    outcome
  }
  list(id = id, kind = id, fetch_fn = fetch_fn, applies_to = applies_to,
       counter = counter)
}

# A minimal canonical obs tibble a rung can "serve" (transport stamped by ladder).
bom_rows <- function(source = "bom_obs") {
  new_obs(make_obs(n = 2, source = source, method = "measured"))
}

# A request descriptor the ladder routes (product decides which rungs apply).
bom_request <- function(product = "obs_72h",
                        variables = "temperature_2m",
                        window = list(from = as.POSIXct("2026-01-01", tz = "UTC"),
                                      to = as.POSIXct("2026-01-02", tz = "UTC"))) {
  list(product = product, variables = variables, window = window)
}
