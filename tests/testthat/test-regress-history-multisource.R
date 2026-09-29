# Regression (found in the live acceptance run, 2026-09-29): with two AWS-like
# obs sources for a site (the on-site eagle.io gauge and BOM's nearby
# station), build_history_daily() overlaid both. Their temperatures at the
# same instant collided in the consistency pass ("Column name
# `temperature_2m` must not be duplicated"), failing history for the site.
# Sources now take precedence in the order the site's YAML lists them.

history_store <- function(order) {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  src_cfg <- list(
    eagleio = list(adapter = "eagleio", nodes = list(temperature_2m = "n1")),
    bom_obs = list(adapter = "bom_obs", allow_web_api = TRUE)
  )[order]
  site <- make_prod_site("kat", store_root = root, sources = src_cfg, env = parent.frame())
  t <- as.POSIXct("2026-09-20 00:00:00", tz = "UTC") + 3600 * 0:2
  mk <- function(source, value) tibble::tibble(
    site_id = "kat", datetime_utc = t, variable = "temperature_2m", value = value,
    source = source, method = "measured", qc_flag = "ok"
  )
  store_write_obs(root, new_obs(rbind(mk("eagleio", c(10, 11, 12)), mk("bom_obs", c(20, 21, 22)))),
                  now = prod_now())
  list(root = root, site = site,
       window = list(from = as.POSIXct("2026-09-19", tz = "UTC"), to = prod_now()))
}

describe("build_history_daily() with several AWS-like sources", {
  it("keeps one value per variable and instant, from the first-listed source", {
    s <- history_store(c("eagleio", "bom_obs"))
    out <- build_history_daily(s$root, s$site, s$window)
    temp <- out[out$variable == "temperature_2m", ]
    expect_equal(anyDuplicated(temp$datetime_utc), 0L)
    expect_setequal(temp$source, "eagleio")
    expect_equal(sort(temp$value), c(10, 11, 12))
  })

  it("follows the YAML order when BOM is listed first", {
    s <- history_store(c("bom_obs", "eagleio"))
    out <- build_history_daily(s$root, s$site, s$window)
    expect_setequal(out$source[out$variable == "temperature_2m"], "bom_obs")
  })
})
