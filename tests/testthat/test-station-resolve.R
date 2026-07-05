# Plan 06 — shared nearest-station resolution (haversine + identity dedup).

describe("nearest_stations()", {
  it("orders a hand-built catalogue by great-circle distance", {
    cat <- make_station_catalogue()
    got <- nearest_stations(-34.75, 148.20, cat, n = 3)
    expect_equal(got$station_id, c("near", "mid", "far"))
    expect_false(is.unsorted(got$distance_km))
  })

  it("computes distances within tolerance of a reference haversine", {
    cat <- make_station_catalogue()
    got <- nearest_stations(-34.75, 148.20, cat, n = 3)
    # reference haversine (R = 6371 km) for the 'near' station
    ref_km <- {
      R <- 6371
      dlat <- (-34.76 - -34.75) * pi / 180
      dlon <- (148.21 - 148.20) * pi / 180
      a <- sin(dlat / 2)^2 +
        cos(-34.75 * pi / 180) * cos(-34.76 * pi / 180) * sin(dlon / 2)^2
      2 * R * asin(sqrt(a))
    }
    expect_equal(got$distance_km[got$station_id == "near"], ref_km,
                 tolerance = 0.05)
  })

  it("returns fewer than n when the catalogue is smaller", {
    cat <- make_station_catalogue()[1, , drop = FALSE]
    got <- nearest_stations(-34.75, 148.20, cat, n = 3)
    expect_equal(nrow(got), 1)
  })
})

describe("station identity dedup", {
  it("collapses one physical station present under two ids to a single donor", {
    # BOM real-time feeds and GHCNh often serve the SAME station via different
    # transports/ids; the fill ladder (Plan 10) must not double-count it.
    cat <- data.frame(
      station_id = c("bom-072150", "ghcnh-ASN00072150"),
      identity   = c("072150", "072150"),        # same physical station
      latitude   = c(-34.76, -34.76),
      longitude  = c(148.21, 148.21),
      stringsAsFactors = FALSE
    )
    got <- nearest_stations(-34.75, 148.20, cat, n = 3)
    expect_equal(nrow(got), 1)                    # collapsed by identity
  })
})
