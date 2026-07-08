# print() shows the provenance banner / renders the compact per-column tier banner (snapshot)

    Code
      print(make_met_table())
    Output
      # met_table provenance:
      #   temperature_2m: qmap (openmeteo, 24h overlap)
      #   wind_speed_10m: qmap (openmeteo, 24h overlap)
      # A tibble: 3 x 3
        time                temperature_2m wind_speed_10m
        <dttm>                       <dbl>          <dbl>
      1 2026-01-01 00:00:00           15              3  
      2 2026-01-01 01:00:00           15.5            3.1
      3 2026-01-01 02:00:00           16              3.2

