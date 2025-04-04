annual_concentration_by_monitor_2012_2023 <- .get_annual_concentration_by_monitor_across_years(
  start = 2012,
  end = 2023
)

aqi_naaqs_monitors_raw <- .get_aqi_naaqs_monitors_by_vintage(
  start = 2012,
  end = 2023
)

aqi_naaqs_monitors_final <- aqi_naaqs_monitors_raw |>
  .join_aqi_naaqs_monitor_monitor_metadata(
    monitor_metadata = aqi_naaqs_monitor_metadata
  )
