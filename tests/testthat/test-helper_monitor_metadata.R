raw_monitor_metadata <- .get_monitor_metadata()

aqi_naaqs_monitor_metadata <- raw_monitor_metadata |>
  .filter_aqi_naaqs_parameters() |>
  .clean_monitor_metadata_parameter_names() |>
  .select_monitor_metadata_columns(monitor_type = "aqi_naaqs")

regulatory_monitor_metadata <- aqi_naaqs_monitor_metadata |>
  .filter_regulatory_parameters() |>
  dplyr::select(-"parameter_code")
