regulatory_monitors_raw <- .get_regulatory_monitors(start = 2012, end = 2023)
regulatory_site_metadata <- .get_regulatory_site_metadata()
regulatory_monitor_metadata <- .get_monitor_metadata() |>
  .get_regulatory_monitor_metadata()
regulatory_monitors <- regulatory_monitors_raw |>
  .join_regulatory_monitors_site_metadata(regulatory_site_metadata) |>
  .join_regulatory_monitors_monitor_metadata(regulatory_monitor_metadata) |>
  dplyr::select(
    "aqs_site_id",
    "poc",
    "parameter_name",
    "latitude",
    "longitude",
    "measurement_scale",
    "measurement_scale_definition",
    "monitoring_objective",
    "year"
  ) |>
  dplyr::distinct() |>
  .split_vintages()
