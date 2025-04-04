test_that("Monitor metadata URL is active", {
  expect_true(httr::HEAD("https://aqs.epa.gov/aqsweb/airdata/aqs_monitors.zip")$status == 200)
})

test_that("Parameter codes are filtered correctly with actual data", {
  expect_equal(
    .filter_aqi_naaqs_parameters(raw_monitor_metadata),
    raw_monitor_metadata |>
      dplyr::filter(parameter_code %in% c(42101, 14129, 85129, 42602, 44201, 81102, 88101, 42401, 88502))
  )
  expect_equal(
    .filter_regulatory_parameters(raw_monitor_metadata),
    raw_monitor_metadata |>
      dplyr::filter(parameter_code %in% c(42101, 14129, 85129, 42602, 44201, 81102, 88101, 42401))
  )
})

test_that("Parameter names are modified correctly", {
  parameter_names <- c(
    "Carbon monoxide",
    "Lead",
    "Nitrogen dioxide (NO2)",
    "Ozone",
    "PM10",
    "PM2.5",
    "Sulfur dioxide"
  )
  expect_equal(
    raw_monitor_metadata |>
      .filter_aqi_naaqs_parameters() |>
      .clean_monitor_metadata_parameter_names() |>
      dplyr::pull(parameter_name) |>
      unique() |>
      sort(),
    parameter_names
  )
})

test_that("Columns are selected appropriately", {
  expect_equal(
    colnames(aqi_naaqs_monitor_metadata),
    c(
      "aqs_site_id",
      "poc",
      "parameter_code",
      "parameter_name",
      "measurement_scale",
      "measurement_scale_definition",
      "monitoring_objective"
    )
  )
  expect_equal(
    colnames(regulatory_monitor_metadata),
    c(
      "aqs_site_id",
      "poc",
      "parameter_name",
      "measurement_scale",
      "measurement_scale_definition",
      "monitoring_objective"
    )
  )
})
