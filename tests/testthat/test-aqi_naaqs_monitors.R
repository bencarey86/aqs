# Unit tests ----------------------------------------------------------------
test_that("Only observations with positive counts are retained", {
  df <- data.frame(
    aqs_site_id = 1:10,
    poc = 1:10,
    observation_count = c(0:5, NA, 0, 8, -10)
  )
  expected_df <- data.frame(
    aqs_site_id = c(2:6, 9),
    poc = c(2:6, 9),
    observation_count = c(1:5, 8)
  )
  expect_equal(
    df |>
      .filter_active_monitors(),
    expected_df
  )
})

test_that("All monitors columns are modified as expected", {
  df <- data.frame(
    state_code = c("01", "02", "03"),
    county_code = c("001", "002", "003"),
    site_num = c("0001", "0002", "0003"),
    poc = c(1, 2, 3),
    parameter_code = c(42101, 14129, 85129),
    latitude = c(30.0, 31.0, 32.0),
    longitude = c(-85.0, -86.0, -87.0),
    year = c(2012, 2013, 2014),
    other_column = c("a", "b", "c")
  )
  expected_df <- data.frame(
    aqs_site_id = c("010010001", "020020002", "030030003"),
    poc = c(1, 2, 3),
    parameter_code = c(42101, 14129, 85129),
    latitude = c(30.0, 31.0, 32.0),
    longitude = c(-85.0, -86.0, -87.0),
    year = c(2012, 2013, 2014)
  )
  expect_equal(
    df |>
      .process_annual_concentration_by_monitor_columns(),
    expected_df
  )
})

test_that("Monitor metadata join does not drop or add observations", {
  aqi_naaqs_monitors <- list(
    vintage_4 = data.frame(
      aqs_site_id = c("010010001", "020020002", "040040004"),
      poc = c(1, 2, 4),
      parameter_code = c(42101, 14129, 42101),
      latitude = c(30.0, 31.0, 33.0),
      longitude = c(-85.0, -86.0, -88.0)
    ),
    vintage_410 = data.frame(
      aqs_site_id = c("030030003", "050050005"),
      poc = c(3, 5),
      parameter_code = c(85129, 85129),
      latitude = c(32.0, 34.0),
      longitude = c(-87.0, -89.0)
    )
  )
  monitor_metadata <- data.frame(
    aqs_site_id = c("010010001", "020020002", "030030003"),
    poc = c(1, 2, 3),
    parameter_code = c(42101, 14129, 85129),
    parameter_name = c("Ozone", "PM2.5", "PM10"),
    measurement_scale = c("Micro", "Neighborhood", "Urban"),
    measurement_scale_definition = c("Micro scale definition", "Neighborhood scale definition", "Urban scale definition"),
    monitoring_objective = c("NAAQS", "NAAQS", "NAAQS")
  )
  expected_list <- list(
    vintage_4 = data.frame(
      aqs_site_id = c("010010001", "020020002", "040040004"),
      poc = c(1, 2, 4),
      parameter_code = c(42101, 14129, 42101),
      latitude = c(30.0, 31.0, 33.0),
      longitude = c(-85.0, -86.0, -88.0),
      parameter_name = c("Ozone", "PM2.5", NA),
      measurement_scale = c("Micro", "Neighborhood", NA),
      measurement_scale_definition = c("Micro scale definition", "Neighborhood scale definition", NA),
      monitoring_objective = c("NAAQS", "NAAQS", NA)
    ),
    vintage_410 = data.frame(
      aqs_site_id = c("030030003", "050050005"),
      poc = c(3, 5),
      parameter_code = c(85129, 85129),
      latitude = c(32.0, 34.0),
      longitude = c(-87.0, -89.0),
      parameter_name = c("PM10", NA),
      measurement_scale = c("Urban", NA),
      measurement_scale_definition = c("Urban scale definition", NA),
      monitoring_objective = c("NAAQS", NA)
    )
  )
  expect_equal(
    aqi_naaqs_monitors |>
      .join_aqi_naaqs_monitor_monitor_metadata(monitor_metadata),
    expected_list
  )
})

test_that("Monitor metadata join with different metadata columns throws error", {
  aqi_naaqs_monitors <- list(
    vintage_4 = data.frame(
      aqs_site_id = c("010010001", "020020002"),
      poc = c(1, 2),
      parameter_code = c(42101, 14129),
      latitude = c(30.0, 31.0),
      longitude = c(-85.0, -86.0)
    ),
    vintage_410 = data.frame(
      aqs_site_id = c("030030003"),
      poc = c(3),
      parameter_code = c(85129),
      latitude = c(32.0),
      longitude = c(-87.0)
    )
  )
  monitor_metadata_missing <- data.frame(
    aqs_site_id = c("010010001", "020020002"),
    poc = c(1, 2),
    parameter_code = c(42101, 14129),
    parameter_name = c("Ozone", "PM2.5")
  )
  monitor_metadata_extra <- data.frame(
    aqs_site_id = c("010010001", "020020002"),
    poc = c(1, 2),
    parameter_code = c(42101, 14129),
    parameter_name = c("Ozone", "PM2.5"),
    extra_column = c("extra1", "extra2")
  )
  expect_error(
    aqi_naaqs_monitors |>
      .join_aqi_naaqs_monitor_monitor_metadata(monitor_metadata_missing),
    "Monitor metadata does not have the expected columns."
  )
  expect_error(
    aqi_naaqs_monitors |>
      .join_aqi_naaqs_monitor_monitor_metadata(monitor_metadata_extra),
    "Monitor metadata does not have the expected columns."
  )
})

# Integration tests ------------------------------------------------------
test_that("No monitors are unexpectedly dropped in initial filters (aqi_naaqs_parameters, active monitors)", {
  vintage_410 <- annual_concentration_by_monitor_2012_2023 |>
    dplyr::filter(year >= 2010 & year < 2020) |>
    dplyr::filter(parameter_code %in% aqi_naaqs_parameters$parameter_code) |>
    dplyr::filter(observation_count >= 1) |>
    dplyr::select("state_code", "county_code", "site_num", "poc", "parameter_code", "latitude", "longitude", "year") |>
    dplyr::distinct() |>
    nrow()
  vintage_4 <- annual_concentration_by_monitor_2012_2023 |>
    dplyr::filter(year >= 2020 & year < 2030) |>
    dplyr::filter(parameter_code %in% aqi_naaqs_parameters$parameter_code) |>
    dplyr::filter(observation_count >= 1) |>
    dplyr::select("state_code", "county_code", "site_num", "poc", "parameter_code", "latitude", "longitude", "year") |>
    dplyr::distinct() |>
    nrow()
  expect_equal(
    vintage_4,
    nrow(aqi_naaqs_monitors_raw$vintage_4)
  )
  expect_equal(
    vintage_410,
    nrow(aqi_naaqs_monitors_raw$vintage_410)
  )
})

test_that("Expected columns are present in initial aqi_naaqs monitors DFs", {
  vintage_410 <- aqi_naaqs_monitors_raw$vintage_410
  vintage_4 <- aqi_naaqs_monitors_raw$vintage_4
  expected_cols <- c(
    "aqs_site_id",
    "poc",
    "parameter_code",
    "latitude",
    "longitude",
    "year"
  )
  expect_equal(
    colnames(vintage_410),
    expected_cols
  )
  expect_equal(
    colnames(vintage_4),
    expected_cols
  )
})

test_that("No duplicates exist in initial aqi_naaqs monitors DFs", {
  expect_equal(
    nrow(aqi_naaqs_monitors_raw$vintage_410),
    nrow(dplyr::distinct(aqi_naaqs_monitors_raw$vintage_410))
  )
  expect_equal(
    nrow(aqi_naaqs_monitors_raw$vintage_4),
    nrow(dplyr::distinct(aqi_naaqs_monitors_raw$vintage_4))
  )
})

# Duplicate check for monitor metadata performed in testing file for monitor metadata
# Move to monitor metadata tests
test_that("No duplicates exist in AQI/NAAQS monitor metadata", {
  expect_equal(
    nrow(aqi_naaqs_monitor_metadata),
    nrow(dplyr::distinct(aqi_naaqs_monitor_metadata))
  )
})

test_that("There are no duplicates in key columns for monitor metadata", {
  expect_equal(
    aqi_naaqs_monitor_metadata |>
      dplyr::group_by(aqs_site_id, poc, parameter_code) |>
      dplyr::count() |>
      dplyr::filter(n > 1) |>
      nrow(),
    0
  )
})

test_that("No unmatched monitors are dropped and monitors are not unexpectedly added in metadata join", {
  vintage_4 <- aqi_naaqs_monitors_raw$vintage_4
  vintage_410 <- aqi_naaqs_monitors_raw$vintage_410
  joined <- aqi_naaqs_monitors_raw |>
    .join_aqi_naaqs_monitor_monitor_metadata(aqi_naaqs_monitor_metadata)
  vintage_4_joined <- joined$vintage_4
  vintage_410_joined <- joined$vintage_410
  expect_equal(
    c(vintage_4$aqs_site_id, vintage_4$poc, vintage_4$parameter_code),
    c(vintage_4_joined$aqs_site_id, vintage_4_joined$poc, vintage_4_joined$parameter_code)
  )
  expect_equal(
    c(vintage_410$aqs_site_id, vintage_410$poc, vintage_410$parameter_code),
    c(vintage_410_joined$aqs_site_id, vintage_410_joined$poc, vintage_410_joined$parameter_code)
  )
})

test_that("Joined DFs have expected columns after metadata join", {
  joined <- aqi_naaqs_monitors_raw |>
    .join_aqi_naaqs_monitor_monitor_metadata(aqi_naaqs_monitor_metadata)
  expected_cols <- c(
    "aqs_site_id",
    "poc",
    "parameter_code",
    "latitude",
    "longitude",
    "year",
    "parameter_name",
    "measurement_scale",
    "measurement_scale_definition",
    "monitoring_objective"
  )
  expect_equal(
    colnames(joined$vintage_410),
    expected_cols
  )
  expect_equal(
    colnames(joined$vintage_4),
    expected_cols
  )
})

test_that("No monitor has multiple CRS coordinates", {
  expect_equal(
    aqi_naaqs_monitors_raw$vintage_4 |>
      dplyr::group_by(aqs_site_id, poc, parameter_code, year) |>
      dplyr::count(latitude, longitude) |>
      dplyr::filter(n > 1) |>
      nrow(),
    0
  )
  expect_equal(
    aqi_naaqs_monitors_raw$vintage_410 |>
      dplyr::group_by(aqs_site_id, poc, parameter_code, year) |>
      dplyr::count(latitude, longitude) |>
      dplyr::filter(n > 1) |>
      nrow(),
    0
  )
})

# Duplicate check for geocoded bg_id performed in testing file for geocoded bg_id
# Move to geocoded bg_id tests
test_that("No duplicate CRS coordinates exist in the geocoded bg_id DF", {
  expect_equal(
    geocoded_bg_id_vintage_4 |>
      dplyr::group_by(latitude, longitude) |>
      dplyr::count() |>
      dplyr::filter(n > 1) |>
      nrow(),
    0
  )
  expect_equal(
    geocoded_bg_id_vintage_410 |>
      dplyr::group_by(latitude, longitude) |>
      dplyr::count() |>
      dplyr::filter(n > 1) |>
      nrow(),
    0
  )
})

test_that("Joined DFs have expected columns after geocoded bg_id join", {
  joined <- aqi_naaqs_monitors_raw |>
    .join_aqi_naaqs_monitor_monitor_metadata(aqi_naaqs_monitor_metadata)
  expected_cols <- c(
    "aqs_site_id",
    "poc",
    "parameter_code",
    "latitude",
    "longitude",
    "year",
    "parameter_name",
    "measurement_scale",
    "measurement_scale_definition",
    "monitoring_objective"
  )
  expect_equal(
    colnames(joined$vintage_410),
    expected_cols
  )
  expect_equal(
    colnames(joined$vintage_4),
    expected_cols
  )
})

test_that("Master function returns expected data", {
  master <- get_aqi_naaqs_monitors(start = 2012, end = 2023, monitor_metadata = all_monitor_metadata)
  expect_equal(
    master,
    aqi_naaqs_monitors_final
  )
})
