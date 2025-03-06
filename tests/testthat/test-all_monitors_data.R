test_that("All monitors URL is defined correctly", {
  expect_equal(
    .define_all_monitors_url(year = 2022),
    "https://aqs.epa.gov/aqsweb/airdata/annual_conc_by_monitor_2022.zip"
  )
})

test_that("All monitors file is downloaded as expected", {
  temp_directory <- .create_temp_subdirectory("all_monitors_testung")
  .download_zip_all_monitors(year = 2022, temp_directory = temp_directory)
  expect_true(file.exists(file.path(temp_directory, "annual_conc_by_monitor_2022.csv")))
  unlink(temp_directory, recursive = TRUE)
})

test_that("All monitors data is imported correctly", {
  temp_directory <- .create_temp_subdirectory("all_monitors_testung")
  zip_file_path <- .download_zip_all_monitors(year = 2022, temp_directory = temp_directory)
  df <- .import_all_monitors_data(zip_file_path = zip_file_path)
  unlink(temp_directory, recursive = TRUE)
  expect_true(
    all(c(
      "state_code",
      "county_code",
      "site_num",
      "parameter_code",
      "poc",
      "parameter_name",
      "observation_count"
    ) %in% colnames(df))
  )
})

test_that("All monitors data are transformed as expected", {
  temp_directory <- .create_temp_subdirectory("all_monitors_testung")
  zip_file_path <- .download_zip_all_monitors(year = 2022, temp_directory = temp_directory)
  df <- .import_all_monitors_data(zip_file_path = zip_file_path) |>
    .transform_all_monitors_cols(year = 2022)
  unlink(temp_directory, recursive = TRUE)
  expect_true(
    all(colnames(df) == c(
      "state_code",
      "county_code",
      "site_number",
      "poc",
      "parameter_code",
      "parameter_name",
      "latitude",
      "longitude",
      "year"
    ))
  )
})
