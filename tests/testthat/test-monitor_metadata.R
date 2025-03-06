test_that("Monitor metadata file is downloaded as epxected", {
  temp_directory <- .create_temp_subdirectory("monitor_metadata_testung")
  .download_zip_monitor_metadata(temp_directory = temp_directory)
  expect_true(file.exists(file.path(temp_directory, "aqs_monitors.csv")))
  unlink(temp_directory, recursive = TRUE)
})

test_that("Monitor metadata file is imported as expected", {
  temp_directory <- .create_temp_subdirectory("monitor_metadata_testung")
  .download_zip_monitor_metadata(temp_directory = temp_directory)
  df <- .import_monitor_metadata(temp_directory = temp_directory)
  unlink(temp_directory, recursive = TRUE)
  expect_true(
    all(c(
      "state_code",
      "county_code",
      "site_number",
      "parameter_code",
      "poc",
      "parameter_name"
    ) %in% colnames(df))
  )
})

test_that("Master monitor metadata function returns expected output", {
  df <- .get_monitor_metadata()
  expect_true(
    all(c(
      "state_code",
      "county_code",
      "site_number",
      "parameter_code",
      "poc",
      "parameter_name"
    ) %in% colnames(df))
  )
})
