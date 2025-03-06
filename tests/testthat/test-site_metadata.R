test_that("Site metadata file is downloaded as expected", {
  temp_directory <- .create_temp_subdirectory("site_metadata_testung")
  .download_zip_sites_metadata(temp_directory = temp_directory)
  expect_true(file.exists(file.path(temp_directory, "aqs_sites.csv")))
  unlink(temp_directory, recursive = TRUE)
})

test_that("Site metadata file is imported as expected", {
  temp_directory <- .create_temp_subdirectory("site_metadata_testung")
  .download_zip_sites_metadata(temp_directory = temp_directory)
  df <- .import_sites_metadata(temp_directory = temp_directory)
  unlink(temp_directory, recursive = TRUE)
  expect_true(
    all(c(
      "state_code",
      "county_code",
      "site_number",
      "site_established_date",
      "site_closed_date"
    ) %in% colnames(df))
  )
})

test_that("land_use and location_setting columns converted to factor", {
  temp_directory <- .create_temp_subdirectory("site_metadata_testung")
  .download_zip_sites_metadata(temp_directory = temp_directory)
  df <- .import_sites_metadata(temp_directory = temp_directory) |>
    .transform_site_metadata_cols()
  unlink(temp_directory, recursive = TRUE)
  expect_true(is.factor(df$land_use))
  expect_true(is.factor(df$location_setting))
  expect_equal(levels(df$land_use), c(
    "AGRICULTURAL", "BLIGHTED AREAS", "COMMERCIAL", "DESERT", "FOREST",
    "INDUSTRIAL", "MILITARY RESERVATION", "MOBILE", "RESIDENTIAL", "UNKNOWN"
  ))
  expect_equal(levels(df$location_setting), c("URBAN AND CENTER CITY", "SUBURBAN", "RURAL", "UNKNOWN"))
})

test_that("Master site metadata function returns expected output", {
  df <- .get_sites_metadata()
  expect_true(
    all(c(
      "state_code",
      "county_code",
      "site_number",
      "site_established_date",
      "site_closed_date"
    ) %in% colnames(df))
  )
  expect_true(is.factor(df$land_use))
  expect_true(is.factor(df$location_setting))
  expect_equal(levels(df$land_use), c(
    "AGRICULTURAL", "BLIGHTED AREAS", "COMMERCIAL", "DESERT", "FOREST",
    "INDUSTRIAL", "MILITARY RESERVATION", "MOBILE", "RESIDENTIAL", "UNKNOWN"
  ))
  expect_equal(levels(df$location_setting), c("URBAN AND CENTER CITY", "SUBURBAN", "RURAL", "UNKNOWN"))
})
