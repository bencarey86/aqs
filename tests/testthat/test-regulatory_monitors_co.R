test_that("CO URL is defined correctly and has active link", {
  year <- sample(2005:2023, 1)
  url <- .define_co_url(year)
  expect_true(is.character(url))
  expect_true(httr::HEAD(url)$status == 200)
})

test_that("CO 8-hour monitor sheet is correctly defined", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    temp_directory <- .create_temp_subdirectory("co")
    url <- .define_co_url(year)
    excel_file_path <- .download_regulatory_monitor_data(year, url, temp_directory)
    sheet <- .define_co_8h_monitors_sheet(year)
    excel_sheets <- readxl::excel_sheets(excel_file_path)
    expect_true(sheet %in% excel_sheets)
    unlink(temp_directory, recursive = TRUE)
  })
})

test_that("CO 8-hour site sheet is correctly defined", {
  years <- 2012:2013
  purrr::map(years, function(year) {
    temp_directory <- .create_temp_subdirectory("co")
    url <- .define_co_url(year)
    excel_file_path <- .download_regulatory_monitor_data(year, url, temp_directory)
    sheet <- .define_co_8h_sites_sheet(year)
    excel_sheets <- readxl::excel_sheets(excel_file_path)
    expect_true(sheet %in% excel_sheets)
    unlink(temp_directory, recursive = TRUE)
  })
})

test_that("CO 1-hour monitor sheet is correctly defined", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    temp_directory <- .create_temp_subdirectory("co")
    url <- .define_co_url(year)
    excel_file_path <- .download_regulatory_monitor_data(year, url, temp_directory)
    sheet <- .define_co_1h_monitors_sheet(year)
    excel_sheets <- readxl::excel_sheets(excel_file_path)
    expect_true(sheet %in% excel_sheets)
    unlink(temp_directory, recursive = TRUE)
  })
})

test_that("CO 1-hour site sheet is correctly defined", {
  years <- 2012:2013
  purrr::map(years, function(year) {
    temp_directory <- .create_temp_subdirectory("co")
    url <- .define_co_url(year)
    excel_file_path <- .download_regulatory_monitor_data(year, url, temp_directory)
    sheet <- .define_co_1h_sites_sheet(year)
    excel_sheets <- readxl::excel_sheets(excel_file_path)
    expect_true(sheet %in% excel_sheets)
    unlink(temp_directory, recursive = TRUE)
  })
})

test_that("CO monitor columns are selected correctly", {
  df <- data.frame(
    site = 1:10,
    aqs_site_id = 1:10,
    poc = 1:10,
    valid_2012_1_hour_design_value = 1:10,
    valid_2013_1_hour_design_value = 1:10,
    valid_2014_1_hour_design_value = 1:10,
    valid_2015_1_hour_design_value = 1:10,
    valid_2016_1_hour_design_value = 1:10,
    valid_2017_1_hour_design_value = 1:10,
    valid_2018_1_hour_design_value = 1:10,
    valid_2019_1_hour_design_value = 1:10,
    valid_2020_1_hour_design_value = 1:10,
    valid_2021_1_hour_design_value = 1:10,
    valid_2022_1_hour_design_value = 1:10,
    valid_2023_1_hour_design_value = 1:10,
    a = 1:10,
    b = 1:10
  )
  expected_df <- df |>
    dplyr::select(-"a", -"b")
  expect_equal(
    .select_cols(
      df,
      col_patterns = "^site$|aqs_site_id|poc|^valid|x20[0-9]{2}_[0-9]_hour_design_value"
    ),
    expected_df
  )
})

test_that("CO site columns are selected correctly", {
  df_2012 <- data.frame(
    site = 1:10,
    dv_2012 = 1:10,
    a = 1:10,
    b = 1:10
  )
  df_2013 <- data.frame(
    site = 1:10,
    dv_2013 = 1:10,
    a = 1:10,
    b = 1:10
  )
  expected_df_2012 <- df_2012 |>
    dplyr::select(-"a", -"b")
  expected_df_2013 <- df_2013 |>
    dplyr::select(-"a", -"b")
  expect_equal(
    .select_cols(
      df_2012,
      col_patterns = "^site$|2012"
    ),
    expected_df_2012
  )
  expect_equal(
    .select_cols(
      df_2013,
      col_patterns = "^site$|2013"
    ),
    expected_df_2013
  )
})

test_that("CO columns are renamed correctly", {
  df <- data.frame(
    a_site = 1:10,
    a_design_value = 1:10,
    c = 1:10
  )
  expected_df <- data.frame(
    aqs_site_id = 1:10,
    valid_dv = 1:10,
    c = 1:10
  )
  expect_equal(
    .rename_co_columns(df),
    expected_df
  )
})

test_that("CO standards are combined correctly", {
  df_8h <- data.frame(
    valid_dv = c("1", "2", "3")
  )
  df_1h <- data.frame(
    valid_dv = c("4", "5", "6")
  )
  expected_df <- data.frame(
    valid_dv = c(1, 2, 3, 4, 5, 6)
  )
  expect_equal(
    .combine_co_standards(df_8h, df_1h),
    expected_df
  )
})

test_that("CO valid_dv is filtered correctly", {
  df <- data.frame(
    valid_dv = c(1, NA, 2, 3, NA, 4)
  )
  expected_df <- data.frame(
    valid_dv = c(1, 2, 3, 4)
  )
  expect_equal(
    .filter_co_valid_dv(df),
    expected_df
  )
})

test_that("CO master function returns expected data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    df <- suppressWarnings(.get_co_monitors(year))
    expect_true(
      all(c("aqs_site_id", "poc", "year", "parameter_name") %in% colnames(df))
    )
  })
})
