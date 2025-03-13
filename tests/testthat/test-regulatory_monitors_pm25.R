test_that("URL is correctly defined and has active link", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    url <- .define_pm25_url(year)
    expect_true(is.character(url))
    expect_true(httr::HEAD(url)$status == 200)
  })
})

test_that("Annual sites sheet is correctly defined", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    temp_directory <- .create_temp_subdirectory("pm25")
    url <- .define_pm25_url(year)
    excel_file_path <- .download_regulatory_monitor_data(
      year = year,
      url = url,
      temp_directory = temp_directory
    )
    sheet <- .define_pm25_annual_sites_sheet(year)
    excel_sheets <- readxl::excel_sheets(excel_file_path)
    expect_true(sheet %in% excel_sheets)
    unlink(temp_directory, recursive = TRUE)
  })
})

test_that("24h sites sheet is correctly defined", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    temp_directory <- .create_temp_subdirectory("pm25")
    url <- .define_pm25_url(year)
    excel_file_path <- .download_regulatory_monitor_data(
      year = year,
      url = url,
      temp_directory = temp_directory
    )
    sheet <- .define_pm25_24h_sites_sheet(year)
    excel_sheets <- readxl::excel_sheets(excel_file_path)
    expect_true(sheet %in% excel_sheets)
    unlink(temp_directory, recursive = TRUE)
  })
})

test_that("Monitor sheet is correctly defined", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    if (year %in% 2012:2015) {
      year <- 2016
    }
    temp_directory <- .create_temp_subdirectory("pm25")
    url <- .define_pm25_url(year)
    excel_file_path <- .download_regulatory_monitor_data(
      year = year,
      url = url,
      temp_directory = temp_directory
    )
    sheet <- .define_pm25_monitors_sheet(year)
    excel_sheets <- readxl::excel_sheets(excel_file_path)
    expect_true(sheet %in% excel_sheets)
    unlink(temp_directory, recursive = TRUE)
  })
})

test_that("Rows are skipped correctly when importing Excel data", {
  levels <- c("site", "monitor")
  purrr::map(levels, function(level) {
    expected_skip <- dplyr::case_when(
      level == "monitor" ~ 1,
      TRUE ~ 3
    )
    expect_equal(
      .define_pm25_skip(level),
      expected_skip
    )
  })
})

test_that("Site columns are selected correctly", {
  df_annual <- data.frame(
    x_aqs_site_id = c(1, 2, 3),
    x_site_id = c(1, 2, 3),
    site = c(1, 2, 3),
    valid_dv = c(1, 2, 3),
    x2012_2012_annual_design_value_y = c(1, 2, 3),
    x_validity = c(1, 2, 3),
    a = c(1, 2, 3),
    b = c(1, 2, 3)
  )
  expected_df_annual <- df_annual |>
    dplyr::select(-"a", -"b")
  expect_equal(
    .select_cols(df_annual, .define_pm25_column_patterns(standard = "annual")),
    expected_df_annual
  )
  df_24h <- data.frame(
    x_aqs_site_id = c(1, 2, 3),
    x_site_id = c(1, 2, 3),
    site = c(1, 2, 3),
    valid_dv = c(1, 2, 3),
    x2012_2012_24h_design_value_y = c(1, 2, 3),
    x_validity = c(1, 2, 3),
    a = c(1, 2, 3),
    b = c(1, 2, 3)
  )
  expected_df_24h <- df_24h |>
    dplyr::select(-"a", -"b")
  expect_equal(
    .select_cols(df_24h, .define_pm25_column_patterns(standard = "24h")),
    expected_df_24h
  )
})

test_that("Site columns are renamed correctly", {
  df <- data.frame(
    x_site_y <- c(1, 2, 3),
    x_validity_y <- c(1, 2, 3),
    x_valid_y = c(1, 1, 1)
  )
  expected_df <- data.frame(
    aqs_site_id = c(1, 2, 3),
    validity = c(1, 2, 3),
    valid_dv = c(1, 1, 1)
  )
  expect_equal(
    .rename_pm25_site_columns(df),
    expected_df
  )
})

test_that("Valid design values are filtered correctly", {
  df <- data.frame(
    valid_dv = c(0.07, NA, 0.05, NA, 0.10),
    validity = c("Y", "Y", "Y", "N", "N")
  )
  expected_df_2013 <- data.frame(
    valid_dv = c(0.07, 0.05, 0.10),
    validity = c("Y", "Y", "N") # 2013 has no validity column, included for 2012 testing
  )
  expect_equal(
    .filter_pm25_valid_dv(df, year = 2013),
    expected_df_2013
  )
  expected_df_2012 <- data.frame(
    valid_dv = c(0.07, 0.05),
    validity = c("Y", "Y")
  )
  expect_equal(
    .filter_pm25_valid_dv(df, year = 2012),
    expected_df_2012
  )
})

test_that("Master function returns expected data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    df <- .get_pm25_monitors(year)
    expect_true(all(c("aqs_site_id", "poc", "year", "parameter_name") %in% colnames(df)))
    expect_true(unique(df$parameter_name) == "PM2.5")
    expect_true(unique(df$year) == year)
  })
})
