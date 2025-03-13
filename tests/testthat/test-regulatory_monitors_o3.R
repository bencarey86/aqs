test_that("O3 URL is defined correctly and has active link", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    url <- .define_o3_url(year)
    expect_true(is.character(url))
    expect_true(httr::HEAD(url)$status == 200)
  })
})

test_that("O3 sites sheet is correctly defined", {
  years <- 2015:2023
  purrr::map(years, function(year) {
    temp_directory <- .create_temp_subdirectory("o3")
    url <- .define_o3_url(year)
    excel_file_path <- .download_regulatory_monitor_data(
      year = year,
      url = url,
      temp_directory = temp_directory
    )
    sheet <- .define_o3_sites_sheet(year)
    excel_sheets <- readxl::excel_sheets(excel_file_path)
    expect_true(sheet %in% excel_sheets)
    unlink(temp_directory, recursive = TRUE)
  })
})

test_that("O3 monitor sheet is correctly defined", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    if (year == 2015) {
      year <- 2016
    }
    temp_directory <- .create_temp_subdirectory("o3")
    url <- .define_o3_url(year)
    excel_file_path <- .download_regulatory_monitor_data(
      year = year,
      url = url,
      temp_directory = temp_directory
    )
    sheet <- .define_o3_monitors_sheet(year)
    excel_sheets <- readxl::excel_sheets(excel_file_path)
    expect_true(sheet %in% excel_sheets)
    unlink(temp_directory, recursive = TRUE)
  })
})

test_that("Rows are skipped correctly when importing Excel data", {
  years <- 2012:2023
  levels <- c("site", "monitor")
  lookup <- expand.grid(year = years, level = levels)
  purrr::map2(lookup$year, lookup$level, function(year, level) {
    expected_skip <- dplyr::case_when(
      level == "monitor" & year %in% 2015:2023 ~ 1,
      TRUE ~ 3
    )
    expect_equal(
      .define_o3_skip(year, level),
      expected_skip
    )
  })
})

test_that("Columns are selected correctly", {
  df <- data.frame(
    x_aqs_site_id_y = c("06-001-0007", "06-001-0007", "06-001-0007"),
    x_poc_y = c(1, 1, 1),
    x_valid_y = c(1, 1, 1),
    x_invalid_y = c(0, 0, 0),
    x_design_value_ppm_y = c(0.07, 0.07, 0.07),
    a = c(1, 1, 1),
    b = c(1, 1, 1)
  )
  expected_df <- df |>
    dplyr::select(-"a", -"b")
  expect_equal(
    .select_cols(df, .define_o3_column_patterns()),
    expected_df
  )
})

test_that("Columns are renamed correctly", {
  df <- data.frame(
    x_invalid_y = c(0, 0, 0),
    x_valid_y = c(1, 1, 1)
  )
  expected_df <- data.frame(
    invalid_dv = c(0, 0, 0),
    valid_dv = c(1, 1, 1)
  )
  expect_equal(
    .rename_o3_columns(df),
    expected_df
  )
})

test_that("Valid design values are filtered correctly", {
  df <- data.frame(
    valid_dv = c(0.07, 0.03, 0.05, NA, 0.10)
  )
  expected_df <- data.frame(
    valid_dv = c(0.07, 0.03, 0.05, 0.10)
  )
  expect_equal(
    .filter_o3_valid_dv(df),
    expected_df
  )
})

test_that("Master function returns expected data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    df <- .get_o3_monitors(year)
    expect_true(all(c("aqs_site_id", "poc", "year", "parameter_name") %in% colnames(df)))
    expect_true(
      unique(df$year) == year
    )
    expect_true(
      unique(df$parameter_name) == "Ozone"
    )
  })
})

test_that("Sites dataframe has expected columns", {
  # 2016-2023
  expect_true(
    all(colnames(o3_intermediate_data_2016_2023$o3_sites) %in% c("aqs_site_id", "valid_dv", "invalid_dv"))
  )
  # 2015
  expect_true(
    all(colnames(o3_intermediate_data_2015$o3_sites) %in% c("aqs_site_id", "valid_dv", "invalid_dv"))
  )
})

test_that("Monitors dataframe has expected columns", {
  # 2016-2023
  expect_true(
    all(colnames(o3_intermediate_data$o3_monitors) %in% c("aqs_site_id", "poc"))
  )
  # 2015
  expect_true(
    all(colnames(o3_intermediate_data_2015$o3_monitors) %in% c("aqs_site_id", "poc"))
  )
  # 2012-2014
  expect_true(
    all(colnames(o3_intermediate_data_2012_2014$o3_monitors) %in% c("aqs_site_id", "poc", "valid_dv", "invalid_dv"))
  )
})
