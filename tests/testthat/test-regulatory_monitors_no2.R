test_that("NO2 URL is defined correctly and has active link", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    url <- .define_no2_url(year)
    expect_true(is.character(url))
    expect_true(httr::HEAD(url)$status == 200)
  })
})

test_that("NO2 sites sheets are correctly defined for both annual and 1 hour standards", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    temp_directory <- .create_temp_subdirectory("no2")
    url <- .define_no2_url(year)
    excel_file_path <- .download_regulatory_monitor_data(
      year = year,
      url = url,
      temp_directory = temp_directory
    )
    sheet_annual <- .define_no2_annual_sites_sheet(year)
    sheet_1h <- .define_no2_1h_sites_sheet(year)
    excel_sheets <- readxl::excel_sheets(excel_file_path)
    expect_true(sheet_annual %in% excel_sheets)
    expect_true(sheet_1h %in% excel_sheets)
    unlink(temp_directory, recursive = TRUE)
  })
})

test_that("NO2 monitor sheet is correctly defined", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    if (year %in% 2012:2015) {
      year <- 2016
    }
    temp_directory <- .create_temp_subdirectory("no2")
    url <- .define_no2_url(year)
    excel_file_path <- .download_regulatory_monitor_data(
      year = year,
      url = url,
      temp_directory = temp_directory
    )
    sheet <- .define_no2_monitors_sheet(year)
    excel_sheets <- readxl::excel_sheets(excel_file_path)
    expect_true(sheet %in% excel_sheets)
    unlink(temp_directory, recursive = TRUE)
  })
})

test_that("Rows are skipped correctly when importing Excel data", {
  levels <- c("site", "monitor")
  purrr::map(levels, function(level) {
    expected_skip <- dplyr::case_when(
      level == "site" ~ 3,
      level == "monitor" ~ 1,
    )
    expect_equal(
      .define_no2_skip(level),
      expected_skip
    )
  })
})

test_that("NO2 site columns are selected correctly", {
  df <- data.frame(
    state = c("CA", "CA", "CA"),
    x_state_name_y = c("California", "California", "California"),
    site = c("0010", "0011", "0012"),
    x_aqs_site_id_y = c("060010001", "060010011", "060010012"),
    x_design_value_y = c(0.1, 0.2, 0.3),
    x_valid_y = c("Y", "Y", "Y"),
    completeness_y = c(100, 100, 100),
    a = c(1, 2, 3),
    b = c(4, 5, 6)
  )
  expected_df <- df |>
    dplyr::select(-"a", -"b")
  expect_equal(
    .select_cols(
      df = df,
      col_patterns = .define_no2_column_patterns(level = "site")
    ),
    expected_df
  )
})

test_that("NO2 site columns are renamed correctly", {
  df <- data.frame(
    state_name = c("CA", "CA", "CA"),
    site = c("0010", "0011", "0012"),
    x_invalid_dv_y = c(0.1, 0.2, 0.3),
    x_valid_dv_y = c(0.1, 0.2, 0.3),
    completeness_y = c(100, 100, 100)
  )
  expected_df <- data.frame(
    state = c("CA", "CA", "CA"),
    aqs_site_id = c("0010", "0011", "0012"),
    invalid_dv = c(0.1, 0.2, 0.3),
    valid_dv = c(0.1, 0.2, 0.3),
    completeness = c(100, 100, 100)
  )
  expect_equal(
    .rename_no2_site_columns(df),
    expected_df
  )
})

test_that("NO2 data are filtered to valid DVs correctly", {
  df_2012 <- data.frame(
    valid_dv = c(1, NA, 3, NA, 5),
    completeness = c("Y", "N", "Y", "Y", "N")
  )
  expected_df_2012 <- data.frame(
    valid_dv = c(1, 3),
    completeness = c("Y", "Y")
  )
  expect_equal(
    .filter_no2_valid_dv(df_2012, year = 2012),
    expected_df_2012
  )
  df_2023 <- data.frame(
    valid_dv = c(1, NA, 3, NA, 5)
  )
  expected_df_2023 <- data.frame(
    valid_dv = c(1, 3, 5)
  )
  expect_equal(
    .filter_no2_valid_dv(df_2023, year = 2023),
    expected_df_2023
  )
})

test_that("NO2 master function returns expected data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    df <- .get_no2_monitors(year)
    expect_true(all(c("aqs_site_id", "poc", "year", "parameter_name") %in% colnames(df)))
    expect_true(
      unique(df$year) == year
    )
    expect_true(
      unique(df$parameter_name) == "Nitrogen dioxide (NO2)"
    )
  })
})
