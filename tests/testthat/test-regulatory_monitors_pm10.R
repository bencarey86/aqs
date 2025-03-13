test_that("URL is defined correctly and has active link", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    url <- .define_pm10_url(year)
    expect_true(is.character(url))
    expect_true(httr::HEAD(url)$status == 200)
  })
})

test_that("Monitor sheet is correctly defined", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    temp_directory <- .create_temp_subdirectory("pm10")
    url <- .define_pm10_url(year)
    excel_file_path <- .download_regulatory_monitor_data(
      year = year,
      url = url,
      temp_directory = temp_directory
    )
    sheet <- .define_pm10_monitors_sheet(year)
    excel_sheets <- readxl::excel_sheets(excel_file_path)
    expect_true(sheet %in% excel_sheets)
    unlink(temp_directory, recursive = TRUE)
  })
})

test_that("Columns are selected correctly", {
  df <- data.frame(
    site = c("site1", "site2"),
    x_aqs_site_id = c("123", "456"),
    x_poc_y = c(1, 2),
    valid_x = c("Y", "N"),
    x2010_2011_average_estimated_exceedances = c(1, 2),
    a = c(1, 2),
    b = c(1, 2)
  )
  years <- 2012:2023
  purrr::map(years, function(year) {
    if (year %in% 2012:2016) {
      new_col_name <- stringr::str_c("x_", year - 2, "_", year, "_y")
      df <- df |>
        dplyr::mutate("new_col" = c(1, 2)) |>
        dplyr::select(-"x2010_2011_average_estimated_exceedances")
      colnames(df)[colnames(df) == "new_col"] <- new_col_name
    }
    expected_df <- df |>
      dplyr::select(-"a", -"b")
    expect_equal(
      .select_cols(df, .define_pm10_column_patterns(year = year)),
      expected_df
    )
  })
})

test_that("Columns are renamed correctly", {
  df <- data.frame(
    x_site_y = c(1, 1),
    x_complete_y = c(1, 1),
    x_exceedances_y = c(1, 1),
    x_valid_2012 = c(1, 1)
  )
  expected_df <- data.frame(
    aqs_site_id = c(1, 1),
    completeness = c(1, 1),
    valid_dv = c(1, 1),
    validity = c(1, 1)
  )
  expect_equal(
    .rename_pm10_columns(df),
    expected_df
  )
})

test_that("Valid design values are filtered correctly", {
  df_2012 <- data.frame(
    valid_dv = c(1, 2, NA, 3, NA, 4),
    validity = c("Y", "Y", "N", "Y", "Y", "N")
  )
  expected_df_2012 <- data.frame(
    valid_dv = c(1, 2, 3),
    validity = c("Y", "Y", "Y")
  )
  expect_equal(
    .filter_pm10_valid_dv(df_2012, 2012),
    expected_df_2012
  )
  df_2023 <- df_2012 |>
    dplyr::select(-"validity")
  expected_df_2023 <- data.frame(
    valid_dv = c(1, 2, 3, 4)
  )
  expect_equal(
    .filter_pm10_valid_dv(df_2023, 2023),
    expected_df_2023
  )
})

test_that("Master function returns expected data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    df <- .get_pm10_monitors(year)
    expect_true(all(c("aqs_site_id", "poc", "year", "parameter_name") %in% colnames(df)))
    expect_true(unique(df$parameter_name) == "PM10")
    expect_true(unique(df$year) == year)
  })
})
