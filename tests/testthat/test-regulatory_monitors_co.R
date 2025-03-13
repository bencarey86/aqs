# Set up -----------------------------------------------------------------------
test_that("CO URL is defined correctly and has active link", {
  year <- sample(2005:2023, 1)
  url <- .define_co_url(year)
  expect_true(is.character(url))
  expect_true(httr::HEAD(url)$status == 200)
})

test_that("C0 8hr monitor sheet is correctly defined", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    setup <- get(stringr::str_c("co_setup_", year))
    excel_sheets <- readxl::excel_sheets(setup$file_definitions$excel_file_path)
    expect_true(setup$sheets$co_8h_monitors %in% excel_sheets)
  })
})

test_that("CO 1hr monitor sheet is correctly defined", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    setup <- get(stringr::str_c("co_setup_", year))
    excel_sheets <- readxl::excel_sheets(setup$file_definitions$excel_file_path)
    expect_true(setup$sheets$co_1h_monitors %in% excel_sheets)
  })
})

test_that("CO 8hr site sheet is correctly defined", {
  years <- 2012:2013
  purrr::map(years, function(year) {
    setup <- get(stringr::str_c("co_setup_", year))
    excel_sheets <- readxl::excel_sheets(setup$file_definitions$excel_file_path)
    expect_true(setup$sheets$co_8h_sites %in% excel_sheets)
  })
})

test_that("CO 1hr site sheet is correctly defined", {
  years <- 2012:2013
  purrr::map(years, function(year) {
    setup <- get(stringr::str_c("co_setup_", year))
    excel_sheets <- readxl::excel_sheets(setup$file_definitions$excel_file_path)
    expect_true(setup$sheets$co_1h_sites %in% excel_sheets)
  })
})

test_that("CO skip is correctly defined", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    setup <- get(stringr::str_c("co_setup_", year))
    if (year == 2013) {
      expect_equal(setup$skip, 2)
    } else {
      (
        expect_equal(setup$skip, 3)
      )
    }
  })
})

# Preliminary data processing --------------------------------------------------
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
      col_patterns = .define_co_monitor_column_patterns()
    ),
    expected_df
  )
})

test_that("Intermediate monitor DFs have correct columns", {
  # 2014-2023 (monitor level valid_dv)
  years <- 2014:2023
  purrr::map(years, function(year) {
    co_initial_data <- get(stringr::str_c("co_initial_data_", year))
    expect_equal(colnames(co_initial_data$monitors$co_8h), c("aqs_site_id", "poc", "valid_dv"))
    expect_equal(colnames(co_initial_data$monitors$co_1h), c("aqs_site_id", "poc", "valid_dv"))
  })
  # 2012-2013 (site level valid_dv)
  years <- 2012:2013
  purrr::map(years, function(year) {
    co_initial_data <- get(stringr::str_c("co_initial_data_", year))
    expect_equal(colnames(co_initial_data$monitors$co_8h), c("aqs_site_id", "poc"))
    expect_equal(colnames(co_initial_data$monitors$co_1h), c("aqs_site_id", "poc"))
  })
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
      col_patterns = .define_co_site_column_patterns(year = 2012)
    ),
    expected_df_2012
  )
  expect_equal(
    .select_cols(
      df_2013,
      col_patterns = .define_co_site_column_patterns(year = 2013)
    ),
    expected_df_2013
  )
})

test_that("Intermediate site DFs have correct columns", {
  # 2012-2013 only
  years <- 2012:2013
  purrr::map(years, function(year) {
    co_initial_data <- get(stringr::str_c("co_initial_data_", year))
    expect_equal(colnames(co_initial_data$sites$co_8h), c("aqs_site_id", "valid_dv"))
    expect_equal(colnames(co_initial_data$sites$co_1h), c("aqs_site_id", "valid_dv"))
  })
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

# Validity determination -------------------------------------------------------
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

test_that("CO validity is determined correctly", {
  # 2014-2023 (monitor level valid_dv)
  years <- 2014:2023
  purrr::map(years, function(year) {
    co_initial_data <- get(stringr::str_c("co_initial_data_", year))
    co_valid_data <- get(stringr::str_c("co_valid_data_", year))
    expect_equal(
      nrow(co_valid_data$co_8h),
      nrow(co_initial_data$monitors$co_8h |>
        dplyr::filter(!is.na(valid_dv)))
    )
    expect_equal(
      nrow(co_valid_data$co_1h),
      nrow(co_initial_data$monitors$co_1h |>
        dplyr::filter(!is.na(valid_dv)))
    )
  })
  # 2012-2013 (site level valid_dv)
  years <- 2012:2013
  purrr::map(years, function(year) {
    co_initial_data <- get(stringr::str_c("co_initial_data_", year))
    co_valid_data <- get(stringr::str_c("co_valid_data_", year))
    expect_equal(
      nrow(co_valid_data$co_8h),
      nrow(co_initial_data$sites$co_8h |>
        dplyr::filter(!is.na(valid_dv)))
    )
    expect_equal(
      nrow(co_valid_data$co_1h),
      nrow(co_initial_data$sites$co_1h |>
        dplyr::filter(!is.na(valid_dv)))
    )
  })
})

# DF combination ----------------------------------------------------------------
test_that("DFs have expected columns prior to join", {
  years <- 2012:2013
  purrr::map(years, function(year) {
    co_initial_data <- get(stringr::str_c("co_initial_data_", year))
    co_valid_data <- get(stringr::str_c("co_valid_data_", year))
    expect_equal(
      colnames(co_initial_data$monitors$co_8h),
      c("aqs_site_id", "poc")
    )
    expect_equal(
      colnames(co_initial_data$monitors$co_1h),
      c("aqs_site_id", "poc")
    )
    expect_equal(
      colnames(co_valid_data$co_8h),
      c("aqs_site_id")
    )
    expect_equal(
      colnames(co_valid_data$co_1h),
      c("aqs_site_id")
    )
  })
})

test_that("No duplicates exist in joining fields", {
  years <- 2012:2013
  purrr::map(years, function(year) {
    co_initial_data <- get(stringr::str_c("co_initial_data_", year))
    co_valid_data <- get(stringr::str_c("co_valid_data_", year))
    expect_true(!anyDuplicated(co_initial_data$monitors$co_8h))
    expect_true(!anyDuplicated(co_initial_data$monitors$co_1h))
    expect_true(!anyDuplicated(co_valid_data$co_8h))
    expect_true(!anyDuplicated(co_valid_data$co_1h))
  })
})

test_that("Joined DFs for 2012 and 2013 have expected number of rows", {
  years <- 2012:2013
  purrr::map(years, function(year) {
    co_initial_data <- get(stringr::str_c("co_initial_data_", year))
    co_valid_data <- get(stringr::str_c("co_valid_data_", year))
    sites_unmatched_8h <- sum(!co_valid_data$co_8h$aqs_site_id %in% co_initial_data$monitors$co_8h$aqs_site_id)
    matched_8h <- sum(co_initial_data$monitors$co_8h$aqs_site_id %in% co_valid_data$co_8h$aqs_site_id)
    expect_equal(
      nrow(.join_co_sites_and_monitors(co_valid_data, co_initial_data)$co_8h),
      (sites_unmatched_8h + matched_8h)
    )
    sites_unmatched_1h <- sum(!co_valid_data$co_1h$aqs_site_id %in% co_initial_data$monitors$co_1h$aqs_site_id)
    matched_1h <- sum(co_initial_data$monitors$co_1h$aqs_site_id %in% co_valid_data$co_1h$aqs_site_id)
    expect_equal(
      nrow(.join_co_sites_and_monitors(co_valid_data, co_initial_data)$co_1h),
      (sites_unmatched_1h + matched_1h)
    )
  })
})

test_that("Final combined DF has correct number of unique monitors", {
  # 2014-2023
  years <- 2014:2023
  purrr::map(years, function(year) {
    co_initial_data <- get(stringr::str_c("co_initial_data_", year))
    co_valid_data <- get(stringr::str_c("co_valid_data_", year))
    expect_equal(
      (nrow(dplyr::inner_join(co_valid_data$co_8h, co_valid_data$co_1h, by = c("aqs_site_id", "poc"))) +
        nrow(dplyr::anti_join(co_valid_data$co_8h, co_valid_data$co_1h, by = c("aqs_site_id", "poc")))),
      nrow(.combine_co_data(year, co_initial_data, co_valid_data))
    )
  })
  # 2012-2013
  years <- 2012:2013
  purrr::map(years, function(year) {
    co_initial_data <- get(stringr::str_c("co_initial_data_", year))
    co_valid_data <- get(stringr::str_c("co_valid_data_", year))
    joined_data <- .join_co_sites_and_monitors(co_valid_data, co_initial_data)
    expect_equal(
      (nrow(dplyr::inner_join(joined_data$co_8h, joined_data$co_1h, by = c("aqs_site_id", "poc"))) +
        nrow(dplyr::anti_join(joined_data$co_8h, joined_data$co_1h, by = c("aqs_site_id", "poc")))),
      nrow(.combine_co_data(year, co_initial_data, co_valid_data))
    )
  })
})

test_that(".combine_co_data joins data correctly", {
  # 2014-2023
  df_1 <- data.frame(
    aqs_site_id = c(1, 2, 3),
    poc = c(1, 2, 3)
  )
  df_2 <- data.frame(
    aqs_site_id = c(3, 4, 5),
    poc = c(3, 4, 5)
  )
  valid_data_list <- list(
    co_8h = df_1,
    co_1h = df_2
  )
  expected_df <- data.frame(
    aqs_site_id = c(1, 2, 3, 4, 5),
    poc = c(1, 2, 3, 4, 5)
  )
  expect_equal(
    .combine_co_data(year = sample(2014:2023, 1), initial_data_list = NULL, valid_data_list),
    expected_df
  )
  # 2012-2013
  # Don't drop sites without corresponding monitors
  # Include all monitors with corresponding sites
  # Drop duplicate monitors when binding
  sites_1 <- data.frame(
    aqs_site_id = c(1, 2, 3, 6)
  )
  sites_2 <- data.frame(
    aqs_site_id = c(3, 4, 5)
  )
  valid_data_list <- list(
    co_8h = sites_1,
    co_1h = sites_2
  )
  monitors_1 <- data.frame(
    aqs_site_id = c(1, 2, 2, 3),
    poc = c(1, 2, 1, 3)
  )
  monitors_2 <- data.frame(
    aqs_site_id = c(3, 4, 4),
    poc = c(3, 4, 1)
  )
  initial_data_list <- list(
    monitors = list(
      co_8h = monitors_1,
      co_1h = monitors_2
    )
  )
  expected_df <- data.frame(
    aqs_site_id = c(1, 2, 2, 3, 6, 4, 4, 5),
    poc = c(1, 2, 1, 3, NA, 4, 1, NA)
  )
  expect_equal(
    .combine_co_data(year = sample(2012:2013, 1), initial_data_list, valid_data_list),
    expected_df
  )
})

# Master function --------------------------------------------------------------
test_that("CO master function returns expected data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    df <- suppressWarnings(.get_co_monitors(year))
    expect_true(
      all(c("aqs_site_id", "poc", "year", "parameter_name") %in% colnames(df))
    )
    expect_equal(df, get(stringr::str_c("co_", year)))
  })
})
