# Set up -----------------------------------------------------------------------
test_that("Lead URL is defined correctly and has active link", {
  year <- sample(2012:2023, 1)
  url <- .define_lead_url(year)
  expect_true(is.character(url))
  expect_true(httr::HEAD(url)$status == 200)
})


test_that("Lead sites sheet is correctly defined", {
  # No monitor data for 2012-2015; data from 2016 is used instead
  years <- 2016:2023
  purrr::map(years, function(year) {
    setup <- get(stringr::str_c("lead_setup_", year))
    excel_sheet <- readxl::excel_sheets(setup$file_definitions$excel_file_path)
    expect_true(setup$sheets$sites %in% excel_sheet)
  })
})

test_that("Lead monitor sheet is correctly defined", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    setup <- get(stringr::str_c("lead_setup_", year))
    excel_sheet <- readxl::excel_sheets(setup$file_definitions$excel_file_path)
    expect_true(setup$sheets$monitors %in% excel_sheet)
  })
})

test_that("Lead skip is correctly defined", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    setup <- get(stringr::str_c("lead_setup_", year))
    if (year %in% 2019:2023) {
      expected_skip_sites <- 3
      expected_skip_monitors <- 1
    } else if (year %in% 2012:2018) {
      expected_skip_sites <- 2
      expected_skip_monitors <- 0
    }
    expect_equal(
      setup$skips$sites,
      expected_skip_sites
    )
    expect_equal(
      setup$skips$monitors,
      expected_skip_monitors
    )
  })
})

# Preliminary data processing --------------------------------------------------
test_that("Lead site columns are selected correctly", {
  df <- data.frame(
    site = c("0010", "0011", "0012"),
    x_aqs_site_id_y = c("060010001", "060010011", "060010012"),
    x_design_value_y = c(0.1, 0.2, 0.3),
    x_validity_y = c("Y", "Y", "Y"),
    a = c(1, 2, 3),
    b = c(4, 5, 6)
  )
  expected_df <- df |>
    dplyr::select(-"a", -"b")
  expect_equal(
    .select_cols(
      df,
      col_patterns = .define_lead_column_patterns()$sites
    ),
    expected_df
  )
})

test_that("Lead site columns are renamed correctly", {
  df <- data.frame(
    site = c("0010", "0011", "0012"),
    x_invalid_dv_y = c(0.1, 0.2, 0.3),
    x_design_value_y = c(0.1, 0.2, 0.3),
    x_validity_y = c("Y", "Y", "Y")
  )
  expected_df <- data.frame(
    aqs_site_id = c("0010", "0011", "0012"),
    invalid_dv = c(0.1, 0.2, 0.3),
    valid_dv = c(0.1, 0.2, 0.3),
    validity = c("Y", "Y", "Y")
  )
  expect_equal(
    .rename_lead_site_columns(df),
    expected_df
  )
})

test_that("Expected modifications are made to 2018 monitor data", {
  df <- data.frame(
    aqs_id = c("06-00100-01-1", "06-00100-01-2", "06-00100-02-13")
  )
  expected_df <- data.frame(
    aqs_site_id = c("060010001", "060010001", "060010002"),
    poc = c("1", "2", "13")
  )
  expect_equal(
    .modify_lead_monitor_2018_data(df),
    expected_df
  )
})

test_that("Initial monitor DF has correct columns", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    lead_initial_data <- get(stringr::str_c("lead_initial_data_", year))
    expect_equal(
      colnames(lead_initial_data$monitors),
      c("aqs_site_id", "poc")
    )
  })
})

test_that("Initial site DF has correct columns", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    lead_initial_data <- get(stringr::str_c("lead_initial_data_", year))
    if (year %in% 2019:2023) {
      expect_equal(
        colnames(lead_initial_data$sites),
        c("aqs_site_id", "valid_dv")
      )
    } else if (year %in% 2012:2018) {
      expect_equal(
        colnames(lead_initial_data$sites),
        c("aqs_site_id", "valid_dv", "validity")
      )
    }
  })
})

years <- 2012:2023
purrr::map(years, function(year) {
  lead_initial_data <- get(stringr::str_c("lead_initial_data_", year))
  colnames(lead_initial_data$sites)
})

test_that("Lead data is filtered to valid DVs correctly", {
  df_2012 <- data.frame(
    valid_dv = c(1, 2, 3, NA),
    validity = c("Y", "Yes", "yes", "N")
  )
  expected_df_2012 <- data.frame(
    valid_dv = c(1, 2, 3),
    validity = c("Y", "Yes", "yes")
  )
  expect_equal(
    .filter_lead_valid_dv(df_2012, 2012),
    expected_df_2012
  )
  df_2022 <- data.frame(
    valid_dv = c(1, 2, 3, NA)
  )
  expected_df_2022 <- data.frame(
    valid_dv = c(1, 2, 3)
  )
  expect_equal(
    .filter_lead_valid_dv(df_2022, 2022),
    expected_df_2022
  )
})

test_that("Intermediate DF has appropriate columns", {
  # 2019-2023
  expect_true(
    all(colnames(lead_intermediate_2019_2023$lead_sites) %in% c("state", "aqs_site_id", "valid_dv", "invalid_dv"))
  )
  expect_true(
    all(colnames(lead_intermediate_2019_2023$lead_monitors) %in% c("aqs_site_id", "poc"))
  )
  # 2018
  expect_true(
    all(colnames(lead_intermediate_2018$lead_sites) %in% c("state", "aqs_site_id", "valid_dv", "invalid_dv", "validity"))
  )
  expect_true(
    all(colnames(lead_intermediate_2018$lead_monitors) %in% c("aqs_site_id", "poc"))
  )
  # 2016-2017
  expect_true(
    all(colnames(lead_intermediate_2016_2017$lead_sites) %in% c("state", "aqs_site_id", "valid_dv", "invalid_dv", "validity"))
  )
  expect_true(
    all(colnames(lead_intermediate_2016_2017$lead_monitors) %in% c("aqs_site_id", "poc"))
  )
  # 2012-2015
  expect_true(
    all(colnames(lead_intermediate_2012_2015$lead_sites) %in% c("state", "aqs_site_id", "valid_dv", "invalid_dv", "validity"))
  )
  expect_true(
    all(colnames(lead_intermediate_2012_2015$lead_monitors) %in% c("aqs_site_id", "poc"))
  )
})

test_that("Valid sites are determined correctly", {
  # 2019-2023
  expect_equal(
    nrow(lead_final_2019_2023$lead_valid_sites),
    nrow(.filter_lead_valid_dv(lead_intermediate_2019_2023$lead_sites, lead_intermediate_2019_2023$year))
  )
  # 2018
  expect_equal(
    nrow(lead_final_2018$lead_valid_sites),
    nrow(.filter_lead_valid_dv(lead_intermediate_2018$lead_sites, lead_intermediate_2018$year))
  )
  # 2016-2017
  expect_equal(
    nrow(lead_final_2016_2017$lead_valid_sites),
    nrow(.filter_lead_valid_dv(lead_intermediate_2016_2017$lead_sites, lead_intermediate_2016_2017$year))
  )
  # 2012-2015
  expect_equal(
    nrow(lead_final_2012_2015$lead_valid_sites),
    nrow(.filter_lead_valid_dv(lead_intermediate_2012_2015$lead_sites, lead_intermediate_2012_2015$year))
  )
})

test_that("Sites DF contains only unique observations", {
  expect_true(!anyDuplicated(lead_final_2019_2023$lead_valid_sites))
  expect_true(!anyDuplicated(lead_final_2018$lead_valid_sites))
  expect_true(!anyDuplicated(lead_final_2016_2017$lead_valid_sites))
  expect_true(!anyDuplicated(lead_final_2012_2015$lead_valid_sites))
})

test_that("Joins yield correct number of observations", {
  # 2019-2023
  site_unmatched_2019_2023 <- sum(!lead_final_2019_2023$lead_valid_sites$aqs_site_id %in% lead_intermediate_2019_2023$lead_sites$aqs_site_id)
  matched_2019_2023 <- sum(lead_intermediate_2019_2023$lead_monitors$aqs_site_id %in% lead_final_2019_2023$lead_valid_sites$aqs_site_id)
  expect_equal(
    nrow(lead_final_2019_2023$lead),
    (site_unmatched_2019_2023 + matched_2019_2023)
  )
  # 2018
  site_unmatched_2018 <- sum(!lead_final_2018$lead_valid_sites$aqs_site_id %in% lead_intermediate_2018$lead_sites$aqs_site_id)
  matched_2018 <- sum(lead_intermediate_2018$lead_monitors$aqs_site_id %in% lead_final_2018$lead_valid_sites$aqs_site_id)
  expect_equal(
    nrow(lead_final_2018$lead),
    (site_unmatched_2018 + matched_2018)
  )
  # 2016-2017
  site_unmatched_2016_2017 <- sum(!lead_final_2016_2017$lead_valid_sites$aqs_site_id %in% lead_intermediate_2016_2017$lead_sites$aqs_site_id)
  matched_2016_2017 <- sum(lead_intermediate_2016_2017$lead_monitors$aqs_site_id %in% lead_final_2016_2017$lead_valid_sites$aqs_site_id)
  expect_equal(
    nrow(lead_final_2016_2017$lead),
    (site_unmatched_2016_2017 + matched_2016_2017)
  )
  # 2012-2015
  site_unmatched_2012_2015 <- sum(!lead_final_2012_2015$lead_valid_sites$aqs_site_id %in% lead_intermediate_2012_2015$lead_sites$aqs_site_id)
  matched_2012_2015 <- sum(lead_intermediate_2012_2015$lead_monitors$aqs_site_id %in% lead_final_2012_2015$lead_valid_sites$aqs_site_id)
  expect_equal(
    nrow(lead_final_2012_2015$lead),
    (site_unmatched_2012_2015 + matched_2012_2015)
  )
})

test_that("Final DF has correct number of unique monitors", {
  # 2019-2023
  expect_equal(
    nrow(lead_final_2019_2023$lead),
    nrow(suppressWarnings(.get_lead_monitors(lead_final_2019_2023$year)))
  )
  # 2018
  expect_equal(
    nrow(lead_final_2018$lead),
    nrow(suppressWarnings(.get_lead_monitors(lead_final_2018$year)))
  )
  # 2016-2017
  expect_equal(
    nrow(lead_final_2016_2017$lead),
    nrow(suppressWarnings(.get_lead_monitors(lead_final_2016_2017$year)))
  )
  # 2012-2015
  expect_equal(
    nrow(lead_final_2012_2015$lead),
    nrow(suppressWarnings(.get_lead_monitors(lead_final_2012_2015$year)))
  )
})


test_that("Lead master function returns expected data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    df <- suppressWarnings(.get_lead_monitors(year))
    expect_true(
      all(c("aqs_site_id", "poc", "year", "parameter_name") %in% colnames(df))
    )
    expect_true(
      unique(df$year) == year
    )
    expect_true(
      unique(df$parameter_name) == "Lead"
    )
  })
})
