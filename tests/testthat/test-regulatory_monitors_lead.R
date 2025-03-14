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

test_that("Initial data does not contain duplicate observations", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    lead_initial_data <- get(stringr::str_c("lead_initial_data_", year))
    expect_true(!anyDuplicated(lead_initial_data$sites))
    expect_true(!anyDuplicated(lead_initial_data$monitors))
  })
})

# Validity determination -------------------------------------------------------
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

test_that("Lead validity is determined correctly based on actual row numbers", {
  # 2019-2023 (valid_dv only)
  years <- 2019:2023
  purrr::map(years, function(year) {
    lead_initial_data <- get(stringr::str_c("lead_initial_data_", year))
    lead_valid_data <- get(stringr::str_c("lead_valid_sites_", year))
    expect_equal(
      nrow(lead_valid_data),
      nrow(lead_initial_data$sites |> dplyr::filter(!is.na(valid_dv)))
    )
  })
  # 2012-2018 (valid_dv and validity)
  years <- 2012:2018
  purrr::map(years, function(year) {
    lead_initial_data <- get(stringr::str_c("lead_initial_data_", year))
    lead_valid_data <- get(stringr::str_c("lead_valid_sites_", year))
    expect_equal(
      nrow(lead_valid_data),
      nrow(
        lead_initial_data$sites |>
          dplyr::filter(!is.na(valid_dv) & stringr::str_detect(validity, "^Y|yes"))
      )
    )
  })
})

test_that("Validity responses adhere to expected pattern for 2012-2018", {
  years <- 2012:2018
  validity_responses <- purrr::map(years, function(year) {
    lead_initial_data <- get(stringr::str_c("lead_initial_data_", year))
    unique(lead_initial_data$sites$validity)
  }) |>
    unlist() |>
    unique()
  expect_true(all(validity_responses %in% c("Y", "yes", "N", "no")))
})

# DF combination ----------------------------------------------------------------
test_that("DFs have expected columns prior to join", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    lead_initial_data <- get(stringr::str_c("lead_initial_data_", year))
    lead_valid_sites <- get(stringr::str_c("lead_valid_sites_", year))
    expect_equal(
      colnames(lead_valid_sites),
      "aqs_site_id"
    )
    expect_equal(
      colnames(lead_initial_data$monitors),
      c("aqs_site_id", "poc")
    )
  })
})

test_that("No duplicates exist in joining fields", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    lead_initial_data <- get(stringr::str_c("lead_initial_data_", year))
    lead_valid_sites <- get(stringr::str_c("lead_valid_sites_", year))
    expect_true(!anyDuplicated(lead_valid_sites))
    expect_true(!anyDuplicated(lead_initial_data$monitors))
  })
})

test_that("Joined DFs have expected number of rows", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    lead_initial_data <- get(stringr::str_c("lead_initial_data_", year))
    lead_valid_sites <- get(stringr::str_c("lead_valid_sites_", year))
    sites_unmatched <- sum(!lead_valid_sites$aqs_site_id %in% lead_initial_data$monitors$aqs_site_id)
    matched <- sum(lead_initial_data$monitors$aqs_site_id %in% lead_valid_sites$aqs_site_id)
    expect_equal(
      nrow(.join_lead_sites_and_monitors(lead_valid_sites, lead_initial_data$monitors)),
      (sites_unmatched + matched)
    )
  })
})

test_that("Join preserves unmatched sites and adds expected number of monitors", {
  sites <- data.frame(
    aqs_site_id = c("01", "02", "03", "04", "05")
  )
  monitors <- data.frame(
    aqs_site_id = c("01", "02", "03", "03", "04", "04", "04"),
    poc = c(1, 2, 3, 5, 1, 2, 3)
  )
  expected_df <- data.frame(
    aqs_site_id = c("01", "02", "03", "03", "04", "04", "04", "05"),
    poc = c(1, 2, 3, 5, 1, 2, 3, NA)
  )
  expect_equal(
    .join_lead_sites_and_monitors(sites, monitors),
    expected_df
  )
})

# Master function --------------------------------------------------------------
test_that("Lead master function returns expected data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    df <- suppressWarnings(.get_lead_monitors(year))
    expect_true(
      all(c("aqs_site_id", "poc", "year", "parameter_name") %in% colnames(df))
    )
    expect_equal(
      df,
      get(stringr::str_c("lead_", year))
    )
  })
})
