# Set up -----------------------------------------------------------------------
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
    setup <- get(stringr::str_c("pm25_setup_", year))
    excel_sheets <- readxl::excel_sheets(setup$file_definitions$excel_file_path)
    expect_true(setup$sheets$sites_annual %in% excel_sheets)
  })
})

test_that("24h sites sheet is correctly defined", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    setup <- get(stringr::str_c("pm25_setup_", year))
    excel_sheets <- readxl::excel_sheets(setup$file_definitions$excel_file_path)
    expect_true(setup$sheets$sites_24h %in% excel_sheets)
  })
})

test_that("Monitor sheet is correctly defined", {
  years <- 2016:2023
  purrr::map(years, function(year) {
    setup <- get(stringr::str_c("pm25_setup_", year))
    excel_sheets <- readxl::excel_sheets(setup$file_definitions$excel_file_path)
    expect_true(setup$sheets$monitors %in% excel_sheets)
  })
})

test_that("Rows are skipped correctly when importing Excel data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    setup <- get(stringr::str_c("pm25_setup_", year))
    skip_sites <- 3
    skip_monitors <- 1
    expect_equal(
      setup$skips$sites,
      skip_sites
    )
    expect_equal(
      setup$skips$monitors,
      skip_monitors
    )
  })
})

# Preliminary data processing --------------------------------------------------
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
    .select_cols(df_annual, .define_pm25_column_patterns()$sites_annual),
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
    .select_cols(df_24h, .define_pm25_column_patterns()$sites_24h),
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

test_that("Initial DFs have correct columns", {
  years <- 2012:2023
  # Annual sites
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("pm25_initial_data_", year))$sites_annual
    if (year %in% 2013:2023) {
      expect_equal(colnames(initial_data), c("aqs_site_id", "valid_dv"))
    } else if (year == 2012) {
      expect_equal(colnames(initial_data), c("aqs_site_id", "valid_dv", "validity"))
    }
  })
  # 24h sites
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("pm25_initial_data_", year))$sites_24h
    if (year %in% 2013:2023) {
      expect_equal(colnames(initial_data), c("aqs_site_id", "valid_dv"))
    } else if (year == 2012) {
      expect_equal(colnames(initial_data), c("aqs_site_id", "validity", "valid_dv"))
    }
  })
  # Monitors
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("pm25_initial_data_", year))$monitors
    expect_equal(colnames(initial_data), c("aqs_site_id", "poc"))
  })
})

test_that("Initial data does not contain duplicate observations", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("pm25_initial_data_", year))
    expect_true(!anyDuplicated(initial_data$sites_annual))
    expect_true(!anyDuplicated(initial_data$sites_24h))
    expect_true(!anyDuplicated(initial_data$monitors))
  })
})

test_that("All aqs_site_id_observations in sites DFs are in expected format (string and 9 characters long)", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("pm25_initial_data_", year))
    expect_true(all(stringr::str_detect(initial_data$sites_annual$aqs_site_id, "^[0-9]{9}$")))
    expect_true(all(stringr::str_detect(initial_data$sites_24h$aqs_site_id, "^[0-9]{9}$")))
  })
})

test_that("All aqs_site_id observations in monitors DFs are in expected format (string and 9 characters long)", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("pm25_initial_data_", year))
    expect_true(all(stringr::str_detect(initial_data$monitors$aqs_site_id, "^[0-9]{9}$")))
  })
})

# Validity determination --------------------------------------------------------
test_that("Validity responses are as expected for 2012 data", {
  expect_equal(
    c("N", "N, combo", "Y", "Y, combo"),
    pm25_initial_data_2012$sites_annual$validity |>
      unique() |>
      sort()
  )
  expect_equal(
    c("N", "N, combo", "Y", "Y, combo"),
    pm25_initial_data_2012$sites_24h$validity |>
      unique() |>
      sort()
  )
})

test_that("Valid design values are filtered correctly", {
  df <- data.frame(
    valid_dv = c(0.07, NA, 0.05, NA, 0.10),
    validity = c("Y", "Y", "Y, combo", "N", "N")
  )
  expected_df_2013 <- data.frame(
    valid_dv = c(0.07, 0.05, 0.10),
    validity = c("Y", "Y, combo", "N") # 2013 has no validity column, included for 2012 testing
  )
  expect_equal(
    .filter_pm25_valid_dv(df, year = 2013),
    expected_df_2013
  )
  expected_df_2012 <- data.frame(
    valid_dv = c(0.07, 0.05),
    validity = c("Y", "Y, combo")
  )
  expect_equal(
    .filter_pm25_valid_dv(df, year = 2012),
    expected_df_2012
  )
})

test_that("Validity is determined correctly based on actual data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("pm25_initial_data_", year))
    valid_data <- get(stringr::str_c("pm25_valid_data_", year))
    if (year %in% 2013:2023) {
      expect_equal(
        valid_data$sites_annual,
        initial_data$sites_annual |>
          dplyr::filter(!is.na(valid_dv)) |>
          dplyr::select("aqs_site_id") |>
          dplyr::distinct()
      )
    } else if (year == 2012) {
      expect_equal(
        valid_data$sites_annual,
        initial_data$sites_annual |>
          dplyr::filter(!is.na(valid_dv) & stringr::str_detect(validity, "Y")) |>
          dplyr::select("aqs_site_id") |>
          dplyr::distinct()
      )
    }
  })
})

# DF combination ----------------------------------------------------------------
test_that("DFs have expected columns prior to join", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    valid_data <- get(stringr::str_c("pm25_valid_data_", year))
    initial_data <- get(stringr::str_c("pm25_initial_data_", year))
    # Annual sites
    expect_equal(
      colnames(valid_data$sites_annual),
      c("aqs_site_id")
    )
    # 24h sites
    expect_equal(
      colnames(valid_data$sites_24h),
      c("aqs_site_id")
    )
    # Monitors
    expect_equal(
      colnames(initial_data$monitors),
      c("aqs_site_id", "poc")
    )
  })
})

test_that("No duplicate observations in PM2.5 DFs", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    valid_data <- get(stringr::str_c("pm25_valid_data_", year))
    initial_data <- get(stringr::str_c("pm25_initial_data_", year))
    expect_true(!anyDuplicated(valid_data$sites_annual))
    expect_true(!anyDuplicated(valid_data$sites_24h))
    expect_true(!anyDuplicated(initial_data$monitors))
  })
})

test_that("Joined DFs have expected number of rows", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    valid_data <- get(stringr::str_c("pm25_valid_data_", year))
    initial_data <- get(stringr::str_c("pm25_initial_data_", year))
    sites_unmatched_annual <- sum(!valid_data$sites_annual$aqs_site_id %in% initial_data$monitors$aqs_site_id)
    matched_annual <- sum(initial_data$monitors$aqs_site_id %in% valid_data$sites_annual$aqs_site_id)
    expect_equal(
      nrow(.join_pm25_sites_and_monitors(valid_data, initial_data)$pm25_annual),
      (sites_unmatched_annual + matched_annual)
    )
    sites_unmatched_24h <- sum(!valid_data$sites_24h$aqs_site_id %in% initial_data$monitors$aqs_site_id)
    matched_24h <- sum(initial_data$monitors$aqs_site_id %in% valid_data$sites_24h$aqs_site_id)
    expect_equal(
      nrow(.join_pm25_sites_and_monitors(valid_data, initial_data)$pm25_24h),
      (sites_unmatched_24h + matched_24h)
    )
  })
})

test_that("Join preseverves unmatched sites and adds expected number of monitors", {
  sites_annual <- data.frame(
    aqs_site_id = c("01", "02", "03", "04", "05")
  )
  sites_24h <- data.frame(
    aqs_site_id = c("01", "02", "03", "06")
  )
  valid_data <- list(
    sites_annual = sites_annual,
    sites_24h = sites_24h
  )
  initial_data <- list(
    monitors = data.frame(
      aqs_site_id = c("01", "02", "02", "03", "03", "03", "04"),
      poc = c(1, 2, 3, 1, 2, 3, 1)
    )
  )
  expected <- list(
    pm25_annual = data.frame(
      aqs_site_id = c("01", "02", "02", "03", "03", "03", "04", "05"),
      poc = c(1, 2, 3, 1, 2, 3, 1, NA)
    ),
    pm25_24h = data.frame(
      aqs_site_id = c("01", "02", "02", "03", "03", "03", "06"),
      poc = c(1, 2, 3, 1, 2, 3, NA)
    )
  )
  expect_equal(
    .join_pm25_sites_and_monitors(valid_data, initial_data),
    expected
  )
})

test_that("Final combined DF has correct number of unique monitors", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("pm25_initial_data_", year))
    valid_data <- get(stringr::str_c("pm25_valid_data_", year))
    joined_data <- .join_pm25_sites_and_monitors(valid_data, initial_data)
    shared_monitors <- dplyr::inner_join(
      joined_data$pm25_annual,
      joined_data$pm25_24h,
      by = c("aqs_site_id", "poc")
    ) |>
      nrow()
    unique_monitors_annual <- dplyr::anti_join(
      joined_data$pm25_annual,
      joined_data$pm25_24h,
      by = c("aqs_site_id", "poc")
    ) |>
      nrow()
    unique_monitors_24h <- dplyr::anti_join(
      joined_data$pm25_24h,
      joined_data$pm25_annual,
      by = c("aqs_site_id", "poc")
    ) |>
      nrow()
    expect_equal(
      nrow(.combine_pm25_data(valid_data, initial_data) %>% dplyr::distinct()),
      (shared_monitors + unique_monitors_annual + unique_monitors_24h)
    )
  })
})

test_that(".combine_pm25_data joins data correctly on simulated data", {
  sites_1 <- data.frame(
    aqs_site_id = c(1, 2, 3, 6)
  )
  sites_2 <- data.frame(
    aqs_site_id = c(3, 4, 5)
  )
  valid_data_list <- list(
    sites_annual = sites_1,
    sites_24h = sites_2
  )
  monitors_1 <- data.frame(
    aqs_site_id = c(1, 2, 2, 3, 4, 4),
    poc = c(1, 2, 1, 3, 4, 1)
  )
  initial_data_list <- list(
    monitors = monitors_1
  )
  expected_df <- data.frame(
    aqs_site_id = c(1, 2, 2, 3, 6, 4, 4, 5),
    poc = c(1, 2, 1, 3, NA, 4, 1, NA)
  )
  expect_equal(
    .combine_pm25_data(valid_data_list, initial_data_list),
    expected_df
  )
})

# Main function -----------------------------------------------------------------
test_that("Master function returns expected data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    df <- suppressWarnings(.get_pm25_monitors(year))
    expect_true(all(c("aqs_site_id", "poc", "year", "parameter_name") %in% colnames(df)))
    expect_equal(
      df,
      .finalize_pm25_data(
        valid_data_list = get(stringr::str_c("pm25_valid_data_", year)),
        initial_data_list = get(stringr::str_c("pm25_initial_data_", year)),
        year = year
      )
    )
  })
})
