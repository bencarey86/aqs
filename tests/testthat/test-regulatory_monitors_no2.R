# Set up -----------------------------------------------------------------------
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
    setup <- get(stringr::str_c("no2_setup_", year))
    excel_sheets <- readxl::excel_sheets(setup$file_definitions$excel_file_path)
    expect_true(setup$sheets$sites_annual %in% excel_sheets)
    expect_true(setup$sheets$sites_1h %in% excel_sheets)
  })
})

test_that("NO2 monitor sheet is correctly defined", {
  # No monitor data for 2012-2015; data from 2016 is used instead
  years <- 2016:2023
  purrr::map(years, function(year) {
    setup <- get(stringr::str_c("no2_setup_", year))
    excel_sheets <- readxl::excel_sheets(setup$file_definitions$excel_file_path)
    expect_true(setup$sheets$monitors %in% excel_sheets)
  })
})

test_that("Excel row skips are correctly defined", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    setup <- get(stringr::str_c("no2_setup_", year))
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
test_that("NO2 site columns are selected correctly", {
  df <- data.frame(
    site = c("0010", "0011", "0012"),
    x_aqs_site_id_y = c("060010001", "060010011", "060010012"),
    x2010_2012_1_hr_design_value_y = c(0.1, 0.2, 0.3),
    x2012_annual_design_value_y = c(0.1, 0.2, 0.3),
    valid = c("Y", "Y", "Y"),
    valid_2012_2014_y = c("Y", "Y", "Y"),
    valid_2023_annual_design_value_y = c("Y", "Y", "Y"),
    completeness_y = c(100, 100, 100),
    a = c(1, 2, 3),
    b = c(4, 5, 6)
  )
  expected_df <- df |>
    dplyr::select(-"a", -"b")
  expect_equal(
    .select_cols(
      df = df,
      col_patterns = .define_no2_column_patterns()$sites
    ),
    expected_df
  )
})

test_that("NO2 site columns are renamed correctly", {
  df <- data.frame(
    site = c("0010", "0011", "0012"),
    x_valid_dv_y = c(0.1, 0.2, 0.3),
    completeness_y = c(100, 100, 100)
  )
  expected_df <- data.frame(
    aqs_site_id = c("0010", "0011", "0012"),
    valid_dv = c(0.1, 0.2, 0.3),
    completeness = c(100, 100, 100)
  )
  expect_equal(
    .rename_no2_site_columns(df),
    expected_df
  )
})

test_that("Initial DFs have correct columns", {
  years <- 2012:2023
  # Annual sites
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("no2_initial_data_", year))
    if (year %in% 2013:2023) {
      expect_equal(colnames(initial_data$sites_annual), c("aqs_site_id", "valid_dv"))
    } else if (year %in% 2012) {
      expect_equal(colnames(initial_data$sites_annual), c("aqs_site_id", "valid_dv", "completeness"))
    }
  })
  # 1 hour sites
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("no2_initial_data_", year))
    if (year %in% 2013:2023) {
      expect_equal(colnames(initial_data$sites_1h), c("aqs_site_id", "valid_dv"))
    } else if (year %in% 2012) {
      expect_equal(colnames(initial_data$sites_1h), c("aqs_site_id", "valid_dv", "completeness"))
    }
  })
  # Monitors
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("no2_initial_data_", year))
    expect_equal(colnames(initial_data$monitors), c("aqs_site_id", "poc"))
  })
})

test_that("Initial data does not contain duplicate observations", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("no2_initial_data_", year))
    expect_true(!anyDuplicated(initial_data$sites_annual))
    expect_true(!anyDuplicated(initial_data$sites_1h))
    expect_true(!anyDuplicated(initial_data$monitors))
  })
})

test_that("Sites aqs_site_id observations are in expected format (string, 9 digits)", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("no2_initial_data_", year))
    expect_true(all(stringr::str_detect(initial_data$sites_annual$aqs_site_id, "^[0-9]{9}$")))
    expect_true(all(stringr::str_detect(initial_data$sites_1h$aqs_site_id, "^[0-9]{9}$")))
  })
})

test_that("Monitors aqs_site_id observations are in expected format (string, 9 digits)", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("no2_initial_data_", year))
    expect_true(all(stringr::str_detect(initial_data$monitors$aqs_site_id, "^[0-9]{9}$")))
  })
})

# Validity determination -------------------------------------------------------
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

test_that("Validity is determined correctly based on actual data", {
  # 2013-2023 (valid_dv only), 2012 (valid_dv and completeness)
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("no2_initial_data_", year))
    valid_data <- get(stringr::str_c("no2_valid_sites_", year))
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
          dplyr::filter(!is.na(valid_dv) & completeness == "Y") |>
          dplyr::select("aqs_site_id") |>
          dplyr::distinct()
      )
    }
  })
})

test_that("Completeness responses adhere to expected pattern for 2012", {
  expect_true(
    all(unique(
      c(
        no2_initial_data_2012$sites_annual$completeness,
        no2_initial_data_2012$sites_1h$completeness
      )
    )
    %in% c("Y", "N"))
  )
})

# DF combination ----------------------------------------------------------------
test_that("NO2 DFs have expected columns prior to join", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("no2_initial_data_", year))
    valid_sites <- get(stringr::str_c("no2_valid_sites_", year))
    expect_equal(
      colnames(valid_sites$sites_annual),
      "aqs_site_id"
    )
    expect_equal(
      colnames(valid_sites$sites_1h),
      "aqs_site_id"
    )
    expect_equal(
      colnames(initial_data$monitors),
      c("aqs_site_id", "poc")
    )
  })
})

test_that("No duplicate observations in NO2 DFs", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("no2_initial_data_", year))
    valid_sites <- get(stringr::str_c("no2_valid_sites_", year))
    expect_true(!anyDuplicated(valid_sites$sites_annual))
    expect_true(!anyDuplicated(valid_sites$sites_1h))
    expect_true(!anyDuplicated(initial_data$monitors))
  })
})

test_that("Joined DFs have expected number of rows", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("no2_initial_data_", year))
    valid_sites <- get(stringr::str_c("no2_valid_sites_", year))
    sites_unmatched_annual <- sum(!valid_sites$sites_annual$aqs_site_id %in% initial_data$monitors$aqs_site_id)
    matched_annual <- sum(initial_data$monitors$aqs_site_id %in% valid_sites$sites_annual$aqs_site_id)
    expect_equal(
      nrow(.join_no2_sites_and_monitors(valid_sites, initial_data)$no2_annual),
      (sites_unmatched_annual + matched_annual)
    )
    sites_unmatched_1h <- sum(!valid_sites$sites_1h$aqs_site_id %in% initial_data$monitors$aqs_site_id)
    matched_1h <- sum(initial_data$monitors$aqs_site_id %in% valid_sites$sites_1h$aqs_site_id)
    expect_equal(
      nrow(.join_no2_sites_and_monitors(valid_sites, initial_data)$no2_1h),
      (sites_unmatched_1h + matched_1h)
    )
  })
})

test_that("Join preserves unmatched sites and adds expected number of monitors", {
  sites_annual <- data.frame(
    aqs_site_id = c("01", "02", "03", "04", "05")
  )
  sites_1h <- data.frame(
    aqs_site_id = c("01", "02", "03", "06")
  )
  valid_data <- list(
    sites_annual = sites_annual,
    sites_1h = sites_1h
  )
  initial_data <- list(
    monitors = data.frame(
      aqs_site_id = c("01", "02", "02", "03", "03", "03", "04"),
      poc = c(1, 2, 3, 1, 2, 3, 1)
    )
  )
  expected <- list(
    no2_annual = data.frame(
      aqs_site_id = c("01", "02", "02", "03", "03", "03", "04", "05"),
      poc = c(1, 2, 3, 1, 2, 3, 1, NA)
    ),
    no2_1h = data.frame(
      aqs_site_id = c("01", "02", "02", "03", "03", "03", "06"),
      poc = c(1, 2, 3, 1, 2, 3, NA)
    )
  )
  expect_equal(
    .join_no2_sites_and_monitors(valid_data, initial_data),
    expected
  )
})

test_that("Final combined DF has correct number of unique monitors", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("no2_initial_data_", year))
    valid_sites <- get(stringr::str_c("no2_valid_sites_", year))
    joined_data <- .join_no2_sites_and_monitors(
      valid_data_list = valid_sites,
      initial_data_list = initial_data
    )
    shared_monitors <- dplyr::inner_join(
      joined_data$no2_annual,
      joined_data$no2_1h,
      by = c("aqs_site_id", "poc")
    ) |>
      nrow()
    unique_monitors_annual <- dplyr::anti_join(
      joined_data$no2_annual,
      joined_data$no2_1h,
      by = c("aqs_site_id", "poc")
    ) |>
      nrow()
    unique_monitors_1h <- dplyr::anti_join(
      joined_data$no2_1h,
      joined_data$no2_annual,
      by = c("aqs_site_id", "poc")
    ) |>
      nrow()
    expect_equal(
      nrow(.combine_no2_data(
        valid_data_list = valid_sites,
        initial_data_list = initial_data
      ) |> dplyr::distinct()),
      (shared_monitors + unique_monitors_annual + unique_monitors_1h)
    )
  })
})

test_that(".combine_no2_data joins data correctly", {
  sites_1 <- data.frame(
    aqs_site_id = c(1, 2, 3, 6)
  )
  sites_2 <- data.frame(
    aqs_site_id = c(3, 4, 5)
  )
  valid_data_list <- list(
    sites_annual = sites_1,
    sites_1h = sites_2
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
    .combine_no2_data(valid_data_list, initial_data_list),
    expected_df
  )
})

# Master function --------------------------------------------------------------
test_that("NO2 master function returns expected data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    df <- .get_no2_monitors(year)
    expect_true(all(c("aqs_site_id", "poc", "year", "parameter_name") %in% colnames(df)))
    expect_equal(
      df, get(stringr::str_c("no2_", year))
    )
  })
})
