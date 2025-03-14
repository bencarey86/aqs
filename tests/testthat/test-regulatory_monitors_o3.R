# Set up -----------------------------------------------------------------------
test_that("O3 URL is defined correctly and has active link", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    url <- .define_o3_url(year)
    expect_true(is.character(url))
    expect_true(httr::HEAD(url)$status == 200)
  })
})

test_that("O3 sites sheet is correctly defined", {
  # Sites data only used for 2015-2023
  years <- 2015:2023
  purrr::map(years, function(year) {
    setup <- get(stringr::str_c("o3_setup_", year))
    excel_sheet <- readxl::excel_sheets(setup$file_definitions$excel_file_path)
    expect_true(setup$sheets$sites %in% excel_sheet)
  })
})

test_that("O3 monitor sheet is correctly defined", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    # No monitor data for 2015; data from 2016 is used instead
    if (year == 2015) {
      year <- 2016
    }
    setup <- get(stringr::str_c("o3_setup_", year))
    excel_sheet <- readxl::excel_sheets(setup$file_definitions$excel_file_path)
    expect_true(setup$sheets$monitors %in% excel_sheet)
  })
})

test_that("Rows are skipped correctly when importing Excel data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    setup <- get(stringr::str_c("o3_setup_", year))
    expected_sites_skip <- 3
    expected_monitors_skip <- ifelse(year %in% 2015:2023, 1, 3)
    expect_equal(
      setup$skips$sites,
      expected_sites_skip
    )
    expect_equal(
      setup$skips$monitors,
      expected_monitors_skip
    )
  })
})

# Preliminary data processing --------------------------------------------------
test_that("Columns are selected correctly", {
  df <- data.frame(
    x_aqs_site_id_y = c("06-001-0007", "06-001-0007", "06-001-0007"),
    x_poc_y = c(1, 1, 1),
    valid_2012_2015_design_value = c(1, 1, 1),
    x2012_2015_design_value_ppm = c(0.07, 0.07, 0.07),
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
    x_valid_y = c(1, 1, 1)
  )
  expected_df <- data.frame(
    valid_dv = c(1, 1, 1)
  )
  expect_equal(
    .rename_o3_columns(df),
    expected_df
  )
})

test_that("Initial DFs have correct columns", {
  # Sites (2015-2023)
  years <- 2015:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("o3_initial_data_", year))
    expect_equal(
      colnames(initial_data$sites),
      c("aqs_site_id", "valid_dv")
    )
  })
  # Monitors
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("o3_initial_data_", year))
    if (year %in% 2015:2023) {
      expect_equal(
        colnames(initial_data$monitors),
        c("aqs_site_id", "poc")
      )
    } else if (year %in% 2012:2014) {
      expect_equal(
        colnames(initial_data$monitors),
        c("aqs_site_id", "poc", "valid_dv")
      )
    }
  })
})

test_that("Initial data does not contain duplicate observations", {
  # Sites (2015-2023)
  years <- 2015:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("o3_initial_data_", year))
    expect_true(
      !anyDuplicated(initial_data$sites)
    )
  })
  # Monitors
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("o3_initial_data_", year))
    expect_true(
      !anyDuplicated(initial_data$monitors)
    )
  })
})

test_that("Initial data does not contain any observations with all NA values", {
  # Sites (2015-2023)
  years <- 2015:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("o3_initial_data_", year))
    expect_true(
      all(rowSums(!is.na(initial_data$sites)) > 0)
    )
  })
  # Monitors
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("o3_initial_data_", year))
    expect_true(
      all(rowSums(!is.na(initial_data$monitors)) > 0)
    )
  })
})

# Validity determination --------------------------------------------------------
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

test_that("Validity is determined correctly on actual data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("o3_initial_data_", year))
    valid_data <- get(stringr::str_c("o3_valid_data_", year))
    if (year %in% 2015:2023) {
      expect_equal(
        valid_data$sites,
        initial_data$sites |>
          dplyr::filter(!is.na(valid_dv)) |>
          dplyr::select("aqs_site_id") |>
          dplyr::distinct()
      )
    } else if (year %in% 2012:2014) {
      expect_equal(
        valid_data$monitors,
        initial_data$monitors |>
          dplyr::filter(!is.na(valid_dv)) |>
          dplyr::select("aqs_site_id", "poc") |>
          dplyr::distinct()
      )
    }
  })
})

# DF combination ----------------------------------------------------------------
test_that("DFs have expected columns prior to joining", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    valid_data <- get(stringr::str_c("o3_valid_data_", year))
    initial_data <- get(stringr::str_c("o3_initial_data_", year))
    if (year %in% 2015:2023) {
      expect_equal(
        colnames(valid_data$sites),
        c("aqs_site_id")
      )
      expect_equal(
        colnames(initial_data$monitors),
        c("aqs_site_id", "poc")
      )
    } else if (year %in% 2012:2014) {
      expect_equal(
        colnames(valid_data$monitors),
        c("aqs_site_id", "poc")
      )
    }
  })
})

test_that("No duplicates exist in joining fields", {
  years <- 2015:2023
  purrr::map(years, function(year) {
    valid_data <- get(stringr::str_c("o3_valid_data_", year))
    initial_data <- get(stringr::str_c("o3_initial_data_", year))
    expect_true(
      !anyDuplicated(valid_data$sites)
    )
    expect_true(
      !anyDuplicated(initial_data$monitors)
    )
  })
})

test_that("Joined DFs have expected number of rows", {
  years <- 2015:2023
  purrr::map(years, function(year) {
    valid_data <- get(stringr::str_c("o3_valid_data_", year))
    initial_data <- get(stringr::str_c("o3_initial_data_", year))
    joined_df <- .join_o3_sites_and_monitors(valid_data, initial_data)
    sites_unmatched <- sum(!valid_data$sites$aqs_site_id %in% initial_data$monitors$aqs_site_id)
    matched <- sum(initial_data$monitors$aqs_site_id %in% valid_data$sites$aqs_site_id)
    expect_equal(
      nrow(joined_df),
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
    .join_o3_sites_and_monitors(sites, monitors),
    expected_df
  )
})

test_that("Finalizing function returns data as expected (2015-2023 data joined, earlier not)", {
  valid_data_list <- list(
    sites = data.frame(
      aqs_site_id = c("01", "02", "03", "04", "05")
    ),
    monitors = data.frame(
      aqs_site_id = c("01", "02", "03", "03", "04", "04", "04"),
      poc = c(1, 2, 3, 5, 1, 2, 3)
    )
  )
  initial_data_list <- list(
    monitors = data.frame(
      aqs_site_id = c("01", "02", "03", "03", "04", "04", "04"),
      poc = c(1, 2, 3, 5, 1, 2, 3)
    )
  )
  expected_2015_2023 <- data.frame(
    aqs_site_id = c("01", "02", "03", "03", "04", "04", "04", "05"),
    poc = c(1, 2, 3, 5, 1, 2, 3, NA)
  )
  expected_2012_2014 <- valid_data_list$monitors
  years <- 2015:2023
  purrr::map(years, function(year) {
    if (year %in% 2015:2023) {
      expected_2015_2023 <- expected_2015_2023 |>
        dplyr::mutate(
          year = year,
          parameter_name = "Ozone"
        )
      expect_equal(
        .finalize_o3_data(valid_data_list, initial_data_list, year),
        expected_2015_2023
      )
    } else if (year %in% 2012:2014) {
      expected_2012_2014 <- expected_2012_2014 |>
        dplyr::mutate(
          year = year,
          parameter_name = "Ozone"
        )
      expect_equal(
        .finalize_o3_data(valid_data_list, initial_data_list, year),
        expected_2012_2014
      )
    }
  })
})

# Master function --------------------------------------------------------------
test_that("Master function returns expected data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    df <- .get_o3_monitors(year)
    expect_true(all(c("aqs_site_id", "poc", "year", "parameter_name") %in% colnames(df)))
    expect_equal(
      df,
      get(stringr::str_c("o3_", year))
    )
  })
})
