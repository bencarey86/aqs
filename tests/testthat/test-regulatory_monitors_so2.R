# Set up -----------------------------------------------------------------------
test_that("URL is defined correctly and has active link", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    url <- .define_so2_url(year)
    expect_true(is.character(url))
    expect_true(httr::HEAD(url)$status == 200)
  })
})

test_that("Sites sheet is correctly defined", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    setup <- get(stringr::str_c("so2_setup_", year))
    excel_sheet <- readxl::excel_sheets(setup$file_definitions$excel_file_path)
    expect_true(setup$sheets$sites %in% excel_sheet)
  })
})

test_that("Monitors sheet is correctly defined", {
  years <- 2016:2023
  purrr::map(years, function(year) {
    setup <- get(stringr::str_c("so2_setup_", year))
    excel_sheet <- readxl::excel_sheets(setup$file_definitions$excel_file_path)
    expect_true(setup$sheets$monitors %in% excel_sheet)
  })
})

test_that("Rows are skipped correctly when importing Excel data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    setup <- get(stringr::str_c("so2_setup_", year))
    if (year %in% 2019:2023) {
      skip_sites <- 3
      skip_monitors <- 1
    } else if (year %in% 2014:2018) {
      skip_sites <- 2
      skip_monitors <- 0
    } else if (year == 2013) {
      skip_sites <- 3
      skip_monitors <- 0
    } else if (year == 2012) {
      skip_sites <- 4
      skip_monitors <- 0
    }
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
  df <- data.frame(
    x_aqs_site_id_y = c(1, 2, 3),
    site_id = c(1, 2, 3),
    x_site_id = c(4, 5, 6),
    site_id_y = c(7, 8, 9),
    valid_x = c(1, 2, 3),
    x_valid_y = c(4, 5, 6),
    x2012_2012_design_value_y = c(7, 8, 9),
    x3_year_design_value2_3 = c(1, 2, 3),
    x_validity_y = c(4, 5, 6)
  )
  expected_df <- df |>
    dplyr::select(x_aqs_site_id_y, site_id, valid_x, x2012_2012_design_value_y, x3_year_design_value2_3, x_validity_y)
  expect_equal(
    .select_cols(df, .define_so2_column_patterns()$sites),
    expected_df
  )
})

test_that("Site columns are renamed correctly", {
  df <- data.frame(
    x_site_y = c(1, 1),
    x2012_2012_design_value_ppb = c(2, 2),
    x_validity_y = c(3, 3)
  )
  expected_df <- data.frame(
    aqs_site_id = c(1, 1),
    valid_dv = c(2, 2),
    validity = c(3, 3)
  )
  expect_equal(
    .rename_so2_site_columns(df),
    expected_df
  )
})

test_that("All 2018 aqs_id observations are same length", {
  setup <- get("so2_setup_2018")
  df <- .import_regulatory_monitor_data(
    excel_file_path = setup$file_definitions$excel_file_path,
    sheet = setup$sheets$monitors,
    skip = setup$skips$monitors
  )
  expect_equal(
    df$aqs_id |>
      nchar() |>
      unique(),
    13
  )
})

test_that("All 2018 aqs_id observations follow same pattern", {
  setup <- get("so2_setup_2018")
  df <- .import_regulatory_monitor_data(
    excel_file_path = setup$file_definitions$excel_file_path,
    sheet = setup$sheets$monitors,
    skip = setup$skips$monitors
  )
  expect_true(all(stringr::str_detect(df$aqs_id, "^[0-9]{2}-[0-9]{3}-[0-9]{4}-[0-9]$")))
})

test_that("Initial site DFs have correct columns", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("so2_initial_data_", year))
    if (year %in% 2017:2023) {
      expect_equal(
        colnames(initial_data$sites) |>
          sort(),
        c("aqs_site_id", "valid_dv")
      )
    } else if (year %in% 2012:2016) {
      expect_equal(
        colnames(initial_data$sites),
        c("aqs_site_id", "valid_dv", "validity")
      )
    }
  })
})

test_that("Initial monitor DFs have correct columns", {
  years <- 2016:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("so2_initial_data_", year))
    expect_equal(
      colnames(initial_data$monitors),
      c("aqs_site_id", "poc")
    )
  })
})

test_that("Initial data does not contain duplicate observations", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("so2_initial_data_", year))
    expect_true(!anyDuplicated(initial_data$sites))
    expect_true(!anyDuplicated(initial_data$monitors))
  })
})

test_that("All aqs_site_id observations in site DF are in expected format (string and 9 characters long)", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("so2_initial_data_", year))
    expect_true(all(stringr::str_detect(initial_data$sites$aqs_site_id, "^[0-9]{9}$")))
  })
})

test_that("All aqs_site_id observations in monitors DF are in expected format (string and 9 characters long)", {
  years <- 2016:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("so2_initial_data_", year))
    expect_true(all(stringr::str_detect(initial_data$monitors$aqs_site_id, "^[0-9]{9}$")))
  })
})

# Validity determination --------------------------------------------------------
test_that("Validity responses are as expected for 2012-2016", {
  years <- 2012:2016
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("so2_initial_data_", year))
    expect_equal(
      unique(initial_data$sites$validity),
      c("N", "Y")
    )
  })
})

test_that("Valid design values are filtered correctly on simulated data", {
  df_2012_2016 <- data.frame(
    valid_dv = c(1, 2, NA, 3, NA, 4),
    validity = c("Y", "Y", "N", "Y", "Y", "N")
  )
  expected_df_2012_2016 <- data.frame(
    valid_dv = c(1, 2, 3),
    validity = c("Y", "Y", "Y")
  )
  expect_equal(
    .filter_so2_valid_dv(df_2012_2016, 2012),
    expected_df_2012_2016
  )
  df_2017_2023 <- df_2012_2016 |>
    dplyr::select(-"validity")
  expected_df_2017_2023 <- data.frame(
    valid_dv = c(1, 2, 3, 4)
  )
  expect_equal(
    .filter_so2_valid_dv(df_2017_2023, 2017),
    expected_df_2017_2023
  )
})

test_that("Valud design values are determined correctly on actual data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("so2_initial_data_", year))
    valid_data <- get(stringr::str_c("so2_valid_data_", year))
    if (year %in% 2017:2023) {
      expect_equal(
        valid_data,
        initial_data$sites |>
          dplyr::filter(!is.na(valid_dv)) |>
          dplyr::select("aqs_site_id") |>
          dplyr::distinct()
      )
    } else if (year %in% 2012:2016) {
      expect_equal(
        valid_data,
        initial_data$sites |>
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
    valid_data <- get(stringr::str_c("so2_valid_data_", year))
    initial_data <- get(stringr::str_c("so2_initial_data_", year))
    # Annual sites
    expect_equal(
      colnames(valid_data),
      c("aqs_site_id")
    )
    # Monitors
    expect_equal(
      colnames(initial_data$monitors),
      c("aqs_site_id", "poc")
    )
  })
})

test_that("No duplicate observations in SO2 DFs", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    valid_data <- get(stringr::str_c("so2_valid_data_", year))
    initial_data <- get(stringr::str_c("so2_initial_data_", year))
    expect_true(!anyDuplicated(valid_data))
    expect_true(!anyDuplicated(initial_data$monitors))
  })
})

test_that("Joined DFs have expected number of rows", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    valid_data <- get(stringr::str_c("so2_valid_data_", year))
    initial_data <- get(stringr::str_c("so2_initial_data_", year))
    sites_unmatched <- sum(!valid_data$aqs_site_id %in% initial_data$monitors$aqs_site_id)
    matched <- sum(initial_data$monitors$aqs_site_id %in% valid_data$aqs_site_id)
    expect_equal(
      nrow(.join_so2_sites_and_monitors(valid_data, initial_data$monitors)),
      (sites_unmatched + matched)
    )
  })
})

test_that("Join preseverves unmatched sites and adds expected number of monitors", {
  sites <- data.frame(
    aqs_site_id = c("01", "02", "03", "04", "05")
  )
  valid_data <- sites
  initial_data <- list(
    monitors = data.frame(
      aqs_site_id = c("01", "02", "02", "03", "03", "03", "04"),
      poc = c(1, 2, 3, 1, 2, 3, 1)
    )
  )
  expected <- data.frame(
    aqs_site_id = c("01", "02", "02", "03", "03", "03", "04", "05"),
    poc = c(1, 2, 3, 1, 2, 3, 1, NA)
  )
  expect_equal(
    .join_so2_sites_and_monitors(valid_data, initial_data$monitors),
    expected
  )
})

# Main function -----------------------------------------------------------------
test_that("Master function returns expected data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    df <- suppressWarnings(.get_so2_monitors(year))
    expect_true(all(c("aqs_site_id", "poc", "year", "parameter_name") %in% colnames(df)))
    expect_equal(
      df,
      .finalize_so2_data(
        sites = get(stringr::str_c("so2_valid_data_", year)),
        monitors = get(stringr::str_c("so2_initial_data_", year))$monitors,
        year = year
      )
    )
  })
})
