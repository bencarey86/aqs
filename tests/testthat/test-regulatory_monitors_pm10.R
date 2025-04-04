# Set up -----------------------------------------------------------------------
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
    setup <- get(stringr::str_c("pm10_setup_", year))
    excel_sheet <- readxl::excel_sheets(setup$file_definitions$excel_file_path)
    expect_true(setup$sheets %in% excel_sheet)
  })
})

# Preliminary data processing --------------------------------------------------
test_that("Columns are selected correctly", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    setup <- get(stringr::str_c("pm10_setup_", year))
    excel_file_path <- setup$file_definitions$excel_file_path
    sheet <- setup$sheets
    skip <- setup$skips
    column_patterns <- setup$column_patterns
    df <- .import_regulatory_monitor_data(
      excel_file_path = excel_file_path,
      sheet = sheet,
      skip = skip
    ) |>
      .select_cols(column_patterns)
    if (year %in% 2019:2023) {
      expect_equal(
        colnames(df),
        c("aqs_site_id", "poc", paste0("valid_", (year - 2), "_", year, "_average_estimated_exceedances_1_2"))
      )
    } else if (year %in% 2017:2018) {
      expect_equal(
        colnames(df),
        c("site", "poc", paste0("x", (year - 2), "_", year, "_average_estimated_exceedances1_2"))
      )
    } else if (year %in% 2012:2016) {
      expect_equal(
        colnames(df),
        c("site", "poc", paste0("ene_", (year - 2), "_", year), paste0("ene_valid_", (year - 2), "_", year))
      )
    }
  })
})

test_that("Columns are renamed correctly", {
  df <- data.frame(
    x_site_y = c(1, 1),
    x_exceedances_y = c(1, 1),
    x_valid_2012 = c(1, 1)
  )
  expected_df <- data.frame(
    aqs_site_id = c(1, 1),
    valid_dv = c(1, 1),
    validity = c(1, 1)
  )
  expect_equal(
    .rename_pm10_columns(df),
    expected_df
  )
})

test_that("Initial DFs have correct columns", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("pm10_initial_data_", year))
    if (year %in% 2017:2023) {
      expect_equal(
        colnames(initial_data),
        c("aqs_site_id", "poc", "valid_dv")
      )
    } else if (year %in% 2012:2016) {
      expect_equal(
        colnames(initial_data),
        c("aqs_site_id", "poc", "valid_dv", "validity")
      )
    }
  })
})

test_that("Initial data does not contain duplicate observations", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("pm10_initial_data_", year))
    expect_true(
      !anyDuplicated(initial_data)
    )
  })
})

test_that("Initial data does not contain any observations with all NA values", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("pm10_initial_data_", year))
    expect_true(all(rowSums(!is.na(initial_data)) > 0))
  })
})

test_that("aqs_site_id observations match expected pattern ($[0-9]{9}$)", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("pm10_initial_data_", year))
    expect_true(all(stringr::str_detect(initial_data$aqs_site_id, "^[0-9]{9}$")))
  })
})

# Validity determination --------------------------------------------------------
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

test_that("Validity is determined correctly on actual data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    initial_data <- get(stringr::str_c("pm10_initial_data_", year))
    valid_data <- get(stringr::str_c("pm10_valid_data_", year))
    if (year %in% 2017:2023) {
      expect_equal(
        valid_data,
        initial_data |>
          dplyr::filter(!is.na(valid_dv))
      )
    } else if (year %in% 2012:2016) {
      expect_equal(
        valid_data,
        initial_data |>
          dplyr::filter(!is.na(valid_dv) & stringr::str_detect(validity, "^Y"))
      )
    }
  })
})

# Main function ----------------------------------------------------------------
test_that("Master function returns expected data", {
  years <- 2012:2023
  purrr::map(years, function(year) {
    df <- .get_pm10_monitors(year)
    expect_true(all(c("aqs_site_id", "poc", "year", "parameter_name") %in% colnames(df)))
    expect_equal(
      df,
      get(stringr::str_c("pm10_valid_data_", year)) |>
        .finalize_pm10_data(year)
    )
  })
})
