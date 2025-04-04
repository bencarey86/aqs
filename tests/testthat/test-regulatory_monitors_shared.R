test_that("Standards are combined correctly", {
  df_1 <- data.frame(
    valid_dv = c("1", "2", "3")
  )
  df_2 <- data.frame(
    valid_dv = c("4", "5", "6")
  )
  expected_df <- data.frame(
    valid_dv = c(1, 2, 3, 4, 5, 6)
  )
  expect_equal(
    .combine_standards(df_1, df_2),
    expected_df
  )
})

test_that("Site ID and POC column types are converted correctly", {
  df_1 <- data.frame(
    aqs_site_id = c(1, 2, 3),
    poc = c(4, 5, 6)
  )
  df_2 <- aqs_site_id <- data.frame(
    aqs_site_id = c("000000001", "000000002", "000000003"),
    poc = c("4", "5", "6")
  )
  df_3 <- data.frame(
    aqs_site_id = c(1, 2, 3),
    poc = c("4", "5", "6")
  )
  df_4 <- data.frame(
    aqs_site_id = c("000000001", "000000002", "000000003"),
    poc = c(4, 5, 6)
  )
  expected_df <- data.frame(
    aqs_site_id = c("000000001", "000000002", "000000003"),
    poc = c(4, 5, 6)
  )
  df_list <- list(df_1, df_2, df_3, df_4)
  purrr::map(df_list, function(df) {
    expect_equal(
      .convert_site_id_poc_column_types(df),
      expected_df
    )
  })
})

test_that("drop_rows_all_na drops only rows with all NA values", {
  df <- data.frame(
    a = c(1, NA, 3),
    b = c(NA, NA, NA),
    c = c(NA, NA, 3)
  )
  expected_df <- data.frame(
    a = c(1, 3),
    b = c(NA, NA),
    c = c(NA, 3)
  )
  expect_equal(
    .drop_rows_all_na(df),
    expected_df
  )
})

test_that("Parameter names are redefined to match monitor DFs correctly", {
  # Define expected parameter names
  parameter_names <- c("Carbon monoxide", "Lead", "Nitrogen dioxide (NO2)", "Ozone", "PM10", "PM2.5", "Sulfur dioxide")
  # Compare to actual parameter names in regulatory monitor DFs and monitor metadata
  parameter <- c("co", "lead", "no2", "o3", "pm10", "pm25", "so2")
  year <- 2012:2023
  parameter_year_grid <- tidyr::expand_grid(parameter, year)
  monitor_df_parameter_names <- purrr::pmap_chr(parameter_year_grid, function(parameter, year) {
    get(stringr::str_c(parameter, "_", year))$parameter_name |> unique()
  }) |>
    unique() |>
    sort()
  expect_equal(
    .get_regulatory_monitor_metadata()$parameter_name |> unique() |> sort(),
    parameter_names |> sort()
  )
  expect_equal(
    monitor_df_parameter_names,
    parameter_names
  )
})
