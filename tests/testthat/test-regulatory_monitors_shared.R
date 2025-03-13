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
