test_that("Parameter metadata file is imported correctly", {
  df <- .get_parameters()
  expect_true(
    all(c(
      "class_code",
      "class_name",
      "parameter",
      "parameter_code"
    ) %in% colnames(df))
  )
  expect_true(
    all(c(
      "APP_A_PARAMETERS",
      "AQI POLLUTANTS"
    ) %in% unique(df$class_code))
  )
})

test_that("NAAQS parameters are imported correctly", {
  df <- .get_naaqs_parameters()
  expect_true(
    all(c(
      "parameter",
      "parameter_code"
    ) %in% colnames(df))
  )
  expect_true(
    all(c(
      42101,
      14129,
      85129,
      42602,
      44201,
      81102,
      88101,
      42401
    ) %in% unique(df$parameter_code))
  )
})

test_that("AQI parameters are imported correctly", {
  df <- .get_aqi_parameters()
  expect_true(
    all(c(
      "parameter",
      "parameter_code"
    ) %in% colnames(df))
  )
  expect_true(
    all(c(
      42101,
      14129,
      85129,
      42602,
      44201,
      81102,
      88101,
      42401,
      88502
    ) %in% unique(df$parameter_code))
  )
})
