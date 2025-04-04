test_that("Parameter metadata file is imported correctly", {
  expect_true(
    all(c(
      "class_code",
      "class_name",
      "parameter",
      "parameter_code"
    ) %in% colnames(parameter_metadata))
  )
  expect_true(
    all(c(
      "APP_A_PARAMETERS",
      "AQI POLLUTANTS"
    ) %in% unique(parameter_metadata$class_code))
  )
})

test_that("NAAQS parameters are imported correctly", {
  expect_equal(
    colnames(regulatory_parameters),
    c("parameter", "parameter_code")
  )
  expect_equal(
    sort(regulatory_parameters$parameter_code),
    sort(c(42101, 14129, 85129, 42602, 44201, 81102, 88101, 42401))
  )
})

test_that("AQI parameters are imported correctly", {
  expect_equal(
    sort(colnames(aqi_naaqs_parameters)),
    sort(c("parameter", "parameter_code"))
  )
  expect_equal(
    sort(unique(aqi_naaqs_parameters$parameter_code)),
    sort(c(42101, 14129, 85129, 42602, 44201, 81102, 88101, 42401, 88502))
  )
})
