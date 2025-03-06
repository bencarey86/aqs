test_that("All monitors master function returns expected output", {
  aqi_parameters <- .get_aqi_parameters()
  df <- get_all_monitors(start = 2021, end = 2022, parameter_codes = aqi_parameters$parameter_code)
  expect_equal(
    colnames(df),
    c(
      "state_code",
      "county_code",
      "site_number",
      "poc",
      "parameter_code",
      "parameter_name",
      "latitude",
      "longitude",
      "year"
    )
  )
  expect_true(all(c(2021, 2022) %in% unique(df$year)))
  expect_true(all(unique(df$parameter_code) %in% aqi_parameters$parameter_code))
})
