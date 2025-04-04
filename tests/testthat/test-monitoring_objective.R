test_that("Monitoring objective is simplified correctly", {
  df <- data.frame(
    monitoring_objective = c(
      "SOURCE",
      "HIGHEST",
      "MAX OZONE",
      "GENERAL",
      "BACKGROUND",
      "X SOURCE Y",
      "X HIGHEST Y",
      "X MAX OZONE Y",
      "X GENERAL Y",
      "X BACKGROUND Y"
    )
  )
  expected_df <- data.frame(
    monitoring_objective = c(
      "Source/Highest/Max",
      "Source/Highest/Max",
      "Source/Highest/Max",
      "Other",
      "Other",
      "Source/Highest/Max",
      "Source/Highest/Max",
      "Source/Highest/Max",
      "Other",
      "Other"
    )
  )
  expect_equal(
    .simplify_monitoring_objective(df),
    expected_df
  )
})
