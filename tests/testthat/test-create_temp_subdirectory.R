test_that("Temporary subdirectory is created as expected", {
  temp_dir <- .create_temp_subdirectory(name = "sequence_temp_directory")
  expect_true(file.exists(temp_dir))
})
