test_that("NAAQS parameters are correctly filtered for", {
  df <- tibble::tibble(
    parameter_code = c(42101, 14129, 85129, 42602, 44201, 81102, 88101, 42401, 88502, 99999)
  )
  expect_equal(
    sort(.filter_regulatory_parameters(df)$parameter_code),
    c(14129, 42101, 42401, 42602, 44201, 81102, 85129, 88101)
  )
  expect_equal(
    sort(.filter_aqi_naaqs_parameters(df)$parameter_code),
    c(14129, 42101, 42401, 42602, 44201, 81102, 85129, 88101, 88502)
  )
})

test_that("DFs are correctly split by vintage", {
  df <- tibble::tibble(year = 2010:2029)
  filtered <- .split_vintages(df)
  expect_equal(
    filtered$vintage_4$year,
    2020:2029
  )
  expect_equal(
    filtered$vintage_410$year,
    2010:2019
  )
})

test_that("Block group ID join does not unexpectedly add or drop observations", {
  monitor_list <- list(
    vintage_4 = data.frame(
      aqs_site_id = c("010010001", "020020002", "040040004"),
      poc = c(1, 2, 4),
      parameter_name = c("Ozone", "PM2.5", "Ozone"),
      latitude = c(30.0, 31.0, 33.0),
      longitude = c(-85.0, -86.0, -88.0)
    ),
    vintage_410 = data.frame(
      aqs_site_id = c("030030003", "050050005", "060060006"),
      poc = c(3, 5, 6),
      parameter_name = c("PM10", "PM10", "PM10"),
      latitude = c(32.0, 34.0, 35.0),
      longitude = c(-87.0, -89.0, -90.0)
    )
  )
  geocoded_bg_id_list <- list(
    vintage_4 = data.frame(
      latitude = c(30.0, 31.0, 32.0),
      longitude = c(-85.0, -86.0, -87.0),
      bg_id = c("bg1", "bg2", "bg3")
    ),
    vintage_410 = data.frame(
      latitude = c(32.0, 34.0, 36.0),
      longitude = c(-87.0, -89.0, -91.0),
      bg_id = c("bg3", "bg4", "bg5")
    )
  )
  expected_list <- list(
    vintage_4 = data.frame(
      aqs_site_id = c("010010001", "020020002", "040040004"),
      poc = c(1, 2, 4),
      parameter_name = c("Ozone", "PM2.5", "Ozone"),
      latitude = c(30.0, 31.0, 33.0),
      longitude = c(-85.0, -86.0, -88.0),
      bg_id = c("bg1", "bg2", NA)
    ),
    vintage_410 = data.frame(
      aqs_site_id = c("030030003", "050050005", "060060006"),
      poc = c(3, 5, 6),
      parameter_name = c("PM10", "PM10", "PM10"),
      latitude = c(32.0, 34.0, 35.0),
      longitude = c(-87.0, -89.0, -90.0),
      bg_id = c("bg3", "bg4", NA)
    )
  )
  expect_equal(
    .join_geocoded_bg_id(monitor_list, geocoded_bg_id_list),
    expected_list
  )
})
