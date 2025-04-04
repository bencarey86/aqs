test_that("CRS columns are selected correctly", {
  monitors <- list(
    vintage_4 = tibble::tibble(
      latitude = c(1, 2, NA, 4, 2, 3, NA, 1),
      longitude = c(1, 2, 3, 4, 2, NA, NA, 1),
      aqs_site_id = c("010010001", "020020002", "040040004", "050050005", "060060006", "070070007", "080080008", "090090009"),
      poc = c(1, 2, 4, 5, 6, 7, 8, 9)
    ),
    vintage_410 = tibble::tibble(
      latitude = c(1, 2, NA, 4, 2, 3, NA, 1),
      longitude = c(1, 2, 3, 4, 2, NA, NA, 1),
      aqs_site_id = c("010010001", "020020002", "040040004", "050050005", "060060006", "070070007", "080080008", "090090009"),
      poc = c(1, 2, 4, 5, 6, 7, 8, 9)
    )
  )
  expected_list <- list(
    vintage_4 = tibble::tibble(
      latitude = c(1, 2, NA, 4, 2, 3, NA, 1),
      longitude = c(1, 2, 3, 4, 2, NA, NA, 1)
    ),
    vintage_410 = tibble::tibble(
      latitude = c(1, 2, NA, 4, 2, 3, NA, 1),
      longitude = c(1, 2, 3, 4, 2, NA, NA, 1)
    )
  )
  expect_equal(
    .get_crs_columns(monitors),
    expected_list
  )
})

test_that("CRS coordinates are combined and filtered correctly", {
  aqi_naaqs_monitors <- list(
    vintage_4 = tibble::tibble(
      latitude = c(1, 2, NA, 4, 2, 3, NA, 1),
      longitude = c(1, 2, 3, 4, 2, NA, NA, 1),
      aqs_site_id = c("010010001", "020020002", "040040004", "050050005", "060060006", "070070007", "080080008", "090090009"),
    ),
    vintage_410 = tibble::tibble(
      latitude = c(3, 4, NA, 6, 4, 5, NA, 3),
      longitude = c(3, 4, 5, 6, 4, NA, NA, 3),
      aqs_site_id = c("010010001", "020020002", "040040004", "050050005", "060060006", "070070007", "080080008", "090090009"),
    )
  )
  regulatory_monitors <- list(
    vintage_4 = tibble::tibble(
      latitude = c(1, 2, NA, 4, 2, 3, NA, 1, 5, 6),
      longitude = c(1, 2, 3, 4, 2, NA, NA, 1, 5, 6),
      aqs_site_id = c("010010001", "020020002", "040040004", "050050005", "060060006", "070070007", "080080008", "090090009", "100100010", "110110011"),
    ),
    vintage_410 = tibble::tibble(
      latitude = c(3, 4, NA, 6, 4, 5, NA, 3, 7, 8),
      longitude = c(3, 4, 5, 6, 4, NA, NA, 3, 7, 8),
      aqs_site_id = c("010010001", "020020002", "040040004", "050050005", "060060006", "070070007", "080080008", "090090009", "100100010", "110110011"),
    )
  )
  expected_list <- list(
    vintage_4 = tibble::tibble(
      latitude = c(1, 2, 4, 5, 6),
      longitude = c(1, 2, 4, 5, 6)
    ),
    vintage_410 = tibble::tibble(
      latitude = c(3, 4, 6, 7, 8),
      longitude = c(3, 4, 6, 7, 8)
    )
  )
  expect_equal(
    .get_combined_crs_coordinates_by_vintage(aqi_naaqs_monitors, regulatory_monitors),
    expected_list
  )
})

test_that("geocoder url is active", {
  url_vintage_4 <- .define_geocoder_api_call(
    longitude = coordinates$longitude[1],
    latitude = coordinates$latitude[1],
    vintage = 4
  )
  url_vintage_410 <- .define_geocoder_api_call(
    longitude = coordinates$longitude[1],
    latitude = coordinates$latitude[1],
    vintage = 410
  )
  expect_true(httr::GET(url_vintage_4)$status_code == 200)
  expect_true(httr::GET(url_vintage_410)$status_code == 200)
})

test_that("API call returns expected results and that master function correctly implements helper functions", {
  # Vintage 4
  api_call_vintage_4 <- .define_geocoder_api_call(
    longitude = coordinates$longitude[1],
    latitude = coordinates$latitude[1],
    vintage = 4
  )
  api_result_vintage_4 <- .call_geocoder_api(api_call_vintage_4) |>
    .get_api_result(vintage = 4)
  expect_true(stringr::str_detect(api_result_vintage_4, "^[0-9]{12}$"))
  expect_equal(
    api_result_vintage_4,
    .get_bg_id_from_geocoder(
      longitude = coordinates$longitude[1],
      latitude = coordinates$latitude[1],
      vintage = 4
    )
  )

  # Vintage 410
  api_call_vintage_410 <- .define_geocoder_api_call(
    longitude = coordinates$longitude[1],
    latitude = coordinates$latitude[1],
    vintage = 410
  )
  api_result_vintage_410 <- .call_geocoder_api(api_call_vintage_410) |>
    .get_api_result(vintage = 410)
  expect_true(stringr::str_detect(api_result_vintage_410, "^[0-9]{12}$"))
  expect_equal(
    api_result_vintage_410,
    .get_bg_id_from_geocoder(
      longitude = coordinates$longitude[1],
      latitude = coordinates$latitude[1],
      vintage = 410
    )
  )
})

test_that("DF geocoder function returns correct results", {
  # Vintage 4
  actual_v4 <- .get_geocoded_bg_id_individual_df(
    df = coordinates,
    vintage = 4
  )
  expected_v4 <- sf::st_join(coordinate_points, al_sf_v4)
  expect_equal(
    actual_v4$bg_id,
    expected_v4$GEOID
  )
  # Vintage 410
  actual_v410 <- .get_geocoded_bg_id_individual_df(
    df = coordinates,
    vintage = 410
  )
  expected_v410 <- sf::st_join(coordinate_points, al_sf_v410)
  expect_equal(
    actual_v410$bg_id,
    expected_v410$GEOID10
  )
})

test_that("DF geocoder function handles failures gracefully", {
  # Pick CRS coordinates in Asia
  asia_coords <- tibble::tibble(
    latitude = c(35.6895, 35.6895),
    longitude = c(139.6917, 139.6917)
  )
  actual_results <- suppressWarnings(.get_geocoded_bg_id_individual_df(
    df = asia_coords,
    vintage = 4
  ))
  expect_true(all(is.na(actual_results$bg_id)))
})

test_that("address_parallel_geocoding_failure correctly updates rows with NA values and does not change other rows", {
  expected_result <- .get_geocoded_bg_id_individual_df(
    df = coordinates,
    vintage = 4
  )
  test_df <- expected_result[1:5, ] |>
    dplyr::mutate(bg_id = NA_character_) |>
    dplyr::bind_rows(expected_result[6:10, ])
  actual_result <- .address_parallel_geocoding_failures(test_df, vintage = 4)
  expect_equal(
    actual_result,
    expected_result
  )
})

test_that("Master list geocoder function returns correct results", {
  actual <- .get_geocoded_bg_id(coordinate_list)
  expected <- list(
    vintage_4 = sf::st_join(coordinate_points[1:5, ], al_sf_v4),
    vintage_410 = sf::st_join(coordinate_points[6:10, ], al_sf_v410)
  )
  expect_equal(
    actual$vintage_4$bg_id,
    expected$vintage_4$GEOID
  )
  expect_equal(
    actual$vintage_410$bg_id,
    expected$vintage_410$GEOID10
  )
})

test_that("Master list geocoder handles failures gracefully", {
  # Pick CRS coordinates in Asia
  asia_coords <- tibble::tibble(
    latitude = c(35.6895, 36.6895),
    longitude = c(139.6917, 138.6917)
  )
  asia_list <- list(
    vintage_4 = asia_coords[1, ],
    vintage_410 = asia_coords[2, ]
  )
  actual_results <- suppressWarnings(.get_geocoded_bg_id(asia_list))
  expect_true(all(is.na(actual_results$vintage_4$bg_id)))
  expect_true(all(is.na(actual_results$vintage_410$bg_id)))
})
