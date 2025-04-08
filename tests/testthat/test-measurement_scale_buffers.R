# Unit tests -----------------------------------------------------
test_that("Centroid URLs are correctly defined and active", {
  urls <- .define_centroid_urls()
  expect_true(httr::GET(urls$vintage_4)$status_code == 200)
  expect_true(httr::GET(urls$vintage_410)$status_code == 200)
})

test_that("Pop-weighted centroids are imported correctly", {
  urls <- .define_centroid_urls()
  centroids <- .import_pop_weighted_centroids(urls)
  expect_equal(
    colnames(centroids$vintage_4),
    c("STATEFP", "COUNTYFP", "TRACTCE", "BLKGRPCE", "POPULATION", "LATITUDE", "LONGITUDE")
  )
  expect_equal(
    colnames(centroids$vintage_410),
    c("STATEFP", "COUNTYFP", "TRACTCE", "BLKGRPCE", "POPULATION", "LATITUDE", "LONGITUDE")
  )
})

test_that("Centroid columns are cleaned as expected", {
  vintage_4 <- data.frame(
    STATEFP = c("01", "02"),
    COUNTYFP = c("001", "002"),
    TRACTCE = c("000100", "000200"),
    BLKGRPCE = c("1", "2"),
    LATITUDE = c(1.0, 2.0),
    LONGITUDE = c(1.0, 2.0)
  )
  vintage_410 <- data.frame(
    STATEFP = c("01", "02"),
    COUNTYFP = c("001", "002"),
    TRACTCE = c("000100", "000200"),
    BLKGRPCE = c("1", "2"),
    LATITUDE = c(1.0, 2.0),
    LONGITUDE = c(1.0, 2.0)
  )
  centroids <- list(
    vintage_4 = vintage_4,
    vintage_410 = vintage_410
  )
  expected_list <- list(
    vintage_4 = data.frame(
      bg = c("010010001001", "020020002002"),
      centroid_latitude = c(1.0, 2.0),
      centroid_longitude = c(1.0, 2.0)
    ),
    vintage_410 = data.frame(
      bg = c("010010001001", "020020002002"),
      centroid_latitude = c(1.0, 2.0),
      centroid_longitude = c(1.0, 2.0)
    )
  )
  expect_equal(
    .clean_centroid_columns(centroids),
    expected_list
  )
})

test_that(".get_pop_weighted_centroids() returns a list of data frames", {
  centroid_urls <- .define_centroid_urls()
  centroids <- .get_pop_weighted_centroids()
  expect_true(is.list(centroids))
  expect_true(all(sapply(centroids, is.data.frame)))
  expect_equal(
    names(centroids),
    c("vintage_4", "vintage_410")
  )
  expect_equal(
    colnames(centroids$vintage_4),
    c("bg", "centroid_latitude", "centroid_longitude")
  )
  expect_equal(
    colnames(centroids$vintage_410),
    c("bg", "centroid_latitude", "centroid_longitude")
  )
})

test_that(".get_aqs_site_id_crs_columns_by_vintage() returns the correct columns", {
  df_vintage_4 <- data.frame(
    aqs_site_id = c("site1", "site2"),
    latitude = c(1.0, 2.0),
    longitude = c(1.0, 2.0),
    other_column = c("a", "b")
  )
  df_vintage_410 <- data.frame(
    aqs_site_id = c("site3", "site4"),
    latitude = c(3.0, 4.0),
    longitude = c(3.0, 4.0),
    other_column = c("c", "d")
  )
  df_list <- list(
    vintage_4 = df_vintage_4,
    vintage_410 = df_vintage_410
  )
  expected_df_vintage_4 <- data.frame(
    aqs_site_id = c("site1", "site2"),
    latitude = c(1.0, 2.0),
    longitude = c(1.0, 2.0)
  )
  expected_df_vintage_410 <- data.frame(
    aqs_site_id = c("site3", "site4"),
    latitude = c(3.0, 4.0),
    longitude = c(3.0, 4.0)
  )
  expected_list <- list(
    vintage_4 = expected_df_vintage_4,
    vintage_410 = expected_df_vintage_410
  )
  expect_equal(
    .get_aqs_site_id_crs_columns_by_vintage(df_list),
    expected_list
  )
})

test_that(".get_combined_aqs_site_id_crs_coordinates_by_vintage() returns the correct data frames", {
  aqi_naaqs_v4 <- data.frame(
    aqs_site_id = c("site1", "site2", "site3", "site4", "site5", "site1"),
    latitude = c(1.0, 2.0, NA, 4.0, NA, 1.0),
    longitude = c(1.0, 2.0, 3.0, NA, NA, 1.0),
    other_column = c("a", "b", "c", "d", "e", "a")
  )
  aqi_naaqs_v410 <- data.frame(
    aqs_site_id = c("site6", "site7", "site8", "site9", "site10", "site6"),
    latitude = c(5.0, 6.0, NA, 8.0, NA, 5.0),
    longitude = c(5.0, 6.0, 7.0, NA, NA, 5.0),
    other_column = c("f", "g", "h", "i", "j", "f")
  )
  regulatory_v4 <- data.frame(
    aqs_site_id = c("site11", "site12", "site13", "site14", "site15", "site11", "site1"),
    latitude = c(9.0, 10.0, NA, 12.0, NA, 9.0, 1.0),
    longitude = c(9.0, 10.0, 11.0, NA, NA, 9.0, 1.0),
    other_column = c("k", "l", "m", "n", "o", "k", "a")
  )
  regulatory_v410 <- data.frame(
    aqs_site_id = c("site16", "site17", "site18", "site19", "site20", "site16", "site6"),
    latitude = c(13.0, 14.0, NA, 16.0, NA, 13.0, 5.0),
    longitude = c(13.0, 14.0, 15.0, NA, NA, 13.0, 5.0),
    other_column = c("p", "q", "r", "s", "t", "p", "f")
  )
  aqi_naaqs_monitors <- list(
    vintage_4 = aqi_naaqs_v4,
    vintage_410 = aqi_naaqs_v410
  )
  regulatory_monitors <- list(
    vintage_4 = regulatory_v4,
    vintage_410 = regulatory_v410
  )
  expected_v4 <- data.frame(
    aqs_site_id = c("site1", "site2", "site11", "site12"),
    latitude = c(1.0, 2.0, 9.0, 10.0),
    longitude = c(1.0, 2.0, 9.0, 10.0)
  )
  expected_v410 <- data.frame(
    aqs_site_id = c("site6", "site7", "site16", "site17"),
    latitude = c(5.0, 6.0, 13.0, 14.0),
    longitude = c(5.0, 6.0, 13.0, 14.0)
  )
  expected_list <- list(
    vintage_4 = expected_v4,
    vintage_410 = expected_v410
  )
  expect_equal(
    .get_combined_aqs_site_id_crs_coordinates_by_vintage(aqi_naaqs_monitors, regulatory_monitors),
    expected_list
  )
})

test_that(".define_crs_matrices returns a list of matrices with longitude first, latitude second", {
  set.seed(sample(1:10, 1)) # arbitrary seed for reproducibility
  site_longitude <- round(runif(1, min = -120, max = -70), 1)
  site_latitude <- round(runif(1, min = 30, max = 45), 1)
  centroids <- data.frame(
    centroid_latitude = round(runif(10, min = 30, max = 45), 1),
    centroid_longitude = round(runif(10, min = -120, max = -70), 1)
  )
  crs_matrices <- .define_crs_matrices(site_longitude, site_latitude, centroids)
  expect_true(is.list(crs_matrices))
  expect_true(all(sapply(crs_matrices, is.matrix)))
  expect_true(all(names(crs_matrices) == c("monitors", "centroids")))
  expect_true(all(colnames(crs_matrices$monitors) == c("longitude", "latitude")))
  expect_true(all(colnames(crs_matrices$centroids) == c("centroid_longitude", "centroid_latitude")))
  expect_equal(
    site_latitude,
    crs_matrices$monitors[1, 2]
  )
  expect_equal(
    site_longitude,
    crs_matrices$monitors[1, 1]
  )
  expect_equal(
    centroids$centroid_latitude,
    crs_matrices$centroids[, 2]
  )
  expect_equal(
    centroids$centroid_longitude,
    crs_matrices$centroids[, 1]
  )
})

test_that(".calculate_distance_to_centroids() returns correct vector of distances", {
  # Uses synthetic data generated in helper script
  calculated_distances <- .calculate_distance_to_centroids(
    site_longitude = test_centroids$centroid_longitude[1],
    site_latitude = test_centroids$centroid_latitude[1],
    centroids_df = test_centroids[1:25, ]
  )
  expect_equal(
    as.vector(calculated_distances),
    test_centroids$distance[1:25],
    tolerance = 0.005
  )
})

test_that(".define_measurement_scale_buffers correctly determines buffers", {
  measurement_scales <- .define_measurement_scale_buffers()
  expect_equal(
    measurement_scales$microscale,
    0.00
  )
  expect_equal(
    measurement_scales$middle_scale,
    0.10
  )
  expect_equal(
    measurement_scales$neighborhood,
    0.50
  )
  expect_equal(
    measurement_scales$urban_scale,
    4.00
  )
  expect_equal(
    measurement_scales$regional_scale,
    50.00
  )
})

test_that(".determine_block_groups_in_scale_buffers_for_single_site correctly determines block groups in buffer", {
  buffer_test_distances <- c(
    0.09, 0.1, 0.11, 0.05, 0.07, 0.49, 0.5, 0.51, 0.25, 0.35, 3.99, 4, 4.01, 2, 3, 49.99, 50, 50.01,
    25, 35, 75, 100, 200, 250, 300
  )
  buffer_test_bg <- as.character(1:25)
  buffer_test_centroids <- data.frame(
    bg = buffer_test_bg,
    distance = buffer_test_distances
  )
  expected_list <- list(
    microscale = as.character(),
    middle_scale = as.character(c(1, 2, 4, 5)),
    neighborhood = as.character(c(1:7, 9, 10)),
    urban_scale = as.character(c(1:12, 14, 15)),
    regional_scale = as.character(c(1:17, 19, 20))
  )
  expect_equal(
    .determine_block_groups_in_scale_buffers_for_single_site(
      distances = buffer_test_distances,
      centroids = buffer_test_centroids
    ),
    expected_list
  )
})

test_that("get_block_groups_in_measurement_scale_buffers() returns correct list", {
  # Uses synthetic data generated in helper script
  expected_list <- list(
    list(
      aqs_site_id = "100",
      microscale = as.character(),
      middle_scale = as.character(1:5),
      neighborhood = as.character(1:10),
      urban_scale = as.character(1:15),
      regional_scale = as.character(1:20)
    ),
    list(
      aqs_site_id = "200",
      microscale = as.character(),
      middle_scale = as.character(26:30),
      neighborhood = as.character(26:35),
      urban_scale = as.character(26:40),
      regional_scale = as.character(26:45)
    ),
    list(
      aqs_site_id = "300",
      microscale = as.character(),
      middle_scale = as.character(51:55),
      neighborhood = as.character(51:60),
      urban_scale = as.character(51:65),
      regional_scale = as.character(51:70)
    ),
    list(
      aqs_site_id = "400",
      microscale = as.character(),
      middle_scale = as.character(76:80),
      neighborhood = as.character(76:85),
      urban_scale = as.character(76:90),
      regional_scale = as.character(76:95)
    ),
    list(
      aqs_site_id = "500",
      microscale = as.character(),
      middle_scale = as.character(101:105),
      neighborhood = as.character(101:110),
      urban_scale = as.character(101:115),
      regional_scale = as.character(101:120)
    ),
    list(
      aqs_site_id = "600",
      microscale = as.character(),
      middle_scale = as.character(126:130),
      neighborhood = as.character(126:135),
      urban_scale = as.character(126:140),
      regional_scale = as.character(126:145)
    )
  )
  expected_list <- purrr::map(
    expected_list,
    function(x) {
      list(
        aqs_site_id = x$aqs_site_id,
        microscale = stringr::str_pad(x$microscale, 4, pad = "0", side = "left"),
        middle_scale = stringr::str_pad(x$middle_scale, 4, pad = "0", side = "left"),
        neighborhood = stringr::str_pad(x$neighborhood, 4, pad = "0", side = "left"),
        urban_scale = stringr::str_pad(x$urban_scale, 4, pad = "0", side = "left"),
        regional_scale = stringr::str_pad(x$regional_scale, 4, pad = "0", side = "left")
      )
    }
  )
  names(expected_list) <- c("site_100", "site_200", "site_300", "site_400", "site_500", "site_600")
  actual_list <- get_block_groups_in_measurement_scale_buffers(
    site_crs_df = .generate_test_site_coordinates(),
    centroids_df = .generate_block_group_test_coordinates()
  )
  expect_equal(
    actual_list,
    expected_list
  )
})

test_that(".generate_parameter_year_grid returns complete parameter-year grid", {
  df <- data.frame(
    aqs_site_id = 1:20,
    parameter_name = rep(c("PM2.5", "Nitrogen dioxide (NO2)", "Ozone", "PM10", "Lead"), each = 4),
    year = rep(2022:2023, times = 10),
    monitoring_objective = rep(c("Source/Highest/Max", "Other"), times = 10)
  )
  expected_df <- data.frame(
    parameter_name = rep(c("PM2.5", "Nitrogen dioxide (NO2)", "Ozone", "PM10", "Lead"), each = 2),
    year = rep(2022:2023, times = 5)
  ) |>
    dplyr::arrange(parameter_name, year)
  expect_equal(
    .generate_parameter_year_grid(df),
    expected_df
  )
})

test_that(".generate_combined_parameter_abbreviation_year_string returns correct combined string", {
  parameters <- rep(c("Carbon monoxide", "Lead", "Nitrogen dioxide (NO2)", "Ozone", "PM10", "PM2.5", "Sulfur dioxide"), times = 2)
  years <- rep(2022:2023, each = 7)
  expected_results <- c(
    "co_2022", "lead_2022", "no2_2022", "o3_2022", "pm10_2022", "pm25_2022", "so2_2022",
    "co_2023", "lead_2023", "no2_2023", "o3_2023", "pm10_2023", "pm25_2023", "so2_2023"
  )
  purrr::pmap(
    list(
      parameter = parameters,
      year = years,
      expected_result = expected_results
    ),
    function(parameter, year, expected_result) {
      expect_equal(
        .generate_combined_parameter_abbreviation_year_string(parameter, year),
        expected_result
      )
    }
  )
})

test_that(".get_unique_site_scale_combinations_by_objective returns all unique site and scale combinations
  for a given parameter and year, split into a list of data frames by monitoring objective", {
  df <- data.frame(
    aqs_site_id = (rep(1:10, times = 3)),
    parameter_name = rep(c("PM2.5", "Nitrogen dioxide (NO2)", "PM10"), each = 10),
    measurement_scale = rep(c("MICROSCALE", "MIDDLE SCALE", "NEIGHBORHOOD", "URBAN SCALE", "REGIONAL SCALE"), times = 6),
    monitoring_objective = rep(c("Source/Highest/Max", "Other", "Other"), times = 10),
    year = rep(2022:2023, times = 15)
  )
  expected_pm25_2022 <- list(
    source = data.frame(
      aqs_site_id = c(1, 7),
      measurement_scale = c("MICROSCALE", "MIDDLE SCALE")
    ),
    other = data.frame(
      aqs_site_id = c(3, 5, 9),
      measurement_scale = c("NEIGHBORHOOD", "REGIONAL SCALE", "URBAN SCALE")
    )
  )
  expected_pm25_2023 <- list(
    source = data.frame(
      aqs_site_id = c(4, 10),
      measurement_scale = c("URBAN SCALE", "REGIONAL SCALE")
    ),
    other = data.frame(
      aqs_site_id = c(2, 6, 8),
      measurement_scale = c("MIDDLE SCALE", "MICROSCALE", "NEIGHBORHOOD")
    )
  )
  expected_no2_2022 <- list(
    source = data.frame(
      aqs_site_id = c(3, 9),
      measurement_scale = c("NEIGHBORHOOD", "URBAN SCALE")
    ),
    other = data.frame(
      aqs_site_id = c(1, 5, 7),
      measurement_scale = c("MICROSCALE", "REGIONAL SCALE", "MIDDLE SCALE")
    )
  )
  expected_no2_2023 <- list(
    source = data.frame(
      aqs_site_id = 6,
      measurement_scale = "MICROSCALE"
    ),
    other = data.frame(
      aqs_site_id = c(2, 4, 8, 10),
      measurement_scale = c("MIDDLE SCALE", "URBAN SCALE", "NEIGHBORHOOD", "REGIONAL SCALE")
    )
  )
  expected_pm10_2022 <- list(
    source = data.frame(
      aqs_site_id = 5,
      measurement_scale = "REGIONAL SCALE"
    ),
    other = data.frame(
      aqs_site_id = c(1, 3, 7, 9),
      measurement_scale = c("MICROSCALE", "NEIGHBORHOOD", "MIDDLE SCALE", "URBAN SCALE")
    )
  )
  expected_pm10_2023 <- list(
    source = data.frame(
      aqs_site_id = c(2, 8),
      measurement_scale = c("MIDDLE SCALE", "NEIGHBORHOOD")
    ),
    other = data.frame(
      aqs_site_id = c(4, 6, 10),
      measurement_scale = c("URBAN SCALE", "MICROSCALE", "REGIONAL SCALE")
    )
  )
  expect_equal(
    .get_unique_site_scale_combinations_by_objective(df, "PM2.5", 2022),
    expected_pm25_2022
  )
  expect_equal(
    .get_unique_site_scale_combinations_by_objective(df, "PM2.5", 2023),
    expected_pm25_2023
  )
  expect_equal(
    .get_unique_site_scale_combinations_by_objective(df, "Nitrogen dioxide (NO2)", 2022),
    expected_no2_2022
  )
  expect_equal(
    .get_unique_site_scale_combinations_by_objective(df, "Nitrogen dioxide (NO2)", 2023),
    expected_no2_2023
  )
  expect_equal(
    .get_unique_site_scale_combinations_by_objective(df, "PM10", 2022),
    expected_pm10_2022
  )
  expect_equal(
    .get_unique_site_scale_combinations_by_objective(df, "PM10", 2023),
    expected_pm10_2023
  )
})


test_that(".determine_block_groups with_monitors_individual returns correct block groups sorted and unique", {
  # Uses objects from helper file
  test_block_groups_with_monitor_df <- .generate_synthetic_block_groups_with_monitor_df()
  test_parameter_year_grid <- .generate_parameter_year_grid(
    test_block_groups_with_monitor_df
  )
  purrr::map2(
    test_parameter_year_grid$parameter_name,
    test_parameter_year_grid$year,
    function(parameter, year) {
      expect_equal(
        .determine_block_groups_with_monitors_individual(
          test_block_groups_with_monitor_df,
          parameter,
          year,
          "Source/Highest/Max"
        ),
        .get_synthetic_block_groups_with_source_monitors(
          test_block_groups_with_monitor_df,
          parameter,
          year
        )
      )
      expect_equal(
        .determine_block_groups_with_monitors_individual(
          test_block_groups_with_monitor_df,
          parameter,
          year,
          "Other"
        ),
        .get_synthetic_block_groups_with_other_monitors(
          test_block_groups_with_monitor_df,
          parameter,
          year
        )
      )
    }
  )
})

test_that("Block groups in source locations/buffers are removed from other locations/buffers when duplicated", {
  pm25_source <- sprintf("%04d", 1:50)
  pm25_other <- sprintf("%04d", 1:100)
  expect_equal(
    .remove_source_objective_bg_from_other_objective(
      other_bg_ids = pm25_other,
      source_bg_ids = pm25_source
    ),
    sprintf("%04d", 51:100)
  )
})

test_that(".remove_source_objective_bg_from_other_objective() correctly removes shared block groups", {
  source_bg_ids <- letters[1:10]
  other_bg_ids <- letters[5:15]
  expected_result <- letters[11:15]
  expect_equal(
    .remove_source_objective_bg_from_other_objective(
      other_bg_ids = other_bg_ids,
      source_bg_ids = source_bg_ids
    ),
    expected_result
  )
})

test_that(".determine_block_groups_with_monitors returns the expected list of block groups", {
  # Uses objects from helper file
  expect_equal(
    .determine_block_groups_with_monitors(
      monitors = test_block_groups_with_monitor
    ),
    .generate_synthetic_block_groups_with_monitor_list()
  )
})

test_that("Measurement scales are correctly transformed", {
  # Test all expected responses, non-standard response, empty string, NA
  expect_equal(
    .transform_measurement_scale("MICROSCALE"),
    "microscale"
  )
  expect_equal(
    .transform_measurement_scale("MIDDLE SCALE"),
    "middle_scale"
  )
  expect_equal(
    .transform_measurement_scale("NEIGHBORHOOD"),
    "neighborhood"
  )
  expect_equal(
    .transform_measurement_scale("URBAN SCALE"),
    "urban_scale"
  )
  expect_equal(
    .transform_measurement_scale("REGIONAL SCALE"),
    "regional_scale"
  )
  expect_equal(
    .transform_measurement_scale("OTHER"),
    "microscale"
  )
  expect_equal(
    .transform_measurement_scale(""),
    "microscale"
  )
  expect_equal(
    .transform_measurement_scale(NA),
    "microscale"
  )
})

test_that(".determine_block_groups_in_monitor_buffer_individual() returns correct block groups", {
  synthetic_monitor_buffer_list <- get_block_groups_in_measurement_scale_buffers(
    site_crs_df = .generate_test_site_coordinates(),
    centroids_df = .generate_block_group_test_coordinates()
  )
  sites <- c("100", "200", "300", "400", "500", "600")
  measurement_scales <- c(
    "MICROSCALE",
    "MIDDLE SCALE",
    "NEIGHBORHOOD",
    "URBAN SCALE",
    "REGIONAL SCALE"
  )
  purrr::map(sites, function(site) {
    purrr::map(measurement_scales, function(scale) {
      transformed_scale <- .transform_measurement_scale(scale)
      expect_equal(
        .determine_block_groups_in_monitor_buffer_individual(
          monitor_buffer_list = synthetic_monitor_buffer_list,
          aqs_site_id = site,
          measurement_scale = scale
        ),
        synthetic_monitor_buffer_list[[stringr::str_c("site_", site)]][[transformed_scale]]
      )
    })
  })
})

test_that(".determine_block_groups_in_monitor_buffer() returns correct block groups", {
  expect_equal(
    .determine_block_groups_in_monitor_buffer(
      monitors = synthetic_monitor_df,
      monitor_buffer_list = synthetic_monitor_buffer_list
    ),
    .generate_synthetic_block_groups_in_buffer()
  )
})

test_that(".combine_block_groups_with_monitor_and_in_buffer() combines results as expected", {
  test_block_groups_with_monitor <- .generate_synthetic_block_groups_with_monitor_list()
  test_block_groups_in_buffer <- .generate_synthetic_block_groups_in_buffer()
  expected_result <- list(
    no2_2022 = list(
      source = list(
        monitor_location = test_block_groups_with_monitor$no2_2022$source,
        monitor_buffer = test_block_groups_in_buffer$no2_2022$source
      ),
      other = list(
        monitor_location = test_block_groups_with_monitor$no2_2022$other,
        monitor_buffer = test_block_groups_in_buffer$no2_2022$other
      )
    ),
    no2_2023 = list(
      source = list(
        monitor_location = test_block_groups_with_monitor$no2_2023$source,
        monitor_buffer = test_block_groups_in_buffer$no2_2023$source
      ),
      other = list(
        monitor_location = test_block_groups_with_monitor$no2_2023$other,
        monitor_buffer = test_block_groups_in_buffer$no2_2023$other
      )
    ),
    pm10_2022 = list(
      source = list(
        monitor_location = test_block_groups_with_monitor$pm10_2022$source,
        monitor_buffer = test_block_groups_in_buffer$pm10_2022$source
      ),
      other = list(
        monitor_location = test_block_groups_with_monitor$pm10_2022$other,
        monitor_buffer = test_block_groups_in_buffer$pm10_2022$other
      )
    ),
    pm10_2023 = list(
      source = list(
        monitor_location = test_block_groups_with_monitor$pm10_2023$source,
        monitor_buffer = test_block_groups_in_buffer$pm10_2023$source
      ),
      other = list(
        monitor_location = test_block_groups_with_monitor$pm10_2023$other,
        monitor_buffer = test_block_groups_in_buffer$pm10_2023$other
      )
    ),
    pm25_2022 = list(
      source = list(
        monitor_location = test_block_groups_with_monitor$pm25_2022$source,
        monitor_buffer = test_block_groups_in_buffer$pm25_2022$source
      ),
      other = list(
        monitor_location = test_block_groups_with_monitor$pm25_2022$other,
        monitor_buffer = test_block_groups_in_buffer$pm25_2022$other
      )
    ),
    pm25_2023 = list(
      source = list(
        monitor_location = test_block_groups_with_monitor$pm25_2023$source,
        monitor_buffer = test_block_groups_in_buffer$pm25_2023$source
      ),
      other = list(
        monitor_location = test_block_groups_with_monitor$pm25_2023$other,
        monitor_buffer = test_block_groups_in_buffer$pm25_2023$other
      )
    )
  )
  expect_equal(
    .combine_block_groups_with_monitor_and_in_buffer(
      block_groups_with_monitor = test_block_groups_with_monitor,
      block_groups_in_buffer = test_block_groups_in_buffer
    ),
    expected_result
  )
})

test_that("master function to determine monitor status for all block groups returns correct output", {
  test_monitors <- data.frame(
    parameter_name = rep(c("PM2.5", "Nitrogen dioxide (NO2)"), each = 8),
    year = rep(2022:2023, each = 4, times = 2),
    aqs_site_id = c(
      c("100", "200", "100", "300"),
      c("300", "300", "400", "500"),
      c("600", "600", "400", "300"),
      c("200", "100", "200", "300")
    ),
    bg_id = c(
      c("1100", "1200", "1100", "1300"),
      c("1300", "1300", "1400", "1500"),
      c("1600", "1600", "1400", "1300"),
      c("1200", "1100", "1200", "1300")
    ),
    latitude = c(
      c(35, 45, 35, 55),
      c(55, 55, 65, 75),
      c(85, 85, 65, 55),
      c(45, 35, 45, 55)
    ),
    longitude = c(
      c(-87, -97, -87, -107),
      c(-107, -107, -117, -127),
      c(-137, -137, -117, -107),
      c(-97, -87, -97, -107)
    ),
    monitoring_objective = rep(c("Source/Highest/Max", "Other"), each = 2, times = 4),
    measurement_scale = c(
      c("MIDDLE SCALE", "REGIONAL SCALE", "URBAN SCALE", "NEIGHBORHOOD"),
      c("NEIGHBORHOOD", "URBAN SCALE", "MICROSCALE", "MICROSCALE"),
      c("REGIONAL SCALE", "URBAN SCALE", "NEIGHBORHOOD", "MIDDLE SCALE"),
      c("URBAN SCALE", "MIDDLE SCALE", "URBAN SCALE", "NEIGHBORHOOD")
    )
  )

  test_master_list_of_block_groups_in_buffers <- get_block_groups_in_measurement_scale_buffers(
    site_crs_df = test_monitors,
    centroids_df = .generate_block_group_test_coordinates()
  )

  pm25_2022_source <- c(
    test_master_list_of_block_groups_in_buffers$site_100$middle_scale,
    test_master_list_of_block_groups_in_buffers$site_200$regional_scale
  ) |>
    unique()
  pm25_2022_other <- c(
    test_master_list_of_block_groups_in_buffers$site_100$urban_scale,
    test_master_list_of_block_groups_in_buffers$site_300$neighborhood
  ) |>
    unique() |>
    setdiff(pm25_2022_source)
  pm25_2023_source <- c(
    test_master_list_of_block_groups_in_buffers$site_300$neighborhood,
    test_master_list_of_block_groups_in_buffers$site_300$urban_scale
  ) |>
    unique()
  pm25_2023_other <- c(
    test_master_list_of_block_groups_in_buffers$site_400$microscale,
    test_master_list_of_block_groups_in_buffers$site_500$microscale
  ) |>
    unique() |>
    setdiff(pm25_2023_source)

  no2_2022_source <- c(
    test_master_list_of_block_groups_in_buffers$site_600$regional_scale,
    test_master_list_of_block_groups_in_buffers$site_600$urban_scale
  ) |>
    unique()
  no2_2022_other <- c(
    test_master_list_of_block_groups_in_buffers$site_400$neighborhood,
    test_master_list_of_block_groups_in_buffers$site_300$middle_scale
  ) |>
    unique() |>
    setdiff(no2_2022_source)

  no2_2023_source <- c(
    test_master_list_of_block_groups_in_buffers$site_200$urban_scale,
    test_master_list_of_block_groups_in_buffers$site_100$middle_scale
  ) |>
    unique()
  no2_2023_other <- c(
    test_master_list_of_block_groups_in_buffers$site_200$urban_scale,
    test_master_list_of_block_groups_in_buffers$site_300$neighborhood
  ) |>
    unique() |>
    setdiff(no2_2023_source)

  expected_output <- list(
    no2_2022 = list(
      source = list(
        monitor_location = "1600",
        monitor_buffer = no2_2022_source
      ),
      other = list(
        monitor_location = c("1300", "1400"),
        monitor_buffer = no2_2022_other
      )
    ),
    no2_2023 = list(
      source = list(
        monitor_location = c("1100", "1200"),
        monitor_buffer = no2_2023_source
      ),
      other = list(
        monitor_location = "1300",
        monitor_buffer = no2_2023_other
      )
    ),
    pm25_2022 = list(
      source = list(
        monitor_location = c("1100", "1200"),
        monitor_buffer = pm25_2022_source
      ),
      other = list(
        monitor_location = "1300",
        monitor_buffer = pm25_2022_other
      )
    ),
    pm25_2023 = list(
      source = list(
        monitor_location = "1300",
        monitor_buffer = pm25_2023_source
      ),
      other = list(
        monitor_location = c("1400", "1500"),
        monitor_buffer = pm25_2023_other
      )
    )
  )
  expect_equal(
    determine_block_groups_monitor_status(
      monitors = test_monitors,
      master_list_of_block_groups_in_buffers = test_master_list_of_block_groups_in_buffers
    ),
    expected_output
  )
})

# Integration tests ---------------------------------------------------
test_that("No aqs_site_id is represented twice in the same vintage with real data", {

})
