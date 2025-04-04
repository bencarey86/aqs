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

test_that(".get_aqs_site_id_crs_columns() returns the correct columns", {
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
    .get_aqs_site_id_crs_columns(df_list),
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

test_that(".define_crs_matrices() returns a list of matrices with longitude first, latitude second", {
  set.seed(sample(1:10, 1)) # arbitrary seed for reproducibility
  monitors <- data.frame(
    latitude = round(runif(10, min = 30, max = 45), 1),
    longitude = round(runif(10, min = -120, max = -70), 1)
  )
  centroids <- data.frame(
    centroid_latitude = round(runif(10, min = 30, max = 45), 1),
    centroid_longitude = round(runif(10, min = -120, max = -70), 1)
  )
  crs_matrices <- .define_crs_matrices(monitors, centroids)
  expect_true(is.list(crs_matrices))
  expect_true(all(sapply(crs_matrices, is.matrix)))
  expect_true(all(names(crs_matrices) == c("monitors", "centroids")))
  expect_true(all(colnames(crs_matrices$monitors) == c("longitude", "latitude")))
  expect_true(all(colnames(crs_matrices$centroids) == c("centroid_longitude", "centroid_latitude")))
  expect_equal(
    monitors$latitude,
    crs_matrices$monitors[, 2]
  )
  expect_equal(
    monitors$longitude,
    crs_matrices$monitors[, 1]
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

test_that(".define_crs_matrices_single_site returns a list of matrices with longitude first, latitude second", {
  set.seed(sample(1:10, 1)) # arbitrary seed for reproducibility
  site_longitude <- round(runif(1, min = -120, max = -70), 1)
  site_latitude <- round(runif(1, min = 30, max = 45), 1)
  centroids <- data.frame(
    centroid_latitude = round(runif(10, min = 30, max = 45), 1),
    centroid_longitude = round(runif(10, min = -120, max = -70), 1)
  )
  crs_matrices <- .define_crs_matrices_single_site(site_longitude, site_latitude, centroids)
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

test_that(".calculate_distance_single_site_to_centroids() returns correct vector of distances", {
  # Uses synthetic data generated in helper script
  calculated_distances <- .calculate_distance_single_site_to_centroids(
    site_longitude = test_monitor_crs[1],
    site_latitude = test_monitor_crs[2],
    centroids_df = test_points_complete
  )
  expect_equal(
    as.vector(calculated_distances),
    test_points_complete$distance,
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

test_that(".determine_block_groups_in_scale_buffers correctly determines block groups in buffer", {
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
    .determine_block_groups_in_scale_buffers(
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
  names(expected_list) <- c("site_100", "site_200", "site_300", "site_400", "site_500", "site_600")
  calculated_list <- get_block_groups_in_measurement_scale_buffers(
    site_crs_df = site_crs_test,
    centroids_df = test_centroids_df
  )
  expect_equal(
    calculated_list,
    expected_list
  )
})

test_that(".determine_monitor_location() returns correct monitor location", {
  # Same block group in same pollutant-year-objective combination
  # Same block group shared by both objectives within same pollutant-year combination
  #
  df <- data.frame(
    bg_id = rep(sprintf("%04d", 1:100), each = 8),
    parameter_name = rep(c("PM2.5", "Nitrogen dioxide (NO2)"), each = 400),
    year = rep(2022:2023, each = 4, times = 100),
    monitoring_objective = rep(c("Source/Highest/Max", "Other"), each = 2, times = 200)
  )
  expect_equal(
    .determine_monitor_location(df, "PM2.5", 2022, "Source/Highest/Max"),
    sprintf("%04d", 1:50)
  )
  expect_equal(
    .determine_monitor_location(df, "PM2.5", 2022, "Other"),
    sprintf("%04d", 1:50)
  )
  expect_equal(
    .determine_monitor_location(df, "PM2.5", 2023, "Source/Highest/Max"),
    sprintf("%04d", 1:50)
  )
  expect_equal(
    .determine_monitor_location(df, "PM2.5", 2023, "Other"),
    sprintf("%04d", 1:50)
  )
  expect_equal(
    .determine_monitor_location(df, "Nitrogen dioxide (NO2)", 2022, "Source/Highest/Max"),
    sprintf("%04d", 51:100)
  )
  expect_equal(
    .determine_monitor_location(df, "Nitrogen dioxide (NO2)", 2022, "Other"),
    sprintf("%04d", 51:100)
  )
  expect_equal(
    .determine_monitor_location(df, "Nitrogen dioxide (NO2)", 2023, "Source/Highest/Max"),
    sprintf("%04d", 51:100)
  )
  expect_equal(
    .determine_monitor_location(df, "Nitrogen dioxide (NO2)", 2023, "Other"),
    sprintf("%04d", 51:100)
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

test_that("get_block_group_monitoring_status returns correct output", {
  monitors <- data.frame(
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

  pm25_2022_source <- c(
    calculated_list$site_100$middle_scale,
    calculated_list$site_200$regional_scale
  ) |>
    unique()
  pm25_2022_other <- c(
    calculated_list$site_100$urban_scale,
    calculated_list$site_300$neighborhood
  ) |>
    unique() |>
    setdiff(pm25_2022_source)
  pm25_2023_source <- c(
    calculated_list$site_300$neighborhood,
    calculated_list$site_300$urban_scale
  ) |>
    unique()
  pm25_2023_other <- c(
    calculated_list$site_400$microscale,
    calculated_list$site_500$microscale
  ) |>
    unique() |>
    setdiff(pm25_2023_source)

  no2_2022_source <- c(
    calculated_list$site_600$regional_scale,
    calculated_list$site_600$urban_scale
  ) |>
    unique()
  no2_2022_other <- c(
    calculated_list$site_400$neighborhood,
    calculated_list$site_300$middle_scale
  ) |>
    unique() |>
    setdiff(no2_2022_source)

  no2_2023_source <- c(
    calculated_list$site_200$urban_scale,
    calculated_list$site_100$middle_scale
  ) |>
    unique()
  no2_2023_other <- c(
    calculated_list$site_200$urban_scale,
    calculated_list$site_300$neighborhood
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
    determine_block_group_monitor_status(
      monitors = monitors,
      bg_id_buffers = calculated_list
    ),
    expected_output
  )
})
sublist_name <- "pm25_2022"
expected_output[[sublist_name]][["bg_id"]]
calculated_list
expected_output


test_that(".calculate_distances() returns a correct matrix of distances", {
  set.seed(sample(1:10, 1)) # arbitrary seed for reproducibility
  monitors <- data.frame(
    latitude = round(runif(10, min = 30, max = 45), 1),
    longitude = round(runif(10, min = -120, max = -70), 1)
  )
  centroids <- data.frame(
    centroid_latitude = round(runif(10, min = 30, max = 45), 1),
    centroid_longitude = round(runif(10, min = -120, max = -70), 1)
  )
  crs_matrices <- .define_crs_matrices(monitors, centroids)
  distances <- .calculate_distances(crs_matrices)
  expect_true(is.matrix(distances))
  expect_equal(nrow(distances), nrow(crs_matrices$monitors))
  expect_equal(ncol(distances), nrow(crs_matrices$centroids))
})

test_that("Measurement scales are correctly defined", {
  expect_equal(.define_measurement_scale_buffers("MICROSCALE"), 0.00)
  expect_equal(.define_measurement_scale_buffers("MIDDLE SCALE"), 0.10)
  expect_equal(.define_measurement_scale_buffers("NEIGHBORHOOD"), 0.50)
  expect_equal(.define_measurement_scale_buffers("URBAN SCALE"), 4.00)
  expect_equal(.define_measurement_scale_buffers("REGIONAL SCALE"), 50.00)
  expect_equal(.define_measurement_scale_buffers("UNKNOWN SCALE"), 0.00)
  expect_equal(.define_measurement_scale_buffers(""), 0.00)
  expect_equal(.define_measurement_scale_buffers(NA), 0.00)
})

# Integration tests ---------------------------------------------------
test_that("No aqs_site_id is represented twice in the same vintage with real data", {

})







# ------

test_that(".process_crs_measurement_scale_data() filters and selects the correct columns", {
  df_vintage_4 <- data.frame(
    latitude = c(1.0, 2.0, NA),
    longitude = c(1.0, 2.0, NA),
    measurement_scale = c("scale1", "scale2", "scale3"),
    year = c(2020, 2021, 2020)
  )
  df_vintage_410 <- data.frame(
    latitude = c(1.0, 2.0, NA),
    longitude = c(1.0, 2.0, NA),
    measurement_scale = c("scale1", "scale2", "scale3"),
    year = c(2010, 2011, 2010)
  )
  expected_df_vintage_4 <- data.frame(
    latitude = c(1.0, 2.0),
    longitude = c(1.0, 2.0),
    measurement_scale = c("scale1", "scale2")
  )
  expected_df_vintage_410 <- data.frame(
    latitude = c(1.0, 2.0),
    longitude = c(1.0, 2.0),
    measurement_scale = c("scale1", "scale2")
  )
  expect_equal(
    .process_crs_measurement_scale_data(df_vintage_4, vintage_years = 2020:2029),
    expected_df_vintage_4
  )
  expect_equal(
    .process_crs_measurement_scale_data(df_vintage_410, vintage_years = 2010:2019),
    expected_df_vintage_410
  )
})


test_that(".select_crs_measurement_scale_columns() selects the correct columns", {
  df <- data.frame(
    latitude = c(1.0, 2.0),
    longitude = c(1.0, 2.0),
    measurement_scale = c("scale1", "scale2"),
    other_column = c("a", "b")
  )
  expected_df <- data.frame(
    latitude = c(1.0, 2.0),
    longitude = c(1.0, 2.0),
    measurement_scale = c("scale1", "scale2")
  )
  expect_equal(
    .select_crs_measurement_scale_columns(df),
    expected_df
  )
})

test_that(".get_unique_crs_measurement_scale_combinations() returns a list of data frames", {
  combined_monitors <- data.frame(
    latitude = rep(1:10, times = 8),
    longitude = rep(1:10, times = 8),
    measurement_scale = rep(c("scale1", "scale2"), each = 40),
    year = rep(2010:2029, times = 4)
  )
  expected_list <- list(
    vintage_4 = data.frame(
      latitude = rep(1:10, times = 2),
      longitude = rep(1:10, times = 2),
      measurement_scale = rep(c("scale1", "scale2"), each = 10)
    ),
    vintage_410 = data.frame(
      latitude = rep(1:10, times = 2),
      longitude = rep(1:10, times = 2),
      measurement_scale = rep(c("scale1", "scale2"), each = 10)
    )
  )
  expect_equal(
    .get_unique_crs_measurement_scale_combinations(combined_monitors = combined_monitors),
    expected_list
  )
})
