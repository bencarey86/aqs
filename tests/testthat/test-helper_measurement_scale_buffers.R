# Synthetic data for
.generate_test_site_coordinates <- function() {
  data.frame(
    aqs_site_id = c("100", "200", "300", "400", "500", "600"),
    bg_id = c("1001", "2001", "3001", "4001", "5001", "6001"),
    latitude = c(35, 45, 55, 65, 75, 85),
    longitude = c(-87, -97, -107, -117, -127, -137)
  )
}

.generate_block_group_test_coordinates <- function() {
  site_coordinates <- .generate_test_site_coordinates()

  site_coordinates_replicated <- data.frame(
    aqs_site_id = rep(site_coordinates$aqs_site_id, each = 25),
    latitude = rep(site_coordinates$latitude, each = 25),
    longitude = rep(site_coordinates$longitude, each = 25)
  )

  test_distances <- c(
    0.01, 0.02, 0.03, 0.04, 0.05, 0.2, 0.25, 0.3, 0.35, 0.4, 1, 1.5, 2, 2.5, 3, 10, 20, 30, 25, 40,
    75, 100, 150, 200, 250
  )

  test_coordinates_grid <- data.frame(
    distance = rep(test_distances, 6),
    bearing = seq(0, 360, length.out = length(test_distances) * 6),
    bg = sprintf("%04d", 1:150)
  ) |>
    dplyr::mutate(bearing_rad = bearing * (pi / 180))

  test_coordinates <- geosphere::destPoint(
    p = site_coordinates_replicated[, c("longitude", "latitude")],
    b = test_coordinates_grid$bearing_rad,
    d = test_coordinates_grid$distance * 1000
  )

  test_coordinates_grid |>
    dplyr::mutate(
      centroid_longitude = test_coordinates[, 1],
      centroid_latitude = test_coordinates[, 2]
    )
}

.generate_synthetic_monitor_df <- function() {
  data.frame(
    parameter_name = rep(c(
      "Nitrogen dioxide (NO2)", "PM10",
      "PM2.5"
    ), each = 6),
    aqs_site_id = rep(c("100", "200", "300", "400", "500", "600"), each = 3),
    measurement_scale = rep(
      c(
        "MICROSCALE",
        rep(
          c(
            "MIDDLE SCALE",
            "NEIGHBORHOOD",
            "URBAN SCALE",
            "REGIONAL SCALE"
          ),
          each = 2
        )
      ),
      times = 2
    ),
    monitoring_objective = rep(c("Source/Highest/Max", "Other", "Other"), times = 6),
    year = rep(c(2022, 2022, 2023), times = 6)
  )
}

.generate_synthetic_block_groups_in_buffer <- function() {
  synthetic_monitor_df <- .generate_synthetic_monitor_df()
  synthetic_parameter_year_grid <- .generate_parameter_year_grid(
    df = synthetic_monitor_df
  )
  synthetic_monitor_buffer_list <- get_block_groups_in_measurement_scale_buffers(
    site_crs_df = .generate_test_site_coordinates(),
    centroids_df = .generate_block_group_test_coordinates()
  )
  list(
    no2_2022 = list(
      source = synthetic_monitor_buffer_list$site_200$neighborhood,
      other = synthetic_monitor_buffer_list$site_100$middle_scale
    ),
    no2_2023 = list(
      source = NULL,
      other = c(
        synthetic_monitor_buffer_list$site_100$middle_scale,
        synthetic_monitor_buffer_list$site_200$urban_scale
      )
    ),
    pm10_2022 = list(
      source = c(
        synthetic_monitor_buffer_list$site_300$urban_scale,
        synthetic_monitor_buffer_list$site_400$microscale
      ),
      other = c(
        setdiff(synthetic_monitor_buffer_list$site_300$regional_scale, synthetic_monitor_buffer_list$site_300$urban_scale),
        synthetic_monitor_buffer_list$site_400$middle_scale
      )
    ),
    pm10_2023 = list(
      source = NULL,
      other = c(
        synthetic_monitor_buffer_list$site_300$regional_scale,
        synthetic_monitor_buffer_list$site_400$middle_scale
      )
    ),
    pm25_2022 = list(
      source = c(
        synthetic_monitor_buffer_list$site_500$neighborhood,
        synthetic_monitor_buffer_list$site_600$urban_scale
      ),
      other =
        setdiff(synthetic_monitor_buffer_list$site_600$regional_scale, synthetic_monitor_buffer_list$site_600$urban_scale)
    ),
    pm25_2023 = list(
      source = NULL,
      other = c(
        synthetic_monitor_buffer_list$site_500$urban_scale,
        synthetic_monitor_buffer_list$site_600$regional_scale
      )
    )
  )
}

# Synthetic data for block groups with monitor ------------------------------
.generate_synthetic_block_groups_with_monitor_df <- function() {
  initial_df <- data.frame(
    bg_id = rep(as.character(1001:1020), times = 3),
    parameter_name = rep(c("PM2.5", "Nitrogen dioxide (NO2)", "PM10"), each = 20),
    year = rep(2022:2023, times = 30),
    monitoring_objective = rep(c("Source/Highest/Max", "Other", "Other"), times = 20)
  )

  df_to_add <- initial_df |>
    dplyr::filter(monitoring_objective == "Source/Highest/Max") |>
    dplyr::mutate(monitoring_objective = "Other")

  dplyr::bind_rows(initial_df, df_to_add)
}

.get_synthetic_block_groups_with_source_monitors <- function(
    synthetic_block_groups_with_monitor_df,
    parameter,
    given_year) {
  synthetic_block_groups_with_monitor_df$bg_id[
    synthetic_block_groups_with_monitor_df$parameter_name == parameter &
      synthetic_block_groups_with_monitor_df$year == given_year &
      synthetic_block_groups_with_monitor_df$monitoring_objective == "Source/Highest/Max"
  ] |>
    unique() |>
    sort()
}

.get_synthetic_block_groups_with_other_monitors <- function(
    synthetic_block_groups_with_monitor_df,
    parameter,
    given_year) {
  synthetic_block_groups_with_monitor_df$bg_id[
    synthetic_block_groups_with_monitor_df$parameter_name == parameter &
      synthetic_block_groups_with_monitor_df$year == given_year &
      synthetic_block_groups_with_monitor_df$monitoring_objective == "Other"
  ] |>
    unique() |>
    sort()
}

.get_synthetic_block_groups_with_other_monitors_source_dropped <- function(
    synthetic_block_groups_with_monitor_df,
    parameter,
    given_year) {
  source_block_groups <- synthetic_block_groups_with_monitor_df$bg_id[
    synthetic_block_groups_with_monitor_df$parameter_name == parameter &
      synthetic_block_groups_with_monitor_df$year == given_year &
      synthetic_block_groups_with_monitor_df$monitoring_objective == "Source/Highest/Max"
  ] |>
    unique() |>
    sort()

  other_block_groups <- synthetic_block_groups_with_monitor_df$bg_id[
    synthetic_block_groups_with_monitor_df$parameter_name == parameter &
      synthetic_block_groups_with_monitor_df$year == given_year &
      synthetic_block_groups_with_monitor_df$monitoring_objective == "Other"
  ] |>
    unique() |>
    sort()
  setdiff(other_block_groups, source_block_groups)
}

.generate_synthetic_block_groups_with_monitor_list <- function() {
  test_block_groups_with_monitor_df <- .generate_synthetic_block_groups_with_monitor_df()
  list(
    no2_2022 = list(
      source = .get_synthetic_block_groups_with_source_monitors(
        test_block_groups_with_monitor_df,
        "Nitrogen dioxide (NO2)",
        2022
      ),
      other = .get_synthetic_block_groups_with_other_monitors_source_dropped(
        test_block_groups_with_monitor_df,
        "Nitrogen dioxide (NO2)",
        2022
      )
    ),
    no2_2023 = list(
      source = .get_synthetic_block_groups_with_source_monitors(
        test_block_groups_with_monitor_df,
        "Nitrogen dioxide (NO2)",
        2023
      ),
      other = .get_synthetic_block_groups_with_other_monitors_source_dropped(
        test_block_groups_with_monitor_df,
        "Nitrogen dioxide (NO2)",
        2023
      )
    ),
    pm10_2022 = list(
      source = .get_synthetic_block_groups_with_source_monitors(
        test_block_groups_with_monitor_df,
        "PM10",
        2022
      ),
      other = .get_synthetic_block_groups_with_other_monitors_source_dropped(
        test_block_groups_with_monitor_df,
        "PM10",
        2022
      )
    ),
    pm10_2023 = list(
      source = .get_synthetic_block_groups_with_source_monitors(
        test_block_groups_with_monitor_df,
        "PM10",
        2023
      ),
      other = .get_synthetic_block_groups_with_other_monitors_source_dropped(
        test_block_groups_with_monitor_df,
        "PM10",
        2023
      )
    ),
    pm25_2022 = list(
      source = .get_synthetic_block_groups_with_source_monitors(
        test_block_groups_with_monitor_df,
        "PM2.5",
        2022
      ),
      other = .get_synthetic_block_groups_with_other_monitors_source_dropped(
        test_block_groups_with_monitor_df,
        "PM2.5",
        2022
      )
    ),
    pm25_2023 = list(
      source = .get_synthetic_block_groups_with_source_monitors(
        test_block_groups_with_monitor_df,
        "PM2.5",
        2023
      ),
      other = .get_synthetic_block_groups_with_other_monitors_source_dropped(
        test_block_groups_with_monitor_df,
        "PM2.5",
        2023
      )
    )
  )
}
