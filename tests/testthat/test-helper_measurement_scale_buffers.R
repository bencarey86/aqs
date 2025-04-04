# Synthetic data
# Single monitor
test_monitor_crs <- c(-87, 35)
test_distances <- c(
  0.01, 0.02, 0.03, 0.04, 0.05, 0.2, 0.25, 0.3, 0.35, 0.4, 1, 1.5, 2, 2.5, 3, 10, 20, 30, 25, 40,
  75, 100, 150, 200, 250
)
bg <- as.character(1:25)
test_points_grid <- data.frame(
  distance = test_distances,
  bearing = seq(0, 360, length.out = length(test_distances)),
  bg = bg
) |>
  dplyr::mutate(bearing_rad = bearing * (pi / 180))
test_points <- geosphere::destPoint(
  p = test_monitor_crs,
  b = test_points_grid$bearing_rad,
  d = test_points_grid$distance * 1000
)
test_points_complete <- test_points_grid |>
  dplyr::mutate(
    centroid_longitude = test_points[, 1],
    centroid_latitude = test_points[, 2]
  )

# Multiple monitors
site_crs_test <- data.frame(
  aqs_site_id = c("100", "200", "300", "400", "500", "600"),
  latitude = c(35, 45, 55, 65, 75, 85),
  longitude = c(-87, -97, -107, -117, -127, -137)
)
site_crs_test_rep <- data.frame(
  aqs_site_id = rep(site_crs_test$aqs_site_id, each = 25),
  latitude = rep(site_crs_test$latitude, each = 25),
  longitude = rep(site_crs_test$longitude, each = 25)
)
multiple_test_points_grid <- data.frame(
  distance = rep(test_distances, 6),
  bearing = seq(0, 360, length.out = length(test_distances) * 6),
  bg = sprintf("%04d", 1:150)
) |>
  dplyr::mutate(bearing_rad = bearing * (pi / 180))
multiple_test_points <- geosphere::destPoint(
  p = site_crs_test_rep[, c("longitude", "latitude")],
  b = multiple_test_points_grid$bearing_rad,
  d = multiple_test_points_grid$distance * 1000
)
multiple_test_points_complete <- multiple_test_points_grid |>
  dplyr::mutate(
    centroid_longitude = multiple_test_points[, 1],
    centroid_latitude = multiple_test_points[, 2]
  )
test_centroids_df <- multiple_test_points_complete |>
  dplyr::select(
    centroid_longitude,
    centroid_latitude,
    bg
  )
