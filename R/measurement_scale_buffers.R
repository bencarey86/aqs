# Population-weighted centroids ------------------------------------------------
.define_centroid_urls <- function() {
    list(
        vintage_4 = "https://www2.census.gov/geo/docs/reference/cenpop2020/blkgrp/CenPop2020_Mean_BG.txt",
        vintage_410 = "https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG.txt"
    )
}

.import_pop_weighted_centroids <- function(centroid_urls) {
    list(
        vintage_4 = readr::read_csv(centroid_urls$vintage_4),
        vintage_410 = readr::read_csv(centroid_urls$vintage_410)
    )
}

.clean_centroid_columns <- function(centroid_data) {
    purrr::map(centroid_data, function(df) {
        df |>
            janitor::clean_names() |>
            dplyr::mutate(bg = stringr::str_c(statefp, countyfp, tractce, blkgrpce)) |>
            dplyr::select(bg, centroid_latitude = latitude, centroid_longitude = longitude)
    })
}

.get_pop_weighted_centroids <- function() {
    .define_centroid_urls() |>
        .import_pop_weighted_centroids() |>
        .clean_centroid_columns()
}

# AQS Site ID and CRS coordinates ------------------------------------------------
.get_aqs_site_id_crs_columns <- function(df_list) {
    list(
        vintage_4 = df_list$vintage_4 |>
            dplyr::select(aqs_site_id, latitude, longitude),
        vintage_410 = df_list$vintage_410 |>
            dplyr::select(aqs_site_id, latitude, longitude)
    )
}

.get_combined_aqs_site_id_crs_coordinates_by_vintage <- function(aqi_naaqs_monitors, regulatory_monitors) {
    aqi_naaqs_crs <- .get_aqs_site_id_crs_columns(aqi_naaqs_monitors)
    regulatory_crs <- .get_aqs_site_id_crs_columns(regulatory_monitors)
    list(
        vintage_4 = aqi_naaqs_crs$vintage_4 |>
            dplyr::bind_rows(regulatory_crs$vintage_4) |>
            dplyr::filter(!is.na(latitude) & !is.na(longitude)) |>
            dplyr::distinct(),
        vintage_410 = aqi_naaqs_crs$vintage_410 |>
            dplyr::bind_rows(regulatory_crs$vintage_410) |>
            dplyr::filter(!is.na(latitude) & !is.na(longitude)) |>
            dplyr::distinct()
    )
}

# Distance calculations ------------------------------------------------
.define_crs_matrices <- function(monitors, centroids) {
    list(
        monitors = data.matrix(monitors[, c("longitude", "latitude")]),
        centroids = data.matrix(centroids[, c("centroid_longitude", "centroid_latitude")])
    )
}

.define_crs_matrices_single_site <- function(site_longitude, site_latitude, centroids) {
    list(
        monitors = matrix(c(site_longitude, site_latitude), ncol = 2),
        centroids = data.matrix(centroids[, c("centroid_longitude", "centroid_latitude")])
    )
}

.calculate_distance_single_site_to_centroids <- function(site_longitude, site_latitude, centroids_df) {
    crs_matrices <- .define_crs_matrices_single_site(
        site_longitude = site_longitude,
        site_latitude = site_latitude,
        centroids = centroids_df
    )
    geosphere::distGeo(
        p1 = crs_matrices$monitors,
        p2 = crs_matrices$centroids
    ) / 1000
}

# Determine block groups in m
.define_measurement_scale_buffers <- function() {
    list(
        microscale = 0.00,
        middle_scale = 0.10,
        neighborhood = 0.50,
        urban_scale = 4.00,
        regional_scale = 50.00
    )
}

.determine_block_groups_in_scale_buffers <- function(distances, centroids) {
    measurement_scale_buffer <-
        .define_measurement_scale_buffers()
    list(
        microscale = centroids$bg[distances <= measurement_scale_buffer$microscale],
        middle_scale = centroids$bg[distances <= measurement_scale_buffer$middle_scale],
        neighborhood = centroids$bg[distances <= measurement_scale_buffer$neighborhood],
        urban_scale = centroids$bg[distances <= measurement_scale_buffer$urban_scale],
        regional_scale = centroids$bg[distances <= measurement_scale_buffer$regional_scale]
    )
}

get_block_groups_in_measurement_scale_buffers <- function(site_crs_df, centroids_df) {
    stats::setNames(
        purrr::pmap(list(site_crs_df$aqs_site_id, site_crs_df$latitude, site_crs_df$longitude), function(aqs_site_id, latitude, longitude) {
            distances <- .calculate_distance_single_site_to_centroids(
                site_longitude = longitude,
                site_latitude = latitude,
                centroids_df = centroids_df
            )
            block_groups_in_scale_buffers <- .determine_block_groups_in_scale_buffers(
                distances = distances,
                centroids = centroids_df
            )
            list(
                aqs_site_id = aqs_site_id,
                microscale = block_groups_in_scale_buffers$microscale,
                middle_scale = block_groups_in_scale_buffers$middle_scale,
                neighborhood = block_groups_in_scale_buffers$neighborhood,
                urban_scale = block_groups_in_scale_buffers$urban_scale,
                regional_scale = block_groups_in_scale_buffers$regional_scale
            )
        }),
        stringr::str_c("site_", site_crs_df$aqs_site_id)
    )
}

get_block_group_monitoring_status <- function(monitors, bg_id_distances) {
    # For each parameter and year combination, get unique combinations of aqs_site_id, measurement_scale, monitoring_objective
    # For each monitoring_objective, get the unique combinations of aqs_site_id and measurement_scale
    # Get all monitoring status lists matching aqs_site_id and measurement_scale and combine observations
    # into a single list and remove duplicates
    # Output
    years <- unique(monitors$year)
    parameters <- unique(monitors$parameter_name)
    search_grid <- expand.grid(
        year = years,
        parameter_name = parameters,
        stringsAsFactors = FALSE
    )
    purrr::map2(search_grid$parameter_name, search_grid$year, function(parameter_name, year) {
        monitors_by_parameter_year <- monitors |>
            dplyr::filter(parameter_name == parameter_name & year == year) |>
            dplyr::select(aqs_site_id, bg_id, measurement_scale, monitoring_objective) |>
            dplyr::distinct()
        purrr::map(monitors_by_parameter_year$monitoring_objective, function(monitoring_objective) {
            monitors_by_site_scale <- monitors_by_parameter_year |>
                dplyr::filter(monitoring_objective == monitoring_objective) |>
                dplyr::select(aqs_site_id, bg_id, measurement_scale) |>
                dplyr::distinct()

            purrr::map2(monitors_by_site_scale$aqs_site_id, monitors_by_site_scale$measurement_scale, function(aqs_site_id, measurement_scale) {
                site_name <- stringr::str_c("site_", aqs_site_id)
                scale_name <- dplyr::case_when(
                    measurement_scale == "MICROSCALE" ~ "microscale",
                    measurement_scale == "MIDDLE SCALE" ~ "middle_scale",
                    measurement_scale == "NEIGHBORHOOD" ~ "neighborhood",
                    measurement_scale == "URBAN SCALE" ~ "urban_scale",
                    measurement_scale == "REGIONAL SCALE" ~ "regional_scale",
                    TRUE ~ "microscale"
                )
                bg_id_distances[[site_name]][[scale_name]]
            })
        })
    })
}

determine_block_group_monitor_status <- function(monitors, bg_id_buffers) {
    search_grid <- .generate_parameter_year_grid(monitors)
    sublist_names <- .generate_parameter_year_names(
        parameter = search_grid$parameter_name,
        year = search_grid$year
    )

    block_groups_with_monitor <- .determine_block_groups_with_monitors(
        monitors = monitors,
        parameter_year_grid = search_grid
    ) |>
        purrr::set_names(sublist_names)

    block_groups_in_buffer <- .determine_block_groups_in_monitor_buffer(
        search_grid = search_grid,
        monitors = monitors,
        monitor_buffer_list = bg_id_buffers
    ) |>
        purrr::set_names(sublist_names)

    .restructure_monitor_status_output(
        block_groups_with_monitor = block_groups_with_monitor,
        block_groups_in_buffer = block_groups_in_buffer,
        sublist_names = sublist_names
    )
}

.generate_parameter_year_grid <- function(df) {
    df |>
        dplyr::select(parameter_name, year) |>
        dplyr::distinct() |>
        dplyr::arrange(parameter_name, year)
}

.generate_parameter_year_names <- function(parameter, year) {
    parameter <- dplyr::case_when(
        parameter == "Carbon monoxide" ~ "co",
        parameter == "Lead" ~ "lead",
        parameter == "Nitrogen dioxide (NO2)" ~ "no2",
        parameter == "Ozone" ~ "o3",
        parameter == "PM10" ~ "pm10",
        parameter == "PM2.5" ~ "pm25",
        parameter == "Sulfur dioxide" ~ "so2"
    )
    stringr::str_c(parameter, "_", year)
}

.get_unique_filtered_site_observations <- function(df, parameter, given_year) {
    df <- df |>
        dplyr::filter(parameter_name == parameter) |>
        dplyr::filter(year == given_year) |>
        dplyr::select(aqs_site_id, measurement_scale, monitoring_objective) |>
        dplyr::distinct()
    list(
        source = df |>
            dplyr::filter(monitoring_objective == "Source/Highest/Max") |>
            dplyr::select(aqs_site_id, measurement_scale) |>
            dplyr::distinct(),
        other = df |>
            dplyr::filter(monitoring_objective == "Other") |>
            dplyr::select(aqs_site_id, measurement_scale) |>
            dplyr::distinct()
    )
}

.determine_block_groups_with_monitors_individual <- function(df, parameter, given_year, given_monitoring_objective) {
    df |>
        dplyr::filter(parameter_name == parameter) |>
        dplyr::filter(year == given_year) |>
        dplyr::filter(monitoring_objective == given_monitoring_objective) |>
        dplyr::pull(bg_id) |>
        unique() |>
        sort()
}

.determine_block_groups_with_monitors <- function(monitors, parameter_year_grid) {
    purrr::map2(
        parameter_year_grid$parameter_name,
        parameter_year_grid$year,
        function(parameter, yr) {
            block_groups_with_source_monitors <- .determine_block_groups_with_monitors_individual(
                df = monitors,
                parameter = parameter,
                given_year = yr,
                given_monitoring_objective = "Source/Highest/Max"
            )
            block_groups_with_other_monitors <- .determine_block_groups_with_monitors_individual(
                df = monitors,
                parameter = parameter,
                given_year = yr,
                given_monitoring_objective = "Other"
            ) |>
                # Remove block groups that also have monitors with Source/Highest/Max objective
                .remove_source_objective_bg_from_other_objective(
                    source_bg_ids = block_groups_with_source_monitors
                )
            list(
                source = block_groups_with_source_monitors,
                other = block_groups_with_other_monitors
            )
        }
    )
}

.transform_measurement_scale <- function(measurement_scale) {
    # NA values are replaced with "microscale", as microscale contains no buffer
    dplyr::case_when(
        measurement_scale == "MICROSCALE" ~ "microscale",
        measurement_scale == "MIDDLE SCALE" ~ "middle_scale",
        measurement_scale == "NEIGHBORHOOD" ~ "neighborhood",
        measurement_scale == "URBAN SCALE" ~ "urban_scale",
        measurement_scale == "REGIONAL SCALE" ~ "regional_scale",
        TRUE ~ "microscale"
    )
}

.determine_block_groups_in_monitor_buffer_individual <- function(monitor_buffer_list, aqs_site_id, measurement_scale) {
    measurement_scale_transformed <- .transform_measurement_scale(measurement_scale)
    monitor_buffer_list[[stringr::str_c("site_", aqs_site_id)]][[measurement_scale_transformed]]
}

.determine_block_groups_in_monitor_buffer <- function(search_grid, monitors, monitor_buffer_list) {
    purrr::map2(
        search_grid$parameter_name,
        search_grid$year,
        function(parameter, given_year) {
            unique_filtered_sites <- .get_unique_filtered_site_observations(
                df = monitors,
                parameter = parameter,
                given_year = given_year
            )

            source_bg_ids <- purrr::map2(
                unique_filtered_sites$source$aqs_site_id,
                unique_filtered_sites$source$measurement_scale,
                function(site_id, scale) {
                    .determine_block_groups_in_monitor_buffer_individual(
                        monitor_buffer_list = monitor_buffer_list,
                        aqs_site_id = site_id,
                        measurement_scale = scale
                    )
                }
            ) |>
                unlist() |>
                unique()
            other_bg_ids <- purrr::map2(
                unique_filtered_sites$other$aqs_site_id,
                unique_filtered_sites$other$measurement_scale,
                function(site_id, scale) {
                    .determine_block_groups_in_monitor_buffer_individual(
                        monitor_buffer_list = monitor_buffer_list,
                        aqs_site_id = site_id,
                        measurement_scale = scale
                    )
                }
            ) |>
                unlist() |>
                unique() |>
                # Remove block groups that also have monitors with Source/Highest/Max objective
                .remove_source_objective_bg_from_other_objective(
                    source_bg_ids = source_bg_ids
                )
            list(
                source = source_bg_ids,
                other = other_bg_ids
            )
        }
    )
}

.remove_source_objective_bg_from_other_objective <- function(other_bg_ids, source_bg_ids) {
    # Remove source block groups from other objectives
    other_bg_ids[!other_bg_ids %in% source_bg_ids]
}

.restructure_monitor_status_output <- function(block_groups_with_monitor, block_groups_in_buffer, sublist_names) {
    purrr::map2(
        sublist_names,
        seq_along(sublist_names),
        function(name, idx) {
            list(
                source = list(
                    monitor_location = block_groups_with_monitor[[name]]$source,
                    monitor_buffer = block_groups_in_buffer[[name]]$source
                ),
                other = list(
                    monitor_location = block_groups_with_monitor[[name]]$other,
                    monitor_buffer = block_groups_in_buffer[[name]]$other
                )
            )
        }
    ) |>
        purrr::set_names(sublist_names)
}


.determine_monitor_location_by_parameter_year <- function(df, parameter, year) {
    list(
        source = .determine_monitor_location_by_parameter_year_objective(
            df = df,
            parameter = parameter,
            year = year,
            monitoring_objective = "Source/Highest/Max"
        ),
        other = .determine_monitor_location_by_parameter_year_objective(
            df = df,
            parameter = parameter,
            year = year,
            monitoring_objective = "Other"
        )
    )
}

.determine_source_monitor_buffer <- function(df, parameter, year) {

}

list(
    source_max_highest = list(
        monitor_location = "",
        monitor_in_buffer = ""
    ),
    other_objective = list(
        monitor_location = "",
        monitor_in_buffer = ""
    )
)


.process_crs_measurement_scale_data <- function(df, vintage_years) {
    df |>
        dplyr::filter(year %in% vintage_years) |>
        dplyr::select("latitude", "longitude", "measurement_scale") |>
        dplyr::filter(!is.na(latitude) & !is.na(longitude)) |>
        dplyr::distinct()
}

.get_unique_crs_measurement_scale_combinations <- function(combined_monitors) {
    list(
        vintage_4 = .process_crs_measurement_scale_data(
            df = combined_monitors,
            vintage_years = 2020:2029
        ),
        vintage_410 = .process_crs_measurement_scale_data(
            df = combined_monitors,
            vintage_years = 2010:2019
        )
    )
}
