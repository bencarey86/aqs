# Population-weighted centroids ----------------------------------------------

#' Define URLs for population-weighted centroids
#'
#' This function defines the URLs for the population-weighted block group centroids
#' for 2010 and 2020, as deteremined by the Census Bureau.
#'
#' @return A list containing the URLs for the population-weighted centroids for 2010 and 2020.
#' @keywords internal
.define_centroid_urls <- function() {
    list(
        vintage_4 = "https://www2.census.gov/geo/docs/reference/cenpop2020/blkgrp/CenPop2020_Mean_BG.txt",
        vintage_410 = "https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG.txt"
    )
}

#' Define file paths for population-weighted centroids
#'
#'
#' This function defines the file paths for the population-weighted block group centroids for 2010 and 2020.
#'
#' @return A list containing the file paths for the population-weighted centroids for 2010 and 2020.
#' @keywords internal
.define_centroid_path <- function() {
    list(
        vintage_4 = "data/raw/CenPop2020_Mean_BG.txt",
        vintage_410 = "data/raw/CenPop2010_Mean_BG.txt"
    )
}

#' Import population-weighted block group centroid data
#'
#' This function imports the population-weighted block group centroid data from the specified URLs,
#' which are defined in the .define_centroid_urls function.
#'
#' @param centroid_urls A list containing the URLs for the population-weighted centroids for 2010 and 2020.
#' @return A list containing the imported population-weighted block group centroid data for 2010 and 2020.
#' @keywords internal
.import_pop_weighted_centroids <- function(centroid_urls) {
    list(
        vintage_4 = readr::read_csv(centroid_urls$vintage_4),
        vintage_410 = readr::read_csv(centroid_urls$vintage_410)
    )
}

#' Clean population-weighted block group centroid data
#'
#' This function cleans the population-weighted block group centroid data by renaming columns,
#' adding a block group identifier, and selecting relevant columns.
#'
#' @param centroid_data A list containing the population-weighted block group centroid data for 2010 and 2020.
#' @return A list containing the cleaned population-weighted block group centroid data for 2010 and 2020.
#' @keywords internal
.clean_centroid_columns <- function(centroid_data) {
    purrr::map(centroid_data, function(df) {
        df |>
            janitor::clean_names() |>
            dplyr::mutate(bg = stringr::str_c(statefp, countyfp, tractce, blkgrpce)) |>
            dplyr::select(bg, centroid_latitude = latitude, centroid_longitude = longitude)
    })
}

#' Get population-weighted block group centroids
#'
#' This function retrieves the population-weighted block group centroids for 2010 and 2020.
#' It defines the URLs for the centroid data, imports the data from the URLs,
#' and cleans the data by renaming columns and adding a block group identifier.
#'
#' @return A list containing the cleaned population-weighted block group centroid data for 2010 and 2020.
#' @keywords internal
.get_pop_weighted_centroids <- function() {
    .define_centroid_urls() |>
        .import_pop_weighted_centroids() |>
        .clean_centroid_columns()
}

# AQS Site ID and CRS coordinates -------------------------------------------

#' Get AQS site ID and CRS coordinates
#'
#' This function returns unique AQS site IDs and their corresponding latitude and longitude coordinates.
#' It take a list of monitor data frames split by vintage and returns a list of data frames split by vintage.
#'
#' @param df_list A list containing data frames of monitor data for the 2010 and 2020 vintages.
#' @return A list containing the unique AQS site IDs and their corresponding latitude and longitude coordinates for each vintage.
#' @keywords internal
.get_aqs_site_id_crs_columns_by_vintage <- function(df_list) {
    list(
        vintage_4 = df_list$vintage_4 |>
            dplyr::select(aqs_site_id, latitude, longitude),
        vintage_410 = df_list$vintage_410 |>
            dplyr::select(aqs_site_id, latitude, longitude)
    )
}

#' Get AQS site ID and CRS coordinates columns for both regulatory and AQI NAAQS monitors
#'
#' This function returns unique AQS site IDs and their corresponding latitude and longitude coordinates
#' for both regulatory and AQI NAAQS monitors, separated by vintage. Several regulatory monitors are
#' not present in the AQI NAAQS monitors data frames.
#'
#' @ param aqi_naaqs_monitors A list of AQI NAAQS monitors data frames split by vintage.
#' @ param regulatory_monitors A list of regulatory monitors data frames split by vintage.
#' @return A list containing the unique AQS site IDs and their corresponding latitude and longitude coordinates
#' for both AQI NAAQS and regulatory monitors, separated by vintage.
#' @keywords internal
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

# Distance calculations -------------------------------------------

#' Define CRS matrices for a single AQS site latitude and longitude coordinate pair and CRS coordinates
#' for population-weighted block group centroids
#'
#' This function defines the CRS matrices for a single AQS site latitude and longitude coordinate pair
#' and the CRS coordinates for population-weighted block group centroids.
#'
#' @param site_longitude A numeric value representing the longitude of the AQS site.
#' @param site_latitude A numeric value representing the latitude of the AQS site.
#' @param centroids A data frame containing the CRS coordinates for population-weighted block group centroids.
#' @return A list containing the CRS matrices for the AQS site and the population-weighted block group centroids.
#' @keywords internal
.define_crs_matrices <- function(site_longitude, site_latitude, centroids) {
    list(
        monitors = matrix(c(site_longitude, site_latitude), ncol = 2),
        centroids = data.matrix(centroids[, c("centroid_longitude", "centroid_latitude")])
    )
}

#' Calculate distance to centroids
#'
#' This function calculates the distance from a single AQS site to population-weighted block group centroids.
#' It transforms the AQS site and centroid CRS coordinates into matrices and uses the geosphere package
#' to calculate the distance in kilometers.
#'
#' @param site_longitude A numeric value representing the longitude of the AQS site.
#' @param site_latitude A numeric value representing the latitude of the AQS site.
#' @param centroids_df A data frame containing the CRS coordinates for population-weighted block group centroids.
#' @return A vector containing the distances in kilometers from the AQS site to the population-weighted block group centroids.
#' @keywords internal
.calculate_distance_to_centroids <- function(site_longitude, site_latitude, centroids_df) {
    crs_matrices <- .define_crs_matrices(
        site_longitude = site_longitude,
        site_latitude = site_latitude,
        centroids = centroids_df
    )
    # Output is in meters, so divide by 1000 to get kilometers
    geosphere::distGeo(
        p1 = crs_matrices$monitors,
        p2 = crs_matrices$centroids
    ) / 1000
}

# Determine block groups in measurement scale buffers for all sites ---------------

#' Define measurement scale buffers
#'
#' This function defines the buffers (in kilometers) for different measurement scales, as defined by the EPA.
#'
#' @return A list containing the buffer sizes in kilometers for all measurement scales.
#' @keywords internal
.define_measurement_scale_buffers <- function() {
    list(
        microscale = 0.00,
        middle_scale = 0.10,
        neighborhood = 0.50,
        urban_scale = 4.00,
        regional_scale = 50.00
    )
}

#' Determine which block group centroids are within buffer ranges for all measurement scales
#' for a given AQS site
#'
#' This function determines which block group centroids are within the buffer ranges for all measurement scales
#' for a given AQS site. It uses the distances calculated from the AQS site to the block group centroids
#' and the measurement scale buffers defined by the EPA, from the .calculate_distance_to_centroids and
#' .define_measurement_scale_buffers functions.
#'
#' @param distances A vector containing the distances from the AQS site to the block group centroids.
#' @param centroids A data frame containing the CRS coordinates for population-weighted block group
#' centroids.
#' @return A list containing the block group IDs of the centroids that are within the buffer ranges
#' for all measurement scales.
#' @keywords internal
.determine_block_groups_in_scale_buffers_for_single_site <- function(distances, centroids) {
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

#' Get block groups in measurement scale buffers for all sites
#'
#' This function retrieves the block groups with centroid coordinates in all measurement scale
#' buffers ranges for all provided AQS sites.
#'
#' @param site_crs_df A data frame containing the AQS site IDs and their corresponding latitude and
#' longitude coordinates.
#' @param centroids_df A data frame containing the CRS coordinates for population-weighted centroids
#' of all block groups for a given vintage.
#' @return A list of all AQS sites with sub-lists of the block group centroids within each
#' measurement scale buffer.
#' @export
get_block_groups_in_measurement_scale_buffers <- function(site_crs_df, centroids_df) {
    stats::setNames(
        purrr::pmap(list(site_crs_df$aqs_site_id, site_crs_df$latitude, site_crs_df$longitude), function(aqs_site_id, latitude, longitude) {
            distances <- .calculate_distance_to_centroids(
                site_longitude = longitude,
                site_latitude = latitude,
                centroids_df = centroids_df
            )
            block_groups_in_scale_buffers <- .determine_block_groups_in_scale_buffers_for_single_site(
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

# Determine block group monitor status for all parameter and year combinations ------

#' Generate a grid of parameter and year combinations
#'
#' This function generates a grid of parameter and year combinations from the provided monitor data frame.
#'
#' @param df A data frame containing monitor data with columns for parameter name and year.
#' @return A data frame containing distinct parameter and year combinations, sorted by parameter name and year.
#' @keywords internal
.generate_parameter_year_grid <- function(df) {
    df |>
        dplyr::select(parameter_name, year) |>
        dplyr::distinct() |>
        dplyr::arrange(parameter_name, year)
}

#' Generate parameter-year names
#'
#' This function generates parameter-year strings by concatenating the parameter name and year.
#' It transforms parameter names to lowercase chemical notations (except for lead).
#' These strings are used to name the lists of block groups with monitors and those in buffers
#' for each parameter-year combination.
#'
#' @param parameter A character string representing the parameter name.
#' @param year A numeric value representing the year.
#' @return A character string representing the parameter-year combination in lowercase chemical notation.
#' @keywords internal
.generate_combined_parameter_abbreviation_year_string <- function(parameter, year) {
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

#' Get unique site and scale combinations by monitoring objective for a given parameter and year
#'
#' This function retrieves unique AQS site ID and measurement scale combinations
#' for a given parameter and year, separated by monitoring objective.
#'
#' @param df A data frame containing monitor data with columns for parameter name, year, AQS site ID, measurement scale, and monitoring objective.
#' @param parameter A character string representing the parameter name.
#' @param given_year A numeric value representing the year.
#' @return A list containing two data frames: one for the "Source/Highest/Max" monitoring objective and one for the "Other" monitoring objective.
#' @keywords internal
.get_unique_site_scale_combinations_by_objective <- function(df, parameter, given_year) {
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

#' Get vector of block groups with monitors in them for a given parameter, year, and monitoring objective
#'
#' This function retrieves a vector of block groups with monitors in them for a given parameter, year, and monitoring objective.
#' The vector is sorted and unique.
#'
#' @param df A data frame containing monitor data with columns for parameter name, year, block group ID, and monitoring objective.
#' @param parameter A character string representing the parameter name.
#' @param given_year A numeric value representing the year.
#' @param given_monitoring_objective A character string representing the monitoring objective.
#' @return A sorted and unique vector of block group IDs with monitors in them for the given parameter, year, and monitoring objective.
#' @keywords internal
.determine_block_groups_with_monitors_individual <- function(df, parameter, given_year, given_monitoring_objective) {
    df |>
        dplyr::filter(parameter_name == parameter) |>
        dplyr::filter(year == given_year) |>
        dplyr::filter(monitoring_objective == given_monitoring_objective) |>
        dplyr::pull(bg_id) |>
        unique() |>
        sort()
}

#' Remove block groups with monitors with "Source/Highest/Max" objectives
#' from vector of block groups with "Other" objective
#'
#' This function removes block groups with monitors with "Source/Highest/Max" objectives
#' from the vector of block groups with "Other" objectives.
#'
#' @param other_bg_ids A vector of block group IDs with "Other" monitoring objectives.
#' @param source_bg_ids A vector of block group IDs with "Source/Highest/Max" monitoring objectives.
#' @return A vector of block group IDs with "Other" monitoring objectives, excluding those with "Source/Highest/Max" objectives.
#' @keywords internal
.remove_source_objective_bg_from_other_objective <- function(other_bg_ids, source_bg_ids) {
    # Remove source block groups from other objectives
    other_bg_ids[!other_bg_ids %in% source_bg_ids]
}

#' Determine block groups with monitors in them for all parameter and year combinations
#'
#' This function determines the block groups with monitors in them for all parameter and year combinations.
#' It separates the block groups into those with monitors with "Source/Highest/Max" monitoring objectives
#' and those with "Other" monitoring objectives and returns a list of vectors of block group IDs
#' for each objective. It removes block groups with monitors with "Source/Highest/Max" objectives
#' from the vector of block groups with the "Other" objective.
#'
#' @param monitors A data frame containing monitor data with columns for parameter name, year, block group ID, and monitoring objective.
#' @param parameter_year_grid A data frame containing distinct parameter and year combinations.
#' @return A list containing two vectors of block group IDs: one for the "Source/Highest/Max" monitoring objective
#' and one for the "Other" monitoring objective. Each vector is sorted and unique.
#' @keywords internal
.determine_block_groups_with_monitors <- function(monitors) {
    parameter_year_grid <- .generate_parameter_year_grid(df = monitors)
    list_names <- .generate_combined_parameter_abbreviation_year_string(
        parameter = parameter_year_grid$parameter_name,
        year = parameter_year_grid$year
    )

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
    ) |>
        purrr::set_names(list_names)
}

#' Transform measurement scale names
#'
#' This function transforms measurement scale names to a consistent format for easier processing
#' (converts to lowercase and replaces spaces with underscores). Measurement scales that are NA
#' or do not match the exepcted values are replaced with "microscale", as microscale monitors
#' do not have a buffer.
#'
#' @param measurement_scale A character string representing the measurement scale.
#' @return A character string representing the transformed measurement scale.
#' @keywords internal
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

#' Determine block groups in measurement scale buffers for a given AQS site and measurement scale
#'
#' This function determines the block groups within a measurement scale buffer for a given AQS site
#' and measurement scale.
#'
#' @param monitor_buffer_list A list containing the block groups within measurement scale buffers
#' for all AQS sites and measurement scales.
#' @param aqs_site_id A character string representing the given AQS site ID.
#' @param measurement_scale A character string representing the measurement scale.
#' @return A vector of block group IDs within the measurement scale buffer for the given AQS site
#' and measurement scale.
#' @keywords internal
.determine_block_groups_in_monitor_buffer_individual <- function(monitor_buffer_list, aqs_site_id, measurement_scale) {
    measurement_scale_transformed <- .transform_measurement_scale(measurement_scale)
    monitor_buffer_list[[stringr::str_c("site_", aqs_site_id)]][[measurement_scale_transformed]]
}

#' Determine block groups in measurement scale buffers for all parameter and year combinations
#'
#' This function determines the block groups within measurement scale buffers for all parameter and year combinations,
#' and reports them by monitoring objective. When block groups are within the measurement buffers of
#' both "Source/Highest/Max" and "Other" monitoring objectives, the block groups are reported
#' as "Source/Highest/Max" only.
#'
#' @param parameter_year_grid A data frame containing distinct parameter and year combinations.
#' @param monitors A data frame containing monitor data with columns for parameter name, year, AQS site ID, measurement scale, and monitoring objective.
#' @param monitor_buffer_list A list containing the block groups within measurement scale buffers of all AQS sites and measurement scales.
#' @return A list containing two vectors of unique block group IDs: one for the "Source/Highest/Max" monitoring objective
#' and one for the "Other" monitoring objective.
#' @keywords internal
.determine_block_groups_in_monitor_buffer <- function(monitors, monitor_buffer_list) {
    parameter_year_grid <- .generate_parameter_year_grid(df = monitors)
    list_names <- .generate_combined_parameter_abbreviation_year_string(
        parameter = parameter_year_grid$parameter_name,
        year = parameter_year_grid$year
    )
    purrr::map2(
        parameter_year_grid$parameter_name,
        parameter_year_grid$year,
        function(parameter, given_year) {
            unique_filtered_sites <- .get_unique_site_scale_combinations_by_objective(
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
    ) |>
        purrr::set_names(list_names)
}

#' Combine block groups with monitors and in buffer
#'
#' This function combines the lists of block groups with monitors and those in buffers
#' for each parameter-year combination. It ensures that the names of the lists
#' are the same for both block groups with monitors and those in buffers.
#'
#' @param block_groups_with_monitor A list of block groups with monitors for each parameter-year combination.
#' @param block_groups_in_buffer A list of block groups in monitor buffers for each parameter-year combination.
#' @return A list containing the combined block groups with monitors and those in buffers for each parameter-year combination,
#' separated by monitoring objective.
#' @keywords internal
.combine_block_groups_with_monitor_and_in_buffer <- function(block_groups_with_monitor, block_groups_in_buffer) {
    if (all(names(block_groups_with_monitor) != names(block_groups_in_buffer))) {
        stop("Block groups with monitor and in buffer lists do not match.")
    }
    list_names <- names(block_groups_with_monitor)
    purrr::map2(
        list_names,
        seq_along(list_names),
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
        purrr::set_names(list_names)
}

#' Determine block groups with monitors and within the measurement scale buffers for all monitors
#'
#' This function determines the block groups with monitors and those within the measurement scale buffers
#' for all monitors provided. Its output is a list of lists. Each parameter-year combination is
#' represented by a list with two sub-lists: "source" and "other", denoting the monitoring objectives.
#' Each source and other list contains two vectors: "monitor_location" and "monitor_buffer",
#' indicating the block groups with monitors and those in buffers, respectively. Monitors that were
#' originally represented in both the source and other lists for a given parameter-year combination
#' are only represented in the source list, as the source list is of special interest.
#'
#' @param monitors A data frame containing monitor data
#' @param master_list_of_block_groups_in_buffers A list containing the block groups within measurement scale buffers
#' for all AQS sites and measurement scales.
#' @return A list of lists, with each parameter-year combination represented by a list of two sub-lists:
#' "source" and "other". Each sub-list contains two vectors: "monitor_location" and "monitor_buffer",
#' indicating the block groups with monitors and those in buffers, respectively.
#' @export
determine_block_groups_monitor_status <- function(monitors, master_list_of_block_groups_in_buffers) {
    .combine_block_groups_with_monitor_and_in_buffer(
        block_groups_with_monitor = .determine_block_groups_with_monitors(
            monitors = monitors
        ),
        block_groups_in_buffer = .determine_block_groups_in_monitor_buffer(
            monitors = monitors,
            monitor_buffer_list = master_list_of_block_groups_in_buffers
        )
    )
}
