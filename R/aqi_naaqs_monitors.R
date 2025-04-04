.define_annual_concentration_by_monitor_url <- function(year) {
    url <- stringr::str_c(
        "https://aqs.epa.gov/aqsweb/airdata/annual_conc_by_monitor_",
        year,
        ".zip"
    )
}

.download_annual_concentration_by_monitor <- function(year, temp_directory) {
    url <- .define_annual_concentration_by_monitor_url(year = year)
    zip_file_path <- tempfile(tmpdir = temp_directory, fileext = ".zip")
    utils::download.file(url = url, destfile = zip_file_path)
    utils::unzip(zip_file_path, exdir = temp_directory)
    zip_file_path
}

.import_annual_concentration_by_monitor_data <- function(zip_file_path) {
    readr::read_csv(file.path(zip_file_path)) |>
        janitor::clean_names()
}

.get_annual_concentration_by_monitor_by_year <- function(year) {
    temp_directory <- .create_temp_subdirectory("annual_concentration_by_monitor")
    zip_file_path <- .download_annual_concentration_by_monitor(year = year, temp_directory = temp_directory)
    df <- .import_annual_concentration_by_monitor_data(zip_file_path = zip_file_path) |>
        dplyr::mutate("year" = year)
    unlink(temp_directory, recursive = TRUE)
    df
}

.get_annual_concentration_by_monitor_across_years <- function(start, end) {
    years <- seq(start, end)
    purrr::map(years, .get_annual_concentration_by_monitor_by_year) |>
        purrr::list_rbind()
}

.filter_active_monitors <- function(df) {
    df |>
        dplyr::filter(observation_count >= 1)
}

.create_annual_concentration_by_monitor_aqi_site_id <- function(df) {
    df |>
        dplyr::mutate("aqs_site_id" = stringr::str_c(state_code, county_code, site_num))
}

.select_annual_concentration_by_monitor_columns <- function(df) {
    df |>
        dplyr::select(
            "aqs_site_id",
            "poc",
            "parameter_code",
            "latitude",
            "longitude",
            "year"
        )
}

.process_annual_concentration_by_monitor_columns <- function(df) {
    df |>
        .create_annual_concentration_by_monitor_aqi_site_id() |>
        .select_annual_concentration_by_monitor_columns()
}

.get_aqi_naaqs_monitors_by_vintage <- function(start, end) {
    .get_annual_concentration_by_monitor_across_years(start = start, end = end) |>
        .filter_aqi_naaqs_parameters() |>
        .filter_active_monitors() |>
        .process_annual_concentration_by_monitor_columns() |>
        dplyr::distinct() |>
        .split_vintages()
}

.join_aqi_naaqs_monitor_monitor_metadata <- function(monitor_list, monitor_metadata) {
    expected_cols <- c(
        "aqs_site_id",
        "poc",
        "parameter_code",
        "parameter_name",
        "measurement_scale",
        "measurement_scale_definition",
        "monitoring_objective"
    )
    if (!identical(colnames(monitor_metadata), expected_cols)) {
        stop("Monitor metadata does not have the expected columns.")
    } else {
        list(
            vintage_4 = monitor_list$vintage_4 |>
                dplyr::left_join(monitor_metadata, by = c("aqs_site_id", "poc", "parameter_code")),
            vintage_410 = monitor_list$vintage_410 |>
                dplyr::left_join(monitor_metadata, by = c("aqs_site_id", "poc", "parameter_code"))
        )
    }
}

#' Get data on all NAAQS and AQI monitors that were active in a given range of years
#'
#' This function downloads and imports annual concentration data for all AQS monitors in a given range of years.
#' It filters the data to include only active monitors that measure AQI and NAAQS parameters.
#' It joins the data with monitor metadata to include additional information about the monitors
#' (e.g., monitoring objective, measurement scale).
#'
#' @param start The first year of the range
#' @param end The last year of the range
#' @param monitor_metadata A tibble with monitor metadata
#' @return A tibble with columns for AQS site ID, POC, parameter name, latitude, longitude, measurement scale, measurement scale definition, monitoring objective, and year
#' @export
get_aqi_naaqs_monitors <- function(start, end, monitor_metadata, geocoded_bg_id) {
    .get_aqi_naaqs_monitors_by_vintage(start = start, end = end) |>
        .join_aqi_naaqs_monitor_monitor_metadata(monitor_metadata = monitor_metadata)
}
