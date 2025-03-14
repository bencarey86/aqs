.define_all_monitors_url <- function(year) {
    url <- stringr::str_c(
        "https://aqs.epa.gov/aqsweb/airdata/annual_conc_by_monitor_",
        year,
        ".zip"
    )
}

.download_zip_all_monitors <- function(year, temp_directory) {
    url <- .define_all_monitors_url(year = year)
    zip_file_path <- tempfile(tmpdir = temp_directory, fileext = ".zip")
    utils::download.file(url = url, destfile = zip_file_path)
    utils::unzip(zip_file_path, exdir = temp_directory)
    zip_file_path
}

.import_all_monitors_data <- function(zip_file_path) {
    readr::read_csv(file.path(zip_file_path)) |>
        janitor::clean_names()
}

.transform_all_monitors_cols <- function(df, year) {
    df |>
        dplyr::filter(observation_count >= 1) |>
        dplyr::select("state_code", "county_code",
            "site_number" = "site_num", "poc", "parameter_code", "parameter_name",
            "latitude", "longitude"
        ) |>
        dplyr::mutate(year = year)
}

.get_all_monitors_by_year <- function(year) {
    temp_directory <- .create_temp_subdirectory("all_monitors")
    zip_file_path <- .download_zip_all_monitors(year = year, temp_directory = temp_directory)
    df <- .import_all_monitors_data(zip_file_path = zip_file_path) |>
        .transform_all_monitors_cols(year = year)
    unlink(temp_directory, recursive = TRUE)
    df
}

#' Get data for all AQS monitors for a given range of years and parameter codes
#'
#' This function downloads and imports data for all AQS monitors for a given range of years and parameter codes.
#'
#' @param start The first year of the range
#' @param end The last year of the range
#' @param parameter_codes A vector of AQS parameter codes
#' @return A tibble with columns for state code, county code, site number, POC, parameter code, parameter name, latitude, longitude, and year
#' @export
get_all_monitors <- function(start, end, parameter_codes) {
    years <- seq(start, end)
    purrr::map(years, .get_all_monitors_by_year) |>
        purrr::list_rbind() |>
        dplyr::filter(parameter_code %in% parameter_codes)
}
