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
