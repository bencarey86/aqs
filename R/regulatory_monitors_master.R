.get_regulatory_monitors <- function(start, end) {
    years <- start:end
    purrr::map(years, function(year) {
        co <- .get_co_monitors(year)
        lead <- .get_lead_monitors(year)
        no2 <- .get_no2_monitors(year)
        o3 <- .get_o3_monitors(year)
        pm10 <- .get_pm10_monitors(year)
        pm25 <- .get_pm25_monitors(year)
        so2 <- .get_so2_monitors(year)
        dplyr::bind_rows(co, lead, no2, o3, pm10, pm25, so2)
    }) |>
        dplyr::bind_rows()
}

.join_regulatory_monitors_site_metadata <- function(monitors, site_metadata) {
    monitors |>
        dplyr::left_join(site_metadata, by = "aqs_site_id")
}

.join_regulatory_monitors_monitor_metadata <- function(monitors, monitor_metadata) {
    monitors |>
        dplyr::left_join(monitor_metadata, by = c("aqs_site_id", "poc", "parameter_name"))
}

#' Get data on all regulatory monitors that were active in a given range of years
#'
#' This function downloads and imports data for all monitors for which there is a valid design value
#' for a given year in a given range of years. It joins the data with site metadata and monitor metadata
#' to include additional information about the monitors (e.g., latitude, longitude, measurement scale,
#' monitoring objective).
#'
#' @param start The first year of the range
#' @param end The last year of the range
#' @param site_metadata A tibble with site metadata
#' @param monitor_metadata A tibble with monitor metadata
#' @return A tibble with columns for AQS site ID, POC, parameter name, latitude, longitude, measurement scale, measurement scale definition, monitoring objective, and year
#' @export
get_regulatory_monitors <- function(start, end, site_metadata, monitor_metadata) {
    .get_regulatory_monitors(start = start, end = end) |>
        .join_regulatory_monitors_site_metadata(site_metadata = site_metadata) |>
        .join_regulatory_monitors_monitor_metadata(monitor_metadata = monitor_metadata) |>
        dplyr::select(
            "aqs_site_id",
            "poc",
            "parameter_name",
            "latitude",
            "longitude",
            "measurement_scale",
            "measurement_scale_definition",
            "monitoring_objective",
            "year"
        ) |>
        dplyr::distinct() |>
        .split_vintages()
}
