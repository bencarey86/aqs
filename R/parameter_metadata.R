.get_parameters <- function() {
    readr::read_csv("https://aqs.epa.gov/aqsweb/documents/codetables/parameter_classes.csv") |>
        janitor::clean_names() |>
        unique()
}

.get_naaqs_parameters <- function() {
    .get_parameters() |>
        dplyr::filter(class_code == "APP_A_PARAMETERS") |>
        dplyr::select("parameter", "parameter_code")
}

.get_aqi_parameters <- function() {
    .get_parameters() |>
        dplyr::filter(class_code %in% c("APP_A_PARAMETERS", "AQI POLLUTANTS")) |>
        dplyr::select("parameter", "parameter_code") |>
        unique()
}
