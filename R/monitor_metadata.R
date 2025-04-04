.download_zip_monitor_metadata <- function(temp_directory) {
    zip_file_path <- tempfile(tmpdir = temp_directory, fileext = ".zip")
    utils::download.file(
        url = "https://aqs.epa.gov/aqsweb/airdata/aqs_monitors.zip",
        destfile = zip_file_path
    )
    utils::unzip(zip_file_path, exdir = temp_directory)
}

.import_monitor_metadata <- function(temp_directory) {
    readr::read_csv(file.path(temp_directory, "aqs_monitors.csv")) |>
        janitor::clean_names()
}

.get_monitor_metadata <- function() {
    temp_directory <- .create_temp_subdirectory("monitor_metadata")
    .download_zip_monitor_metadata(temp_directory = temp_directory)
    df <- .import_monitor_metadata(temp_directory = temp_directory)
    unlink(temp_directory, recursive = TRUE)
    df
}

.clean_monitor_metadata_parameter_names <- function(df) {
    df |>
        dplyr::mutate(parameter_name = dplyr::case_when(
            parameter_name == "Lead (TSP) LC" ~ "Lead",
            parameter_name == "Lead PM10 LC FRM/FEM" ~ "Lead",
            parameter_name == "PM10 Total 0-10um STP" ~ "PM10",
            parameter_name == "PM2.5 - Local Conditions" ~ "PM2.5",
            parameter_name == "Acceptable PM2.5 AQI & Speciation Mass" ~ "PM2.5",
            TRUE ~ parameter_name
        ))
}

.select_monitor_metadata_columns <- function(df, monitor_type = c("aqi_naaqs", "regulatory")) {
    df <- df |>
        dplyr::mutate("aqs_site_id" = stringr::str_c(state_code, county_code, site_number)) |>
        dplyr::select(
            aqs_site_id,
            poc,
            parameter_code,
            parameter_name,
            measurement_scale,
            measurement_scale_definition,
            monitoring_objective
        ) |>
        dplyr::distinct()
    if (monitor_type == "regulatory") {
        df <- df |>
            dplyr::select(-"parameter_code")
    }
    df
}

.get_aqi_naaqs_monitor_monitor_metadata <- function(raw_monitor_metadata) {
    raw_monitor_metadata |>
        .filter_naaqs_aqi_parameters() |>
        .clean_monitor_metadata_parameter_names() |>
        .select_monitor_metadata_columns(monitor_type = "aqi_naaqs") |>
        dplyr::distinct()
}

.get_regulatory_monitor_metadata <- function(raw_monitor_metadata) {
    raw_monitor_metadata |>
        .filter_regulatory_parameters() |>
        .clean_monitor_metadata_parameter_names() |>
        .select_monitor_metadata_columns(monitor_type = "regulatory") |>
        dplyr::distinct()
}
