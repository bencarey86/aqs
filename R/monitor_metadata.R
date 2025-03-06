.download_zip_monitor_metadata <- function(temp_directory) {
    zip_file_path <- tempfile(tmpdir = temp_directory, fileext = ".zip")
    utils::download.file(
        url = "https://aqs.epa.gov/aqsweb/airdata/aqs_monitors.zip",
        destfile = zip_file_path
    )
    unzip(zip_file_path, exdir = temp_directory)
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
