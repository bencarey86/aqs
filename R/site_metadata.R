.download_zip_sites_metadata <- function(temp_directory) {
    zip_file_path <- tempfile(tmpdir = temp_directory, fileext = ".zip")
    utils::download.file(
        url = "https://aqs.epa.gov/aqsweb/airdata/aqs_sites.zip",
        destfile = zip_file_path
    )
    utils::unzip(zip_file_path, exdir = temp_directory)
}

.import_sites_metadata <- function(temp_directory) {
    readr::read_csv(file.path(temp_directory, "aqs_sites.csv")) |>
        janitor::clean_names()
}

.transform_site_metadata_cols <- function(df) {
    df |>
        dplyr::mutate(
            land_use = factor(land_use,
                levels = c(
                    "AGRICULTURAL", "BLIGHTED AREAS", "COMMERCIAL", "DESERT", "FOREST",
                    "INDUSTRIAL", "MILITARY RESERVATION", "MOBILE", "RESIDENTIAL", "UNKNOWN"
                )
            ),
            location_setting = factor(location_setting,
                levels = c("URBAN AND CENTER CITY", "SUBURBAN", "RURAL", "UNKNOWN"), ordered = TRUE
            )
        )
}

.get_site_metadata <- function() {
    temp_directory <- .create_temp_subdirectory("site_metadata")
    .download_zip_sites_metadata(temp_directory = temp_directory)
    df <- .import_sites_metadata(temp_directory = temp_directory) |>
        .transform_site_metadata_cols()
    unlink(temp_directory, recursive = TRUE)
    df
}

.get_regulatory_site_metadata <- function() {
    .get_site_metadata() |>
        dplyr::mutate("aqs_site_id" = stringr::str_c(state_code, county_code, site_number)) |>
        dplyr::select(aqs_site_id, latitude, longitude) |>
        dplyr::distinct()
}
