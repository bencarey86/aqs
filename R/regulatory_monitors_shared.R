.download_regulatory_monitor_data <- function(year, url, temp_directory) {
    excel_file_path <- tempfile(
        tmpdir = temp_directory,
        fileext = ifelse(year >= 2010, ".xlsx", ".xls")
    )
    utils::download.file(
        url = url,
        destfile = excel_file_path,
        mode = "wb"
    )
    excel_file_path
}

.select_cols <- function(df, col_patterns) {
    df |>
        dplyr::select(matches(col_patterns))
}

.make_aqs_site_id_char <- function(df) {
    # Converts aqs_site_id to character and pads with leading zeros if necessary
    # aqs_site_id is sometimes converted to numeric when read in from Excel and leading zeros are dropped
    if (class(df$aqs_site_id) != "character") {
        df$aqs_site_id <- as.character(df$aqs_site_id)
        df$aqs_site_id <- stringr::str_pad(df$aqs_site_id, 9, side = "left", pad = "0")
    }
    df
}
