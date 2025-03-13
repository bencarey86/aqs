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

.import_regulatory_monitor_data <- function(excel_file_path, sheet, skip) {
    readxl::read_excel(
        path = excel_file_path,
        sheet = sheet,
        skip = skip
    ) |>
        janitor::clean_names()
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

.convert_site_id_poc_column_types <- function(df) {
    df$aqs_site_id <- as.character(df$aqs_site_id)
    df$aqs_site_id <- stringr::str_pad(df$aqs_site_id, 9, side = "left", pad = "0")
    df$poc <- as.integer(df$poc)
    df
}

.combine_standards <- function(df_1, df_2) {
    # Ensure valid_dv is numeric in both DFs before binding
    df_1$valid_dv <- as.numeric(df_1$valid_dv)
    df_2$valid_dv <- as.numeric(df_2$valid_dv)
    combined_df <- rbind(df_1, df_2)
}

.drop_rows_all_na <- function(df) {
    rows_to_keep <- rowSums(!is.na(df)) > 0
    df <- df[rows_to_keep, ]
    rownames(df) <- NULL
    df
}
