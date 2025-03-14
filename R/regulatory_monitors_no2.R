.define_no2_url <- function(year) {
    url <- switch(as.character(year),
        "2023" = "https://www.epa.gov/system/files/documents/2024-06/no2_designvalues_2021_2023_final_05_07_24.xlsx",
        "2022" = "https://www.epa.gov/system/files/documents/2023-05/NO2_DesignValues_2020_2022_FINAL_05_02_23.xlsx",
        "2021" = "https://www.epa.gov/system/files/documents/2022-05/NO2_DesignValues_2019_2021_FINAL_05_20_22.xlsx",
        "2020" = "https://www.epa.gov/sites/default/files/2021-05/no2_designvalues_2018_2020_final_05_21_21.xlsx",
        "2019" = "https://www.epa.gov/sites/default/files/2020-05/no2_designvalues_2017_2019_final_05_05_20.xlsx",
        "2018" = "https://www.epa.gov/sites/default/files/2019-07/no2_designvalues_20162018_final_06_27_19_0.xlsx",
        "2017" = "https://www.epa.gov/sites/default/files/2018-07/no2_designvalues_20152017_final_07_24_18.xlsx",
        "2016" = "https://www.epa.gov/sites/default/files/2017-07/no2_designvalues_20142016_final_06_28_17.xlsx",
        "2015" = "https://www.epa.gov/sites/default/files/2016-07/no2_designvalues_20132015_final_07_29_16.xlsx",
        "2014" = "https://www.epa.gov/sites/default/files/2016-09/no2_designvalues_20122014_final_08_06_15.xlsx",
        "2013" = "https://www.epa.gov/sites/default/files/2016-09/no2_designvalues_20112013_final_8_06_2014.xlsx",
        "2012" = "https://www.epa.gov/sites/default/files/2016-09/no2_designvalues_20102012_final_10_22_2013.xlsx",
        "2011" = "https://www.epa.gov/sites/default/files/2016-09/no2_designvalues_20092011_final_7_26_12.xlsx",
        "2010" = "https://www.epa.gov/sites/default/files/2016-09/no2_designvalues_20082010_update.xlsx",
        "2009" = "https://www.epa.gov/sites/default/files/2016-09/no2_designvalues_20072009_final.xls",
        "2008" = "https://www.epa.gov/sites/default/files/2016-09/dv_no2_2006_2008.xls",
        "2007" = "https://www.epa.gov/sites/default/files/2016-09/dv_no2_2005_2007.xls"
    )
}

.define_no2_file_setup <- function(year, directory_name) {
    temp_directory <- .create_temp_subdirectory(directory_name)
    url <- .define_no2_url(year)
    excel_file_path <- .download_regulatory_monitor_data(
        year = year,
        url = url,
        temp_directory = temp_directory
    )
    list(
        temp_directory = temp_directory,
        url = url,
        excel_file_path = excel_file_path
    )
}

.define_no2_annual_sites_sheet <- function(year) {
    sheet <- dplyr::case_when(
        year %in% 2014:2023 ~ "Table5a. Site Status Annual",
        year == 2013 ~ "Table 5a. Site Annual",
        year == 2012 ~ "Table5a"
    )
}

.define_no2_1h_sites_sheet <- function(year) {
    sheet <- dplyr::case_when(
        year == 2023 ~ "Table5b. Site Status 1-hour",
        year %in% 2014:2022 ~ "Table 5b. Site Status 1-hour",
        year == 2013 ~ "Table 5b. Site 1hr",
        year == 2012 ~ "Table5b"
    )
}

.define_no2_monitors_sheet <- function(year) {
    sheet <- dplyr::case_when(
        year %in% 2019:2023 ~ "Appendix. Monitor Information",
        year == 2018 ~ "Monitor Metadata",
        year == 2017 ~ "Technical Information",
        year %in% 2012:2016 ~ "Appendix. Monitor Information"
    )
}

.define_no2_excel_sheets <- function(year) {
    list(
        sites_annual = .define_no2_annual_sites_sheet(year = year),
        sites_1h = .define_no2_1h_sites_sheet(year = year),
        monitors = .define_no2_monitors_sheet(year = year)
    )
}

.define_no2_excel_rows_to_skip <- function() {
    list(
        sites = 3,
        monitors = 1
    )
}

.define_no2_column_patterns <- function() {
    list(
        sites = "^site$|aqs_site_id|^valid$|^completeness|^x2010_2012_1_hr_design_value|x2012_annual_design_value|^valid_[0-9{4}_[0-9]{4}|^valid_20[1-9]{2}_annual_design_value",
        monitors = "^aqs_site_id$|^poc$"
    )
}

.set_up_no2_processing <- function(year) {
    file_definitions <- .define_no2_file_setup(year = year, directory_name = "no2_design_values")
    sheets <- .define_no2_excel_sheets(year = year)
    skips <- .define_no2_excel_rows_to_skip()
    column_patterns <- .define_no2_column_patterns()
    list(
        file_definitions = file_definitions,
        sheets = sheets,
        skips = skips,
        column_patterns = column_patterns
    )
}

# Preliminary data processing --------------------------------------------------
.rename_no2_site_columns <- function(df) {
    colnames(df)[grep("site", colnames(df))] <- "aqs_site_id"
    colnames(df)[grep("invalid", colnames(df))] <- "invalid_dv"
    colnames(df)[grep("_valid|^valid|design", colnames(df))] <- "valid_dv"
    colnames(df)[grep("^completeness", colnames(df))] <- "completeness"
    df
}

.process_initial_no2_sites <- function(excel_file_path, sheet, skip, column_patterns) {
    .import_regulatory_monitor_data(
        excel_file_path = excel_file_path,
        sheet = sheet,
        skip = skip
    ) |>
        .select_cols(column_patterns) |>
        .rename_no2_site_columns() |>
        .make_aqs_site_id_char() |>
        .drop_rows_all_na() |>
        dplyr::distinct()
}

.process_initial_no2_monitors <- function(excel_file_path, sheet, skip, column_patterns) {
    .import_regulatory_monitor_data(
        excel_file_path = excel_file_path,
        sheet = sheet,
        skip = skip
    ) |>
        .select_cols(column_patterns) |>
        .convert_site_id_poc_column_types() |>
        .drop_rows_all_na() |>
        dplyr::distinct()
}

.get_initial_no2_data <- function(year, setup_definitions) {
    # Sites
    sites_annual <- .process_initial_no2_sites(
        excel_file_path = setup_definitions$file_definitions$excel_file_path,
        sheet = setup_definitions$sheets$sites_annual,
        skip = setup_definitions$skips$sites,
        column_patterns = setup_definitions$column_patterns$sites
    )
    sites_1h <- .process_initial_no2_sites(
        excel_file_path = setup_definitions$file_definitions$excel_file_path,
        sheet = setup_definitions$sheets$sites_1h,
        skip = setup_definitions$skips$sites,
        column_patterns = setup_definitions$column_patterns$sites
    )
    # Monitors
    # Monitor data unavailable for 2012-2015; data for 2016 is used instead
    if (year %in% 2016:2023) {
        monitors <- .process_initial_no2_monitors(
            excel_file_path = setup_definitions$file_definitions$excel_file_path,
            sheet = setup_definitions$sheets$monitors,
            skip = setup_definitions$skips$monitors,
            column_patterns = setup_definitions$column_patterns$monitors
        )
    } else if (year %in% 2012:2015) {
        setup_definitions_2016 <- .set_up_no2_processing(year = 2016)
        monitors <- .process_initial_no2_monitors(
            excel_file_path = setup_definitions_2016$file_definitions$excel_file_path,
            sheet = setup_definitions_2016$sheets$monitors,
            skip = setup_definitions_2016$skips$monitors,
            column_patterns = setup_definitions_2016$column_patterns$monitors
        )
        unlink(setup_definitions_2016$file_definitions$temp_directory, recursive = TRUE)
    }

    # Return
    list(
        sites_annual = sites_annual,
        sites_1h = sites_1h,
        monitors = monitors
    )
}

# Validity determination -------------------------------------------------------
.filter_no2_valid_dv <- function(df, year) {
    if (year %in% 2013:2023) {
        df |>
            dplyr::filter(!is.na(valid_dv))
    } else if (year == 2012) {
        df |>
            dplyr::filter(!is.na(valid_dv) & completeness == "Y")
    }
}

.get_unique_valid_no2_sites <- function(df, year) {
    df |>
        .filter_no2_valid_dv(year = year) |>
        dplyr::select("aqs_site_id") |>
        dplyr::distinct()
}

.determine_no2_validity <- function(year, initial_data_list) {
    list(
        sites_annual = .get_unique_valid_no2_sites(initial_data_list$sites_annual, year = year),
        sites_1h = .get_unique_valid_no2_sites(initial_data_list$sites_1h, year = year)
    )
}

# DF combination ----------------------------------------------------------------
# Standards combined and sites and monitors joined

.join_no2_sites_and_monitors <- function(valid_data_list, initial_data_list) {
    no2_annual <- dplyr::left_join(
        valid_data_list$sites_annual,
        initial_data_list$monitors,
        by = "aqs_site_id"
    )
    no2_1h <- dplyr::left_join(
        valid_data_list$sites_1h,
        initial_data_list$monitors,
        by = "aqs_site_id"
    )
    list(
        no2_annual = no2_annual,
        no2_1h = no2_1h
    )
}

.combine_no2_data <- function(valid_data_list, initial_data_list) {
    joined_list <- .join_no2_sites_and_monitors(
        valid_data_list = valid_data_list,
        initial_data_list = initial_data_list
    )
    rbind(joined_list$no2_annual, joined_list$no2_1h) |>
        dplyr::distinct()
}

# Main function -----------------------------------------------------------------
.get_no2_monitors <- function(year) {
    setup_definitions <- .set_up_no2_processing(year = year)
    initial_data <- .get_initial_no2_data(year = year, setup_definitions = setup_definitions)
    valid_data <- .determine_no2_validity(year = year, initial_data_list = initial_data)
    no2 <- .combine_no2_data(valid_data_list = valid_data, initial_data_list = initial_data) |>
        dplyr::mutate(
            year = year,
            parameter_name = "Nitrogen dioxide (NO2)"
        )
    unlink(setup_definitions$file_definitions$temp_directory, recursive = TRUE)
    no2
}
