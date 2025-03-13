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

.define_no2_skip <- function(level = c("site", "monitor")) {
    if (level == "site") {
        skip <- 3
    } else if (level == "monitor") {
        skip <- 1
    }
    skip
}

.define_no2_column_patterns <- function(level = c("site", "monitor")) {
    if (level == "site") {
        col_patterns <- "^state$|state_name|^site$|aqs_site_id|design_value|valid|^completeness"
    } else if (level == "monitor") {
        col_patterns <- "^aqs_site_id$|^poc$"
    }
}

.rename_no2_site_columns <- function(df) {
    colnames(df)[grep("state", colnames(df))] <- "state"
    colnames(df)[grep("site", colnames(df))] <- "aqs_site_id"
    colnames(df)[grep("invalid", colnames(df))] <- "invalid_dv"
    colnames(df)[grep("_valid|^valid|design", colnames(df))] <- "valid_dv"
    colnames(df)[grep("^completeness", colnames(df))] <- "completeness"
    df
}

.filter_no2_valid_dv <- function(df, year) {
    if (year %in% 2013:2023) {
        df |>
            dplyr::filter(!is.na(valid_dv))
    } else if (year == 2012) {
        df |>
            dplyr::filter(!is.na(valid_dv) & completeness == "Y")
    }
}

.get_no2_monitors <- function(year) {
    # General
    temp_directory <- .create_temp_subdirectory("no2")
    url <- .define_no2_url(year)
    excel_file_path <- .download_regulatory_monitor_data(
        year = year,
        url = url,
        temp_directory = temp_directory
    )

    # Sites
    sheet_annual_sites <- .define_no2_annual_sites_sheet(year = year)
    sheet_1h_sites <- .define_no2_1h_sites_sheet(year = year)
    skip_sites <- .define_no2_skip(level = "site")
    site_column_patterns <- .define_no2_column_patterns(level = "site")

    no2_annual <- .import_regulatory_monitor_data(
        excel_file_path = excel_file_path,
        sheet = sheet_annual_sites,
        skip = skip_sites
    ) |>
        .select_cols(site_column_patterns) |>
        .rename_no2_site_columns() |>
        .make_aqs_site_id_char()

    no2_1h <- .import_regulatory_monitor_data(
        excel_file_path = excel_file_path,
        sheet = sheet_1h_sites,
        skip = skip_sites
    ) |>
        .select_cols(site_column_patterns) |>
        .rename_no2_site_columns() |>
        .make_aqs_site_id_char()

    # Monitors
    sheet_monitors <- .define_no2_monitors_sheet(year = year)
    skip_monitors <- .define_no2_skip(level = "monitor")
    monitor_column_patterns <- .define_no2_column_patterns(level = "monitor")
    # Monitor data unavailable for 2012-2015; data for 2016 is used instead
    if (year %in% 2012:2015) {
        temp_directory_2016_monitors <- .create_temp_subdirectory("no2_monitors")
        url_2016_monitors <- .define_no2_url(year = 2016)
        excel_file_path_2016_monitors <- .download_regulatory_monitor_data(
            year = 2016,
            url = url_2016_monitors,
            temp_directory = temp_directory_2016_monitors
        )
        no2_monitors <- .import_regulatory_monitor_data(
            excel_file_path = excel_file_path_2016_monitors,
            sheet = sheet_monitors,
            skip = skip_monitors
        )
        unlink(temp_directory_2016_monitors, recursive = TRUE)
    } else {
        no2_monitors <- .import_regulatory_monitor_data(
            excel_file_path = excel_file_path,
            sheet = sheet_monitors,
            skip = skip_monitors
        )
    }
    no2_monitors <- no2_monitors |>
        .select_cols(monitor_column_patterns) |>
        .make_aqs_site_id_char() |>
        dplyr::distinct()

    # Combined data
    no2_sites <- .combine_standards(df_annual = no2_annual, df_1h = no2_1h) |>
        .filter_no2_valid_dv(year = year) |>
        dplyr::select("aqs_site_id") |>
        dplyr::distinct()
    no2 <- no2_sites |>
        dplyr::left_join(no2_monitors, by = "aqs_site_id") |>
        dplyr::select("aqs_site_id", "poc") |>
        dplyr::distinct() |>
        dplyr::mutate(
            poc = as.integer(poc),
            year = year,
            parameter_name = "Nitrogen dioxide (NO2)"
        )

    # Clean up and output
    unlink(temp_directory, recursive = TRUE)
    no2
}
