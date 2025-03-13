.define_o3_url <- function(year) {
    url <- switch(as.character(year),
        "2023" = "https://www.epa.gov/system/files/documents/2024-06/o3_designvalues_2021_2023_final_06_04_24.xlsx",
        "2022" = "https://www.epa.gov/system/files/documents/2023-05/O3_DesignValues_2020_2022_FINAL_05_22_23.xlsx",
        "2021" = "https://www.epa.gov/system/files/documents/2022-05/O3_DesignValues_2019_2021_FINAL_05_25_22.xlsx",
        "2020" = "https://www.epa.gov/sites/default/files/2021-05/o3_designvalues_2018_2020_final_05_11_21.xlsx",
        "2019" = "https://www.epa.gov/sites/default/files/2020-05/o3_designvalues_2017_2019_final_05_26_20.xlsx",
        "2018" = "https://www.epa.gov/sites/default/files/2019-07/ozone_designvalues_20162018_final_06_28_19.xlsx",
        "2017" = "https://www.epa.gov/sites/default/files/2018-07/ozone_designvalues_20152017_final_07_24_18.xlsx",
        "2016" = "https://www.epa.gov/sites/default/files/2017-10/ozone_designvalues_20142016_final_10_02_17_0.xlsx",
        "2015" = "https://www.epa.gov/sites/default/files/2016-07/ozone_designvalues_20132015_final_07_29_16.xlsx",
        "2014" = "https://www.epa.gov/sites/default/files/2016-09/ozone_designvalues_20122014_final_08_03_15.xlsx",
        "2013" = "https://www.epa.gov/sites/default/files/2016-09/ozone_designvalues_20112013_final_08_01_14.xlsx",
        "2012" = "https://www.epa.gov/sites/default/files/2016-09/ozone_designvalues_20102012_final_02_07_14.xlsx",
        "2011" = "https://www.epa.gov/sites/default/files/2016-09/ozone_designvalues_20092011_final_08_06_12.xlsx",
        "2010" = "https://www.epa.gov/sites/default/files/2016-09/ozone_designvalues_20082010_update.xlsx",
        "2009" = "https://www.epa.gov/sites/default/files/2016-09/ozone_designvalues_20072009_finalr03dec10.xls",
        "2008" = "https://www.epa.gov/sites/default/files/2016-09/dv_ozone_2006_2008.xls",
        "2007" = "https://www.epa.gov/sites/default/files/2016-09/dv_ozone_2005_2007.xls",
        "2006" = "https://www.epa.gov/sites/default/files/2016-09/dv_ozone_2004_2006.xls",
        "2005" = "https://www.epa.gov/sites/default/files/2016-09/dv_ozone_2003_2005.xls"
    )
}

.define_o3_sites_sheet <- function(year) {
    sheet <- dplyr::case_when(
        year %in% 2019:2023 ~ "Table5. Site Status",
        year %in% 2015:2018 ~ "Table5. Monitor Status"
    )
}

.define_o3_monitors_sheet <- function(year) {
    sheet <- dplyr::case_when(
        year %in% 2019:2023 ~ "Appendix. Monitor Information",
        year == 2018 ~ "Monitor Metadata",
        year == 2017 ~ "Technical Information",
        year %in% 2015:2016 ~ "Appendix. Monitor Information",
        year %in% 2013:2014 ~ "Table5. Monitor Status",
        year == 2012 ~ "Table5"
    )
}

.define_o3_skip <- function(year, level = c("site", "monitor")) {
    skip <- 3
    if (year %in% 2015:2023 && level == "monitor") {
        skip <- 1
    }
    skip
}

.define_o3_column_patterns <- function() {
    col_patterns <- "aqs_site_id|poc|valid|invalid|design_value_ppm"
}

.rename_o3_columns <- function(df) {
    colnames(df)[grep("invalid|incomplete", colnames(df))] <- "invalid_dv"
    colnames(df)[grep("_valid|design_value", colnames(df))] <- "valid_dv"
    df
}

.filter_o3_valid_dv <- function(df) {
    df |>
        dplyr::filter(!is.na(valid_dv))
}

.get_o3_monitors <- function(year) {
    # General
    temp_directory <- .create_temp_subdirectory("o3")
    url <- .define_o3_url(year)
    excel_file_path <- .download_regulatory_monitor_data(
        year = year,
        url = url,
        temp_directory = temp_directory
    )
    sheet_sites <- .define_o3_sites_sheet(year)
    skip_sites <- .define_o3_skip(year, level = "site")
    sheet_monitors <- .define_o3_monitors_sheet(year)
    skip_monitors <- .define_o3_skip(year, level = "monitor")
    column_patterns <- .define_o3_column_patterns()

    # Validity deteremined at site level for 2015-2023, monitor level for 2012-2014
    # No monitor data available for 2015; data for 2016 is used instead
    if (year %in% 2015:2023) {
        o3_sites <- .import_regulatory_monitor_data(
            excel_file_path = excel_file_path,
            sheet = sheet_sites,
            skip = skip_sites
        ) |>
            .select_cols(column_patterns) |>
            .rename_o3_columns() |>
            .make_aqs_site_id_char() |>
            .filter_o3_valid_dv() |>
            dplyr::select("aqs_site_id") |>
            dplyr::distinct()
    }

    if (year %in% c(2012:2014, 2016:2023)) {
        o3_monitors <- .import_regulatory_monitor_data(
            excel_file_path = excel_file_path,
            sheet = sheet_monitors,
            skip = skip_monitors
        )
    } else if (year == 2015) {
        temp_directory_2016 <- .create_temp_subdirectory("o3_2016")
        url_2016 <- .define_o3_url(2016)
        excel_file_path_2016 <- .download_regulatory_monitor_data(
            year = 2016,
            url = url_2016,
            temp_directory = temp_directory_2016
        )
        o3_monitors <- .import_regulatory_monitor_data(
            excel_file_path = excel_file_path_2016,
            sheet = sheet_monitors,
            skip = skip_monitors
        )
    }

    o3_monitors <- o3_monitors |>
        .select_cols(column_patterns) |>
        .rename_o3_columns() |>
        .convert_site_id_poc_column_types()

    if (year %in% 2015:2023) {
        o3_monitors <- o3_monitors |>
            dplyr::right_join(o3_sites, by = "aqs_site_id")
    } else if (year %in% 2012:2014) {
        o3_monitors <- o3_monitors |>
            .filter_o3_valid_dv()
    }

    o3_monitors <- o3_monitors |>
        dplyr::select("aqs_site_id", "poc") |>
        dplyr::distinct() |>
        dplyr::mutate(
            year = year,
            parameter_name = "Ozone"
        )

    # Clean up and output
    unlink(temp_directory, recursive = TRUE)
    o3_monitors
}
