.define_co_url <- function(year) {
    url <- switch(as.character(year),
        "2023" = "https://www.epa.gov/system/files/documents/2024-06/co_designvalues_2022_2023_final_05_07_24.xlsx",
        "2022" = "https://www.epa.gov/system/files/documents/2023-05/CO_DesignValues_2021_2022_FINAL_05_05_23.xlsx",
        "2021" = "https://www.epa.gov/system/files/documents/2022-05/CO_DesignValues_2020_2021_FINAL_05_04_22.xlsx",
        "2020" = "https://www.epa.gov/sites/default/files/2021-05/co_designvalues_2019_2020_final_05_25_21.xlsx",
        "2019" = "https://www.epa.gov/sites/default/files/2020-05/co_designvalues_2018_2019_final_05_07_20.xlsx",
        "2018" = "https://www.epa.gov/sites/default/files/2019-07/co_designvalues_2018_final_06_27_19.xlsx",
        "2017" = "https://www.epa.gov/sites/default/files/2018-07/co_designvalues_2017_final_07_24_18_0.xlsx",
        "2016" = "https://www.epa.gov/sites/default/files/2017-07/co_designvalues_2016_final_07_19_17.xlsx",
        "2015" = "https://www.epa.gov/sites/default/files/2016-07/co_designvalues_20132015_final_07_29_16.xlsx",
        "2014" = "https://www.epa.gov/sites/default/files/2016-09/co_designvalues_2014_final_08_06_15.xlsx",
        "2013" = "https://www.epa.gov/sites/default/files/2016-09/co_designvalues_20112013_final_08_13_14.xlsx",
        "2012" = "https://www.epa.gov/sites/default/files/2016-09/co_designvalues_20102012_final_10_22_2013.xlsx",
        "2011" = "https://www.epa.gov/sites/default/files/2016-09/co_designvalues_20092011_final_7_26_12.xlsx",
        "2010" = "https://www.epa.gov/sites/default/files/2016-09/co_designvalues_2010_final.xlsx",
        "2009" = "https://www.epa.gov/sites/default/files/2016-09/co_designvalues_2009_final.xls",
        "2008" = "https://www.epa.gov/sites/default/files/2016-09/dv_co_2006_2008.xls",
        "2007" = "https://www.epa.gov/sites/default/files/2016-09/dv_co_2005_2007.xls",
        "2006" = "https://www.epa.gov/sites/default/files/2016-09/dv_co_2004_2006.xls",
        "2005" = "https://www.epa.gov/sites/default/files/2016-09/dv_ozone_2003_2005.xls"
    )
}

.define_co_8h_monitors_sheet <- function(year) {
    sheet <- dplyr::case_when(
        year %in% 2019:2023 ~ "Table5a. Monitor Status 8hr",
        year %in% 2014:2018 ~ "Table5a. Monitor 8hr",
        year == 2013 ~ "Table 5a. Monitor 8hr",
        year == 2012 ~ "Table5a"
    )
}

.define_co_8h_sites_sheet <- function(year) {
    sheet <- dplyr::case_when(
        year == 2013 ~ " Table 6a.  SiteTrends 8hr",
        year == 2012 ~ "Table6a"
    )
}

.define_co_1h_monitors_sheet <- function(year) {
    sheet <- dplyr::case_when(
        year %in% 2019:2023 ~ "Table5b. Monitor Status 1hr",
        year %in% 2013:2018 ~ "Table5b. Monitor 1hr",
        year == 2012 ~ "Table5b"
    )
}

.define_co_1h_sites_sheet <- function(year) {
    sheet <- dplyr::case_when(
        year == 2013 ~ "Table6b.  Site Trends 1hr",
        year == 2012 ~ "Table6b"
    )
}

.import_co_data <- function(year, excel_file_path, sheet) {
    readxl::read_excel(
        path = excel_file_path,
        sheet = sheet,
        skip = ifelse(year == 2013, 2, 3)
    ) |>
        janitor::clean_names()
}

#' Define column patterns to use for selecting columns
#'
#' This function defines column patterns to use for selecting columns from the
#' regulatory monitor data. The column patterns are defined based on the year
#' and the level of data (site or monitor).
#'
#' @param year The year of the data
#' @param level The level of data (site or monitor)
#' @return A character vector of column patterns
#' @keywords internal
.define_co_column_patterns <- function(year, level = c("site", "monitor")) {
    if (level == "monitor") {
        col_patterns <- "^site$|aqs_site_id|poc|^valid|x20[0-9]{2}_[0-9]_hour_design_value"
    } else if (level == "site") {
        col_patterns <- ifelse(year == 2012, "^site$|2012", "^site$|2013")
    }
}

.rename_co_columns <- function(df) {
    colnames(df)[grep("site", colnames(df))] <- "aqs_site_id"
    colnames(df)[grep("design_value", colnames(df))] <- "valid_dv"
    df
}

.combine_co_standards <- function(df_8h, df_1h) {
    # Ensure valid_dv is numeric in both DFs before binding
    df_8h$valid_dv <- as.numeric(df_8h$valid_dv)
    df_1h$valid_dv <- as.numeric(df_1h$valid_dv)
    combined_df <- rbind(df_8h, df_1h)
}

.filter_co_valid_dv <- function(df) {
    df |>
        dplyr::filter(!is.na(valid_dv))
}

.get_co_monitors <- function(year) {
    temp_directory <- .create_temp_subdirectory("co")
    url <- .define_co_url(year = year)
    excel_file_path <- .download_regulatory_monitor_data(
        year = year,
        url = url,
        temp_directory = temp_directory
    )
    sheet_8h_monitors <- .define_co_8h_monitors_sheet(year = year)
    sheet_1h_monitors <- .define_co_1h_monitors_sheet(year = year)
    monitor_column_patterns <- .define_co_column_patterns(year = year, level = "monitor")

    co_8h <- .import_co_data(
        year = year,
        excel_file_path = excel_file_path,
        sheet = sheet_8h_monitors
    ) |>
        .select_cols(monitor_column_patterns) |>
        .rename_co_columns() |>
        .make_aqs_site_id_char()

    co_1h <- .import_co_data(
        year = year,
        excel_file_path = excel_file_path,
        sheet = sheet_1h_monitors
    ) |>
        .select_cols(monitor_column_patterns) |>
        .rename_co_columns() |>
        .make_aqs_site_id_char()

    if (year %in% c(2012, 2013)) {
        sheet_8h_sites <- .define_co_8h_sites_sheet(year)
        sheet_1h_sites <- .define_co_1h_sites_sheet(year)
        site_column_patterns <- .define_co_column_patterns(year = year, level = "site")
        co_8h_sites <- .import_co_data(
            year = year,
            excel_file_path = excel_file_path,
            sheet = sheet_8h_sites
        ) |>
            .select_cols(site_column_patterns) |>
            .rename_co_columns() |>
            .make_aqs_site_id_char()

        co_1h_sites <- .import_co_data(
            year = year,
            excel_file_path = excel_file_path,
            sheet = sheet_1h_sites
        ) |>
            .select_cols(site_column_patterns) |>
            .rename_co_columns() |>
            .make_aqs_site_id_char()

        co_8h <- co_8h |>
            dplyr::right_join(co_8h_sites, by = "aqs_site_id")
        co_1h <- co_1h |>
            dplyr::right_join(co_1h_sites, by = "aqs_site_id")
    }

    co <- .combine_co_standards(co_8h, co_1h) |>
        .filter_co_valid_dv() |>
        dplyr::select("aqs_site_id", "poc") |>
        dplyr::mutate(
            poc = as.integer(poc),
            year = year,
            parameter_name = "Carbon monoxide"
        )

    unlink(temp_directory, recursive = TRUE)
    co
}
