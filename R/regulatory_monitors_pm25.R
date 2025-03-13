.define_pm25_url <- function(year) {
    url <- switch(as.character(year),
        "2023" = "https://www.epa.gov/system/files/documents/2024-08/pm25_designvalues_2021_2023_final_08_08_24_0.xlsx",
        "2022" = "https://www.epa.gov/system/files/documents/2023-05/PM25_DesignValues_2020_2022_FINAL_05_23_23.xlsx",
        "2021" = "https://www.epa.gov/system/files/documents/2022-05/PM25_DesignValues_2019_2021_FINAL_05_24_22.xlsx",
        "2020" = "https://www.epa.gov/sites/default/files/2021-05/pm25_designvalues_2018_2020_final_05_24_21.xlsx",
        "2019" = "https://www.epa.gov/sites/default/files/2020-05/pm25_designvalues_2017_2019_final_05_26_20.xlsx",
        "2018" = "https://www.epa.gov/sites/default/files/2019-12/pm25_designvalues_20162018_final_12_03_19.xlsx",
        "2017" = "https://www.epa.gov/sites/default/files/2018-07/pm25_designvalues_20152017_final_07_24_18.xlsx",
        "2016" = "https://www.epa.gov/sites/default/files/2017-07/pm25_designvalues_20142016_final_07_14_17.xlsx",
        "2015" = "https://www.epa.gov/sites/default/files/2016-07/pm25_designvalues_20132015_final_07_29_16.xlsx",
        "2014" = "https://www.epa.gov/sites/default/files/2016-09/pm25_designvalues_20122014_final_08_19_15.xlsx",
        "2013" = "https://www.epa.gov/sites/default/files/2016-09/pm25_designvalues_20112013_final_08_28_14.xlsx",
        "2012" = "https://www.epa.gov/sites/default/files/2016-09/pm25_designvalues_20102012_final_12_12_13.xlsx",
        "2011" = "https://www.epa.gov/sites/default/files/2016-09/pm25_designvalues_20092011_final_07_30_12.xlsx",
        "2010" = "https://www.epa.gov/sites/default/files/2016-09/pm25_designvalues_20082010_finalrevised.xlsx",
        "2009" = "https://www.epa.gov/sites/default/files/2016-09/pm25dv20072009finalrevised.xls",
        "2008" = "https://www.epa.gov/sites/default/files/2016-09/dv_pm25_2006_2008rev102809.xls",
        "2007" = "https://www.epa.gov/sites/default/files/2016-09/dv_pm25_2005_2007.xls"
    )
}

.define_pm25_annual_sites_sheet <- function(year) {
    dplyr::case_when(
        year %in% 2019:2023 ~ "Table5a. Site Status Ann",
        year %in% 2017:2018 ~ "Table 5a, Ann Site Listing",
        year == 2016 ~ "Table 5, 2014-2016 Site Listing",
        year == 2015 ~ "Table 5, 2013-2015 Site Listing",
        year == 2014 ~ "Table 5, 2012-2014 Site Listing",
        year == 2013 ~ "Table 5, 2011-2013 Site Listing",
        year == 2012 ~ "Table 5, 2010-2012 site listing"
    )
}

.define_pm25_24h_sites_sheet <- function(year) {
    dplyr::case_when(
        year %in% 2019:2023 ~ "Table5b. Site Status 24hr",
        year %in% 2017:2018 ~ "Table 5b, 24hr Site Listing",
        year == 2016 ~ "Table 5, 2014-2016 Site Listing",
        year == 2015 ~ "Table 5, 2013-2015 Site Listing",
        year == 2014 ~ "Table 5, 2012-2014 Site Listing",
        year == 2013 ~ "Table 5, 2011-2013 Site Listing",
        year == 2012 ~ "Table 5, 2010-2012 site listing"
    )
}

.define_pm25_monitors_sheet <- function(year) {
    dplyr::case_when(
        # 2012-2015 do not have monitor data, but 2016 so given same sheet name
        year %in% 2019:2023 ~ "Appendix. Monitor Information",
        year == 2018 ~ "Monitor Metadata",
        year %in% 2012:2017 ~ "Technical Information"
    )
}

.define_pm25_skip <- function(level = c("site", "monitor")) {
    ifelse(level == "site", 3, 1)
}

.define_pm25_column_patterns <- function(standard = c("annual", "24h")) {
    ifelse(
        standard == "annual",
        "aqs_site_id|site_id|^site$|^valid_|^x[0-9]{4}_[0-9]{4}_annual_design_value|validity$",
        "aqs_site_id|site_id|^site$|^valid_|^x[0-9]{4}_[0-9]{4}_(24h|24_hr)_design_value|validity$"
    )
}

.rename_pm25_site_columns <- function(df) {
    colnames(df)[grep("site", colnames(df))] <- "aqs_site_id"
    colnames(df)[grep("validity", colnames(df))] <- "validity"
    colnames(df)[grep("valid_|[0-9]{4}_[0-9]{4}.*design_value", colnames(df))] <- "valid_dv"
    df
}

.filter_pm25_valid_dv <- function(df, year) {
    if (year %in% 2013:2023) {
        df |>
            dplyr::filter(!is.na(valid_dv))
    } else if (year == 2012) {
        df |>
            dplyr::filter(!is.na(valid_dv) & stringr::str_detect(validity, "Y"))
    }
}

.get_pm25_monitors <- function(year) {
    temp_directory <- .create_temp_subdirectory("pm25")
    url <- .define_pm25_url(year)
    excel_file_path <- .download_regulatory_monitor_data(
        year = year,
        url = url,
        temp_directory = temp_directory
    )
    sheet_sites_annual <- .define_pm25_annual_sites_sheet(year)
    sheet_sites_24h <- .define_pm25_24h_sites_sheet(year)
    skip_sites <- .define_pm25_skip(level = "site")
    column_patterns_annual <- .define_pm25_column_patterns(standard = "annual")
    column_patterns_24h <- .define_pm25_column_patterns(standard = "24h")
    pm25_sites_annual <- .import_regulatory_monitor_data(
        excel_file_path = excel_file_path,
        sheet = sheet_sites_annual,
        skip = skip_sites
    ) |>
        .select_cols(column_patterns_annual) |>
        .rename_pm25_site_columns() |>
        .make_aqs_site_id_char()
    pm25_sites_24h <- .import_regulatory_monitor_data(
        excel_file_path = excel_file_path,
        sheet = sheet_sites_24h,
        skip = skip_sites
    ) |>
        .select_cols(column_patterns_24h) |>
        .rename_pm25_site_columns() |>
        .make_aqs_site_id_char()

    sheet_monitors <- .define_pm25_monitors_sheet(year)
    skip_monitors <- .define_pm25_skip(level = "monitor")
    if (year %in% 2012:2015) {
        temp_directory_2016 <- .create_temp_subdirectory("pm25_2016")
        url_2016 <- .define_pm25_url(2016)
        excel_file_path_2016 <- .download_regulatory_monitor_data(
            year = 2016,
            url = url_2016,
            temp_directory = temp_directory_2016
        )
        pm25_monitors <- .import_regulatory_monitor_data(
            excel_file_path = excel_file_path_2016,
            sheet = sheet_monitors,
            skip = skip_monitors
        )
        unlink(temp_directory_2016, recursive = TRUE)
    } else {
        pm25_monitors <- .import_regulatory_monitor_data(
            excel_file_path = excel_file_path,
            sheet = sheet_monitors,
            skip = skip_monitors
        )
    }
    pm25_monitors <- pm25_monitors |>
        dplyr::select("aqs_site_id", "poc") |>
        .make_aqs_site_id_char() |>
        dplyr::distinct()

    pm25 <- pm25_sites_annual |>
        .combine_standards(pm25_sites_24h) |>
        .filter_pm25_valid_dv(year) |>
        dplyr::distinct() |>
        dplyr::left_join(pm25_monitors, by = "aqs_site_id") |>
        dplyr::select("aqs_site_id", "poc") |>
        dplyr::distinct() |>
        dplyr::mutate(
            poc = as.integer(poc),
            year = year,
            parameter_name = "PM2.5"
        )

    unlink(temp_directory, recursive = TRUE)
    pm25
}
