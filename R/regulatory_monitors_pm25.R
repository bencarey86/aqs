# Set up -----------------------------------------------------------------------
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

.define_pm25_file_setup <- function(year, directory_name) {
    temp_directory <- .create_temp_subdirectory(directory_name)
    url <- .define_pm25_url(year)
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
        # 2012-2015 do not have monitor data
        # 2016 monitor data is used for 2012-2015 so 2012-2015 are given same sheet name
        year %in% 2019:2023 ~ "Appendix. Monitor Information",
        year == 2018 ~ "Monitor Metadata",
        year %in% 2012:2017 ~ "Technical Information"
    )
}

.define_pm25_excel_sheets <- function(year) {
    list(
        sites_annual = .define_pm25_annual_sites_sheet(year),
        sites_24h = .define_pm25_24h_sites_sheet(year),
        monitors = .define_pm25_monitors_sheet(year)
    )
}

.define_pm25_excel_rows_to_skip <- function() {
    list(
        sites = 3,
        monitors = 1
    )
}

.define_pm25_column_patterns <- function() {
    list(
        sites_annual = "aqs_site_id|site_id|^site$|^valid_|^x[0-9]{4}_[0-9]{4}_annual_design_value|validity$",
        sites_24h = "aqs_site_id|site_id|^site$|^valid_|^x[0-9]{4}_[0-9]{4}_(24h|24_hr)_design_value|validity$",
        monitors = "aqs_site_id|poc"
    )
}

.set_up_pm25_processing <- function(year) {
    file_definitions <- .define_pm25_file_setup(year, "pm25_design_values")
    sheets <- .define_pm25_excel_sheets(year)
    skips <- .define_pm25_excel_rows_to_skip()
    column_patterns <- .define_pm25_column_patterns()
    list(
        file_definitions = file_definitions,
        sheets = sheets,
        skips = skips,
        column_patterns = column_patterns
    )
}

# Preliminary data processing --------------------------------------------------
.rename_pm25_site_columns <- function(df) {
    colnames(df)[grep("site", colnames(df))] <- "aqs_site_id"
    colnames(df)[grep("validity", colnames(df))] <- "validity"
    colnames(df)[grep("valid_|[0-9]{4}_[0-9]{4}.*design_value", colnames(df))] <- "valid_dv"
    df
}

.fix_erroneous_pm25_site_ids <- function(df) {
    # 2012: 1 site with superscript
    # 2013: 1 site missing leading 0 (fixed with make_aqs_site_id_char function); several sites with "**1" following site ID
    # 2014: 1 site with 10 characters (last should be dropped after looking at master site list), several sites with "**1" following site ID
    df$aqs_site_id[nchar(df$aqs_site_id) > 9 & !is.na(df$aqs_site_id)] <- stringr::str_sub(df$aqs_site_id[nchar(df$aqs_site_id) > 9 & !is.na(df$aqs_site_id)], 1L, 9L)
    df$aqs_site_id[nchar(df$aqs_site_id) == 8 & !is.na(df$aqs_site_id)] <- stringr::str_pad(df$aqs_site_id[nchar(df$aqs_site_id) == 8 & !is.na(df$aqs_site_id)], width = 9, side = "left", pad = "0")
    df
}

.process_initial_pm25_sites <- function(excel_file_path, sheet, skip, column_patterns, year) {
    df <- .import_regulatory_monitor_data(
        excel_file_path = excel_file_path,
        sheet = sheet,
        skip = skip
    ) |>
        .select_cols(column_patterns) |>
        .rename_pm25_site_columns()

    if (year %in% 2012:2014) {
        df <- .fix_erroneous_pm25_site_ids(df)
    }

    df |>
        .make_aqs_site_id_char() |>
        .drop_rows_all_na() |>
        dplyr::distinct()
}

.process_initial_pm25_monitors <- function(excel_file_path, sheet, skip, column_patterns) {
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

.get_initial_pm25_data <- function(year, setup_definitions) {
    # Sites
    sites_annual <- .process_initial_pm25_sites(
        excel_file_path = setup_definitions$file_definitions$excel_file_path,
        sheet = setup_definitions$sheets$sites_annual,
        skip = setup_definitions$skips$sites,
        column_patterns = setup_definitions$column_patterns$sites_annual,
        year = year
    )
    sites_24h <- .process_initial_pm25_sites(
        excel_file_path = setup_definitions$file_definitions$excel_file_path,
        sheet = setup_definitions$sheets$sites_24h,
        skip = setup_definitions$skips$sites,
        column_patterns = setup_definitions$column_patterns$sites_24h,
        year = year
    )
    # Monitors
    # Monitor data unavailable for 2012-2015; data for 2016 is used instead
    if (year %in% 2016:2023) {
        monitors <- .process_initial_pm25_monitors(
            excel_file_path = setup_definitions$file_definitions$excel_file_path,
            sheet = setup_definitions$sheets$monitors,
            skip = setup_definitions$skips$monitors,
            column_patterns = setup_definitions$column_patterns$monitors
        )
    } else if (year %in% 2012:2015) {
        setup_definitions_2016 <- .set_up_pm25_processing(year = 2016)
        monitors <- .process_initial_pm25_monitors(
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
        sites_24h = sites_24h,
        monitors = monitors
    )
}

# Validity determination --------------------------------------------------------
.filter_pm25_valid_dv <- function(df, year) {
    if (year %in% 2013:2023) {
        df |>
            dplyr::filter(!is.na(valid_dv))
    } else if (year == 2012) {
        df |>
            dplyr::filter(!is.na(valid_dv) & stringr::str_detect(validity, "Y"))
    }
}

.get_unique_valid_pm25_sites <- function(df, year) {
    df |>
        .filter_pm25_valid_dv(year = year) |>
        dplyr::select("aqs_site_id") |>
        dplyr::distinct()
}

.determine_pm25_validity <- function(initial_data_list, year) {
    list(
        sites_annual = .get_unique_valid_pm25_sites(
            df = initial_data_list$sites_annual,
            year = year
        ),
        sites_24h = .get_unique_valid_pm25_sites(
            df = initial_data_list$sites_24h,
            year = year
        )
    )
}

# DF combination ----------------------------------------------------------------
# Sites and monitors joined and standards combined

.join_pm25_sites_and_monitors <- function(valid_data_list, initial_data_list) {
    pm25_annual <- dplyr::left_join(
        valid_data_list$sites_annual,
        initial_data_list$monitors,
        by = "aqs_site_id"
    )
    pm25_24h <- dplyr::left_join(
        valid_data_list$sites_24h,
        initial_data_list$monitors,
        by = "aqs_site_id"
    )
    list(
        pm25_annual = pm25_annual,
        pm25_24h = pm25_24h
    )
}

.combine_pm25_data <- function(valid_data_list, initial_data_list) {
    joined_list <- .join_pm25_sites_and_monitors(
        valid_data_list = valid_data_list,
        initial_data_list = initial_data_list
    )
    rbind(joined_list$pm25_annual, joined_list$pm25_24h) |>
        dplyr::distinct()
}

.finalize_pm25_data <- function(valid_data_list, initial_data_list, year) {
    .combine_pm25_data(
        valid_data_list = valid_data_list,
        initial_data_list = initial_data_list
    ) |>
        dplyr::mutate(
            year = year,
            parameter_name = "PM2.5"
        )
}

# Main function ----------------------------------------------------------------
.get_pm25_monitors <- function(year) {
    setup_definitions <- .set_up_pm25_processing(year = year)
    initial_data <- .get_initial_pm25_data(year = year, setup_definitions = setup_definitions)
    valid_data <- .determine_pm25_validity(initial_data_list = initial_data, year = year)
    pm25 <- .finalize_pm25_data(
        valid_data_list = valid_data,
        initial_data_list = initial_data,
        year = year
    )
    unlink(setup_definitions$file_definitions$temp_directory, recursive = TRUE)
    pm25
}
