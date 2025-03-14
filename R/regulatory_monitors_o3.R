# Set up ------------------------------------------------------------------------
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

.define_o3_file_setup <- function(year, directory_name) {
    temp_directory <- .create_temp_subdirectory(directory_name)
    url <- .define_o3_url(year)
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

.define_o3_sites_sheet <- function(year) {
    dplyr::case_when(
        year %in% 2019:2023 ~ "Table5. Site Status",
        year %in% 2015:2018 ~ "Table5. Monitor Status"
    )
}

.define_o3_monitors_sheet <- function(year) {
    dplyr::case_when(
        year %in% 2019:2023 ~ "Appendix. Monitor Information",
        year == 2018 ~ "Monitor Metadata",
        year == 2017 ~ "Technical Information",
        year %in% 2015:2016 ~ "Appendix. Monitor Information",
        year %in% 2013:2014 ~ "Table5. Monitor Status",
        year == 2012 ~ "Table5"
    )
}

.define_o3_excel_sheets <- function(year) {
    list(
        sites = .define_o3_sites_sheet(year),
        monitors = .define_o3_monitors_sheet(year)
    )
}

.define_o3_excel_rows_to_skip <- function(year) {
    list(
        sites = 3,
        monitors = ifelse(year %in% 2015:2023, 1, 3)
    )
}

.define_o3_column_patterns <- function() {
    "aqs_site_id|poc|^valid_[0-9]{4}_[0-9]{4}_design_value|x[0-9]{4}_[0-9]{4}_design_value_ppm"
}

.set_up_o3_processing <- function(year) {
    file_definitions <- .define_o3_file_setup(year = year, directory_name = "o3_design_values")
    sheets <- .define_o3_excel_sheets(year = year)
    skips <- .define_o3_excel_rows_to_skip(year = year)
    column_patterns <- .define_o3_column_patterns()
    list(
        file_definitions = file_definitions,
        sheets = sheets,
        skips = skips,
        column_patterns = column_patterns
    )
}

# Preliminary data processing --------------------------------------------------
.rename_o3_columns <- function(df) {
    colnames(df)[grep("_valid|design_value", colnames(df))] <- "valid_dv"
    df
}

.process_initial_o3_sites <- function(excel_file_path, sheet, skip, column_patterns) {
    .import_regulatory_monitor_data(
        excel_file_path = excel_file_path,
        sheet = sheet,
        skip = skip
    ) |>
        .select_cols(column_patterns) |>
        .rename_o3_columns() |>
        .make_aqs_site_id_char() |>
        .drop_rows_all_na() |>
        dplyr::distinct()
}

.process_initial_o3_monitors <- function(excel_file_path, sheet, skip, column_patterns) {
    .import_regulatory_monitor_data(
        excel_file_path = excel_file_path,
        sheet = sheet,
        skip = skip
    ) |>
        .select_cols(column_patterns) |>
        .rename_o3_columns() |>
        .convert_site_id_poc_column_types() |>
        .drop_rows_all_na() |>
        dplyr::distinct()
}

.get_initial_o3_data <- function(year, setup_definitions) {
    # Monitors
    if (year %in% c(2012:2014, 2016:2023)) {
        monitors <- .process_initial_o3_monitors(
            excel_file_path = setup_definitions$file_definitions$excel_file_path,
            sheet = setup_definitions$sheets$monitors,
            skip = setup_definitions$skips$monitors,
            column_patterns = setup_definitions$column_patterns
        )
    } else if (year == 2015) {
        setup_definitions_2016 <- .set_up_o3_processing(year = 2016)
        monitors <- .process_initial_o3_monitors(
            excel_file_path = setup_definitions_2016$file_definitions$excel_file_path,
            sheet = setup_definitions_2016$sheets$monitors,
            skip = setup_definitions_2016$skips$monitors,
            column_patterns = setup_definitions_2016$column_patterns
        )
    }
    # Sites (2015-2023)
    if (year %in% 2015:2023) {
        sites <- .process_initial_o3_sites(
            excel_file_path = setup_definitions$file_definitions$excel_file_path,
            sheet = setup_definitions$sheets$sites,
            skip = setup_definitions$skips$sites,
            column_patterns = setup_definitions$column_patterns
        )
        list(
            sites = sites,
            monitors = monitors
        )
    } else if (year %in% 2012:2014) {
        list(
            monitors = monitors
        )
    }
}

# Validity determination --------------------------------------------------------
.filter_o3_valid_dv <- function(df) {
    df |>
        dplyr::filter(!is.na(valid_dv))
}

.get_unique_valid_o3_monitors <- function(df) {
    df |>
        .filter_o3_valid_dv() |>
        dplyr::select("aqs_site_id", "poc") |>
        dplyr::distinct()
}

.get_unique_valid_o3_sites <- function(df) {
    df |>
        .filter_o3_valid_dv() |>
        dplyr::select("aqs_site_id") |>
        dplyr::distinct()
}

.determine_o3_validity <- function(initial_data_list, year) {
    if (year %in% 2015:2023) {
        list(
            sites = .get_unique_valid_o3_sites(initial_data_list$sites)
        )
    } else if (year %in% 2012:2014) {
        list(
            monitors = .get_unique_valid_o3_monitors(initial_data_list$monitors)
        )
    }
}

# DF combination ----------------------------------------------------------------
.join_o3_sites_and_monitors <- function(sites, monitors) {
    dplyr::left_join(
        sites,
        monitors,
        by = "aqs_site_id"
    )
}

.finalize_o3_data <- function(valid_data_list, initial_data_list, year) {
    if (year %in% 2015:2023) {
        df <- .join_o3_sites_and_monitors(
            sites = valid_data_list$sites,
            monitors = initial_data_list$monitors
        )
    } else if (year %in% 2012:2014) {
        df <- valid_data_list$monitors
    }
    df |>
        dplyr::mutate(
            year = year,
            parameter_name = "Ozone"
        )
}

# Main function -----------------------------------------------------------------
.get_o3_monitors <- function(year) {
    setup_definitions <- .set_up_o3_processing(year = year)
    initial_data_list <- .get_initial_o3_data(year = year, setup_definitions = setup_definitions)
    valid_data_list <- .determine_o3_validity(initial_data_list = initial_data_list, year = year)
    o3 <- .finalize_o3_data(
        valid_data_list = valid_data_list,
        initial_data_list = initial_data_list,
        year = year
    )
    unlink(setup_definitions$file_definitions$temp_directory, recursive = TRUE)
    o3
}
