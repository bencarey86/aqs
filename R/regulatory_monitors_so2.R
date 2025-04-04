# Set up -----------------------------------------------------------------------
.define_so2_url <- function(year) {
    switch(as.character(year),
        "2023" = "https://www.epa.gov/system/files/documents/2024-06/so2_designvalues_2021_2023_final_05_07_24.xlsx",
        "2022" = "https://www.epa.gov/system/files/documents/2023-05/SO2_DesignValues_2020_2022_FINAL_05_02_23.xlsx",
        "2021" = "https://www.epa.gov/system/files/documents/2022-05/SO2_DesignValues_2019_2021_FINAL_05_25_22.xlsx",
        "2020" = "https://www.epa.gov/sites/default/files/2021-05/so2_designvalues_2018_2020_final_05_24_21.xlsx",
        "2019" = "https://www.epa.gov/sites/default/files/2020-05/so2_designvalues_2017_2019_final_05_05_20.xlsx",
        "2018" = "https://www.epa.gov/sites/default/files/2019-07/so2_designvalues_20162018_final_07_18_19.xlsx",
        "2017" = "https://www.epa.gov/sites/default/files/2018-07/so2_designvalues_20152017_final_07_24_18.xlsx",
        "2016" = "https://www.epa.gov/sites/default/files/2017-07/so2_designvalues_20142016_final_07_19_16.xlsx",
        "2015" = "https://www.epa.gov/sites/default/files/2016-07/so2_designvalues_20132015_final_07_29_16.xlsx",
        "2014" = "https://www.epa.gov/sites/default/files/2016-09/so2_designvalues_20122014_final_8_3_15.xlsx",
        "2013" = "https://www.epa.gov/sites/default/files/2016-09/so2_designvalues_20112013_final_08_1_14.xlsx",
        "2012" = "https://www.epa.gov/sites/default/files/2016-09/so2_designvalues_20102012_final_08_28_13.xlsx",
        "2011" = "https://www.epa.gov/sites/default/files/2016-09/so2_designvalues_20092011_final_7_26_12.xlsx",
        "2010" = "https://www.epa.gov/sites/default/files/2016-09/so2_designvalues_20082010_update.xlsx",
        "2009" = "https://www.epa.gov/sites/default/files/2016-09/so2_designvalues_20072009_final.xls",
        "2008" = "https://www.epa.gov/sites/default/files/2016-09/dv_so2_2006_2008.xls",
        "2007" = "https://www.epa.gov/sites/default/files/2016-09/dv_so2_2005_2007.xls"
    )
}

.define_so2_file_setup <- function(year, directory_name) {
    temp_directory <- .create_temp_subdirectory(name = directory_name)
    url <- .define_so2_url(year = year)
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

.define_so2_sites_sheet <- function(year) {
    dplyr::case_when(
        year %in% 2021:2023 ~ "Table5. Site Status",
        year %in% 2019:2020 ~ "Table5. Site Status 1hr",
        year %in% 2017:2018 ~ "Table5 - Site Level 1hr",
        year %in% 2014:2016 ~ "Table5c - Site Level 1hr",
        year == 2013 ~ "Table5c Monitor 1hr",
        year == 2012 ~ "Table5c"
    )
}

.define_so2_monitors_sheet <- function(year) {
    # Monitor metadata not available for 2012-2015
    dplyr::case_when(
        year %in% 2019:2023 ~ "Appendix. Monitor Information",
        year == 2018 ~ "Monitor Metadata",
        year %in% 2016:2017 ~ "Technical Information"
    )
}

.define_so2_excel_sheets <- function(year) {
    list(
        sites = .define_so2_sites_sheet(year = year),
        monitors = .define_so2_monitors_sheet(year = year)
    )
}

.define_so2_excel_rows_to_skip <- function(year) {
    list(
        sites = dplyr::case_when(
            year %in% 2019:2023 ~ 3,
            year %in% 2014:2018 ~ 2,
            year == 2013 ~ 3,
            year == 2012 ~ 4
        ),
        monitors = ifelse(year %in% 2019:2023, 1, 0)
    )
}

.define_so2_column_patterns <- function(year) {
    site_patterns_shared <- "^x3_year_design_value2_3$|validity|^valid|x[0-9]{4}_[0-9]{4}_design_value"
    site_id_patterns <- dplyr::case_when(
        year %in% 2017:2023 ~ "aqs_site_id",
        year %in% 2012:2016 ~ "fips|fi_ps|^site_id$"
    )
    list(
        sites = paste0(c(site_patterns_shared, site_id_patterns), collapse = "|"),
        monitors = "aqs_site_id|poc"
    )
}

.set_up_so2_processing <- function(year) {
    file_definitions <- .define_so2_file_setup(year = year, directory_name = "so2_design_values")
    sheets <- .define_so2_excel_sheets(year = year)
    skips <- .define_so2_excel_rows_to_skip(year = year)
    column_patterns <- .define_so2_column_patterns(year = year)
    list(
        file_definitions = file_definitions,
        sheets = sheets,
        skips = skips,
        column_patterns = column_patterns
    )
}

# Preliminary data processing --------------------------------------------------
.rename_so2_site_columns <- function(df) {
    colnames(df)[grep("site", colnames(df))] <- "aqs_site_id"
    colnames(df)[grep("^valid|^x[0-9]{4}_[0-9]{4}_design_value_ppb|x3_year_design_value2_3", colnames(df))] <- "valid_dv"
    colnames(df)[grep("validity", colnames(df))] <- "validity"
    df
}

.drop_so2_site_id_superscript <- function(df) {
    # 2016 data includes 2 site IDs with 5 characters (last character is superscript for annotation)
    # Must be run after column renaming
    df$aqs_site_id[nchar(df$aqs_site_id) == 5 & !is.na(df$aqs_site_id)] <- stringr::str_sub(df$aqs_site_id[nchar(df$aqs_site_id) == 5 & !is.na(df$aqs_site_id)], 1L, 4L)
    df
}

.create_so2_sites_aqs_site_id_2012_2016 <- function(df) {
    df |>
        dplyr::mutate(aqs_site_id = stringr::str_c(state_fips, county_fi_ps, aqs_site_id)) |>
        dplyr::select(-"state_fips", -"county_fi_ps")
}

.process_initial_so2_sites <- function(excel_file_path, sheet, skip, column_patterns, year) {
    df <- .import_regulatory_monitor_data(
        excel_file_path = excel_file_path,
        sheet = sheet,
        skip = skip
    ) |>
        .select_cols(col_patterns = column_patterns) |>
        .rename_so2_site_columns()

    if (year == 2016) {
        df <- df |>
            .drop_so2_site_id_superscript()
    }

    # 2012-2016 data contains separate columns for state and county FIPS and site ID
    if (year %in% 2012:2016) {
        df <- df |>
            .create_so2_sites_aqs_site_id_2012_2016()
    }

    df |>
        .make_aqs_site_id_char() |>
        .drop_rows_all_na() |>
        dplyr::distinct()
}

.process_initial_so2_monitors <- function(excel_file_path, sheet, skip, column_patterns, year) {
    df <- .import_regulatory_monitor_data(
        excel_file_path = excel_file_path,
        sheet = sheet,
        skip = skip
    )

    if (year == 2018) {
        df <- df |>
            dplyr::rename(aqs_site_id = aqs_id) |>
            dplyr::mutate(aqs_site_id = stringr::str_replace_all(aqs_site_id, "-", "")) |>
            dplyr::mutate(poc = stringr::str_sub(aqs_site_id, 10L, -1L)) |>
            dplyr::mutate(aqs_site_id = stringr::str_sub(aqs_site_id, 1L, 9L))
    }

    df <- df |>
        .select_cols(col_patterns = column_patterns) |>
        .convert_site_id_poc_column_types() |>
        .drop_rows_all_na() |>
        dplyr::distinct()
}

.get_initial_so2_data <- function(year, setup_definitions) {
    # Sites
    sites <- .process_initial_so2_sites(
        excel_file_path = setup_definitions$file_definitions$excel_file_path,
        sheet = setup_definitions$sheets$sites,
        skip = setup_definitions$skips$sites,
        column_patterns = setup_definitions$column_patterns$sites,
        year = year
    )
    # Monitors
    # Monitor data unavailable for 2012-2015; data for 2016 is used instead
    if (year %in% 2016:2023) {
        monitors <- .process_initial_so2_monitors(
            excel_file_path = setup_definitions$file_definitions$excel_file_path,
            sheet = setup_definitions$sheets$monitors,
            skip = setup_definitions$skips$monitors,
            column_patterns = setup_definitions$column_patterns$monitors,
            year = year
        )
    } else if (year %in% 2012:2015) {
        setup_definitions_2016 <- .set_up_so2_processing(year = 2016)
        monitors <- .process_initial_so2_monitors(
            excel_file_path = setup_definitions_2016$file_definitions$excel_file_path,
            sheet = setup_definitions_2016$sheets$monitors,
            skip = setup_definitions_2016$skips$monitors,
            column_patterns = setup_definitions_2016$column_patterns$monitors,
            year = 2016
        )
        unlink(setup_definitions_2016$file_definitions$temp_directory)
    }

    # Return
    list(
        sites = sites,
        monitors = monitors
    )
}

# Validity determination --------------------------------------------------------
.filter_so2_valid_dv <- function(df, year) {
    if (year %in% 2017:2023) {
        df |>
            dplyr::filter(!is.na(valid_dv))
    } else if (year %in% 2012:2016) {
        df |>
            dplyr::filter(!is.na(valid_dv) & stringr::str_detect(validity, "Y"))
    }
}

.get_unique_valid_so2_sites <- function(df, year) {
    df |>
        .filter_so2_valid_dv(year = year) |>
        dplyr::select("aqs_site_id") |>
        dplyr::distinct()
}

# DF combination ----------------------------------------------------------------
.join_so2_sites_and_monitors <- function(sites, monitors) {
    dplyr::left_join(sites, monitors, by = "aqs_site_id") |>
        dplyr::distinct()
}

.finalize_so2_data <- function(sites, monitors, year) {
    .join_so2_sites_and_monitors(sites, monitors) |>
        dplyr::mutate(
            "year" = year,
            "parameter_name" = "Sulfur dioxide"
        )
}

# Main function -----------------------------------------------------------------
.get_so2_monitors <- function(year) {
    setup_definitions <- .set_up_so2_processing(year)
    initial_data <- .get_initial_so2_data(year, setup_definitions)
    valid_data <- .get_unique_valid_so2_sites(initial_data$sites, year)
    so2 <- .finalize_so2_data(
        sites = valid_data,
        monitors = initial_data$monitors,
        year = year
    )
    unlink(setup_definitions$file_definitions$temp_directory, recursive = TRUE)
    so2
}
