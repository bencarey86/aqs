# Set up -----------------------------------------------------------------------
.define_lead_url <- function(year) {
    url <- switch(as.character(year),
        "2023" = "https://www.epa.gov/system/files/documents/2024-06/pb_designvalues_2021_2023_final_05_07_24.xlsx",
        "2022" = "https://www.epa.gov/system/files/documents/2023-05/Pb_DesignValues_2020_2022_FINAL_05_02_23.xlsx",
        "2021" = "https://www.epa.gov/system/files/documents/2022-05/Pb_DesignValues_2019_2021_FINAL_05_04_22.xlsx",
        "2020" = "https://www.epa.gov/sites/default/files/2021-05/pb_designvalues_2018_2020_final_05_24_21.xlsx",
        "2019" = "https://www.epa.gov/sites/default/files/2020-05/pb_designvalues_2017_2019_final_05_05_20.xlsx",
        "2018" = "https://www.epa.gov/sites/default/files/2019-07/pb_designvalues_20162018_final_06_28_19.xlsx",
        "2017" = "https://www.epa.gov/sites/default/files/2018-07/pb_designvalues_20152017_final_07_24_18.xlsx",
        "2016" = "https://www.epa.gov/sites/default/files/2017-07/pb_designvalues_20142016_final_29june17.xlsx",
        "2015" = "https://www.epa.gov/sites/default/files/2016-08/pb_designvalues_20132015_final_08_29_16.xlsx",
        "2014" = "https://www.epa.gov/sites/default/files/2016-09/pb_designvalues_20122014_final_08_03_15.xlsx",
        "2013" = "https://www.epa.gov/sites/default/files/2016-09/pb_designvalues_20112013_final_08_13_14.xlsx",
        "2012" = "https://www.epa.gov/sites/default/files/2016-09/pb_designvalues_20102012_final_07_22_13.xlsx"
    )
}

.define_lead_file_setup <- function(year, directory_name) {
    temp_directory <- .create_temp_subdirectory(directory_name)
    url <- .define_lead_url(year)
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

.define_lead_monitors_sheet <- function(year) {
    sheet <- dplyr::case_when(
        year %in% 2019:2023 ~ "Appendix. Monitor Information",
        year == 2018 ~ "Monitor Metadata",
        year %in% 2012:2017 ~ "Technical Information"
    )
}

.define_lead_sites_sheet <- function(year) {
    sheet <- dplyr::case_when(
        year %in% 2019:2023 ~ "Table5. Site Status",
        year %in% 2015:2018 ~ "Table 5 Site Listing",
        year %in% c(2013, 2014) ~ "TABLE 5 Site",
        year == 2012 ~ "Table 5, Site detail listing"
    )
}

.define_lead_excel_sheets <- function(year) {
    list(
        sites = .define_lead_sites_sheet(year = year),
        monitors = .define_lead_monitors_sheet(year = year)
    )
}

.define_lead_excel_rows_to_skip <- function(year) {
    if (year %in% 2019:2023) {
        sites <- 3
    } else if (year %in% 2012:2018) {
        sites <- 2
    }
    list(
        sites = sites,
        monitors = sites - 2
    )
}

.define_lead_column_patterns <- function() {
    list(
        sites = "^site$|aqs_site_id|x[0-9]{4}_[0-9]{4}_design_value|[0-9]{5}_[0-9]{4}_design_value|design_value_valid|validity|^valid_[0-9]{4}_[0-9]{4}_design_value",
        monitors = "^aqs_site_id$|^poc$"
    )
}

.set_up_lead_processing <- function(year) {
    file_definitions <- .define_lead_file_setup(year = year, directory_name = "lead_design_values")
    sheets <- .define_lead_excel_sheets(year = year)
    skips <- .define_lead_excel_rows_to_skip(year = year)
    column_patterns <- .define_lead_column_patterns()
    list(
        file_definitions = file_definitions,
        sheets = sheets,
        skips = skips,
        column_patterns = column_patterns
    )
}

# Preliminary data processing --------------------------------------------------
.rename_lead_site_columns <- function(df) {
    colnames(df)[grep("^site$", colnames(df))] <- "aqs_site_id"
    colnames(df)[grep("_design_value", colnames(df))] <- "valid_dv"
    colnames(df)[grep("validity|design_value_valid", colnames(df))] <- "validity"
    df
}

.modify_lead_monitor_2018_data <- function(df) {
    df |>
        dplyr::rename(aqs_site_id = aqs_id) |>
        dplyr::mutate(aqs_site_id = stringr::str_replace_all(aqs_site_id, "-", "")) |>
        dplyr::mutate(poc = stringr::str_sub(aqs_site_id, 10L, -1L)) |>
        dplyr::mutate(aqs_site_id = stringr::str_sub(aqs_site_id, 1L, 9L))
}

.process_initial_lead_sites <- function(excel_file_path, sheet, skip, column_patterns) {
    .import_regulatory_monitor_data(
        excel_file_path = excel_file_path,
        sheet = sheet,
        skip = skip
    ) |>
        .select_cols(column_patterns) |>
        .rename_lead_site_columns() |>
        .make_aqs_site_id_char() |>
        .drop_rows_all_na() |>
        dplyr::distinct()
}

.process_intial_lead_monitors <- function(year, excel_file_path, sheet, skip, column_patterns) {
    df <- .import_regulatory_monitor_data(
        excel_file_path = excel_file_path,
        sheet = sheet,
        skip = skip
    )
    if (year == 2018) {
        df <- .modify_lead_monitor_2018_data(df)
    }
    df |>
        .select_cols(column_patterns) |>
        .convert_site_id_poc_column_types() |>
        .drop_rows_all_na() |>
        dplyr::distinct()
}

.get_initial_lead_data <- function(year, setup_definitions) {
    # Sites
    sites <- .process_initial_lead_sites(
        excel_file_path = setup_definitions$file_definitions$excel_file_path,
        sheet = setup_definitions$sheets$sites,
        skip = setup_definitions$skips$sites,
        column_patterns = setup_definitions$column_patterns$sites
    )

    # Monitors
    if (year %in% 2016:2023) {
        monitors <- .process_intial_lead_monitors(
            year = year,
            excel_file_path = setup_definitions$file_definitions$excel_file_path,
            sheet = setup_definitions$sheets$monitors,
            skip = setup_definitions$skips$monitors,
            column_patterns = setup_definitions$column_patterns$monitors
        )
        # Monitor data unavailable for 2012-2015; data for 2016 is used instead
    } else if (year %in% 2012:2015) {
        setup_definitions_2016 <- .set_up_lead_processing(year = 2016)
        monitors <- .process_intial_lead_monitors(
            year = 2016,
            excel_file_path = setup_definitions_2016$file_definitions$excel_file_path,
            sheet = setup_definitions_2016$sheets$monitors,
            skip = setup_definitions_2016$skips$monitors,
            column_patterns = setup_definitions_2016$column_patterns$monitor
        )
        unlink(setup_definitions_2016$file_definitions$temp_directory, recursive = TRUE)
    }

    # Return
    list(
        sites = sites,
        monitors = monitors
    )
}

# Validity determination -------------------------------------------------------
.filter_lead_valid_dv <- function(df, year) {
    if (year %in% 2019:2023) {
        df |>
            dplyr::filter(!is.na(valid_dv))
    } else if (year %in% 2012:2018) {
        df |>
            dplyr::filter(!is.na(valid_dv) & stringr::str_detect(validity, "^Y|yes"))
    }
}

.determine_lead_validity <- function(df, year) {
    df |>
        .filter_lead_valid_dv(year = year) |>
        dplyr::select("aqs_site_id") |>
        dplyr::distinct()
}

# DF combination ----------------------------------------------------------------
.join_lead_sites_and_monitors <- function(sites, monitors) {
    sites |>
        dplyr::left_join(monitors, by = "aqs_site_id")
}

# Main function -----------------------------------------------------------------
.get_lead_monitors <- function(year) {
    setup <- .set_up_lead_processing(year = year)
    initial_data <- .get_initial_lead_data(year = year, setup_definitions = setup)
    valid_lead_sites <- .determine_lead_validity(
        df = initial_data$sites,
        year = year
    )
    lead <- .join_lead_sites_and_monitors(
        sites = valid_lead_sites,
        monitors = initial_data$monitors
    ) |>
        dplyr::mutate(
            year = year,
            parameter_name = "Lead"
        )
    unlink(setup$file_definitions$temp_directory, recursive = TRUE)
    lead
}
