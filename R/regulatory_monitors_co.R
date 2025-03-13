# Set up -----------------------------------------------------------------------
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

.define_co_file_setup <- function(year, directory_name) {
    temp_directory <- .create_temp_subdirectory(directory_name)
    url <- .define_co_url(year)
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

.define_co_excel_sheets <- function(year) {
    if (year %in% 2012:2013) {
        co_8h_sites <- .define_co_8h_sites_sheet(year = year)
        co_1h_sites <- .define_co_1h_sites_sheet(year = year)
        co_8h_monitors <- .define_co_8h_monitors_sheet(year = year)
        co_1h_monitors <- .define_co_1h_monitors_sheet(year = year)
        list(
            co_8h_sites = co_8h_sites,
            co_1h_sites = co_1h_sites,
            co_8h_monitors = co_8h_monitors,
            co_1h_monitors = co_1h_monitors
        )
    } else {
        co_8h_monitors <- .define_co_8h_monitors_sheet(year = year)
        co_1h_monitors <- .define_co_1h_monitors_sheet(year = year)
        list(
            co_8h_monitors = co_8h_monitors,
            co_1h_monitors = co_1h_monitors
        )
    }
}

.define_co_excel_rows_to_skip <- function(year) {
    if (year %in% c(2012, 2014:2023)) {
        skip <- 3
    } else if (year == 2013) {
        skip <- 2
    }
}

.define_co_monitor_column_patterns <- function() {
    "^site$|aqs_site_id|poc|^valid|x20[0-9]{2}_[0-9]_hour_design_value"
}

.define_co_site_column_patterns <- function(year) {
    ifelse(year == 2012, "^site$|2012", "^site$|2013")
}

#' Define column patterns to use for selecting columns
#'
#' This function defines column patterns to use for selecting columns from the
#' regulatory monitor data. The column patterns are defined based on the year
#' and the level of data (site or monitor).
#'
#' @param year The year of the data
#' @return A list of column patterns
#' @keywords internal
.define_co_column_patterns <- function(year) {
    if (year %in% 2014:2023) {
        list(monitors = .define_co_monitor_column_patterns())
    } else if (year %in% 2012:2013) {
        list(
            sites = .define_co_site_column_patterns(year = year),
            monitors = .define_co_monitor_column_patterns()
        )
    }
}

.set_up_co_processing <- function(year) {
    file_definitions <- .define_co_file_setup(year = year, directory_name = "co_design_values")
    sheets <- .define_co_excel_sheets(year = year)
    skip <- .define_co_excel_rows_to_skip(year = year)
    column_patterns <- .define_co_column_patterns(year = year)
    list(
        file_definitions = file_definitions,
        sheets = sheets,
        skip = skip,
        column_patterns = column_patterns
    )
}

# Preliminary data processing ---------------------------------------------------
.rename_co_columns <- function(df) {
    colnames(df)[grep("site", colnames(df))] <- "aqs_site_id"
    colnames(df)[grep("design_value", colnames(df))] <- "valid_dv"
    df
}

.get_initial_co_monitors <- function(excel_file_path, sheet, skip, column_patterns) {
    .import_regulatory_monitor_data(
        excel_file_path = excel_file_path,
        sheet = sheet,
        skip = skip
    ) |>
        .select_cols(column_patterns) |>
        .rename_co_columns() |>
        .convert_site_id_poc_column_types() |>
        .drop_rows_all_na() |>
        dplyr::distinct()
}

.get_all_co_monitors <- function(excel_file_path, sheets, skip, column_patterns) {
    co_8h <- .get_initial_co_monitors(
        excel_file_path = excel_file_path,
        sheet = sheets$co_8h_monitors,
        skip = skip,
        column_patterns = column_patterns$monitors
    )

    co_1h <- .get_initial_co_monitors(
        excel_file_path = excel_file_path,
        sheet = sheets$co_1h_monitors,
        skip = skip,
        column_patterns = column_patterns$monitors
    )

    list(
        co_8h = co_8h,
        co_1h = co_1h
    )
}

.get_initial_co_sites <- function(excel_file_path, sheet, skip, column_patterns) {
    .import_regulatory_monitor_data(
        excel_file_path = excel_file_path,
        sheet = sheet,
        skip = skip
    ) |>
        .select_cols(column_patterns) |>
        .rename_co_columns() |>
        .make_aqs_site_id_char() |>
        .drop_rows_all_na() |>
        dplyr::distinct()
}

.get_all_co_sites <- function(excel_file_path, sheets, skip, column_patterns) {
    co_8h <- .get_initial_co_sites(
        excel_file_path = excel_file_path,
        sheet = sheets$co_8h_sites,
        skip = skip,
        column_patterns = column_patterns$sites
    )

    co_1h <- .get_initial_co_sites(
        excel_file_path = excel_file_path,
        sheet = sheets$co_1h_sites,
        skip = skip,
        column_patterns = column_patterns$sites
    )

    list(
        co_8h = co_8h,
        co_1h = co_1h
    )
}

.get_initial_co_data <- function(year, setup_definitions) {
    # Import monitor data for all years
    monitors <- .get_all_co_monitors(
        excel_file_path = setup_definitions$file_definitions$excel_file_path,
        sheets = setup_definitions$sheets,
        skip = setup_definitions$skip,
        column_patterns = setup_definitions$column_patterns
    )

    if (year %in% 2014:2023) {
        list(monitors = monitors)
    } else if (year %in% 2012:2013) {
        sites <- .get_all_co_sites(
            excel_file_path = setup_definitions$file_definitions$excel_file_path,
            sheets = setup_definitions$sheets,
            skip = setup_definitions$skip,
            column_patterns = setup_definitions$column_patterns
        )

        list(
            monitors = monitors,
            sites = sites
        )
    }
}

# Validity determination --------------------------------------------------------
.filter_co_valid_dv <- function(df) {
    df |>
        dplyr::filter(!is.na(valid_dv))
}

.get_unique_valid_co_monitors <- function(df) {
    df |>
        .filter_co_valid_dv() |>
        dplyr::select("aqs_site_id", "poc") |>
        dplyr::distinct()
}

.get_co_unique_valid_sites <- function(df) {
    df |>
        .filter_co_valid_dv() |>
        dplyr::select("aqs_site_id") |>
        dplyr::distinct()
}

.determine_co_validity <- function(year, initial_data_list) {
    if (year %in% 2014:2023) {
        list(
            co_8h = .get_unique_valid_co_monitors(initial_data_list$monitors$co_8h),
            co_1h = .get_unique_valid_co_monitors(initial_data_list$monitors$co_1h)
        )
    } else if (year %in% 2012:2013) {
        list(
            co_8h = .get_co_unique_valid_sites(initial_data_list$sites$co_8h),
            co_1h = .get_co_unique_valid_sites(initial_data_list$sites$co_1h)
        )
    }
}

# DF combination ----------------------------------------------------------------
# For 2012-2013, sites and monitors must be joined
# DFs for each standard are bound into single DF for all years

.join_co_sites_and_monitors <- function(valid_data_list, initial_data_list) {
    co_8h <- dplyr::left_join(valid_data_list$co_8h, initial_data_list$monitors$co_8h, by = "aqs_site_id")
    co_1h <- dplyr::left_join(valid_data_list$co_1h, initial_data_list$monitors$co_1h, by = "aqs_site_id")
    list(
        co_8h = co_8h,
        co_1h = co_1h
    )
}

.combine_co_data <- function(year, initial_data_list, valid_data_list) {
    if (year %in% 2014:2023) {
        rbind(valid_data_list$co_8h, valid_data_list$co_1h) |>
            dplyr::distinct()
    } else if (year %in% 2012:2013) {
        joined_list <- .join_co_sites_and_monitors(
            valid_data_list = valid_data_list,
            initial_data_list = initial_data_list
        )
        rbind(joined_list$co_8h, joined_list$co_1h) |>
            dplyr::distinct()
    }
}

# Main function ----------------------------------------------------------------
.get_co_monitors <- function(year) {
    setup <- .set_up_co_processing(year = year)
    initial_data <- .get_initial_co_data(year = year, setup_definitions = setup)
    valid_data <- .determine_co_validity(year = year, initial_data_list = initial_data)
    co <- .combine_co_data(year = year, initial_data_list = initial_data, valid_data_list = valid_data) |>
        dplyr::mutate(
            year = year,
            parameter_name = "Carbon monoxide"
        )
    unlink(setup$file_definitions$temp_directory, recursive = TRUE)
    co
}
