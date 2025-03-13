.define_pm10_url <- function(year) {
    switch(as.character(year),
        "2023" = "https://www.epa.gov/system/files/documents/2024-06/pm10_designvalues_2021_2023_final_05_09_24.xlsx",
        "2022" = "https://www.epa.gov/system/files/documents/2023-05/PM10_DesignValues_2020_2022_FINAL_05_23_23.xlsx",
        "2021" = "https://www.epa.gov/system/files/documents/2022-05/PM10_DesignValues_2019_2021_FINAL_05_24_22.xlsx",
        "2020" = "https://www.epa.gov/sites/default/files/2021-05/pm10_designvalues_2018_2020_final_05_24_21.xlsx",
        "2019" = "https://www.epa.gov/sites/default/files/2020-05/pm10_designvalues_2017_2019_final_05_26_20.xlsx",
        "2018" = "https://www.epa.gov/sites/default/files/2019-07/pm10_designvalues_20162018_final_07_19_19.xlsx",
        "2017" = "https://www.epa.gov/sites/default/files/2018-07/pm10_designvalues_20152017_final_07_24_18.xlsx",
        "2016" = "https://www.epa.gov/sites/default/files/2017-07/pm10_designvalues_20142016_final_07_24_17.xlsx",
        "2015" = "https://www.epa.gov/sites/default/files/2016-07/pm10_designvalues_20132015_final_07_29_16.xlsx",
        "2014" = "https://www.epa.gov/sites/default/files/2016-09/pm10_designvalues_20122014_final_08_06_15.xlsx",
        "2013" = "https://www.epa.gov/sites/default/files/2016-09/pm10_designvalues_20112013_final_08_25_14.xlsx",
        "2012" = "https://www.epa.gov/sites/default/files/2016-09/pm10_designvalues_20102012_final_09_05_13.xlsx",
        "2011" = "https://www.epa.gov/sites/default/files/2016-09/pm10_designvalues_20092011_final_07_30_12.xlsx",
        "2010" = "https://www.epa.gov/sites/default/files/2016-09/pm10_designvalues_20082010_finalrevised.xlsx",
        "2009" = "https://www.epa.gov/sites/default/files/2016-09/pm10_designvalues_20072009_revisedfinal.xls",
        "2008" = "https://www.epa.gov/sites/default/files/2016-09/dv_pm10_2006_2008.xls",
        "2007" = "https://www.epa.gov/sites/default/files/2016-09/dv_pm10_2005_2007.xls",
        "2006" = "https://www.epa.gov/sites/default/files/2016-09/dv_pm10_2004_2006.xls",
        "2005" = "https://www.epa.gov/sites/default/files/2016-09/dv_pm10_2003_2005.xls"
    )
}

.define_pm10_monitors_sheet <- function(year) {
    dplyr::case_when(
        year %in% 2019:2023 ~ "Table5. Monitor Status",
        year %in% 2017:2018 ~ "Table 5, Monitor",
        year %in% 2014:2016 ~ "Table6. Monitor Trends",
        year == 2013 ~ "Table 6. Monitor Trends ",
        year == 2012 ~ "Table 6. Monitor ENE History  "
    )
}

.define_pm10_column_patterns <- function(year) {
    # Pattern has some general columns that are always included, with some years having additional columns
    col_patterns_general <- "^site$|aqs_site_id|poc|^valid|^x[0-9]{4}_[0-9]{4}_average_estimated_exceedances"
    col_patterns <- if (year >= 2017) {
        col_patterns_general
    } else if (year >= 2012) {
        paste0(c(col_patterns_general, paste0((year - 2), "_", year)), collapse = "|")
    }
}

.rename_pm10_columns <- function(df) {
    colnames(df)[grep("site", colnames(df))] <- "aqs_site_id"
    colnames(df)[grep("complete", colnames(df))] <- "completeness"
    colnames(df)[grep("exceedances|^ene_[0-9]{4}_[0-9]{4}$", colnames(df))] <- "valid_dv"
    colnames(df)[grep("_valid_[0-9]{4}", colnames(df))] <- "validity"
    df
}

.filter_pm10_valid_dv <- function(df, year) {
    if (year %in% 2017:2023) {
        df <- df |>
            dplyr::filter(!is.na(valid_dv))
    } else if (year %in% 2012:2016) {
        df <- df |>
            dplyr::filter(!is.na(valid_dv) & stringr::str_detect(validity, "^Y"))
    }
}

.get_pm10_monitors <- function(year) {
    temp_directory <- .create_temp_subdirectory("pm10")
    url <- .define_pm10_url(year)
    excel_file_path <- .download_regulatory_monitor_data(
        year = year,
        url = url,
        temp_directory = temp_directory
    )
    sheet <- .define_pm10_monitors_sheet(year)
    column_patterns <- .define_pm10_column_patterns(year)
    df <- .import_regulatory_monitor_data(
        excel_file_path = excel_file_path,
        sheet = sheet,
        skip = 3
    ) |>
        .select_cols(col_patterns = column_patterns) |>
        .rename_pm10_columns() |>
        .make_aqs_site_id_char() |>
        .filter_pm10_valid_dv(year = year) |>
        dplyr::select("aqs_site_id", "poc") |>
        dplyr::distinct() |>
        dplyr::mutate(
            "poc" = as.character(poc),
            "year" = year,
            "parameter_name" = "PM10"
        )
    unlink(temp_directory, recursive = TRUE)
    df
}
