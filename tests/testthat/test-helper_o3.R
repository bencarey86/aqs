# 2015-2023
# 1. Sites DF prepared correctly
# 2. Valid DV sites identified correctly
# 3. DF represents unique valid sites
# 4. Monitors DF prepared correctly (2015 uses 2016 data)
# 5. Join yields correct number of rows
# 6. Combined DF represents unique valid sites and monitors
# 2012-2014
# 1. Monitor DF prepared correctly
# 2. Valid DV monitors identified correctly
# 3. DF represents unique valid monitors

.get_o3_intermediate_data <- function(start, end) {
  directory_name <- paste0("o3_testing_", start, "_", end)
  temp_directory <- .create_temp_subdirectory(directory_name)
  year <- ifelse(start == 2015 && end == 2015, 2015, sample(start:end, 1))
  url <- .define_o3_url(year)
  excel_file_path <- .download_regulatory_monitor_data(
    year = year,
    url = url,
    temp_directory = temp_directory
  )

  if (year %in% 2015:2023) {
    sheet_sites <- .define_o3_sites_sheet(year)
    skip_sites <- .define_o3_skip(year, level = "site")
    site_column_patterns <- .define_o3_column_patterns()
    o3_sites <- .import_regulatory_monitor_data(
      excel_file_path = excel_file_path,
      sheet = sheet_sites,
      skip = skip_sites
    ) |>
      .select_cols(site_column_patterns) |>
      .rename_o3_columns() |>
      .make_aqs_site_id_char() |>
      dplyr::distinct()
  }

  sheet_monitors <- .define_o3_monitors_sheet(year)
  skip_monitors <- .define_o3_skip(year, level = "monitor")
  monitor_column_patterns <- .define_o3_column_patterns()

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
    .select_cols(monitor_column_patterns) |>
    .rename_o3_columns() |>
    .convert_site_id_poc_column_types() |>
    dplyr::distinct()
  if (year %in% 2015:2023) {
    intermediate_data <- list(
      year = year,
      o3_sites = o3_sites,
      o3_monitors = o3_monitors
    )
  } else if (year %in% 2012:2014) {
    intermediate_data <- list(
      year = year,
      o3_monitors = o3_monitors
    )
  }
}

.get_o3_final_data <- function(intermediate_data) {
  year <- intermediate_data$year
  o3_sites <- intermediate_data$o3_sites
  o3_monitors <- intermediate_data$o3_monitors

  if (year %in% 2015:2023) {
    o3_valid_sites <- o3_sites |>
      dplyr::filter(!is.na(valid_dv)) |>
      dplyr::select("aqs_site_id") |>
      dplyr::distinct()

    o3 <- o3_valid_sites |>
      dplyr::left_join(o3_monitors, by = "aqs_site_id") |>
      dplyr::distinct()

    final_data <- list(
      year = year,
      o3_valid_sites = o3_valid_sites,
      o3 = o3
    )
  } else if (year %in% 2012:2014) {
    o3_valid_monitors <- o3_monitors |>
      dplyr::filter(!is.na(valid_dv)) |>
      dplyr::select("aqs_site_id", "poc") |>
      dplyr::distinct()

    final_data <- list(
      year = year,
      o3_valid_monitors = o3_valid_monitors,
      o3 = o3_valid_monitors
    )
  }
}

o3_intermediate_data_2016_2023 <- .get_o3_intermediate_data(2016, 2023)
o3_intermediate_data_2015 <- .get_o3_intermediate_data(2015, 2015)
o3_intermediate_data_2012_2014 <- .get_o3_intermediate_data(2012, 2014)
o3_final_data_2016_2023 <- .get_o3_final_data(o3_intermediate_data_2016_2023)
o3_final_data_2015 <- .get_o3_final_data(o3_intermediate_data_2015)
o3_final_data_2012_2014 <- .get_o3_final_data(o3_intermediate_data_2012_2014)
