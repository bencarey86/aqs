lead_setup_2012 <- .set_up_lead_processing(year = 2012)
lead_setup_2013 <- .set_up_lead_processing(year = 2013)
lead_setup_2014 <- .set_up_lead_processing(year = 2014)
lead_setup_2015 <- .set_up_lead_processing(year = 2015)
lead_setup_2016 <- .set_up_lead_processing(year = 2016)
lead_setup_2017 <- .set_up_lead_processing(year = 2017)
lead_setup_2018 <- .set_up_lead_processing(year = 2018)
lead_setup_2019 <- .set_up_lead_processing(year = 2019)
lead_setup_2020 <- .set_up_lead_processing(year = 2020)
lead_setup_2021 <- .set_up_lead_processing(year = 2021)
lead_setup_2022 <- .set_up_lead_processing(year = 2022)
lead_setup_2023 <- .set_up_lead_processing(year = 2023)

lead_initial_data_2012 <- .get_initial_lead_data(year = 2012, setup_definitions = lead_setup_2012)
lead_initial_data_2013 <- .get_initial_lead_data(year = 2013, setup_definitions = lead_setup_2013)
lead_initial_data_2014 <- .get_initial_lead_data(year = 2014, setup_definitions = lead_setup_2014)
lead_initial_data_2015 <- .get_initial_lead_data(year = 2015, setup_definitions = lead_setup_2015)
lead_initial_data_2016 <- .get_initial_lead_data(year = 2016, setup_definitions = lead_setup_2016)
lead_initial_data_2017 <- .get_initial_lead_data(year = 2017, setup_definitions = lead_setup_2017)
lead_initial_data_2018 <- .get_initial_lead_data(year = 2018, setup_definitions = lead_setup_2018)
lead_initial_data_2019 <- .get_initial_lead_data(year = 2019, setup_definitions = lead_setup_2019)
lead_initial_data_2020 <- .get_initial_lead_data(year = 2020, setup_definitions = lead_setup_2020)
lead_initial_data_2021 <- .get_initial_lead_data(year = 2021, setup_definitions = lead_setup_2021)
lead_initial_data_2022 <- .get_initial_lead_data(year = 2022, setup_definitions = lead_setup_2022)
lead_initial_data_2023 <- .get_initial_lead_data(year = 2023, setup_definitions = lead_setup_2023)

lead_valid_sites_2012 <- .determine_lead_validity(df = lead_initial_data_2012$sites, year = 2012)
lead_valid_sites_2013 <- .determine_lead_validity(df = lead_initial_data_2013$sites, year = 2013)
lead_valid_sites_2014 <- .determine_lead_validity(df = lead_initial_data_2014$sites, year = 2014)
lead_valid_sites_2015 <- .determine_lead_validity(df = lead_initial_data_2015$sites, year = 2015)
lead_valid_sites_2016 <- .determine_lead_validity(df = lead_initial_data_2016$sites, year = 2016)
lead_valid_sites_2017 <- .determine_lead_validity(df = lead_initial_data_2017$sites, year = 2017)
lead_valid_sites_2018 <- .determine_lead_validity(df = lead_initial_data_2018$sites, year = 2018)
lead_valid_sites_2019 <- .determine_lead_validity(df = lead_initial_data_2019$sites, year = 2019)
lead_valid_sites_2020 <- .determine_lead_validity(df = lead_initial_data_2020$sites, year = 2020)
lead_valid_sites_2021 <- .determine_lead_validity(df = lead_initial_data_2021$sites, year = 2021)
lead_valid_sites_2022 <- .determine_lead_validity(df = lead_initial_data_2022$sites, year = 2022)
lead_valid_sites_2023 <- .determine_lead_validity(df = lead_initial_data_2023$sites, year = 2023)

lead_2012 <- .join_lead_sites_and_monitors(
  sites = lead_valid_sites_2012,
  monitors = lead_initial_data_2012$monitors
) |>
  dplyr::mutate(
    year = 2012,
    parameter_name = "Lead"
  )
lead_2013 <- .join_lead_sites_and_monitors(
  sites = lead_valid_sites_2013,
  monitors = lead_initial_data_2013$monitors
) |>
  dplyr::mutate(
    year = 2013,
    parameter_name = "Lead"
  )
lead_2014 <- .join_lead_sites_and_monitors(
  sites = lead_valid_sites_2014,
  monitors = lead_initial_data_2014$monitors
) |>
  dplyr::mutate(
    year = 2014,
    parameter_name = "Lead"
  )
lead_2015 <- .join_lead_sites_and_monitors(
  sites = lead_valid_sites_2015,
  monitors = lead_initial_data_2015$monitors
) |>
  dplyr::mutate(
    year = 2015,
    parameter_name = "Lead"
  )
lead_2016 <- .join_lead_sites_and_monitors(
  sites = lead_valid_sites_2016,
  monitors = lead_initial_data_2016$monitors
) |>
  dplyr::mutate(
    year = 2016,
    parameter_name = "Lead"
  )
lead_2017 <- .join_lead_sites_and_monitors(
  sites = lead_valid_sites_2017,
  monitors = lead_initial_data_2017$monitors
) |>
  dplyr::mutate(
    year = 2017,
    parameter_name = "Lead"
  )
lead_2018 <- .join_lead_sites_and_monitors(
  sites = lead_valid_sites_2018,
  monitors = lead_initial_data_2018$monitors
) |>
  dplyr::mutate(
    year = 2018,
    parameter_name = "Lead"
  )
lead_2019 <- .join_lead_sites_and_monitors(
  sites = lead_valid_sites_2019,
  monitors = lead_initial_data_2019$monitors
) |>
  dplyr::mutate(
    year = 2019,
    parameter_name = "Lead"
  )
lead_2020 <- .join_lead_sites_and_monitors(
  sites = lead_valid_sites_2020,
  monitors = lead_initial_data_2020$monitors
) |>
  dplyr::mutate(
    year = 2020,
    parameter_name = "Lead"
  )
lead_2021 <- .join_lead_sites_and_monitors(
  sites = lead_valid_sites_2021,
  monitors = lead_initial_data_2021$monitors
) |>
  dplyr::mutate(
    year = 2021,
    parameter_name = "Lead"
  )
lead_2022 <- .join_lead_sites_and_monitors(
  sites = lead_valid_sites_2022,
  monitors = lead_initial_data_2022$monitors
) |>
  dplyr::mutate(
    year = 2022,
    parameter_name = "Lead"
  )
lead_2023 <- .join_lead_sites_and_monitors(
  sites = lead_valid_sites_2023,
  monitors = lead_initial_data_2023$monitors
) |>
  dplyr::mutate(
    year = 2023,
    parameter_name = "Lead"
  )

unlink(lead_setup_2012$file_definitions$temp_directory, recursive = TRUE)
unlink(lead_setup_2013$file_definitions$temp_directory, recursive = TRUE)
unlink(lead_setup_2014$file_definitions$temp_directory, recursive = TRUE)
unlink(lead_setup_2015$file_definitions$temp_directory, recursive = TRUE)
unlink(lead_setup_2016$file_definitions$temp_directory, recursive = TRUE)
unlink(lead_setup_2017$file_definitions$temp_directory, recursive = TRUE)
unlink(lead_setup_2018$file_definitions$temp_directory, recursive = TRUE)
unlink(lead_setup_2019$file_definitions$temp_directory, recursive = TRUE)
unlink(lead_setup_2020$file_definitions$temp_directory, recursive = TRUE)
unlink(lead_setup_2021$file_definitions$temp_directory, recursive = TRUE)
unlink(lead_setup_2022$file_definitions$temp_directory, recursive = TRUE)
unlink(lead_setup_2023$file_definitions$temp_directory, recursive = TRUE)
