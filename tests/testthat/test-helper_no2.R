no2_setup_2012 <- .set_up_no2_processing(year = 2012)
no2_setup_2013 <- .set_up_no2_processing(year = 2013)
no2_setup_2014 <- .set_up_no2_processing(year = 2014)
no2_setup_2015 <- .set_up_no2_processing(year = 2015)
no2_setup_2016 <- .set_up_no2_processing(year = 2016)
no2_setup_2017 <- .set_up_no2_processing(year = 2017)
no2_setup_2018 <- .set_up_no2_processing(year = 2018)
no2_setup_2019 <- .set_up_no2_processing(year = 2019)
no2_setup_2020 <- .set_up_no2_processing(year = 2020)
no2_setup_2021 <- .set_up_no2_processing(year = 2021)
no2_setup_2022 <- .set_up_no2_processing(year = 2022)
no2_setup_2023 <- .set_up_no2_processing(year = 2023)

no2_initial_data_2012 <- .get_initial_no2_data(year = 2012, setup_definitions = no2_setup_2012)
no2_initial_data_2013 <- .get_initial_no2_data(year = 2013, setup_definitions = no2_setup_2013)
no2_initial_data_2014 <- .get_initial_no2_data(year = 2014, setup_definitions = no2_setup_2014)
no2_initial_data_2015 <- .get_initial_no2_data(year = 2015, setup_definitions = no2_setup_2015)
no2_initial_data_2016 <- .get_initial_no2_data(year = 2016, setup_definitions = no2_setup_2016)
no2_initial_data_2017 <- .get_initial_no2_data(year = 2017, setup_definitions = no2_setup_2017)
no2_initial_data_2018 <- .get_initial_no2_data(year = 2018, setup_definitions = no2_setup_2018)
no2_initial_data_2019 <- .get_initial_no2_data(year = 2019, setup_definitions = no2_setup_2019)
no2_initial_data_2020 <- .get_initial_no2_data(year = 2020, setup_definitions = no2_setup_2020)
no2_initial_data_2021 <- .get_initial_no2_data(year = 2021, setup_definitions = no2_setup_2021)
no2_initial_data_2022 <- .get_initial_no2_data(year = 2022, setup_definitions = no2_setup_2022)
no2_initial_data_2023 <- .get_initial_no2_data(year = 2023, setup_definitions = no2_setup_2023)

no2_valid_sites_2012 <- .determine_no2_validity(year = 2012, initial_data_list = no2_initial_data_2012)
no2_valid_sites_2013 <- .determine_no2_validity(year = 2013, initial_data_list = no2_initial_data_2013)
no2_valid_sites_2014 <- .determine_no2_validity(year = 2014, initial_data_list = no2_initial_data_2014)
no2_valid_sites_2015 <- .determine_no2_validity(year = 2015, initial_data_list = no2_initial_data_2015)
no2_valid_sites_2016 <- .determine_no2_validity(year = 2016, initial_data_list = no2_initial_data_2016)
no2_valid_sites_2017 <- .determine_no2_validity(year = 2017, initial_data_list = no2_initial_data_2017)
no2_valid_sites_2018 <- .determine_no2_validity(year = 2018, initial_data_list = no2_initial_data_2018)
no2_valid_sites_2019 <- .determine_no2_validity(year = 2019, initial_data_list = no2_initial_data_2019)
no2_valid_sites_2020 <- .determine_no2_validity(year = 2020, initial_data_list = no2_initial_data_2020)
no2_valid_sites_2021 <- .determine_no2_validity(year = 2021, initial_data_list = no2_initial_data_2021)
no2_valid_sites_2022 <- .determine_no2_validity(year = 2022, initial_data_list = no2_initial_data_2022)
no2_valid_sites_2023 <- .determine_no2_validity(year = 2023, initial_data_list = no2_initial_data_2023)

no2_2012 <- .combine_no2_data(
  valid_data_list = no2_valid_sites_2012,
  initial_data_list = no2_initial_data_2012
) |>
  dplyr::mutate(
    year = 2012,
    parameter_name = "Nitrogen dioxide (NO2)"
  )
no2_2013 <- .combine_no2_data(
  valid_data_list = no2_valid_sites_2013,
  initial_data_list = no2_initial_data_2013
) |>
  dplyr::mutate(
    year = 2013,
    parameter_name = "Nitrogen dioxide (NO2)"
  )
no2_2014 <- .combine_no2_data(
  valid_data_list = no2_valid_sites_2014,
  initial_data_list = no2_initial_data_2014
) |>
  dplyr::mutate(
    year = 2014,
    parameter_name = "Nitrogen dioxide (NO2)"
  )
no2_2015 <- .combine_no2_data(
  valid_data_list = no2_valid_sites_2015,
  initial_data_list = no2_initial_data_2015
) |>
  dplyr::mutate(
    year = 2015,
    parameter_name = "Nitrogen dioxide (NO2)"
  )
no2_2016 <- .combine_no2_data(
  valid_data_list = no2_valid_sites_2016,
  initial_data_list = no2_initial_data_2016
) |>
  dplyr::mutate(
    year = 2016,
    parameter_name = "Nitrogen dioxide (NO2)"
  )
no2_2017 <- .combine_no2_data(
  valid_data_list = no2_valid_sites_2017,
  initial_data_list = no2_initial_data_2017
) |>
  dplyr::mutate(
    year = 2017,
    parameter_name = "Nitrogen dioxide (NO2)"
  )
no2_2018 <- .combine_no2_data(
  valid_data_list = no2_valid_sites_2018,
  initial_data_list = no2_initial_data_2018
) |>
  dplyr::mutate(
    year = 2018,
    parameter_name = "Nitrogen dioxide (NO2)"
  )
no2_2019 <- .combine_no2_data(
  valid_data_list = no2_valid_sites_2019,
  initial_data_list = no2_initial_data_2019
) |>
  dplyr::mutate(
    year = 2019,
    parameter_name = "Nitrogen dioxide (NO2)"
  )
no2_2020 <- .combine_no2_data(
  valid_data_list = no2_valid_sites_2020,
  initial_data_list = no2_initial_data_2020
) |>
  dplyr::mutate(
    year = 2020,
    parameter_name = "Nitrogen dioxide (NO2)"
  )
no2_2021 <- .combine_no2_data(
  valid_data_list = no2_valid_sites_2021,
  initial_data_list = no2_initial_data_2021
) |>
  dplyr::mutate(
    year = 2021,
    parameter_name = "Nitrogen dioxide (NO2)"
  )
no2_2022 <- .combine_no2_data(
  valid_data_list = no2_valid_sites_2022,
  initial_data_list = no2_initial_data_2022
) |>
  dplyr::mutate(
    year = 2022,
    parameter_name = "Nitrogen dioxide (NO2)"
  )
no2_2023 <- .combine_no2_data(
  valid_data_list = no2_valid_sites_2023,
  initial_data_list = no2_initial_data_2023
) |>
  dplyr::mutate(
    year = 2023,
    parameter_name = "Nitrogen dioxide (NO2)"
  )

unlink(no2_setup_2012$file_definitions$temp_directory, recursive = TRUE)
unlink(no2_setup_2013$file_definitions$temp_directory, recursive = TRUE)
unlink(no2_setup_2014$file_definitions$temp_directory, recursive = TRUE)
unlink(no2_setup_2015$file_definitions$temp_directory, recursive = TRUE)
unlink(no2_setup_2016$file_definitions$temp_directory, recursive = TRUE)
unlink(no2_setup_2017$file_definitions$temp_directory, recursive = TRUE)
unlink(no2_setup_2018$file_definitions$temp_directory, recursive = TRUE)
unlink(no2_setup_2019$file_definitions$temp_directory, recursive = TRUE)
unlink(no2_setup_2020$file_definitions$temp_directory, recursive = TRUE)
unlink(no2_setup_2021$file_definitions$temp_directory, recursive = TRUE)
unlink(no2_setup_2022$file_definitions$temp_directory, recursive = TRUE)
unlink(no2_setup_2023$file_definitions$temp_directory, recursive = TRUE)
