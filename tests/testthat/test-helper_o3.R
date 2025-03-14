o3_setup_2012 <- .set_up_o3_processing(year = 2012)
o3_setup_2013 <- .set_up_o3_processing(year = 2013)
o3_setup_2014 <- .set_up_o3_processing(year = 2014)
o3_setup_2015 <- .set_up_o3_processing(year = 2015)
o3_setup_2016 <- .set_up_o3_processing(year = 2016)
o3_setup_2017 <- .set_up_o3_processing(year = 2017)
o3_setup_2018 <- .set_up_o3_processing(year = 2018)
o3_setup_2019 <- .set_up_o3_processing(year = 2019)
o3_setup_2020 <- .set_up_o3_processing(year = 2020)
o3_setup_2021 <- .set_up_o3_processing(year = 2021)
o3_setup_2022 <- .set_up_o3_processing(year = 2022)
o3_setup_2023 <- .set_up_o3_processing(year = 2023)

o3_initial_data_2012 <- .get_initial_o3_data(year = 2012, setup_definitions = o3_setup_2012)
o3_initial_data_2013 <- .get_initial_o3_data(year = 2013, setup_definitions = o3_setup_2013)
o3_initial_data_2014 <- .get_initial_o3_data(year = 2014, setup_definitions = o3_setup_2014)
o3_initial_data_2015 <- .get_initial_o3_data(year = 2015, setup_definitions = o3_setup_2015)
o3_initial_data_2016 <- .get_initial_o3_data(year = 2016, setup_definitions = o3_setup_2016)
o3_initial_data_2017 <- .get_initial_o3_data(year = 2017, setup_definitions = o3_setup_2017)
o3_initial_data_2018 <- .get_initial_o3_data(year = 2018, setup_definitions = o3_setup_2018)
o3_initial_data_2019 <- .get_initial_o3_data(year = 2019, setup_definitions = o3_setup_2019)
o3_initial_data_2020 <- .get_initial_o3_data(year = 2020, setup_definitions = o3_setup_2020)
o3_initial_data_2021 <- .get_initial_o3_data(year = 2021, setup_definitions = o3_setup_2021)
o3_initial_data_2022 <- .get_initial_o3_data(year = 2022, setup_definitions = o3_setup_2022)
o3_initial_data_2023 <- .get_initial_o3_data(year = 2023, setup_definitions = o3_setup_2023)

o3_valid_data_2012 <- .determine_o3_validity(initial_data_list = o3_initial_data_2012, year = 2012)
o3_valid_data_2013 <- .determine_o3_validity(initial_data_list = o3_initial_data_2013, year = 2013)
o3_valid_data_2014 <- .determine_o3_validity(initial_data_list = o3_initial_data_2014, year = 2014)
o3_valid_data_2015 <- .determine_o3_validity(initial_data_list = o3_initial_data_2015, year = 2015)
o3_valid_data_2016 <- .determine_o3_validity(initial_data_list = o3_initial_data_2016, year = 2016)
o3_valid_data_2017 <- .determine_o3_validity(initial_data_list = o3_initial_data_2017, year = 2017)
o3_valid_data_2018 <- .determine_o3_validity(initial_data_list = o3_initial_data_2018, year = 2018)
o3_valid_data_2019 <- .determine_o3_validity(initial_data_list = o3_initial_data_2019, year = 2019)
o3_valid_data_2020 <- .determine_o3_validity(initial_data_list = o3_initial_data_2020, year = 2020)
o3_valid_data_2021 <- .determine_o3_validity(initial_data_list = o3_initial_data_2021, year = 2021)
o3_valid_data_2022 <- .determine_o3_validity(initial_data_list = o3_initial_data_2022, year = 2022)
o3_valid_data_2023 <- .determine_o3_validity(initial_data_list = o3_initial_data_2023, year = 2023)

o3_2012 <- .finalize_o3_data(
  valid_data_list = o3_valid_data_2012,
  initial_data_list = o3_initial_data_2012,
  year = 2012
) |>
  dplyr::mutate(
    year = 2012,
    parameter_name = "Ozone"
  )
o3_2013 <- .finalize_o3_data(
  valid_data_list = o3_valid_data_2013,
  initial_data_list = o3_initial_data_2013,
  year = 2013
) |>
  dplyr::mutate(
    year = 2013,
    parameter_name = "Ozone"
  )
o3_2014 <- .finalize_o3_data(
  valid_data_list = o3_valid_data_2014,
  initial_data_list = o3_initial_data_2014,
  year = 2014
) |>
  dplyr::mutate(
    year = 2014,
    parameter_name = "Ozone"
  )
o3_2015 <- .finalize_o3_data(
  valid_data_list = o3_valid_data_2015,
  initial_data_list = o3_initial_data_2015,
  year = 2015
) |>
  dplyr::mutate(
    year = 2015,
    parameter_name = "Ozone"
  )
o3_2016 <- .finalize_o3_data(
  valid_data_list = o3_valid_data_2016,
  initial_data_list = o3_initial_data_2016,
  year = 2016
) |>
  dplyr::mutate(
    year = 2016,
    parameter_name = "Ozone"
  )
o3_2017 <- .finalize_o3_data(
  valid_data_list = o3_valid_data_2017,
  initial_data_list = o3_initial_data_2017,
  year = 2017
) |>
  dplyr::mutate(
    year = 2017,
    parameter_name = "Ozone"
  )
o3_2018 <- .finalize_o3_data(
  valid_data_list = o3_valid_data_2018,
  initial_data_list = o3_initial_data_2018,
  year = 2018
) |>
  dplyr::mutate(
    year = 2018,
    parameter_name = "Ozone"
  )
o3_2019 <- .finalize_o3_data(
  valid_data_list = o3_valid_data_2019,
  initial_data_list = o3_initial_data_2019,
  year = 2019
) |>
  dplyr::mutate(
    year = 2019,
    parameter_name = "Ozone"
  )
o3_2020 <- .finalize_o3_data(
  valid_data_list = o3_valid_data_2020,
  initial_data_list = o3_initial_data_2020,
  year = 2020
) |>
  dplyr::mutate(
    year = 2020,
    parameter_name = "Ozone"
  )
o3_2021 <- .finalize_o3_data(
  valid_data_list = o3_valid_data_2021,
  initial_data_list = o3_initial_data_2021,
  year = 2021
) |>
  dplyr::mutate(
    year = 2021,
    parameter_name = "Ozone"
  )
o3_2022 <- .finalize_o3_data(
  valid_data_list = o3_valid_data_2022,
  initial_data_list = o3_initial_data_2022,
  year = 2022
) |>
  dplyr::mutate(
    year = 2022,
    parameter_name = "Ozone"
  )
o3_2023 <- .finalize_o3_data(
  valid_data_list = o3_valid_data_2023,
  initial_data_list = o3_initial_data_2023,
  year = 2023
) |>
  dplyr::mutate(
    year = 2023,
    parameter_name = "Ozone"
  )

unlink(o3_setup_2012$file_definitions$temp_directory, recursive = TRUE)
unlink(o3_setup_2013$file_definitions$temp_directory, recursive = TRUE)
unlink(o3_setup_2014$file_definitions$temp_directory, recursive = TRUE)
unlink(o3_setup_2015$file_definitions$temp_directory, recursive = TRUE)
unlink(o3_setup_2016$file_definitions$temp_directory, recursive = TRUE)
unlink(o3_setup_2017$file_definitions$temp_directory, recursive = TRUE)
unlink(o3_setup_2018$file_definitions$temp_directory, recursive = TRUE)
unlink(o3_setup_2019$file_definitions$temp_directory, recursive = TRUE)
unlink(o3_setup_2020$file_definitions$temp_directory, recursive = TRUE)
unlink(o3_setup_2021$file_definitions$temp_directory, recursive = TRUE)
unlink(o3_setup_2022$file_definitions$temp_directory, recursive = TRUE)
unlink(o3_setup_2023$file_definitions$temp_directory, recursive = TRUE)
