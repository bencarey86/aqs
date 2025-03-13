co_setup_2012 <- .set_up_co_processing(2012)
co_setup_2013 <- .set_up_co_processing(2013)
co_setup_2014 <- .set_up_co_processing(2014)
co_setup_2015 <- .set_up_co_processing(2015)
co_setup_2016 <- .set_up_co_processing(2016)
co_setup_2017 <- .set_up_co_processing(2017)
co_setup_2018 <- .set_up_co_processing(2018)
co_setup_2019 <- .set_up_co_processing(2019)
co_setup_2020 <- .set_up_co_processing(2020)
co_setup_2021 <- .set_up_co_processing(2021)
co_setup_2022 <- .set_up_co_processing(2022)
co_setup_2023 <- .set_up_co_processing(2023)

co_initial_data_2012 <- .get_initial_co_data(year = 2012, setup_definitions = co_setup_2012)
co_initial_data_2013 <- .get_initial_co_data(year = 2013, setup_definitions = co_setup_2013)
co_initial_data_2014 <- .get_initial_co_data(year = 2014, setup_definitions = co_setup_2014)
co_initial_data_2015 <- .get_initial_co_data(year = 2015, setup_definitions = co_setup_2015)
co_initial_data_2016 <- .get_initial_co_data(year = 2016, setup_definitions = co_setup_2016)
co_initial_data_2017 <- .get_initial_co_data(year = 2017, setup_definitions = co_setup_2017)
co_initial_data_2018 <- .get_initial_co_data(year = 2018, setup_definitions = co_setup_2018)
co_initial_data_2019 <- .get_initial_co_data(year = 2019, setup_definitions = co_setup_2019)
co_initial_data_2020 <- .get_initial_co_data(year = 2020, setup_definitions = co_setup_2020)
co_initial_data_2021 <- .get_initial_co_data(year = 2021, setup_definitions = co_setup_2021)
co_initial_data_2022 <- .get_initial_co_data(year = 2022, setup_definitions = co_setup_2022)
co_initial_data_2023 <- .get_initial_co_data(year = 2023, setup_definitions = co_setup_2023)

co_valid_data_2012 <- .determine_co_validity(year = 2012, initial_data_list = co_initial_data_2012)
co_valid_data_2013 <- .determine_co_validity(year = 2013, initial_data_list = co_initial_data_2013)
co_valid_data_2014 <- .determine_co_validity(year = 2014, initial_data_list = co_initial_data_2014)
co_valid_data_2015 <- .determine_co_validity(year = 2015, initial_data_list = co_initial_data_2015)
co_valid_data_2016 <- .determine_co_validity(year = 2016, initial_data_list = co_initial_data_2016)
co_valid_data_2017 <- .determine_co_validity(year = 2017, initial_data_list = co_initial_data_2017)
co_valid_data_2018 <- .determine_co_validity(year = 2018, initial_data_list = co_initial_data_2018)
co_valid_data_2019 <- .determine_co_validity(year = 2019, initial_data_list = co_initial_data_2019)
co_valid_data_2020 <- .determine_co_validity(year = 2020, initial_data_list = co_initial_data_2020)
co_valid_data_2021 <- .determine_co_validity(year = 2021, initial_data_list = co_initial_data_2021)
co_valid_data_2022 <- .determine_co_validity(year = 2022, initial_data_list = co_initial_data_2022)
co_valid_data_2023 <- .determine_co_validity(year = 2023, initial_data_list = co_initial_data_2023)

co_2012 <- .combine_co_data(year = 2012, initial_data_list = co_initial_data_2012, valid_data = co_valid_data_2012) |>
    dplyr::mutate(
        year = 2012,
        parameter_name = "Carbon monoxide"
    )
co_2013 <- .combine_co_data(year = 2013, initial_data_list = co_initial_data_2013, valid_data = co_valid_data_2013) |>
    dplyr::mutate(
        year = 2013,
        parameter_name = "Carbon monoxide"
    )
co_2014 <- .combine_co_data(year = 2014, initial_data_list = co_initial_data_2014, valid_data = co_valid_data_2014) |>
    dplyr::mutate(
        year = 2014,
        parameter_name = "Carbon monoxide"
    )
co_2015 <- .combine_co_data(year = 2015, initial_data_list = co_initial_data_2015, valid_data = co_valid_data_2015) |>
    dplyr::mutate(
        year = 2015,
        parameter_name = "Carbon monoxide"
    )
co_2016 <- .combine_co_data(year = 2016, initial_data_list = co_initial_data_2016, valid_data = co_valid_data_2016) |>
    dplyr::mutate(
        year = 2016,
        parameter_name = "Carbon monoxide"
    )
co_2017 <- .combine_co_data(year = 2017, initial_data_list = co_initial_data_2017, valid_data = co_valid_data_2017) |>
    dplyr::mutate(
        year = 2017,
        parameter_name = "Carbon monoxide"
    )
co_2018 <- .combine_co_data(year = 2018, initial_data_list = co_initial_data_2018, valid_data = co_valid_data_2018) |>
    dplyr::mutate(
        year = 2018,
        parameter_name = "Carbon monoxide"
    )
co_2019 <- .combine_co_data(year = 2019, initial_data_list = co_initial_data_2019, valid_data = co_valid_data_2019) |>
    dplyr::mutate(
        year = 2019,
        parameter_name = "Carbon monoxide"
    )
co_2020 <- .combine_co_data(year = 2020, initial_data_list = co_initial_data_2020, valid_data = co_valid_data_2020) |>
    dplyr::mutate(
        year = 2020,
        parameter_name = "Carbon monoxide"
    )
co_2021 <- .combine_co_data(year = 2021, initial_data_list = co_initial_data_2021, valid_data = co_valid_data_2021) |>
    dplyr::mutate(
        year = 2021,
        parameter_name = "Carbon monoxide"
    )
co_2022 <- .combine_co_data(year = 2022, initial_data_list = co_initial_data_2022, valid_data = co_valid_data_2022) |>
    dplyr::mutate(
        year = 2022,
        parameter_name = "Carbon monoxide"
    )
co_2023 <- .combine_co_data(year = 2023, initial_data_list = co_initial_data_2023, valid_data = co_valid_data_2023) |>
    dplyr::mutate(
        year = 2023,
        parameter_name = "Carbon monoxide"
    )

unlink(co_setup_2012$file_definitions$temp_directory, recursive = TRUE)
unlink(co_setup_2013$file_definitions$temp_directory, recursive = TRUE)
unlink(co_setup_2014$file_definitions$temp_directory, recursive = TRUE)
unlink(co_setup_2015$file_definitions$temp_directory, recursive = TRUE)
unlink(co_setup_2016$file_definitions$temp_directory, recursive = TRUE)
unlink(co_setup_2017$file_definitions$temp_directory, recursive = TRUE)
unlink(co_setup_2018$file_definitions$temp_directory, recursive = TRUE)
unlink(co_setup_2019$file_definitions$temp_directory, recursive = TRUE)
unlink(co_setup_2020$file_definitions$temp_directory, recursive = TRUE)
unlink(co_setup_2021$file_definitions$temp_directory, recursive = TRUE)
unlink(co_setup_2022$file_definitions$temp_directory, recursive = TRUE)
unlink(co_setup_2023$file_definitions$temp_directory, recursive = TRUE)
