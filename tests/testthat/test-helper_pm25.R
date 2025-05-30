pm25_setup_2012 <- .set_up_pm25_processing(year = 2012)
pm25_setup_2013 <- .set_up_pm25_processing(year = 2013)
pm25_setup_2014 <- .set_up_pm25_processing(year = 2014)
pm25_setup_2015 <- .set_up_pm25_processing(year = 2015)
pm25_setup_2016 <- .set_up_pm25_processing(year = 2016)
pm25_setup_2017 <- .set_up_pm25_processing(year = 2017)
pm25_setup_2018 <- .set_up_pm25_processing(year = 2018)
pm25_setup_2019 <- .set_up_pm25_processing(year = 2019)
pm25_setup_2020 <- .set_up_pm25_processing(year = 2020)
pm25_setup_2021 <- .set_up_pm25_processing(year = 2021)
pm25_setup_2022 <- .set_up_pm25_processing(year = 2022)
pm25_setup_2023 <- .set_up_pm25_processing(year = 2023)

pm25_initial_data_2012 <- .get_initial_pm25_data(year = 2012, setup_definitions = pm25_setup_2012)
pm25_initial_data_2013 <- .get_initial_pm25_data(year = 2013, setup_definitions = pm25_setup_2013)
pm25_initial_data_2014 <- .get_initial_pm25_data(year = 2014, setup_definitions = pm25_setup_2014)
pm25_initial_data_2015 <- .get_initial_pm25_data(year = 2015, setup_definitions = pm25_setup_2015)
pm25_initial_data_2016 <- .get_initial_pm25_data(year = 2016, setup_definitions = pm25_setup_2016)
pm25_initial_data_2017 <- .get_initial_pm25_data(year = 2017, setup_definitions = pm25_setup_2017)
pm25_initial_data_2018 <- .get_initial_pm25_data(year = 2018, setup_definitions = pm25_setup_2018)
pm25_initial_data_2019 <- .get_initial_pm25_data(year = 2019, setup_definitions = pm25_setup_2019)
pm25_initial_data_2020 <- .get_initial_pm25_data(year = 2020, setup_definitions = pm25_setup_2020)
pm25_initial_data_2021 <- .get_initial_pm25_data(year = 2021, setup_definitions = pm25_setup_2021)
pm25_initial_data_2022 <- .get_initial_pm25_data(year = 2022, setup_definitions = pm25_setup_2022)
pm25_initial_data_2023 <- .get_initial_pm25_data(year = 2023, setup_definitions = pm25_setup_2023)

pm25_valid_data_2012 <- .determine_pm25_validity(
  initial_data_list = pm25_initial_data_2012,
  year = 2012
)
pm25_valid_data_2013 <- .determine_pm25_validity(
  initial_data_list = pm25_initial_data_2013,
  year = 2013
)
pm25_valid_data_2014 <- .determine_pm25_validity(
  initial_data_list = pm25_initial_data_2014,
  year = 2014
)
pm25_valid_data_2015 <- .determine_pm25_validity(
  initial_data_list = pm25_initial_data_2015,
  year = 2015
)
pm25_valid_data_2016 <- .determine_pm25_validity(
  initial_data_list = pm25_initial_data_2016,
  year = 2016
)
pm25_valid_data_2017 <- .determine_pm25_validity(
  initial_data_list = pm25_initial_data_2017,
  year = 2017
)
pm25_valid_data_2018 <- .determine_pm25_validity(
  initial_data_list = pm25_initial_data_2018,
  year = 2018
)
pm25_valid_data_2019 <- .determine_pm25_validity(
  initial_data_list = pm25_initial_data_2019,
  year = 2019
)
pm25_valid_data_2020 <- .determine_pm25_validity(
  initial_data_list = pm25_initial_data_2020,
  year = 2020
)
pm25_valid_data_2021 <- .determine_pm25_validity(
  initial_data_list = pm25_initial_data_2021,
  year = 2021
)
pm25_valid_data_2022 <- .determine_pm25_validity(
  initial_data_list = pm25_initial_data_2022,
  year = 2022
)
pm25_valid_data_2023 <- .determine_pm25_validity(
  initial_data_list = pm25_initial_data_2023,
  year = 2023
)

pm25_2012 <- .finalize_pm25_data(
  valid_data_list = pm25_valid_data_2012,
  initial_data_list = pm25_initial_data_2012,
  year = 2012
)
pm25_2013 <- .finalize_pm25_data(
  valid_data_list = pm25_valid_data_2013,
  initial_data_list = pm25_initial_data_2013,
  year = 2013
)
pm25_2014 <- .finalize_pm25_data(
  valid_data_list = pm25_valid_data_2014,
  initial_data_list = pm25_initial_data_2014,
  year = 2014
)
pm25_2015 <- .finalize_pm25_data(
  valid_data_list = pm25_valid_data_2015,
  initial_data_list = pm25_initial_data_2015,
  year = 2015
)
pm25_2016 <- .finalize_pm25_data(
  valid_data_list = pm25_valid_data_2016,
  initial_data_list = pm25_initial_data_2016,
  year = 2016
)
pm25_2017 <- .finalize_pm25_data(
  valid_data_list = pm25_valid_data_2017,
  initial_data_list = pm25_initial_data_2017,
  year = 2017
)
pm25_2018 <- .finalize_pm25_data(
  valid_data_list = pm25_valid_data_2018,
  initial_data_list = pm25_initial_data_2018,
  year = 2018
)
pm25_2019 <- .finalize_pm25_data(
  valid_data_list = pm25_valid_data_2019,
  initial_data_list = pm25_initial_data_2019,
  year = 2019
)
pm25_2020 <- .finalize_pm25_data(
  valid_data_list = pm25_valid_data_2020,
  initial_data_list = pm25_initial_data_2020,
  year = 2020
)
pm25_2021 <- .finalize_pm25_data(
  valid_data_list = pm25_valid_data_2021,
  initial_data_list = pm25_initial_data_2021,
  year = 2021
)
pm25_2022 <- .finalize_pm25_data(
  valid_data_list = pm25_valid_data_2022,
  initial_data_list = pm25_initial_data_2022,
  year = 2022
)
pm25_2023 <- .finalize_pm25_data(
  valid_data_list = pm25_valid_data_2023,
  initial_data_list = pm25_initial_data_2023,
  year = 2023
)

unlink(pm25_setup_2012$file_definitions$temp_directory, recursive = TRUE)
unlink(pm25_setup_2013$file_definitions$temp_directory, recursive = TRUE)
unlink(pm25_setup_2014$file_definitions$temp_directory, recursive = TRUE)
unlink(pm25_setup_2015$file_definitions$temp_directory, recursive = TRUE)
unlink(pm25_setup_2016$file_definitions$temp_directory, recursive = TRUE)
unlink(pm25_setup_2017$file_definitions$temp_directory, recursive = TRUE)
unlink(pm25_setup_2018$file_definitions$temp_directory, recursive = TRUE)
unlink(pm25_setup_2019$file_definitions$temp_directory, recursive = TRUE)
unlink(pm25_setup_2020$file_definitions$temp_directory, recursive = TRUE)
unlink(pm25_setup_2021$file_definitions$temp_directory, recursive = TRUE)
unlink(pm25_setup_2022$file_definitions$temp_directory, recursive = TRUE)
unlink(pm25_setup_2023$file_definitions$temp_directory, recursive = TRUE)
