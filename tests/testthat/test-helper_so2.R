so2_setup_2012 <- .set_up_so2_processing(year = 2012)
so2_setup_2013 <- .set_up_so2_processing(year = 2013)
so2_setup_2014 <- .set_up_so2_processing(year = 2014)
so2_setup_2015 <- .set_up_so2_processing(year = 2015)
so2_setup_2016 <- .set_up_so2_processing(year = 2016)
so2_setup_2017 <- .set_up_so2_processing(year = 2017)
so2_setup_2018 <- .set_up_so2_processing(year = 2018)
so2_setup_2019 <- .set_up_so2_processing(year = 2019)
so2_setup_2020 <- .set_up_so2_processing(year = 2020)
so2_setup_2021 <- .set_up_so2_processing(year = 2021)
so2_setup_2022 <- .set_up_so2_processing(year = 2022)
so2_setup_2023 <- .set_up_so2_processing(year = 2023)

so2_initial_data_2012 <- .get_initial_so2_data(year = 2012, setup_definitions = so2_setup_2012)
so2_initial_data_2013 <- .get_initial_so2_data(year = 2013, setup_definitions = so2_setup_2013)
so2_initial_data_2014 <- .get_initial_so2_data(year = 2014, setup_definitions = so2_setup_2014)
so2_initial_data_2015 <- .get_initial_so2_data(year = 2015, setup_definitions = so2_setup_2015)
so2_initial_data_2016 <- .get_initial_so2_data(year = 2016, setup_definitions = so2_setup_2016)
so2_initial_data_2017 <- .get_initial_so2_data(year = 2017, setup_definitions = so2_setup_2017)
so2_initial_data_2018 <- .get_initial_so2_data(year = 2018, setup_definitions = so2_setup_2018)
so2_initial_data_2019 <- .get_initial_so2_data(year = 2019, setup_definitions = so2_setup_2019)
so2_initial_data_2020 <- .get_initial_so2_data(year = 2020, setup_definitions = so2_setup_2020)
so2_initial_data_2021 <- .get_initial_so2_data(year = 2021, setup_definitions = so2_setup_2021)
so2_initial_data_2022 <- .get_initial_so2_data(year = 2022, setup_definitions = so2_setup_2022)
so2_initial_data_2023 <- .get_initial_so2_data(year = 2023, setup_definitions = so2_setup_2023)

so2_valid_data_2012 <- .get_unique_valid_so2_sites(
  df = so2_initial_data_2012$sites,
  year = 2012
)
so2_valid_data_2013 <- .get_unique_valid_so2_sites(
  df = so2_initial_data_2013$sites,
  year = 2013
)
so2_valid_data_2014 <- .get_unique_valid_so2_sites(
  df = so2_initial_data_2014$sites,
  year = 2014
)
so2_valid_data_2015 <- .get_unique_valid_so2_sites(
  df = so2_initial_data_2015$sites,
  year = 2015
)
so2_valid_data_2016 <- .get_unique_valid_so2_sites(
  df = so2_initial_data_2016$sites,
  year = 2016
)
so2_valid_data_2017 <- .get_unique_valid_so2_sites(
  df = so2_initial_data_2017$sites,
  year = 2017
)
so2_valid_data_2018 <- .get_unique_valid_so2_sites(
  df = so2_initial_data_2018$sites,
  year = 2018
)
so2_valid_data_2019 <- .get_unique_valid_so2_sites(
  df = so2_initial_data_2019$sites,
  year = 2019
)
so2_valid_data_2020 <- .get_unique_valid_so2_sites(
  df = so2_initial_data_2020$sites,
  year = 2020
)
so2_valid_data_2021 <- .get_unique_valid_so2_sites(
  df = so2_initial_data_2021$sites,
  year = 2021
)
so2_valid_data_2022 <- .get_unique_valid_so2_sites(
  df = so2_initial_data_2022$sites,
  year = 2022
)
so2_valid_data_2023 <- .get_unique_valid_so2_sites(
  df = so2_initial_data_2023$sites,
  year = 2023
)

so2_2012 <- .finalize_so2_data(
  sites = so2_valid_data_2012,
  monitors = so2_initial_data_2012$monitors,
  year = 2012
)
so2_2013 <- .finalize_so2_data(
  sites = so2_valid_data_2013,
  monitors = so2_initial_data_2013$monitors,
  year = 2013
)
so2_2014 <- .finalize_so2_data(
  sites = so2_valid_data_2014,
  monitors = so2_initial_data_2014$monitors,
  year = 2014
)
so2_2015 <- .finalize_so2_data(
  sites = so2_valid_data_2015,
  monitors = so2_initial_data_2015$monitors,
  year = 2015
)
so2_2016 <- .finalize_so2_data(
  sites = so2_valid_data_2016,
  monitors = so2_initial_data_2016$monitors,
  year = 2016
)
so2_2017 <- .finalize_so2_data(
  sites = so2_valid_data_2017,
  monitors = so2_initial_data_2017$monitors,
  year = 2017
)
so2_2018 <- .finalize_so2_data(
  sites = so2_valid_data_2018,
  monitors = so2_initial_data_2018$monitors,
  year = 2018
)
so2_2019 <- .finalize_so2_data(
  sites = so2_valid_data_2019,
  monitors = so2_initial_data_2019$monitors,
  year = 2019
)
so2_2020 <- .finalize_so2_data(
  sites = so2_valid_data_2020,
  monitors = so2_initial_data_2020$monitors,
  year = 2020
)
so2_2021 <- .finalize_so2_data(
  sites = so2_valid_data_2021,
  monitors = so2_initial_data_2021$monitors,
  year = 2021
)
so2_2022 <- .finalize_so2_data(
  sites = so2_valid_data_2022,
  monitors = so2_initial_data_2022$monitors,
  year = 2022
)
so2_2023 <- .finalize_so2_data(
  sites = so2_valid_data_2023,
  monitors = so2_initial_data_2023$monitors,
  year = 2023
)

unlink(so2_setup_2012$file_definitions$temp_directory, recursive = TRUE)
unlink(so2_setup_2013$file_definitions$temp_directory, recursive = TRUE)
unlink(so2_setup_2014$file_definitions$temp_directory, recursive = TRUE)
unlink(so2_setup_2015$file_definitions$temp_directory, recursive = TRUE)
unlink(so2_setup_2016$file_definitions$temp_directory, recursive = TRUE)
unlink(so2_setup_2017$file_definitions$temp_directory, recursive = TRUE)
unlink(so2_setup_2018$file_definitions$temp_directory, recursive = TRUE)
unlink(so2_setup_2019$file_definitions$temp_directory, recursive = TRUE)
unlink(so2_setup_2020$file_definitions$temp_directory, recursive = TRUE)
unlink(so2_setup_2021$file_definitions$temp_directory, recursive = TRUE)
unlink(so2_setup_2022$file_definitions$temp_directory, recursive = TRUE)
unlink(so2_setup_2023$file_definitions$temp_directory, recursive = TRUE)
