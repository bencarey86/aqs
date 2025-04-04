.filter_regulatory_parameters <- function(df) {
    df |>
        dplyr::filter(parameter_code %in% c(42101, 14129, 85129, 42602, 44201, 81102, 88101, 42401))
}

.filter_aqi_naaqs_parameters <- function(df) {
    df |>
        dplyr::filter(parameter_code %in% c(42101, 14129, 85129, 42602, 44201, 81102, 88101, 42401, 88502))
}

.split_vintages <- function(df) {
    list(
        vintage_4 = df |>
            dplyr::filter(year %in% 2020:2029),
        vintage_410 = df |>
            dplyr::filter(year %in% 2010:2019)
    )
}

.combine_aqi_naaqs_regulatory_monitors_by_vintage <- function(aqi_naaqs_monitors, regulatory_monitors) {
    list(
        vintage_4 = aqi_naaqs_monitors$vintage_4 |>
            dplyr::bind_rows(regulatory_monitors$vintage_4),
        vintage_410 = aqi_naaqs_monitors$vintage_410 |>
            dplyr::bind_rows(regulatory_monitors$vintage_410)
    )
}

.join_geocoded_bg_id <- function(monitor_list, geocoded_bg_id_list) {
    list(
        vintage_4 = monitor_list$vintage_4 |>
            dplyr::left_join(geocoded_bg_id_list$vintage_4, by = c("latitude", "longitude")) |>
            dplyr::relocate(bg_id, .after = longitude),
        vintage_410 = monitor_list$vintage_410 |>
            dplyr::left_join(geocoded_bg_id_list$vintage_410, by = c("latitude", "longitude")) |>
            dplyr::relocate(bg_id, .after = longitude)
    )
}
