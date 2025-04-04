.simplify_monitoring_objective <- function(df) {
    df |>
        dplyr::mutate(
            monitoring_objective = ifelse(
                stringr::str_detect(monitoring_objective, "SOURCE|HIGHEST|MAX OZONE"),
                "Source/Highest/Max",
                "Other"
            )
        )
}
