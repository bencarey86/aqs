.get_crs_columns <- function(df_list) {
    list(
        vintage_4 = df_list$vintage_4 |>
            dplyr::select(latitude, longitude),
        vintage_410 = df_list$vintage_410 |>
            dplyr::select(latitude, longitude)
    )
}

.get_combined_crs_coordinates_by_vintage <- function(aqi_naaqs_monitors, regulatory_monitors) {
    aqi_naaqs_crs <- .get_crs_columns(aqi_naaqs_monitors)
    regulatory_crs <- .get_crs_columns(regulatory_monitors)
    list(
        vintage_4 = aqi_naaqs_crs$vintage_4 |>
            dplyr::bind_rows(regulatory_crs$vintage_4) |>
            dplyr::filter(!is.na(latitude) & !is.na(longitude)) |>
            dplyr::distinct(),
        vintage_410 = aqi_naaqs_crs$vintage_410 |>
            dplyr::bind_rows(regulatory_crs$vintage_410) |>
            dplyr::filter(!is.na(latitude) & !is.na(longitude)) |>
            dplyr::distinct()
    )
}

.define_geocoder_api_call <- function(longitude, latitude, vintage) {
    stringr::str_c(
        "https://geocoding.geo.census.gov/geocoder/geographies/coordinates?x=",
        longitude, "&y=", latitude, "&benchmark=4&vintage=", vintage,
        ifelse(vintage == 410, "&format=json", "&layers=10&format=json")
    )
}

.call_geocoder_api <- function(api_call) {
    jsonlite::fromJSON(api_call)
}

.get_api_result <- function(api_result, vintage) {
    ifelse(
        vintage == 410,
        api_result$result$geographies$"Census Blocks"$GEOID |>
            stringr::str_sub(1, 12),
        api_result$result$geographies$"Census Block Groups"$GEOID
    )
}

.get_bg_id_from_geocoder <- function(longitude, latitude, vintage) {
    purrr::possibly(
        function() {
            .define_geocoder_api_call(longitude, latitude, vintage) |>
                .call_geocoder_api() |>
                .get_api_result(vintage)
        },
        otherwise = NA_character_
    )()
}

.get_geocoded_bg_id_individual_df <- function(df, vintage) {
    # Set up progress tracking
    progressr::handlers(progressr::handler_progress(
        format = "[:bar] :percent | ETA: :eta | Elapsed: :elapsed",
        width = 60
    ))

    # Set up parallel processing
    workers <- max(1, (future::availableCores() - 1))
    future::plan(future::multisession, workers = workers)

    # Initialize bg_id column
    df$bg_id <- NA_character_

    # Run with progress tracking
    progressr::with_progress({
        p <- progressr::progressor(steps = nrow(df))

        df$bg_id <- furrr::future_map2(
            df$longitude,
            df$latitude,
            function(longitude, latitude) {
                result <- .get_bg_id_from_geocoder(longitude, latitude, vintage = vintage)
                p() # Update progress
                result
            }
        ) |> unlist()
    })

    # Return to sequential processing
    future::plan(future::sequential)

    # Return
    df
}

.address_parallel_geocoding_failures <- function(df, vintage) {
    if (sum(is.na(df$bg_id)) == 0) {
        return(df)
    } else {
        # Create indices of rows that need updating
        na_indices <- which(is.na(df$bg_id))

        # Update those rows
        df$bg_id[na_indices] <- purrr::map_chr(
            na_indices,
            ~ .get_bg_id_from_geocoder(df$longitude[.x], df$latitude[.x], vintage)
        )
    }
    df
}

.get_geocoded_bg_id <- function(df_list) {
    # Validate input
    # DF 1 is vintage 4, DF 2 is vintage 410
    if (names(df_list)[1] != "vintage_4" || names(df_list)[2] != "vintage_410") {
        stop("Input list must contain two dataframes named 'vintage_4' and 'vintage_410'")
    }
    vintages <- c(4, 410)
    purrr::map2(df_list, vintages, function(df, vintage) {
        .get_geocoded_bg_id_individual_df(df, vintage) |>
            .address_parallel_geocoding_failures(vintage = vintage)
    })
}
