test_that("All pollutants and years are represented", {
  pollutant <- c(
    "Carbon monoxide",
    "Lead",
    "Nitrogen dioxide (NO2)",
    "Ozone",
    "PM10",
    "PM2.5",
    "Sulfur dioxide"
  )
  year <- 2012:2023
  search_grid <- tidyr::expand_grid(
    pollutant = pollutant,
    year = year
  )
  purrr::pmap(search_grid, function(pollutant, year) {
    expect_true(
      all(pollutant %in% regulatory_monitors_raw$parameter_name & year %in% regulatory_monitors_raw$year)
    )
  })
})

test_that("No observations are dropped or added when site metadata is joined", {
  joined_df <- .join_regulatory_monitors_site_metadata(regulatory_monitors_raw, regulatory_site_metadata)
  expect_equal(
    regulatory_monitors_raw$aqs_site_id,
    joined_df$aqs_site_id
  )
})

test_that("No observations are dropped or added when monitor metadata is joined", {
  monitor_composite_key <- paste(
    regulatory_monitors_raw$aqs_site_id,
    regulatory_monitors_raw$poc,
    regulatory_monitors_raw$parameter_name,
    sep = "_"
  )
  monitor_metadata_composite_key <- paste(
    regulatory_monitor_metadata$aqs_site_id,
    regulatory_monitor_metadata$poc,
    regulatory_monitor_metadata$parameter_name,
    sep = "_"
  )
  matched <- sum(monitor_composite_key %in% monitor_metadata_composite_key)
  unmatched <- sum(!monitor_composite_key %in% monitor_metadata_composite_key)
  joined_df <- .join_regulatory_monitors_monitor_metadata(regulatory_monitors_raw, regulatory_monitor_metadata)
  expect_equal(
    c(regulatory_monitors_raw$aqs_site_id, regulatory_monitors_raw$poc, regulatory_monitors_raw$parameter_name),
    c(joined_df$aqs_site_id, joined_df$poc, joined_df$parameter_name)
  )
  expect_equal(
    (matched + unmatched),
    nrow(joined_df)
  )
})

test_that("Master function returns expected data", {
  master <- suppressWarnings(get_regulatory_monitors(
    start = 2012,
    end = 2023,
    site_metadata = regulatory_site_metadata,
    monitor_metadata = regulatory_monitor_metadata
  ))
  expect_equal(
    master,
    regulatory_monitors
  )
})
