data <- raw_data %>%
  process_sites()


# add_site_ID
test_that("add_site_ID adds an integer site_ID column to the inteded table", {

  data %<>% add_site_ID()

  expect_true('site_ID' %in% colnames(data$NAEI_clean))

  expect_type(data$NAEI_clean$site_ID, 'integer')

})


# add_site_ID
test_that("add_site_ID creates a unique set of IDs", {

  data %<>% add_site_ID()

  expect_equal(unique(data$NAEI_clean$site_ID), data$NAEI_clean$site_ID)

})


test_that("There are no duplicate sites", {

  distinct_sites <- data$NAEI_clean %>%
    select(site_name, IPM_sector, Latitude, Longitude, PlantID) %>%
    distinct()

  expect_equal(nrow(data$NAEI_clean), nrow(distinct_sites))

})



# naei_pipe_to_spur_adjustment
test_that('Pipe distances are updated after NAEI adjustment', {

  raw_values <- data$CCS_spur_sites %>% arrange(PlantID) %>% pull(pipe_dist)

  embedded_values <- naei_pipe_to_spur_adjustment(data) %>%
    filter(PlantID %in% data$CCS_spur_sites$PlantID) %>%
    arrange(PlantID) %>%
    pull(pipe_dist)

  expect_equal(embedded_values, raw_values)

})

test_that('use_CCS_spur makes adjustment to CCS pipes', {

  data %<>%
    interpolate_data() %>%
    tidy() %>%
    add_site_ID()

  data$site_demand <- site_demand(data)


  # with adjustment
  data$model_parameters$use_CCS_Spur <- TRUE

  site_H2C02_transport_with <- site_H2C02_transport(data)

  # without
  data$model_parameters$use_CCS_Spur <- FALSE

  site_H2C02_transport_without <- site_H2C02_transport(data)

  # identify mismatches
  mismatched <- anti_join(site_H2C02_transport_with,
                          site_H2C02_transport_without,
                          by = c('site_ID', 'nearest_distance.CO2'))

  expect_length(mismatched$site_ID, nrow(data$CCS_spur_sites))

  # and another quick check - pipe_dist should be equal regardless
  mismatched <- anti_join(site_H2C02_transport_with,
                          site_H2C02_transport_without,
                          by = c('site_ID', 'pipe_dist'))

  expect_length(mismatched$site_ID, 0)


})
