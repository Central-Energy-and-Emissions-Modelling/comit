

#create_decision_variables
test_that('create_decision_variables produces columns of the correct type', {

  expect_type(decision_variables$variable_index, 'integer')
  expect_type(decision_variables$variable_name, 'character')
  expect_true(is.numeric(decision_variables$year))
  expect_type(decision_variables$site_ID, 'integer')
  expect_type(decision_variables$code, 'character')
  expect_type(decision_variables$cluster, 'character')
  expect_type(decision_variables$variable_type, 'character')
})


#create_decision_variables
test_that('No duplicate decision variables exist', {

  unique_indexes <- unique(decision_variables$variable_index)
  all_indexes <- decision_variables$variable_index

  expect_identical(unique_indexes, all_indexes)

})

#create_decision_variables, inter_cluster_hydrogen and non_industry_hydrogen (should be one or the other)
test_that('decision vars present for each condition only when respective parameter is true',
          {
            input_data$model_parameters$model_H2_production <- FALSE
            input_data$H2_plant_size$minimum_available_capacity_EndYear <- 0 # for min_H2 test
            decision_variables <- create_decision_variables(input_data)

            expect_true(!'H2_national_pipe_new_capacity' %in% decision_variables$variable_type)
            expect_true(!'H2_national_pipe_available_capacity' %in% decision_variables$variable_type)
            expect_true(!'H2_outflow' %in% decision_variables$variable_type)
            expect_true(!'b_H2_available_capacity' %in% decision_variables$variable_type)
            expect_in('non_industry_H2', unique(decision_variables$variable_type))


            #### now test other condition
            input_data$model_parameters$model_H2_production <- TRUE
            input_data$H2_plant_size$minimum_available_capacity_EndYear <- 1 # for min_H2 test

            input_data$constraints_to_include$include[
              input_data$constraints_to_include$constraint == 'minimum_hydrogen_plant_size'
              ] <- TRUE

            # also need to add H2 sites to the data for the below to all work as expected
            input_data %<>% add_H2_plants(input_data$model_parameters$model_H2_production)
            decision_variables <- create_decision_variables(input_data)

            expect_in('H2_national_pipe_new_capacity', decision_variables$variable_type)
            expect_in('H2_national_pipe_available_capacity', decision_variables$variable_type)
            expect_in('H2_outflow', decision_variables$variable_type)
            expect_in('b_H2_available_capacity', decision_variables$variable_type)
            expect_true(!'non_industry_H2' %in% unique(decision_variables$variable_type))

          })


## TODO Do this for all other conditions/values expected.
test_that('always present decision variables are included', {

  always_included_vars <- c(
    "new_capacity",
    "available_capacity",
    "used_capacity",
    "CO2_pipe_new_capacity",
    "CO2_pipe_available_capacity",
    "CO2_truck_used_capacity",
    "CO2_transported",
    "H2_pipe_new_capacity",
    "H2_pipe_available_capacity",
    "H2_truck_used_capacity"
  )

  expect_in(always_included_vars, unique(decision_variables$variable_type))
})


# get_H2_parameters
test_that('Warning raised when H2 parameters conflict', {

  input_data$model_parameters$model_H2_production <- FALSE
  input_data$H2_plant_size$minimum_available_capacity_EndYear <- 1

  input_data$constraints_to_include$include[
    input_data$constraints_to_include$constraint == 'minimum_hydrogen_plant_size'
  ] <- TRUE

  expect_warning(get_H2_parameters(input_data))

})


# expand_df_by_model_years
test_that('Number of rows created by year expansion is correct', {

  df_single <- as.data.frame(1)
  df_long <- as.data.frame(seq(5000))
  df_null <- as.data.frame(NULL)

  expect_equal(nrow(expand_df_by_model_years(df_long, raw_data)),
                    nrow(df_long) * number_of_time_steps)

  expect_equal(nrow(expand_df_by_model_years(df_single, raw_data)),
               nrow(df_single) * number_of_time_steps)

  expect_equal(nrow(expand_df_by_model_years(df_null, raw_data)),
               nrow(df_null) * number_of_time_steps)

})




# get_site_technologies
# should be (number of sites * number of years * number of technologies) - filtered rows
test_that('All required sites and technologies combinations are used before
          filtering', {

  total_combinations <- 0

  for (this_sector in unique(input_data$NAEI_clean$IPM_sector)) {

    sites <- input_data$NAEI_clean %>%
      filter(IPM_sector == this_sector) %>%
      nrow()

    tech <- input_data$Technologies %>%
      filter(sector == this_sector) %>%
      nrow()

    total_combinations <- total_combinations + (sites*tech*number_of_time_steps)

  }

  expect_equal(nrow(get_site_technologies(input_data)), total_combinations)

})


# get_site_technologies_filter
test_that("Site/technology data filter matches dual coded versions", {

  site_technologies_full <- get_site_technologies(input_data)

  # filter used in system
  system_filter <- get_site_technologies_filter(site_technologies_full)

  # this is the code taken from the original version (before refactor)
  dual_code <- (
    site_technologies_full$year >= site_technologies_full$start_year
    & (is.na(site_technologies_full$technology_category) # explicitily filter for NAs as want to keep
      | (
          !(site_technologies_full$in_cluster_H2 == FALSE
            & site_technologies_full$technology_category == "Hydrogen")
          &
            !(site_technologies_full$in_cluster_CCS == FALSE
              & site_technologies_full$technology_category == "CCS")
          &
            !(site_technologies_full$year < site_technologies_full$H2_first_year
              & site_technologies_full$technology_category == "Hydrogen")
          &
            !(site_technologies_full$year < site_technologies_full$CCS_first_year
              & site_technologies_full$technology_category == "CCS")
        )
    )
  )


  expect_identical(system_filter, dual_code)

})


#get_capacity_variables(site_technologies)
test_that('get_capacity_variables creates the correct shape table with the
          expected capacity variables', {

  site_technologies_mock <- data.frame(
    year = rep(2025, 1000),
    site_ID = rep(c(seq(500)), 2),
    code = c(rep('tech_a', 500), rep('tech_b', 500)),
    H2_point = rep('cluster_a', 1000)
  )

  mock_capacities <- get_capacity_variables(site_technologies_mock)

  expect_equal(ncol(mock_capacities), 5)
  expect_equal(nrow(mock_capacities), 3 * nrow(site_technologies_mock))


  mock_capacity_types <- str_extract(mock_capacities$variable_name, '^.*(?=\\()')

  expect_setequal(mock_capacity_types, c("new_capacity",
                                         "available_capacity",
                                         "used_capacity"))

  # quick check on real data
  site_technologies <- get_site_technologies(input_data)
  system_capacities <- get_capacity_variables(site_technologies)
  # note that filter hasn't been used here, so this
  # isn't exactly how used in system, but still works for test.

  expect_equal(ncol(system_capacities), 5)
  expect_equal(nrow(system_capacities), 3 * nrow(site_technologies))

})



# get_hydrogen_sites
# test_that('',
#           {}) # come back to this one. Needs some refactoring first
# as currently hard to test.

# get_hydrogen_sites_transport
# test_that('',
#           {})


# get_CCS_sites
test_that('get_CCS_sites outputs 1 row for every combination of site and year
          for sites using at least one CCS technology', {

  site_technologies <- get_site_technologies(input_data)

  CCS_sites_to_include <- site_technologies %>%
    filter(emissions_released < 1) %>%
    select(site_ID, year, H2_point) %>%
    distinct()

  CCS_sites_from_system <- get_CCS_sites(input_data, site_technologies) %>%
    select(site_ID, year, H2_point)

  expect_equal(CCS_sites_to_include, CCS_sites_from_system)
})


#combine_decision_variables
test_that('combine_decision_variables appends all rows from non_null list items',
          {
            # mock data
            df1 <- data.frame(
              variable_name = rep('type(year, site)', 500),
              year = rep(seq(from = 2001, to = 2050, by = 1), 10),
              site_id = rep(c(1:50), 10),
              code = 'code',
              cluster = 'cluster'
            )

            df2 <- rbind(df1, df1)

            existing_tables <- list(df1, NULL, df2, NULL, NULL)

            decision_variables <- combine_decision_variables(existing_tables)

            expect_equal(nrow(decision_variables), 1500)

          })







