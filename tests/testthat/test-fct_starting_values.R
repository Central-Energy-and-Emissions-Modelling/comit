


# the below needs to be moved to setup after development
mock_demand <- data.frame(
  year = c(2020:2050),
  commodity_a = 0,
  commodity_b = 1,
  commodity_c = seq(from = 50, to = 200, by = 5)
)

mock_re <- data.frame(
  year = c(2020:2050),
  commodity_a = NA,
  commodity_b = 0.01,
  commodity_c = seq(from = 0, to = 0.3, by = 0.01)
)

mock_ee <- data.frame(
  year = c(2020:2050),
  commodity_a = 0.005,
  commodity_b = c(rep(NA, 15), rep(0.005, 16)),
  commodity_c = seq(from = 0, to = 0.3, by = 0.01)
)

mock_tech <- data.frame(
  code = c(1, 2, 3),
  name = c('tech1', 'tech2', 'tech3'),
  output_commodity = c('commodity_a', 'commodity_b', 'commodity_c'),
  existing_capacity_2020 = c(0, 1, 100)
)

mock_tech_in_out <- data.frame(
  technology_code = c(rep(1, 3),
                      rep(2, 3),
                      3),
  commodity = c('commodity_a',
                'commodity_d',
                'commodity_e',
                'commodity_b',
                'commodity_d',
                'commodity_e',
                'commodity_c')
)

mock_commodity_data <- data.frame(
  commodity = c('commodity_a',
                'commodity_b',
                'commodity_c',
                'commodity_d',
                'commodity_e'),
  output = c(1, 1, 1, -0.5, -1),
  primary_commodity = c(TRUE, TRUE, TRUE, FALSE, FALSE)
)

mock_tech_in_out <- left_join(mock_tech_in_out,
                              mock_commodity_data,
                              by = 'commodity')


starting_values_mock_data <- list(
  Demand_drivers = mock_demand,
  resource_efficiency = mock_re,
  energy_efficiency = mock_ee,
  Technologies = mock_tech,
  technology_in_out = mock_tech_in_out
) %>%
  tidy()



# apply_energy_efficiency
test_that('apply_energy_efficiency makes the expected adjustment for energy and
          resource efficiency on demand using mock data.', {

            # created a copy of the data to manually make efficiency adjustments
            expected_output <- starting_values_mock_data

            # 0 values must be NA
            re_factor <- 1 - (ifelse(is.na(expected_output$resource_efficiency$r_efficiency),
                                     0,
                                     expected_output$resource_efficiency$r_efficiency)
            )


            ee_factor <- 1 - (ifelse(is.na(expected_output$energy_efficiency$efficiency),
                                     0,
                                     expected_output$energy_efficiency$efficiency))


            expected_output$Demand_drivers$demand <- (
              expected_output$Demand_drivers$demand
              * re_factor
              * ee_factor
            )

            test_output <- starting_values_mock_data %>%
              apply_energy_efficiency()

            expect_equal(test_output, expected_output)

            # this tests the following:
            # all other tables should be unchanged
            # if NA then 0, otherwise demand is: demand * (1 - re) * (1 - ee)
          })


# apply_energy_efficiency
test_that('using apply_energy_efficiency on the default input spreadsheet
          creates a table with the correct number of rows and cols', {

            # get input data to the same stage as when used in model
            test_data <- raw_data %>%
              interpolate_data() %>%
              tidy() %>%
              apply_energy_efficiency()

            test_demand <- test_data$Demand_drivers

            # for demand table, nrows should be: n commodities * n years
            expected_nrows <- (
              (ncol(raw_data$Demand_drivers) - 1)
              * number_of_time_steps
            )

            expect_equal(nrow(test_demand), expected_nrows)
            expect_equal(colnames(test_demand), c('year', 'commodity', 'demand'))

          })


# apply_energy_efficiency
test_that('apply_energy_efficiency does not change any tables other than
          Demand_drivers, and all commodities still exist in Demand_drivers',
          {
            data <- raw_data %>%
              process_sites() %>%
              interpolate_data() %>%
              tidy() %>%
              round_years() %>%
              adjust_for_optimism()

            test_output <- data %>%
              apply_energy_efficiency()


            constant_data <- names(data)[names(data) != 'Demand_drivers']

            expect_equal(data[constant_data], test_output[constant_data])

            # Technologies dataframe still has the same number of rows (one per tech)
            expect_equal(nrow(test_output$Demand_drivers), nrow(data$Demand_drivers))
            expect_equal(test_output$Demand_drivers$commodity,
                         test_output$Demand_drivers$commodity)

          })



# adjust_existing_capacity
test_that('all tables other than technologies are identical and Technologies
          still has correct number of rows after adjust_existing_capacity', {

            data <- raw_data %>%
              interpolate_data() %>%
              tidy()

            test_output <- data %>%
              adjust_existing_capacity()

            # Test all tables other than technologies are identical
            constant_data <- names(data)[names(data) != 'Technologies']

            expect_equal(data[constant_data], test_output[constant_data])

            # Technologies dataframe still has the same number of rows (one per tech)
            expect_equal(nrow(test_output$Technologies), nrow(data$Technologies))
            expect_equal(test_output$Technologies$code, test_output$Technologies$code)


          })


#adjust_existing_capacity
test_that('there is now enough capacity to meet demand after applying
          adjust_existing_capacity', {

            data <- raw_data %>%
              interpolate_data() %>%
              tidy()

            test_output <- data %>%
              adjust_existing_capacity()

            test_capacity <- test_output$Technologies %>%
              left_join(test_output$technology_input_output,
                        by = c('code' = 'technology_code'))%>%
              mutate(total_2020_capacity = existing_capacity_2020
                     * capacity_to_activity_factor
                     * availability_factor)

            test_output_capacity <- test_capacity %>%
              group_by(commodity) %>%
              summarise(total_starting_capacity = sum(total_2020_capacity))

            test_output_capacity %<>%
              left_join(test_output$Demand_drivers %>% filter(year == 2020),
                        by = 'commodity') %>%
              drop_na()

            # Make sure all commodities are accounted for
            expect_equal(nrow(test_output_capacity),
                         nrow(test_output$Demand_drivers %>%
                                filter(year == 2020)))

            expect_true(all(test_output_capacity$total_starting_capacity
                            >= test_output_capacity$demand))


            ## what about process technologies??
          })




# get_existing_techs_matrix
test_that('get_existing_techs_matrix produces a matrix with the correct
          number of rows and columns', {

            data <- raw_data %>%
              interpolate_data() %>%
              tidy()

            test_output <- get_existing_techs_matrix(data)


            ## matrix is produced
            expect_true(is.matrix(test_output))


            ## correct number of rows
            existing_comodities <- get_existing_techs(data) %>%
              pull('commodity') %>%
              unique()

            expect_equal(nrow(test_output), length(existing_comodities))


            ## correct number of columns
            output_comodities <- get_existing_techs(data) %>%
              pull('output_commodity') %>%
              unique()

            expect_equal(ncol(test_output), length(output_comodities))



          })


# get_existing_techs_matrix
test_that('get_existing_techs_matrix produces the expected values', {

  data <- raw_data %>%
    interpolate_data() %>%
    tidy()

  test_output <- get_existing_techs_matrix(data)

  ## Manually code up the matrix in a different way to ensure it matches
  expected_comodities <- get_existing_techs(data) %>%
    select(!existing) %>%
    pivot_wider(names_from = 'output_commodity',
                values_from = 'average_output')

  # arrange and tidy to match the format
  expected_comodities %<>%
    arrange(commodity) %>%
    select(sort(colnames(.))) %>%
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    column_to_rownames(var = 'commodity') %>%
    as.matrix()


  expect_true(all(test_output == expected_comodities))

})



## Below is a draft test:
# get_existing_techs
#
# data <- raw_data %>%
#   interpolate_data() %>%
#   tidy()
#
# test_output <- get_existing_techs(data)
#
# expect_setequal(colnames(test_output), c('output_commodity',
#                                          'commodity',
#                                          'average_output',
#                                          'existing'))
#
#
# commodity_pairs <- data$Technologies %>%
#   filter(existing_capacity_2020 > 0) %>%
#   left_join(data$technology_input_output,
#             by = c('code' = 'technology_code'))
#
# primary_commodities <- commodity_pairs$commodity[commodity_pairs$primary_commodity]
#
#
# commodity_pairs$commodity[(commodity_pairs$output < 0)
#                           & (commodity_pairs$commodity == "INDMAINSHYG")] <- 'HYGEN'
#
# commodity_pairs$commodity[!commodity_pairs$commodity %in% primary_commodities] <- 'fuel'
#
#
#
# ## get unique pairs
# commodity_pairs %<>%
#   select(output_commodity, commodity) %>%
#   distinct()
#
# # add generic fuel var
# generic_fuel <- data.frame(
#   output_commodity = 'fuel',
#   commodity = 'fuel'
# )
#
# commodity_pairs %<>%
#   rbind(generic_fuel) %>%
#   mutate(output_commodity = as.factor(output_commodity),
#          commodity = as.factor(commodity))
#
# commodity_pairs %<>%
#   arrange(output_commodity, commodity)
#
#
# test_output %<>%
#   arrange(output_commodity, commodity) %>%
#   select(output_commodity, commodity)
#
#
# expect_equal(test_output, commodity_pairs)



# get_required_capacities

# adjust_capacity

