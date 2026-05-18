
## Things to test:
# what should happen if first val is 0?
# what should happen if a val is NA?
# check we get the expected values from a preset set of values
# what happens if we miss a year
# what happens if we have unequal year steps
# constant values when two adjacent times have equal values

# test all effected data tables have the same number of rows as there should be
# number of modelled years.


# note: need to set timestep value manually to avoid changes when different templates are used


#interpolate_all_columns
test_that('interpolate_all_columns creates the expected values within columns',
          {
            ## test case 1: 5 yearly specified data
            test_input <- data.frame(
              years = c(2020:2050),
              var1 = c(0, NA, NA, NA, NA,
                       rep(c(1, NA, NA, NA, NA), 5),
                       10),
              var2 = c(10, NA, NA, NA, NA,
                       rep(c(100, NA, NA, NA, NA), 5),
                       100),
              var3 = c(0:30)
            )

            expected_output <- data.frame(
              years = c(2020:2050),
              var1 = c(0, 0.2, 0.4, 0.6, 0.8,
                       rep(1, 21),
                       2.8, 4.6, 6.4, 8.2, 10.0),
              var2 = c(10, 28, 46, 64, 82,
                       rep(100, 26)),
              var3 = c(0:30)
            )

            test_output <- interpolate_all_columns(test_input)

            expect_equal(test_output, expected_output)


            ## test case 2: 2 yearly specified data and some changes in direction over time
            test_input <- data.frame(
              years = c(2020:2030),
              var1 = c(10, NA, 12, NA, 14, NA, 12, NA, 10, NA, 8)
            )

            expected_output <- data.frame(
              years = c(2020:2030),
              var1 = c(10:14, 13:8)
            )

            test_output <- interpolate_all_columns(test_input)

            expect_equal(test_output, expected_output)


            ## test case 3: sporadically specified data, with different range of years
            test_input <- data.frame(
              years = c(2025:2035),
              var1 = c(10, 11, NA, NA, NA, 15, NA, 17, NA, NA, 20),
              var2 = c(0, 20, 30, NA, 50, NA, 70, NA, 80, NA, 95)
            )

            expected_output <- data.frame(
              years = c(2025:2035),
              var1 = c(10:20),
              var2 = c(0, seq(20, 70, by = 10), 75, 80, 87.5, 95)
            )

            test_output <- interpolate_all_columns(test_input)

            expect_equal(test_output, expected_output)

          })



#interpolate_all_columns
test_that('interpolate_all_columns triggers a warning if start or end values are
           missing', {

             # missing end value
             test_input <- data.frame(
               years = c(2020:2030),
               var1 = c(10, NA, 12, NA, 14, NA, 12, NA, 10, NA, NA)
             )

             expect_warning(interpolate_all_columns(test_input))

             # missing start value
             test_input <- data.frame(
               years = c(2020:2030),
               var1 = c(NA, NA, 12, NA, 14, NA, 12, NA, 10, NA, 8)
             )

             expect_warning(interpolate_all_columns(test_input))
           })


#interpolate_all_columns
test_that('interpolate_all_columns does not interpolate when there is no missing data',
          {
            mock_data <- data.frame(
              year = c(2020:2025),
              c(0, 100, 1000, 10000, 10000, 10000) # use non-linear data to check this
            )

            test_out <- mock_data %>%
              interpolate_all_columns()

            expect_equal(test_out, mock_data) # out and in should be the same
          })



#interpolate_for_years
test_that('interpolate_for_years returns a dataframe containing only rows for
          modelled years and then the interpolated values', {

            all_years <- data.frame(year = c(2020:2050))

            # use 2 year timestep for testing
            modelled_years <- data.frame(year = seq(from = 2020, to = 2050, by = 2))

            # use a 5 year interval df to test
            input_df <- data.frame(
              year = seq(from = 2020, to = 2050, by = 5),
              var1 = c(0:6),
              var2 = c(2, 4, 6, 8, 10, 12, 14)
            )

            expected_output <- data.frame(
              year = modelled_years,
              var1 = seq(from = 0, to = 6, by = 0.4),
              var2 = seq(from = 2, to = 14, by = 0.8)
            )

            test_output <- interpolate_for_years(input_df, all_years, modelled_years)

            expect_equal(test_output, expected_output)


            # check 1 timestep works too

            modelled_years <- data.frame(year = seq(from = 2020, to = 2050, by = 1))

            expected_output <- data.frame(
              year = modelled_years,
              var1 = seq(from = 0, to = 6, by = 0.2),
              var2 = seq(from = 2, to = 14, by = 0.4)
            )

            test_output <- interpolate_for_years(input_df, all_years, modelled_years)

            expect_equal(test_output, expected_output)
          })


#interpolate_for_years
test_that('interpolate_for_years raises an error if a string is supplied in a
          table to be interpolated', {

            all_years <- data.frame(year = c(2020:2050))

            # use 2 year timestep for testing
            modelled_years <- data.frame(year = seq(from = 2020, to = 2050, by = 2))

            input_df <- data.frame(
              year = seq(from = 2020, to = 2050, by = 5),
              var1 = c(0:6),
              var2 = 'test_string')

            expect_error(interpolate_for_years(input_df,
                                                all_years,
                                                modelled_years)) %>%
              suppressWarnings()
            })



#interpolate_data
test_that('interpolate_data works on test input template',
          {
            test_data <- read_excel_data_template(input_template_test_file)

            # need to add a few basic params to list of data
            test_data[['model_parameters']] <- data.frame(
              start_year = 2020,
              end_year = 2050,
              timestep = 2
            )

            test_output <- interpolate_data(test_data)

            # create expected df
            expected_min_fuel_constraints <- data.frame(
              year = c('apply_to_industry_only',
                       seq(from = 2020, to = 2050, by = 2)),
              test_fuel1 = c(1,
                             10, 14, 18, 22, 26, 30,
                             rep(30, 10)),
              test_fuel2 = c(0, rep(1, 16))
            ) %>% as_tibble()
            # ^ remember to account for the 'apply_to_industry_only' first row


            # check interpolation has occured in min_fuel_constraints
            expect_equal(test_output$min_fuel_constraints,
                         expected_min_fuel_constraints)

            # check that all other tables are unchanged
            expect_equal(test_data[names(test_data) != 'min_fuel_constraints'],
                         test_output[names(test_output) != 'min_fuel_constraints'])

            # most tables should be missing - check that the message is flagged
            expect_message(interpolate_data(test_data))
          })


#interpolate_data
test_that('interpolate_data works on actual input template', {

  test_data2 <- raw_data

  # test with 2 year timestep
  test_data2$model_parameters$timestep <- 2

  test_data2 %<>% interpolate_data()

  n_timesteps <- 1 + ((test_data2$model_parameters$end_year
                      - test_data2$model_parameters$start_year)
                      / test_data2$model_parameters$timestep)

  # test a few random tables to make sure they have one row for each year
  expect_equal(nrow(test_data2$min_fuel_constraints),
               n_timesteps + 1) # +1 becacuse of first row in fuel col

  expect_equal(nrow(test_data2$supply_chain_constraints),
               n_timesteps)

  expect_equal(nrow(test_data2$Carbon_price),
               n_timesteps)

  # all tables should be there when reading this in, so shouldn't get a message
  expect_no_message(raw_data %>% interpolate_data())

  # check some non-interpolated tables are unchanged.
  expect_equal(raw_data$cluster_radius, test_data2$cluster_radius)
  expect_equal(raw_data$Technologies, test_data2$Technologies)

})





