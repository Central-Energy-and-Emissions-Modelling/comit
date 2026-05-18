

# read_excel_data_template
test_that('read_excel_data_template returns a list of dataframes only',
          {
            test_input <- read_excel_data_template(input_template_test_file)

            expect_equal(class(test_input), 'list')

            expect_true(is.data.frame(test_input[[1]]))
            expect_true(is.data.frame(test_input[[3]]))
            expect_true(is.data.frame(test_input[[6]]))

          })


# read_excel_data_template
test_that('read_excel_data_template returns values as expected based on a test
          template', {

  test_input <- read_excel_data_template(input_template_test_file)

  # first and third tab
  expected_values <- tibble(
    index = c(1:20),
    y1 = c(rep(0, 10), rep(1, 10)),
    y2 = c(rep(1, 10), rep(0, 10)),
    y3 = rep(c(1,0), 10)
  )

  # second tab
  extra_cols <- tibble(
    y4 = rep(1, 20),
    y5 = as.numeric(c(1, 1,
                      NA_character_, NA_character_, NA_character_,
                      1, 1, 1, 1, 1,
                      NA_character_,
                      1, 1, 1, 1, 1,
                      NA_character_,
                      1, 1,
                      NA_character_))
  )

  tab2_expected_values <- cbind(expected_values, extra_cols) %>%
    as_tibble()


  # test statements
  expect_equal(length(test_input), 7)
  expect_equal(test_input[[1]], expected_values)
  expect_equal(test_input[[3]], tab2_expected_values)
  expect_equal(test_input[[6]], expected_values)

  })


#get_template_sheet_names
test_that('get_template_sheet_names returns the correct sheet names from the
          test file',
          {
            sections <- c('Constraints',
                          'Assumptions',
                          'Data',
                          'Attribution') # this fails if 'Data Prepartions', doesn't
            # exactly match, should we make the code more robust???
            # a good approach could be to lowercase all sheet names and set spaces to _

            test_sheets <- get_template_sheet_names(input_template_test_file, sections)

            expect_equal(test_sheets, c('test_tab1',
                                        'min_fuel_constraints',
                                        'test_tab2',
                                        'Technologies',
                                        'technology_input_output',
                                        'test_tab3',
                                        'Cluster_connections'))

            # make sure the pre sheet doesn't get read
            expect_false('pre_sheet' %in% test_sheets)

          })



# read_input_sheet
test_that('read_input_sheet returns a dataframe matching the expected values
          from the test file',
          {
            test_sheet1 <- read_input_sheet(input_template_test_file, 'test_tab1')
            test_sheet3 <- read_input_sheet(input_template_test_file, 'test_tab3')

            expected_values <- tibble(
              index = c(1:20),
              y1 = c(rep(0, 10), rep(1, 10)),
              y2 = c(rep(1, 10), rep(0, 10)),
              y3 = rep(c(1,0), 10)
            )

            expect_true(is.data.frame(test_sheet1))
            expect_equal(test_sheet1, expected_values)

            expect_true(is.data.frame(test_sheet3))
            expect_equal(test_sheet3, expected_values)
          })


# pivot_specified_table
test_that('pivot_specified_table performs the correct pivot and makes no other
          changes',
          {
            pivot_details <- c('min_fuel_constraints', 'fuel', 'min')

            test_input <- read_excel_data_template(input_template_test_file)

            test_pivoted <- pivot_specified_table(test_input, pivot_details)

            test_input$min_fuel_constraints %<>%
              pivot_longer(cols = !year,
                           names_to = 'fuel',
                           values_to = 'min')


            expect_equal(test_pivoted$min_fuel_constraints,
                         test_input$min_fuel_constraints)

            expect_equal(test_pivoted, test_input)

          })


# tidy_fuel_constraints
test_that('tidy_fuel_constraints produces a dataframe with the correct number
          of rows and the expected columns',
          {
            test_input <- read_excel_data_template(input_template_test_file)

            test_input <- pivot_specified_table(test_input, c('min_fuel_constraints',
                                                              'fuel',
                                                              'min'))

            test_input_fuel <- test_input$min_fuel_constraints %>%
              filter(year!='apply_to_industry_only')

            min_or_max <- 'min'

            test_output <- tidy_fuel_constraints(test_input,
                                                 'min_fuel_constraints',
                                                 min_or_max)

            expected_rows <- (length(unique(test_input_fuel$year))
                              * (length(unique(test_input_fuel$fuel)))
            )

            expect_equal(nrow(test_output), expected_rows)

            expect_equal(colnames(test_output),
                         c('year', 'fuel', 'group', 'apply_to_industry_only',
                           min_or_max))
          })


#tidy_cluster_connections
test_that('tidy_cluster_connections creates the correct number of rows, and the
          right columns with the right types',
          {

  test_input <- read_excel_data_template(input_template_test_file)

  n_clusters <- test_input$Cluster_connections$Cluster %>%
    length()

  test_output <- tidy_cluster_connections(test_input)

  expect_equal(nrow(test_output), combos_with_repitition(r = 2, n = n_clusters))

  expect_equal(colnames(test_output),
               c('cluster_1', 'cluster_2', 'allowed_route'))

  expect_type(test_output$cluster_1, 'character')
  expect_type(test_output$cluster_2, 'character')
  expect_type(test_output$allowed_route, 'logical')
})


#sort_retrofit
test_that('sort_retrofit when retrofit is TRUE and column is non-null',
          {
            test_input <- read_excel_data_template(input_template_test_file)

            # need to mimic model parameters df
            test_input$model_parameters <- data.frame(use_retrofit = TRUE)

            test_output <- sort_retrofit(test_input)

            # No changes are made
            expect_equal(test_input, test_output)
          })


#sort_retrofit
test_that('sort_retrofit when retrofit is FALSE and column is non-null',
          {

            test_input <- read_excel_data_template(input_template_test_file)

            # need to mimic model parameters df
            test_input$model_parameters <- data.frame(use_retrofit = FALSE)

            test_output <- sort_retrofit(test_input)


            removed_vals <- test_input$Technologies[
              !test_input$Technologies$code %in% test_output$Technologies$code, ]

            # No NA vals for retrofit_to removed
            expect_true(all(!is.na(removed_vals$retrofit_to)))

            # only get the values we want
            expect_true(all(is.na(test_output$Technologies$retrofit_to)))
            expect_true(all(
              !str_detect(test_output$technology_input_output$technology_code, '_R$')
            ))


            # no other tables effected
            cols_to_drop <- c('Technologies',
                              'technology_input_output')

            expect_equal(test_output[!names(test_output) %in% cols_to_drop],
                         test_input[!names(test_output) %in% cols_to_drop])

          })


#sort_retrofit
test_that('#sort_retrofit when retrofit column is non-null', {

  test_input <- read_excel_data_template(input_template_test_file)

  test_input$model_parameters <- data.frame(use_retrofit = TRUE)

  test_input$Technologies %<>%
    select(!retrofit_to)

  test_output <- sort_retrofit(test_input)

  expect_false(test_output$model_parameters$use_retrofit)
  expect_true(all(is.na(test_output$Technologies$retrofit_to)))

})


# tidy
test_that('tidy does not effect tables it shouldnt and does effect tables it
          should', {

            test_input <- read_excel_data_template(input_template_test_file)

            # need to mimic model parameters df
            test_input$model_parameters <- data.frame(use_retrofit = TRUE)

            test_output <- test_input %>%
              tidy()

            # returns list of same length as input
            expect_equal(class(test_output), 'list')
            expect_equal(length(test_input), length(test_output))

            # non effected tables remain the same
            expect_equal(test_input[[1]], test_output[[1]])
            expect_equal(test_input[[3]], test_output[[3]])

            # finally check that some changes are actioned, i.e. we do get changes
            test_input <- pivot_specified_table(test_input, c('min_fuel_constraints',
                                                              'fuel',
                                                              'min'))

            test_input$min_fuel_constraints <- tidy_fuel_constraints(test_input,
                                                                     'min_fuel_constraints',
                                                                     'min')

            expect_equal(test_input[['min_fuel_constraints']],
                         test_output[['min_fuel_constraints']]
            )

          })


# round years
test_that('round_years produces values for the columns effected that are
          multiples of the timestep used', {

            test_input <- raw_data

            # 5 year timestep
            test_input$model_parameters <- data.frame(timestep = 5)

            test_output <- test_input %>% round_years()

            poss_values <- seq(from = 0, to = 5000,
                               by = test_output$model_parameters$timestep)

            expect_true(all(test_output$Technologies$start_year %in% poss_values))
            expect_true(all(test_output$Technologies$lifetime %in% poss_values))

            # 2 year timestep
            test_input <- raw_data

            test_input$model_parameters <- data.frame(timestep = 2)

            test_output <- test_input %>% round_years()

            poss_values <- seq(from = 0, to = 5000,
                               by = test_output$model_parameters$timestep)

            expect_true(all(test_output$Technologies$start_year %in% poss_values))
            expect_true(all(test_output$Technologies$lifetime %in% poss_values))
          })


# adjust_for_optimism
test_that('adjust_for_optimism applies the expected multiplication to the expected
          columns', {

            test_input <- raw_data

            # first set of values
            test_input$model_parameters$tech_optimism_adjustment <- 0.5
            test_input$model_parameters$pipes_optimism_adjustment <- 1

            test_output <- adjust_for_optimism(test_input)

            expect_equal(test_input$Technologies$capex * 0.5,
                         test_output$Technologies$capex)

            expect_equal(test_input$CO2_transport_cost$Capex,
                         test_output$CO2_transport_cost$Capex)


            # second set of values
            test_input$model_parameters$tech_optimism_adjustment <- 2
            test_input$model_parameters$pipes_optimism_adjustment <- 3

            test_output <- adjust_for_optimism(test_input)

            expect_equal(test_input$Technologies$capex * 2,
                         test_output$Technologies$capex)

            expect_equal(test_input$CO2_transport_cost$Capex * 3,
                         test_output$CO2_transport_cost$Capex)

          })




### Below can be used to test parameter tabs are joined correctly

# data <- raw_data
#
# data$model_parameters_a <- tibble(parameter = c('a', 'b', 'c'), value = c('1', '2', '3'))
# data$model_parameters_b <- tibble(parameter = c('d', 'e', 'f'), value = c(TRUE, TRUE, FALSE))
#
# data$model_parameters_a %<>%
#   pivot_wider(names_from = parameter,
#               values_from = value)
#
# data$model_parameters_b %<>%
#   pivot_wider(names_from = parameter,
#               values_from = value)
#
# data$model_parameters <- cbind(data$model_parameters_a, data$model_parameters_b)
#
#
# data <- data[!names(data) %in% c('model_parameters_a', 'model_parameters_b') ]





