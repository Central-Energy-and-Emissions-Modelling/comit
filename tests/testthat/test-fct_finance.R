

#present_value
test_that('present_value produces expected values from a table of inputs', {

  input_df <- data.frame(
    fixed_opex = c(1, 1, 1, 1),
    year = raw_data$model_parameters$start_year + c(0, 5, 10, 15)
  )

  # test with single year (no_timestep)
  test_df <- input_df %>%
    mutate(
      pv = present_value(
        FV = fixed_opex,
        rate = 0.01,
        start_period = year - raw_data$model_parameters$start_year,
        n_periods = 1
      )
    )

  expected_pv <- c(1, 0.95, 0.91, 0.86)

  expect_equal(round(test_df$pv, 2), expected_pv)


  # test with five year aggregate (five year timestep)
  test_df_2 <- input_df %>%
    mutate(
      pv = present_value(
        FV = fixed_opex,
        rate = 0.01,
        start_period = year - raw_data$model_parameters$start_year,
        n_periods = 5
      )
    )

  expected_pv <- c(4.9, 4.65, 4.45, 4.22)

  expect_equal(test_df_2$pv, expected_pv, tolerance = 0.01)

})


#present_value
test_that('present_value triggers a warning when a negative interest rate is
          provided', {
            expect_warning(present_value(1000, -1, 1, 1))
          })



#PV_calculation
test_that('PV_calculation produces expected_values', {

  ## test we get the same value when the year is 0.
  expect_equal(PV_calculation(1000, 0.1, 0, 1), 1000)

  ## test we get the same value when the rate is 0.
  expect_equal(PV_calculation(1000, 0, 10, 1), 1000)

  # test we get an expected amount from pre-calculated value
  expect_equal(PV_calculation(2200, 0.03, 1, 1),
               2135.92, # this is from an example on investopedia
               tolerance = 0.01)
})


#PMT
test_that('PMT produces correct values for payments', {

  # test case 1
  test_df <- data.frame(pv = c(10, 50, 100))

  test_df %<>% mutate(payment = PMT(pv, 0.01, 5))

  expected_values <- c(2.06, 10.30, 20.60)

  expect_equal(round(test_df$payment, 2), expected_values)


  # test case 2
  test_df2 <- data.frame(pv = c(1, 5, 6))

  test_df2 %<>% mutate(payment = PMT(pv, 0.1, 25))

  expected_values2 <- c(0.11, 0.55, 0.66)

  expect_equal(round(test_df2$payment, 2), expected_values2)


  # test case 3 - check value is purely a split if interest is close to 0
  # note if rate is actually 0, result is NaN
  expect_equal(PMT(1000, 0.000001, 4), 250, tolerance = 0.001)


  # test case 4 - check value is principle + (interest * principle)
  # if only 1 period
  expect_equal(PMT(1000, 0.1, 1), 1000 + (1000 * 0.1))
})



# base_year_adjustment
test_that('base_year_adjustment is calculated correctly', {

  # set up mock parameters and deflator data
  parameter_data <- list()

  parameter_data$gdp_deflators <- data.frame(year = c(2000:2010),
                                             deflator = c(90:100))

  parameter_data$model_parameters$base_price_year <- 2005
  parameter_data$rates$base_year_of_input <- 2008

  deflators <- parameter_data$gdp_deflators

  ratio <- (deflators[deflators$year == 2005, 'deflator']
            / deflators[deflators$year == 2008, 'deflator'])

  # test some values
  expect_equal(base_year_adjustment(1, parameter_data), 1 * ratio)
  expect_equal(base_year_adjustment(35, parameter_data), 35 * ratio)
  expect_equal(base_year_adjustment(10000, parameter_data), 10000 * ratio)

})







