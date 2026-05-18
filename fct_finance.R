# Functions associated with calculating costs and other financial calculations

#' Function to calculate present value in given periods
#'
#' Perform present value calculations on future values at a specified year. This
#'  allows for values for things such as fuels and technologies to be considered
#'  at the present value, to allow for more accurate decision making in the model.
#'  Note that this function can also be used to aggregate the present values at
#'  multiple years, which is helpful when using a model with a timestep, to account
#'  for accrued costs (such as opex) in non-modelled years.
#'
#' @param FV numeric, or numeric column of a df.
#'  This Future Value: the actual value at a given point in time.
#' @param rate numeric. The assumed interest rate. This should be provided as a
#'  decimal value, e.g. 1% interest is provided as 0.01.
#' @param start_period integer, or integer column of a df.
#'  The period for which the Future Value is provided.
#'  This value should be provided as the number of years from the model's start
#'  year, where the start year is 0. It is not the actual year (i.e. do not
#'  input the year as "2050").
#' @param n_periods integer. The number of periods after the period for which
#'  the present value should be evaluated. This is usually the model's timestep
#'  parameter. If it is set to 1, it is just a standard present value calculation,
#'  otherwise it allows for the summing of values for years not modelled, when it
#'  is appropriate, such as for in the calculation of opex or fuel values.
#'
#' @return vector of numeric values, of same length as FV input parameter. Each
#'  value is the present value calculated for the respective future value provided.
#'  In cases where n_periods is > 1, this value is the sum of the present values
#'  for the start_period and the subsequent n_periods that follow.
#'
#' @export
present_value <- function(FV, rate, start_period, n_periods = 1) {

  # Do some sense checking on function arguments
  if(rate < 0) {
    warning("rate is negative. Expected positive value")
    }

  # Use mapply so we can pass multiple columns of a dataframe as arguments when
  # the function gets called in the system
  PV <- mapply(PV_calculation, FV, rate, start_period, n_periods)

  return(PV)
}



### developing a quicker method using matrices and avoiding mapply #############

present_value_quick <- function(FV, rate, start_period, n_periods = 1) {

  periods <- outer(start_period, 0:(n_periods - 1), `+`)
  discount_matrix <- (1 + rate) ^ periods
  PV <- rowSums(FV / discount_matrix)

  return(PV)
}

################################################################################


#' Calculate a single present value
#'
#' Helper function to present_value. By specifying the calculation separately,
#'  we can call it in within `present_value()` to allow for mapply to be used in
#'  order to provide the function within 'mutate' and provide multiple columns
#'  as arguments.
#'
#' Present value is defined as PV = FV * (1 / (1+rate)^period)
#'
#' @param FV, numeric. Future value.
#' @param rate numeric. Assumed interest rate.
#' @param start_period integer. The year of the respective future value. First
#'  modelled year is 0, i.e. not 2020.
#' @param n_periods number of years after the year provided to aggregate, useful
#'  if a timestep model is ran and costs need to be accrued.
#'
#' @return numeric. The present value or accrued present value.
PV_calculation <- function(FV, rate, start_period, n_periods) {

  PV <- sum(FV / (1 + rate) ^ (start_period:(start_period + n_periods - 1)))

  return(PV)
}




#' Calculates the payment for a loan based on constant payments and a constant
#' interest rate
#'
#' Estimates the value of periods to be made in each period. Similar to the
#'  excel formula PMT.
#'
#' @param principal numeric, or vector of numeric values. The principal present
#'  value.
#' @param rate numeric. The interest rate for the loan.
#' @param n_payments integer. The total number of payments for the loan. Usually
#'  a lifetime variable.
#'
#' @return numeric, or vector of numeric values with same length as principle.
#'  Required payment per loan period.
#'
#' @author Parameter descriptions from Microsoft Corporation 2021
PMT <- function(principal, rate, n_payments) {

  payment_per_period <- (principal
                         * (rate * (1 + rate) ^ n_payments)
                         / ((1 + rate) ^ n_payments - 1)
                         )

  return(payment_per_period)
}






#' Get prices in base year value.
#'
#' Inflates prices by required amount in order to set prices to the base year
#' specified in the input spreadsheet. This is used to convert the costs in the
#' outputs.
#'
#' @param price_at_actual numeric. The price to be inflated.
#'
#' @return numeric. The inflated price.
base_year_adjustment <- function(price_at_actual, parameter_data) {

  required_base_price_year <- parameter_data$model_parameters$base_price_year
  actual_base_price_year <- parameter_data$rates$base_year_of_input

  deflator_at_required <- get_deflator(required_base_price_year, parameter_data)
  deflator_at_actual <- get_deflator(actual_base_price_year, parameter_data)

  return(price_at_actual * deflator_at_required / deflator_at_actual)
}


#' Helper function to pull correct deflator and given year
get_deflator <- function(year, parameter_data) {

  gdp_deflators <- parameter_data$gdp_deflators

  return(gdp_deflators[gdp_deflators$year == year, ]$deflator)
}

