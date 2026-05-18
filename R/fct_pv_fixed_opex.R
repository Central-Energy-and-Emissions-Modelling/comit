

#' Function which gets the coefficient associated with fixed opex for each
#' decision variable
#'
#' @inheritParams comit_objective_function
#' @param decision_variables dataframe for the decision variables.
#'
#' @return A two column dataframe. The first column specifies the index of the
#' decision variable. The second column gives the the coefficient associated
#' with fixed opex.
#' @export
PV_fixed_opex <- function(data, decision_variables) {


  fixed_opex_coefficient <- decision_variables %>%

    # filter for only available capacity decision variable
    filter(variable_type == "available_capacity") %>%

    # combine with data for fixed opex
    left_join(data$Technologies, by = "code") %>%

    # calculate coefficient for fixed opex based on present value
    # fixed opex needs to be paid annually for the amount of years specified as the model timestep
    # hence, calculate costs over 5 years (or whatever the timestep is)
    mutate(
      coefficient = present_value(
        FV = fixed_opex,
        rate = data$rates$discount,
        start_period = year - data$model_parameters$start_year,
        n_periods = data$model_parameters$timestep
      )
    ) %>%
    select(variable_index, coefficient)

  return(fixed_opex_coefficient)
}
