
#' Function which gets coefficient associated with capex of each technology
#' @param data list of data tables read in from excel data template
#' @param decision_variables dataframe of decision variables
#' @return A two column dataframe. The first column specifies the index of the decision variable.
#' The second column gives the the coefficient associated with capex costs
PV_technology_capex <- function(data, decision_variables)
{
  # If capex cost isn't included in the objective function, return nothing
  if (data$objective_function$include[data$objective_function == "PV_technology_capex"] == FALSE)
  {
    return(NULL)
  }

  # Filter out the new_capacity decision variables
  tech_capex <- decision_variables %>%
    filter(variable_type == "new_capacity") %>%

    # add in the technology start year and filter out rows where the year is lower than the start year
    left_join(select(data$Technologies, code, capex, lifetime, start_year),
              by = "code") %>%
    filter(year >= start_year) %>%

    # capex is paid off as a loan. The number of payments for the loan is the lifetime of the technology
    mutate(payment_per_period = PMT(capex, data$rates$interest, lifetime)) %>%

    # the number of payments that the system actually has to pay is only up to end year
    mutate(loan_periods = pmin(year + lifetime - 1, data$model_parameters$end_year) - year + 1) %>%

    # discount the costs
    mutate(
      coefficient = present_value(
        payment_per_period,
        rate = data$rates$discount,
        start_period = year - data$model_parameters$start_year,
        n_periods = loan_periods
      )
    ) %>%

    # tidy up
    select(variable_index, coefficient)

  return(tech_capex)
}
