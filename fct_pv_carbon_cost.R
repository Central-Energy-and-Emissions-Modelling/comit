
#' Function which gets coefficient associated with carbon cost of each used technology
#' @param data list of data tables read in from excel data template
#' @param decision_variables dataframe of decision variables
#' @return A two column dataframe. The first column specifies the index of the decision variable.
#' The second column gives the the coefficient associated with carbon cost
PV_carbon_cost <- function(data, decision_variables) {

  carbon_cost <- total_emissions_for_variables(decision_variables, data)

  carbon_cost %<>%
    # work out how much is traded and untraded
    mutate(
      traded_CO2e = traded_site * net_CO2,
      untraded_CO2e = as.numeric(!traded_site) * net_CO2 + net_non_CO2
    ) %>%

    # join carbon prices and work out total carbon cost
    left_join(data$Carbon_price, by = c("year" = "year")) %>%
    mutate(carbon_cost = CarbonCost_traded * traded_CO2e + CarbonCost_untraded * untraded_CO2e)  %>%

    # discount carbon cost. Also, carbon cost needs to paid annually.
    mutate(
      coefficient = present_value(
        FV = carbon_cost,
        rate = data$rates$discount,
        start_period = year - data$model_parameters$start_year,
        n_periods = data$model_parameters$timestep
      )
    ) %>%
    # NEW RETROFIT CODE
    left_join(select(data$Technologies, code, retrofit_to), by = c("code")) %>% left_join(
      x = .,
      y = select(., code, year, site_ID, coefficient),
      by = c("retrofit_to" = "code", "site_ID", "year")
    ) %>%
    mutate(coefficient = ifelse(
      is.na(retrofit_to),
      coefficient.x,
      coefficient.x - coefficient.y
    )) %>%
    select(variable_index, coefficient)

  return(carbon_cost)
}




total_emissions_for_variables <- function(decision_variables, data) {

  # In the model we have traded and non-traded emissions
  # All emissions are non-traded except CO2 emitted into the atmosphere from traded sites

  # Carbon emitted depends entirely on used_capacity
  carbon_cost <- decision_variables %>%
    filter(variable_type %in% c("used_capacity", "non_industry_H2"))

  # get traded status
  carbon_cost %<>%
    left_join(
      data$NAEI_clean %>%
        mutate(traded_site = grepl(".*(?<!npsg)$", site_name, perl = TRUE)) %>%
        select(site_ID, traded_site),
      by = "site_ID"
    ) %>%
    # non industrial H2 demand is classed as a non traded site
    mutate(traded_site = if_else(variable_type == "non_industry_H2", FALSE, traded_site))


  # add net emissions per unit of used capacity
  carbon_cost %<>%
    mutate(
      net_CO2 = net_emissions(
        code,
        year = year,
        gas = "CO2",
        .data = data
      ),
      net_non_CO2 = net_emissions(
        code,
        year = year,
        gas = "nonCO2",
        .data = data
      )
    )


  carbon_cost %<>%
    # join emissions from non-industrial hydrogen and combine with released_CO2 and released_non_CO2
    left_join(data$Fuel_emissions,
              by = c("code" = "commodity", "year" = "year")) %>%
    left_join(data$commodities, by = c("code" = "commodity")) %>%
    mutate(
      net_CO2 = if_else(
        variable_type == "non_industry_H2",
        CO2e * proportion_emissions_CO2,
        net_CO2
      ),
      net_non_CO2 = if_else(
        variable_type == "non_industry_H2",
        CO2e * (1 - proportion_emissions_CO2),
        net_non_CO2
      )
    )

  return(carbon_cost)

}

