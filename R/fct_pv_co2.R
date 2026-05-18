
#===============================================================================
# national

#' Function which gets coefficient associated with CO2 transport from cluster to storage site
#' @param data list of data tables read in from excel data template
#' @param decision_variables dataframe of decision variables
#' @return A two column dataframe. The first column specifies the index of the decision variable.
#' The second column gives the the coefficient associated with CO2 transport costs between cluster and storage site
PV_CO2_national_transport <- function(data, decision_variables) {

  national_CO2_transport <- decision_variables %>%

    # filter out only CO2_transported decision variable
    filter(variable_type == "CO2_transported") %>%

    # combine with data for transport costs
    left_join(
      data$`CO2_T&S_cost`,
      by = c(
        "cluster" = "cluster",
        "terminal" = "terminal",
        "storage_site" = "storage_site"
      )
    ) %>%

    # calculate coefficient in ?m/kt
    mutate(
      coefficient = present_value(
        cost,
        rate = data$rates$discount,
        start_period = year - data$model_parameters$start_year,
        n_periods = data$model_parameters$timestep
      )
    ) %>%
    select(variable_index, coefficient)

  #
  if (nrow(national_CO2_transport) == 0) {
    return(NULL)
  }

  return(national_CO2_transport)

}




#===============================================================================
# cluster to site


#' Function which gets coefficient associated with opex and capex of CO2 pipes from site to cluster
#' @param data list of data tables read in from excel data template
#' @param decision_variables dataframe of decision variables
#' @return A two column dataframe. The first column specifies the index of the decision variable.
#' The second column gives the the coefficient associated with capex or opex costs of H2 pipes between
#' site and cluster
PV_CO2_pipe_cluster_to_site <- function(data, decision_variables)
{

  if (data$model_parameters$use_CCS_Spur == TRUE) {

    data$NAEI_clean <- naei_pipe_to_spur_adjustment(data)
    # note for a future development: add separate columns for ccs and h2 pipes.
    # that way we adjust once and avoid the repetition (this is done in
    # fct_sites.R too)

  }


  # Create a regression model for each flow_rate. Relationship appears linear
  models <-
    lapply(unique(data$CO2_transport_cost$flow_rate_kt), function(flow_rate)
    {
      capex_model <-
        lm(
          Capex ~ 0 + distance_km,
          data = data$CO2_transport_cost %>% filter(flow_rate_kt == flow_rate)
        )
      opex_model <-
        lm(
          Opex ~ 0 + distance_km,
          data = data$CO2_transport_cost %>% filter(flow_rate_kt == flow_rate)
        )

      return(list(capex = capex_model, opex = opex_model))
    })

  names(models) <- unique(data$CO2_transport_cost$flow_rate_kt)

  # We need to get the cost per capacity for each site
  capex_cost_per_capacity <-
    sapply(1:nrow(data$site_H2C02_transport), function(row)
    {
      if (data$site_H2C02_transport$max_CO2[row] == 0)
      {
        return(0)
      }
      distance = data$site_H2C02_transport$pipe_dist[row]
      flow_rate = data$site_H2C02_transport$nearest_flow_rate.CO2[row]
      capex_total = predict(models[[as.character(flow_rate)]]$capex, newdata = data.frame(distance_km = distance))
      cost_per_capacity = capex_total / flow_rate
      return(cost_per_capacity)
    })

  opex_cost_per_capacity <-
    sapply(1:nrow(data$site_H2C02_transport), function(row)
    {
      if (data$site_H2C02_transport$max_CO2[row] == 0)
      {
        return(0)
      }
      distance = data$site_H2C02_transport$pipe_dist[row]
      flow_rate = data$site_H2C02_transport$nearest_flow_rate.CO2[row]
      opex_total = predict(models[[as.character(flow_rate)]]$opex, newdata = data.frame(distance_km = distance))
      cost_per_capacity = opex_total / flow_rate

      return(cost_per_capacity)

    })

  # repeat for trucking_________________________________________________________

  models_truck <-
    lapply(unique(data$CO2_transport_cost$flow_rate_kt), function(flow_rate)
    {
      opex_model <-
        lm(
          fixed_opex_truck ~ 0 + distance_km,
          data = data$CO2_transport_cost %>% filter(flow_rate_kt == flow_rate)
        )

      return(list(opex = opex_model))
    })

  names(models_truck) <-
    unique(data$CO2_transport_cost$flow_rate_kt)


  opex_cost_per_capacity_truck <-
    sapply(1:nrow(data$site_H2C02_transport), function(row)
    {
      if (data$site_H2C02_transport$max_CO2[row] == 0)
      {
        return(0)
      }
      distance = data$site_H2C02_transport$pipe_dist[row]
      flow_rate = data$site_H2C02_transport$nearest_flow_rate.CO2[row]
      opex_total = predict(models_truck[[as.character(flow_rate)]]$opex, newdata = data.frame(distance_km = distance))
      cost_per_capacity = opex_total / flow_rate

      return(cost_per_capacity)

    })

  #__________________________________________________________________________

  # Add these to the dataframe
  max_CO2 <- data$site_H2C02_transport %>%
    add_column(
      capex_cost_per_capacity = capex_cost_per_capacity,
      opex_cost_per_capacity = opex_cost_per_capacity,
      opex_cost_per_capacity_truck = opex_cost_per_capacity_truck
    )

  #### now we want to produce the dataframe of coefficients ####
  # start with opex, which is easiest
  opex_coefficients <- decision_variables %>%
    filter(variable_type == "CO2_pipe_available_capacity") %>%
    left_join(max_CO2, by = "site_ID") %>%

    mutate(
      opex_coefficient = present_value(
        opex_cost_per_capacity,
        rate = data$rates$discount,
        start_period = year - data$model_parameters$start_year,
        n_periods = data$model_parameters$timestep
      )
    ) %>%
    select(variable_index, opex_coefficient)


  # start with opex, which is easiest
  truck_coefficients <- decision_variables %>%
    filter(variable_type == "CO2_truck_used_capacity") %>%
    left_join(max_CO2, by = "site_ID")
  if (nrow(truck_coefficients) == 0) {
    truck_coefficients = NULL
  }
  else {
    truck_coefficients = truck_coefficients %>% mutate(
      coefficient = present_value(
        opex_cost_per_capacity_truck,
        rate = data$rates$discount,
        start_period = year - data$model_parameters$start_year,
        n_periods = data$model_parameters$timestep
      )
    ) %>%
      select(variable_index, coefficient)
  }


  # Now create capex of CO2 pipes
  capex_coefficients <- decision_variables %>%
    filter(variable_type == "CO2_pipe_new_capacity") %>%

    # join in the capex price per capacity
    left_join(select(max_CO2, site_ID, capex_cost_per_capacity), by = "site_ID") %>%

    # Capex is paid off as a loan. The number of payments for the loan is the lifetime of the technology
    mutate(
      payment_per_period = PMT(
        capex_cost_per_capacity,
        data$rates$interest,
        data$Pipes_lifetime$CO2Pipe_lifetime
      )
    ) %>%

    # the number of payments that the system actually has to pay is only up to 2050
    mutate(
      loan_periods = pmin(
        year + data$Pipes_lifetime$CO2Pipe_lifetime - 1,
        data$model_parameters$end_year
      ) - year + 1
    ) %>%

    # discount the costs
    mutate(
      capex_coefficient = present_value(
        payment_per_period,
        rate = data$rates$discount,
        start_period = year - data$model_parameters$start_year,
        n_periods = loan_periods
      )
    ) %>%
    select(variable_index, capex_coefficient)

  # combine capex and opex
  opex_and_capex <-
    bind_rows(opex_coefficients, capex_coefficients) %>%
    rowwise() %>%
    mutate(coefficient = sum(opex_coefficient, capex_coefficient, na.rm = TRUE)) %>%
    select(variable_index, coefficient)

  if (nrow(opex_and_capex) == 0) {
    opex_and_capex = NULL
  }


  all_coefficients = bind_rows(opex_and_capex, truck_coefficients)

  return(all_coefficients)
}

