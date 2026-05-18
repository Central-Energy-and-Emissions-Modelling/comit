
#===============================================================================
# national

#' Function which gets coefficient associated with H2 pipes between clusters
#' @param data list of data tables read in from excel data template
#' @param decision_variables dataframe of decision variables
#' @return A two column dataframe. The first column specifies the index of the decision variable.
#' The second column gives the the coefficient associated with opex and capex of H2 pipes connecting clusters
PV_H2_pipe_national <- function(data, decision_variables)
{
  # if we are not modelling hydrogen production technologies explicitly, return nothing
  if (!data$model_parameters$model_H2_production) { return(NULL) }

  # We assume pipes are built to 1GW capacity, create model of capex and opex vs distance.
  # Both capex and opex and related quadratically to distance
  capacity_MW <- 1000
  capacity_PJ <- capacity_MW * 0.03154

  H2_capex_mod <-
    lm(
      capex ~ poly(distance, 2),
      data = data$H2_transport_cost %>% filter(capacity_MW == !!capacity_MW)
    )
  H2_fixed_opex_mod <-
    lm(
      fixed_opex ~ poly(distance, 2),
      data = data$H2_transport_cost %>% filter(capacity_MW == !!capacity_MW)
    )

  # Now create capex coefficients
  H2_pipe_distances <- decision_variables %>%
    filter(
      variable_type == "H2_national_pipe_new_capacity" |
        variable_type == "H2_national_pipe_available_capacity"
    ) %>%

    # add distances between clusters
    left_join(
      select(data$Cluster_location, Cluster, Latitude, Longitude),
      by = c("cluster" = "Cluster")
    ) %>%
    left_join(
      select(data$Cluster_location, Cluster, Latitude, Longitude),
      by = c("pipe_cluster_end" = "Cluster"),
      suffix = c("", ".pipe_end")
    ) %>%
    mutate(distance = hav.dist(Longitude, Latitude, Longitude.pipe_end, Latitude.pipe_end)) %>%

    # Add in the capex and opex cost for a 31.5PJ pipe
    modelr::spread_predictions(H2_capex_mod, H2_fixed_opex_mod) %>%

    # divide by 31.5PJ to get per PJ caosts
    mutate(capex = H2_capex_mod / capacity_PJ,
           opex = H2_fixed_opex_mod / capacity_PJ) %>%
    mutate(capex_or_opex = if_else(
      variable_type == "H2_national_pipe_new_capacity",
      capex,
      opex
    )) %>%

    # clean up
    group_by(variable_index) %>%
    summarise(coefficient = min(capex_or_opex), .groups = "drop")

  return(H2_pipe_distances)
}




#===============================================================================
# cluster to site

#' Function which gets coefficient associated with opex and capex of H2 pipes from site to cluster
#' @param data list of data tables read in from excel data template
#' @param decision_variables dataframe of decision variables
#' @return A two column dataframe. The first column specifies the index of the decision variable.
#' The second column gives the the coefficient associated with capex or opex costs of H2 pipes between
#' site and cluster
PV_H2_pipe_cluster_to_site <- function(data, decision_variables) {

  capacity_PJ <- 0.03154   #pJ to seconds in year - need to check
  # choose the transport option ( Trucking = 1, Pipeline = 2, Grid = 3)

  # Create a regression model for each flow_rate________________________________
  models_grid <-
    lapply(unique(data$H2_transport_cost$capacity_MW), function(flow_rate)
    {
      opex_model <-
        lm(
          fixed_opex_grid ~ 0 + distance,
          data = data$H2_transport_cost %>% filter(capacity_MW == flow_rate)
        )

      return(list(opex = opex_model))
    })
  names(models_grid) <- unique(data$H2_transport_cost$capacity_MW)

  # We need to get the cost per capacity for each site
  grid_cost_per_capacity <-
    sapply(1:nrow(data$site_H2C02_transport), function(row)
    {
      if (data$site_H2C02_transport$max_H2[row] == 0)
      {
        return(0)
      }
      distance = data$site_H2C02_transport$pipe_dist[row]
      flow_rate = data$site_H2C02_transport$nearest_flow_rate.H2[row]
      opex_total = predict(models_grid[[as.character(flow_rate)]]$opex, newdata = data.frame(distance = distance))
      cost_per_capacity = opex_total / flow_rate
      return(cost_per_capacity)
    })


  # Create a regression model for each flow_rate________________________________
  models_truck <-
    lapply(unique(data$H2_transport_cost$capacity_MW), function(flow_rate)
    {
      opex_model <-
        lm(
          fixed_opex_truck ~ 0 + distance,
          data = data$H2_transport_cost %>% filter(capacity_MW == flow_rate)
        )
      return(list(opex = opex_model))
    })


  names(models_truck) <- unique(data$H2_transport_cost$capacity_MW)

  # We need to get the cost per capacity for each site
  truck_cost_per_capacity <-
    sapply(1:nrow(data$site_H2C02_transport), function(row)
    {
      if (data$site_H2C02_transport$max_H2[row] == 0)
      {
        return(0)
      }
      distance = data$site_H2C02_transport$pipe_dist[row]
      flow_rate = data$site_H2C02_transport$nearest_flow_rate.H2[row]
      opex_total = predict(models_truck[[as.character(flow_rate)]]$opex, newdata = data.frame(distance = distance))
      cost_per_capacity = opex_total / flow_rate
      return(cost_per_capacity)
    })



  # Create a regression model for each flow_rate________________________________
  models_pipe <-
    lapply(unique(data$H2_transport_cost$capacity_MW), function(flow_rate)
    {
      capex_model <-
        lm(capex ~ 0 + distance,
           data = data$H2_transport_cost %>% filter(capacity_MW == flow_rate))
      opex_model <-
        lm(
          fixed_opex_pipe ~ 0 + distance,
          data = data$H2_transport_cost %>% filter(capacity_MW == flow_rate)
        )

      return(list(capex = capex_model,
                  opex = opex_model))
    })
  names(models_pipe) <- unique(data$H2_transport_cost$capacity_MW)


  # We need to get the cost per capacity for each site
  capex_cost_per_capacity <-
    sapply(1:nrow(data$site_H2C02_transport), function(row)
    {
      if (data$site_H2C02_transport$max_H2[row] == 0)
      {
        return(0)
      }
      distance = data$site_H2C02_transport$pipe_dist[row]
      flow_rate = data$site_H2C02_transport$nearest_flow_rate.H2[row]
      capex_total = predict(models_pipe[[as.character(flow_rate)]]$capex, newdata = data.frame(distance = distance))
      cost_per_capacity = capex_total / flow_rate
      return(cost_per_capacity)
    })

  opex_cost_per_capacity <-
    sapply(1:nrow(data$site_H2C02_transport), function(row)
    {
      if (data$site_H2C02_transport$max_H2[row] == 0)
      {
        return(0)
      }
      distance = data$site_H2C02_transport$pipe_dist[row]
      flow_rate = data$site_H2C02_transport$nearest_flow_rate.H2[row]
      opex_total = predict(models_pipe[[as.character(flow_rate)]]$opex, newdata = data.frame(distance = distance))
      cost_per_capacity = opex_total / flow_rate
      return(cost_per_capacity)
    })

  # Add these to the dataframe
  max_H2 <- data$site_H2C02_transport %>%
    add_column(
      capex_cost_per_capacity = capex_cost_per_capacity,
      opex_cost_per_capacity = opex_cost_per_capacity,
      grid_cost_per_capacity = grid_cost_per_capacity,
      truck_cost_per_capacity = truck_cost_per_capacity
    )


  #### now we want to produce the dataframe of coefficients ####
  # start with opex, which is easiest
  opex_coefficients <- decision_variables %>%
    filter(variable_type == "H2_pipe_available_capacity") %>%
    left_join(max_H2, by = "site_ID") %>%
    # discount opex costs to base year, over 5 years
    mutate(
      opex_coefficient = present_value(
        opex_cost_per_capacity,
        rate = data$rates$discount,
        start_period = year - data$model_parameters$start_year,
        n_periods = data$model_parameters$timestep
      )
    )

  # Now create capex of h2 pipes
  capex_coefficients <- decision_variables %>%
    filter(variable_type == "H2_pipe_new_capacity") %>%
    # join in the capex price per capacity
    left_join(select(max_H2, site_ID, capex_cost_per_capacity), by = "site_ID") %>%
    # Capex is paid off as a loan. The number of payments for the loan is the lifetime of the technology
    mutate(
      payment_per_period = PMT(
        capex_cost_per_capacity,
        data$rates$interest,
        data$Pipes_lifetime$H2Pipe_lifetime
      )
    ) %>%
    # the number of payments that the system actually has to pay is only up to 2050
    mutate(
      loan_periods = pmin(
        year + data$Pipes_lifetime$H2Pipe_lifetime - 1,
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
    )

  grid_coefficients <- decision_variables %>%
    filter(variable_type == "H2_grid_used_capacity") %>%
    left_join(max_H2, by = "site_ID")


  if (nrow(grid_coefficients) == 0) {
    grid_coefficients = NULL
  }
  else     {
    grid_coefficients = grid_coefficients %>%
      # discount opex costs to base year, over 5 years
      mutate(
        coefficient = present_value(
          grid_cost_per_capacity,
          rate = data$rates$discount,
          start_period = year - data$model_parameters$start_year,
          n_periods = data$model_parameters$timestep
        )
      ) %>%
      select(variable_index, coefficient)


  }


  truck_coefficients <- decision_variables %>%
    filter(variable_type == "H2_truck_used_capacity") %>%
    left_join(max_H2, by = "site_ID")

  if (nrow(truck_coefficients) == 0) {
    truck_coefficients = NULL
  }
  else {
    # discount opex costs to base year, over 5 years
    truck_coefficients = truck_coefficients %>% mutate(
      coefficient = present_value(
        truck_cost_per_capacity,
        rate = data$rates$discount,
        start_period = year - data$model_parameters$start_year,
        n_periods = data$model_parameters$timestep
      )
    ) %>%
      select(variable_index, coefficient)
  }


  # combine capex and opex
  opex_and_capex <-
    bind_rows(opex_coefficients, capex_coefficients) %>%
    rowwise() %>%
    mutate(coefficient = sum(opex_coefficient, capex_coefficient, na.rm = TRUE)) %>%
    select(variable_index, coefficient)
  if (nrow(opex_and_capex) == 0) {
    opex_and_capex = NULL
  }


  all_coefficients = bind_rows(opex_and_capex, grid_coefficients, truck_coefficients)

  return(all_coefficients)
}

