# Constraints on Hydrogen

#-------------------------------------------------------------------------------
#H2_infrastructure_capacity

#' Constrain H2 infrastructure/H2 technology deployment to ensure enough H2 can
#' be transported to each site
#'
#' Creates a constraint to ensure that there is at least as much H2 infrastructure
#'  to a site as there is capacity to use H2 by all of the sites hydrogen
#'  consuming technologies.
#'
#' @inheritParams comit_constraints
#'
#' @returns list of constraints. One constraint for each site with the potential
#'  to use hydrogen, in each time period. Each element contains a nested list
#'  with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
H2_infrastructure_capacity <- function(data, decision_variables) {

  # get the H2 infrastructure variables
  H2_available_infrastructure <- decision_variables %>%
    filter(
      variable_type %in% c(
        "H2_pipe_available_capacity",
        "H2_grid_used_capacity",
        "H2_truck_used_capacity"
      )
    )

  H2_consuming_variables <- get_H2_consuming_variables(data, decision_variables)

  # join infrastructure variables to consuming variables
  H2_consuming_variables <- left_join(H2_consuming_variables,
                                      H2_available_infrastructure,
                                      by = c('year', 'site_ID'))

  H2_infrastructure_capacity_constraints <-
    formulate_H2_infrastructure_capacity_constraint(H2_consuming_variables)


  return(H2_infrastructure_capacity_constraints)
}



#' Return all hydrogen consuming decision variables
#'
#' Get all technology decision variables that use hydrogen as an input fuel.
#'
#' @inheritParams comit_constraints
#'
#' @returns dataframe, with one row per decision variable and the following
#'  columns:
#'  * variable_index.technology - the variable index. '.technology' suffix is
#'    used to distinguish the source of data when joins are used later.
#'  * year
#'  * site_ID
#'  * input
#' @export
get_H2_consuming_variables <- function(data, decision_variables) {

  H2_consuming_variables <- get_consuming_variables(data, decision_variables) %>%
    left_join(data$commodities %>% select(commodity, commodity_category),
              by = 'commodity') %>%
    filter(commodity_category == 'Hydrogen') %>%
    select(variable_index.technology = variable_index, year, site_ID, input)

  return(H2_consuming_variables)
}



#' Formulates the H2 infrastructure capacity constraints
#'
#' @param H2_consuming_variables, dataframe produced by
#'  `get_H2_consuming_variables(data, decision_variables)` joined with infrastructure
#'  decision variables. One row per combination of hydrogen consuming technology
#'  variables and H2 infrastructure variables.
#'
#' @returns list of constraints. One constraint for each site with the potential
#'  to use hydrogen, in each time period. Each element contains a nested list
#'  with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
formulate_H2_infrastructure_capacity_constraint <- function(H2_consuming_variables) {

  H2_infrastructure_capacity_constraints <- H2_consuming_variables %>%
    group_by(site_ID, year) %>%
    group_map(function(rows, key) {
      # the key indicates the index of the H2 pipe available capacity variable
      techs = distinct(rows, variable_index.technology,input)

      list(
        column_indices = c(unique(rows$variable_index),
                           techs$variable_index.technology),
        values = c(rep(1,length(unique(rows$variable_index))),
                   -1 * techs$input),
        direction = ">=",
        rhs = 0
      )
    })

  return(H2_infrastructure_capacity_constraints)
}



#-------------------------------------------------------------------------------

# non_industry_H2_demand

#' Constrain non-industry hydrogen variables to ensure non-industry demand of
#' hydrogen is met
#'
#' Make sure enough non-industry hydrogen is included in the model to meet the
#'  non-industry demand in each cluster and in each year.
#'
#' @inheritParams comit_constraints
#'
#' @returns list of constraints. One constraint for each cluster, in each time
#'  point. Each element contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
non_industry_H2_demand <- function(data, decision_variables) {

  # don't constraint if we  are modelling H2 production
  if (data$model_parameters$model_H2_production){ return(NULL) }

  # Get the non_industry_H2 decision variables and join demand
  H2_non_industry <- decision_variables %>%
    filter(variable_type == "non_industry_H2") %>%
    left_join(data$Non_industry_H2_demand, by = c("year", "cluster"))

  # formulate the constraint
  H2_non_industry_constraints <- H2_non_industry %>%
    group_by(year, cluster) %>%
    group_map(function(rows, key) {
      list(
        column_indices = rows$variable_index,
        values = rep(1, length(rows$variable_index)),
        direction = "==",
        rhs = rows$demand[1]
      )
    })

  return(H2_non_industry_constraints)
}

#-------------------------------------------------------------------------------
# Hydrogen availability

#' Constrain the amount of hydrogen technologies used in each cluster so that
#' the amount of available hydrogen is not exceeded
#'
#' Limit the amount of used capacity for hydrogen consuming technologies, so
#'  that the amount of hydrogen used in each cluster does not exceed the amount
#'  of hydrogen each cluster has available.
#'
#' @inheritParams comit_constraints
#'
#' @returns list of constraints. One constraint for each cluster at each time
#' point. Each element contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
H2_availability <- function(data, decision_variables) {

  H2_variables <- get_H2_consuming_variables(data, decision_variables)

  H2_availability_data <- get_H2_availability_data(data,
                                                   decision_variables)

  H2_variables_with_availability <- get_H2_variables_with_availability(
    decision_variables,
    H2_variables,
    H2_availability_data
    )

  H2_availability_constraint <- formulate_H2_availability_constraint(
    H2_variables_with_availability
    )

  return(H2_availability_constraint)
}




#' Get the amount of hydrogen available to technologies in each cluster, in each
#' year
#'
#' Pulls the data on overall H2 availability and non-industry H2 demand from
#'  the input data and then calculates the net amount of hydrogen available to
#'  be used for industry (net_availability = overall_availability - non_industry_demand).
#'
#' @inheritParams comit_constraints
#'
#' @returns dataframe, with one row per year and cluster combination. Contains
#'  the following columns:
#'  * year
#'  * cluster
#'  * net_availability (the amount of hydrogen available for industry use)
#' @export
get_H2_availability_data <- function(data, decision_variables) {

  # pivot input data into the format we need
  H2_available <- data$H2_availability %>%
    pivot_longer(cols = !year,
                 names_to = "cluster",
                 values_to = "availability")

  # join on non-industry demand and find net availability at each cluster
  H2_available %<>%
    left_join(data$Non_industry_H2_demand, by = c('year', 'cluster')) %>%
    rename(non_industry_demand = demand) %>%
    mutate(net_availability = availability - non_industry_demand) %>%
    select(year, cluster, net_availability)

  return(H2_available)

}


#' Join hydrogen availability data to the hydrogen decision variables
#'
#' Joins on the data for the amounts of hydrogen available in each cluster and
#'  in each year to the relevant decision variables, to allow setting up the
#'  constraint later.
#'
#' @inheritParams comit_constraints
#' @param H2_variables dataframe for the hydrogen decision variables, produced
#'  by `get_H2_availability_data()`.
#' @param H2_availability_data dataframe for the hydrogen available in each cluster and
#'  in each year. Produced by `get_H2_availability_data()`.
#'
#' @returns dataframe, one row per hydrogen technology decision variable. Columns
#'  include:
#'  * variable_index.technology
#'  * year
#'  * site_ID
#'  * cluster
#'  * input
#'  * net_availability
#' @export
get_H2_variables_with_availability <- function(decision_variables,
                                               H2_variables,
                                               H2_availability_data) {
  # get site to cluster look up for joining
  site_clusters <- decision_variables %>%
    select(site_ID, cluster) %>%
    distinct()

  # join cluster labels
  H2_variables <- left_join(H2_variables, site_clusters, by = 'site_ID') %>%
    mutate(cluster = ifelse(cluster == 'Humberside2', 'Humberside', cluster))

  # join H2 availability to each hydrogen vriables
  H2_variables <- left_join(H2_variables,
                            H2_availability_data,
                            by = c('year', 'cluster'),
                            relationship = 'many-to-many') %>%
    filter(!is.na(net_availability))

  return(H2_variables)
}



#' Formulates the H2 availability constraints
#'
#' Used by `H2_availability` to produce hydrogen availability constraints.
#'
#' @param H2_variables dataframe produced by `get_H2_variables_with_availability()`,
#'  containing one row per decision variable to be constrained and including
#'  the column 'net_availability' which informs the limit in the constraint.
#'
#' @returns list of constraints. One constraint for each cluster at each time
#' point. Each element contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
formulate_H2_availability_constraint <- function(H2_variables){

  H2_availability_constraint <- H2_variables %>%
    group_by(year, cluster) %>%
    group_map(function(rows, key) {
      list(
        column_indices = rows$variable_index.technology,
        values = rows$input,
        direction = "<=",
        rhs = max(rows$net_availability)
      )
    })

  return(H2_availability_constraint)
}


#-------------------------------------------------------------------------------

# Hydrogen conversion

#' Constrain hydrogen conversion, so that the correct amount of hydrogen is
#' produced overall by summing the different hydrogen types to meet demand
#'
#' To account for different hydrogen types in the model, each with their own
#'  costs, this function ensures that there is enough hydrogen input overall to
#'  meet the demand of all hydrogen consuming technologies.
#'
#' All hydrogen consuming technologies consume hydrogen as a generic commodity
#'  named 'INDMAINSHYG'. In the model coefficients INDMAINSHYG has no cost.
#'  Instead the hydrogen_conversion variables take the separate types of hydrogen
#'  'INDMAINSHYGG', 'INDMAINSHYGB' and 'INDMAINSHYGR' as inputs and produces
#'  a generic generated hydrogen commodity 'HYGEN' as output. This constraint
#'  ensures that as much of the hydrogen_conversion variables are used to ensure
#'  as much of the separate hydrogen types (which are costed) are used to meet
#'  demand for hydrogen consumption overall.
#'
#' More briefly, here we are making the HYGEN produced by the hydrogen_conversion
#'  variables equal to the hydrogen consumed in each year.
#'
#' @inheritParams comit_constraints
#'
#' @returns list of constraints. One constraint for each time point where
#'  there are hydrogen used capacity decision variables in the model. Each
#'  element contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
H2_conversion <- function(data, decision_variables) {

  # don't run when modelling H2 production
  if (data$model_parameters$model_H2_production) {return(NULL)}

  # Make the HYGEN produced by the hydrogen_conversion sector equal to the hydrogen
  # consumed in each year

  hydrogen_conversion_variables <- get_hydrogen_conversion_variables(
    data, decision_variables
    )

  hydrogen_variables <- get_hydrogen_conversion_constraint_data(
    data, decision_variables, hydrogen_conversion_variables
    )

  hydrogen_conversion_constraint <- formulate_H2_conversion_constraint(
    hydrogen_variables
  )

  return(hydrogen_conversion_constraint)
}



#' Get decision variables for hydrogen conversion technologies
#'
#' @inheritParams comit_constraints
#'
#' @returns dataframe, with one row per hydrogen conversion variable. Columns
#'  include:
#'  * variable_index.production - the variable index, with 'production' suffix
#'    to distinguish from consumption variables in later join.
#'  * year
#'  * code - technology code
#' @export
get_hydrogen_conversion_variables <- function(data, decision_variables) {

  hydrogen_conversion_variables <- decision_variables %>%
    filter(variable_type == 'used_capacity') %>%
    inner_join(data$Technologies %>% filter(sector == 'hydrogen_conversion'),
               by = 'code') %>% # use inner join to keep only the hydrogen_conversion sector technologies
    select(variable_index.production = variable_index, year, code)

  return(hydrogen_conversion_variables)
}




#' Get the data required to generate the hydrogen conversion constraint
#'
#' Joins data from hydrogen consumption and hydrogen conversion decision
#'  variables to allow the hydrogen conversion constraint to be formulated.
#'
#' @inheritParams comit_constraints
#' @param hydrogen_conversion_variables dataframe of the hydrogen_conversion
#'  decision variables, produced by `get_hydrogen_conversion_variables()`,
#'
#' @returns dataframe, with one row per combination of hydrogen consuming
#'  decision variables and each hydrogen conversion variable, in each year.
#'  Columns include:
#'  * variable_index.consumption - variable index for the hydrogen consumption variables
#   * variable_index.production - variable index for hydrogen conversion variables.
#   * year
#   * input - input amount of hydrogen required per unit of capacity by the hydrogen
#     consumption variables.
#' @export
get_hydrogen_conversion_constraint_data <- function(data,
                                                    decision_variables,
                                                    hydrogen_conversion_variables) {

  hydrogen_variables <- get_consuming_variables(data, decision_variables) %>%
    filter(commodity == "INDMAINSHYG") %>%
    rename(variable_index.consumption = variable_index)

  hydrogen_variables <- left_join(hydrogen_variables,
                                  hydrogen_conversion_variables,
                                  by = 'year',
                                  relationship = 'many-to-many')

  return(hydrogen_variables)

}


#' Formulates the H2 conversion constraints
#'
#' Used by `H2_conversion` to produce hydrogen conversion constraints.
#'
#' @param hydrogen_variables, dataframe produced by
#'  `get_hydrogen_conversion_constraint_data()`. Contains data for the hydrogen
#'  consuming and hydrogen producing (converting) variables.
#'
#' @returns list of constraints. One constraint for each time point where
#'  there are hydrogen used capacity decision variables in the model. Each
#'  element contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
formulate_H2_conversion_constraint <- function(hydrogen_variables) {

  hydrogen_conversion_constraint <- hydrogen_variables %>%
    group_by(year) %>%
    group_map(function(rows, key) {

      # the key indicates the year
      consumption <- distinct(rows, variable_index.consumption, input)

      production <- unique(rows$variable_index.production)

      list(
        column_indices = c(consumption$variable_index.consumption, production),
        values = c(consumption$input, rep(-1, length(production))),
        direction = "==",
        rhs = 0
      )
    })


  if (length(hydrogen_conversion_constraint) == 0) {
    return(NULL)
  } else {
    return(hydrogen_conversion_constraint)
  }

}



#===============================================================================
#===============================================================================
# The below functions aren't currently in use and have not been refactored

#' Formulates the hydrogen production constraint
hydrogen_production <- function(data, decision_variables) {
  # If we are not modeling hydrogen production, we can return prematurely
  if (data$model_parameters$model_H2_production == FALSE)
  {
    return(NULL)
  }

  # start with list of clusters and years
  H2_production <- data$NAEI_clean %>%
    distinct(H2_point) %>%
    left_join(decision_variables %>% distinct(year), by = character()) %>%
    rename(cluster = H2_point) %>%

    # for each cluster and year, get variable indexes of the hydrogen production technologies
    left_join(
      decision_variables %>%
        filter(variable_type == "used_capacity") %>%
        left_join(data$Technologies, by = "code") %>%
        filter(output_commodity == "HYGEN") %>%
        select(code, cluster, year, variable_index),
      by = c("cluster", "year")
    ) %>%
    rename(code.H2_production = code,
           variable_index.H2_production = variable_index) %>%

    # we also need to get all the outflows from the cluster
    left_join(
      decision_variables %>%
        filter(variable_type == "H2_outflow") %>%
        select(variable_index.outflow = variable_index, year, cluster),
      by = c("year", "cluster")
    ) %>%

    # ...and the outflows to the cluster (i.e. the inflows)
    left_join(
      decision_variables %>%
        filter(variable_type == "H2_outflow") %>%
        select(variable_index.inflow = variable_index, year, pipe_cluster_end),
      by = c("year", "cluster" = "pipe_cluster_end")
    ) %>%

    # We also need hydrogen consumption by industrial technologies
    left_join(
      decision_variables %>%
        filter(variable_type == "used_capacity") %>%

        # filter for only industrial sites
        filter(site_ID %in% data$NAEI_clean$site_ID[data$NAEI_clean$IPM_sector != "Hydrogen"]) %>%

        left_join(
          data$technology_input_output,
          by = c("code" = "technology_code")
        ) %>%
        filter(commodity == "INDMAINSHYG") %>%
        mutate(amount_consumed = -output) %>%
        select(
          variable_index.H2_tech_consumption = variable_index,
          year,
          cluster,
          amount_consumed
        ),
      by = c("year", "cluster")
    ) %>%

    # finally, we also need non industrial demand
    left_join(data$Non_industry_H2_demand, by = c("year" = "year", "cluster")) %>%
    rename(non_industry_demand = demand) %>%

    group_by(year, cluster) %>%
    group_map(function(rows, keys) {
      # the keys tell you the current cluster and year
      # Again, the constraint at each cluster in each year is:
      # H2 produced - H2 outflows + H2 inflows - industrial use = non industrial use

      H2_produced_i <- unique(rows$variable_index.H2_production)
      H2_produced_v <- rep(1, length(H2_produced_i))

      #if there are no outflows or inflows, set indices and thus values to null
      H2_outflows_i <- unique(rows$variable_index.outflow)
      if (is.na(H2_outflows_i)) {
        H2_outflows_i <- NULL
      }
      H2_outflows_v <- rep(-1, length(H2_outflows_i))

      H2_inflows_i <- unique(rows$variable_index.inflow)
      if (is.na(H2_inflows_i)) {
        H2_inflows_i <- NULL
      }
      H2_inflows_v <- rep(1, length(H2_inflows_i))

      industrial_use_i <- rows$variable_index.H2_tech_consumption
      industrial_use_v <- -1 * rows$amount_consumed

      list(
        column_indices = c(
          H2_produced_i,
          H2_outflows_i,
          H2_inflows_i,
          industrial_use_i
        ),
        values = c(
          H2_produced_v,
          H2_outflows_v,
          H2_inflows_v,
          industrial_use_v
        ),
        direction = "==",
        rhs = rows$non_industry_demand[1]
      )
    })

  return(H2_production)
}


#' Formulates hydrogen flows constraint
hydrogen_flows <- function(data, decision_variables)
{
  # If we are not modeling hydrogen production, we can return prematurely
  if (data$model_parameters$model_H2_production == FALSE)
  {
    return(NULL)
  }

  # get list of all outflows
  H2_flows <- decision_variables %>%
    filter(variable_type == "H2_outflow") %>%
    select(variable_index, year, cluster, pipe_cluster_end) %>%

    # outflow describes flow from a to b. Join in the respective pipe
    # because we are joining on arbitrary combination of two columns, need to create unambiguous identifier
    mutate(pipe_name = paste(
      pmin(cluster, pipe_cluster_end),
      pmax(cluster, pipe_cluster_end)
    )) %>%
    left_join(
      decision_variables %>%
        filter(variable_type == "H2_national_pipe_available_capacity") %>%
        select(variable_index, year, cluster, pipe_cluster_end) %>%
        mutate(pipe_name = paste(
          pmin(cluster, pipe_cluster_end),
          pmax(cluster, pipe_cluster_end)
        )),
      by = c("pipe_name", "year"),
      suffix = c(".outflow", ".available_pipe_capacity")
    ) %>%

    # group by each pipe_available_capacity
    group_by(variable_index.available_pipe_capacity) %>%
    group_map(function(rows, keys) {
      list(
        # the key contains the index of the available_pipe_Capacity variable
        column_indices = c(rows$variable_index.outflow, keys[[1]]),
        values = c(1, 1,-1),
        direction = "<=",
        rhs = 0
      )
    })


  return(H2_flows)
}




minimum_hydrogen_plant_size <- function(data, decision_variables) {
  # check if minimum plant size needs to be enforced
  model_H2_production <- data$model_parameters$model_H2_production

  enforce_H2_plant_size <- (
    data$H2_plant_size$minimum_available_capacity_EndYear > 0
  )

  if (!model_H2_production &
      enforce_H2_plant_size) {
    warning("Not modeling H2 production explicitly, but minimum H2 plant size is specified")
  }

  # exit if we can
  if (enforce_H2_plant_size == FALSE)
  {
    return(NULL)
  }

  #### Min H2 plant size ####
  # We add a constraint which specifies that in 2050, the amount of available capacity of every hydrogen producing technology
  # must either be zero, or above a certain number
  hydrogen_techs <- decision_variables %>%
    filter(variable_type == "available_capacity") %>%
    left_join(data$technology_input_output,
              by = c("code" = "technology_code")) %>%
    filter(commodity == "HYGEN" &
             primary_commodity == TRUE & output > 0 & year == data$model_parameters$end_year) %>%

    # left join binary variables
    left_join(
      filter(
        decision_variables,
        variable_type == "b_H2_available_capacity"
      ),
      by = c("year", "site_ID", "code"),
      suffix = c(".technology", ".binary")
    )

  min_H2_1 <- hydrogen_techs %>%
    group_by(variable_index.technology) %>%
    group_map(function(row, key) {
      # the key indicates the index of the technology variable
      # plant size <= inf * binary
      # inf * binary - plant_size >= 0
      list(
        column_indices = c(row$variable_index.binary, key[[1]]),
        values = c(999999,-1),
        direction = ">=",
        rhs = 0
      )
    })

  min_H2_2 <- hydrogen_techs %>%
    group_by(variable_index.technology) %>%
    group_map(function(row, key) {
      # the key indicates the index of the technology variable
      # plant_size >= min_plant_size * binary
      # min_plant_size * binary - plant size <= 0
      list(
        column_indices = c(row$variable_index.binary, key[[1]]),
        values = c(data$H2_plant_size$minimum_available_capacity_EndYear-1),
        direction = "<=",
        rhs = 0
      )
    })

  # combine and return
  x <- c(min_H2_1, min_H2_2)

  return(x)
}

#===============================================================================
