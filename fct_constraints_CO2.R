#-------------------------------------------------------------------------------
# CO2 infrastructure (including trucking)

#' Constrain CO2 infrastructure and CCS technology deployment, to ensure enough
#' CO2 can be transported from each site
#'
#' Ensure that there is at least as much infrastructure capacity to accommodate
#'  for the amount of carbon captured by CCS technologies. This constraint ensures
#'  there is enough CO2 infrastructure implemented to transport the CO2 that is
#'  captured.
#'
#' Infrastructure here includes both pipes and trucking options. Each site has
#'  only one CO2 infrastructure option available to it (pipes or trucking) which
#'  is identified pre-model in the input template tab 'CO2_transport_cost'.
#'
#' @inheritParams comit_constraints
#'
#' @returns list of constraints. One constraint for each site with the potential
#'  to use CCS technologies, in each time period. Each element contains a nested
#'  list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
CO2_infrastructure_capacity <- function(data, decision_variables) {

  # get the CO2 infrastructure variables
  CO2_available_infrastructure <- decision_variables %>%
    filter(
      variable_type %in% c(
        "CO2_pipe_available_capacity",
        "CO2_truck_used_capacity"
      )
    )

  CO2_capture_variables <- get_CO2_capture_variables(data, decision_variables)

  # now join the capture and infrastructure variables to get the df for the constraint
  CO2_infrastructure_constraint_data <- left_join(
    CO2_available_infrastructure,
    CO2_capture_variables,
    by = c('site_ID', 'year'),
    suffix = c('.pipe', '.technology')
  )

  CO2_infrastructure_constraint <- formulate_CO2_infrastructure_constraint(
    CO2_infrastructure_constraint_data
    )


  return(CO2_infrastructure_constraint)

}


#' Create a dataframe of the decision variables that use CCS
#'
#' Get the decision variables for technologies that use carbon capture. These
#'  are then used constrain the amount of infrastructure needed through the
#'  'formulate_CO2_infrastructure_constraint()' function.
#'
#' @inheritParams comit_constraints
#' @param biomass_setting, boolean, default = FALSE. The setting to pass as
#'  zero_emissions_from_biomass in the `get_emissions()` function used to
#'  calculat the amount of captured emisssion.
#'
#' @returns dataframe with one row per decision variable that is a technology
#'  that captures CO2. Columns are:
#'   * variable_index
#'   * year
#'   * site_ID
#'   * code
#'   * captured_CO2 - numeric, the amount of CO2 captured per unit of capacity.
#' @export
get_CO2_capture_variables <- function(data,
                                      decision_variables,
                                      biomass_setting = FALSE) {

  # get decision variables that have captured emissions
  CO2_capture_variables <- decision_variables %>%
    filter(variable_type == 'used_capacity') %>%
    select(variable_index, year, site_ID, code, cluster) %>%
    mutate(
      captured_CO2 = get_emissions(
        code,
        year = year,
        capture = TRUE,
        .data = data,
        zero_emissions_from_biomass = biomass_setting
      )) %>%
    filter(captured_CO2 > 0)

  return(CO2_capture_variables)
}



#' Formulates the CO2 infrastructure capacity constraints
#'
#' The constraint is essentially: *CO2 infrastructure capacity >= CO2 caputred*,
#'  at each site and in each year.
#'
#' @param CO2_infrastructure_constraint_data dataframe containing both data on
#'  the CO2 infrastructure variables, and the CO2 capture technology variables for
#'  each site and in each year.
#'
#' @returns list of constraints. One constraint for each site with the potential
#'  to use CCS technologies, in each time period. Each element contains a nested
#'  list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
formulate_CO2_infrastructure_constraint <- function(CO2_infrastructure_constraint_data) {

  CO2_infrastructure_constraint <- CO2_infrastructure_constraint_data %>%
    group_by(variable_index.pipe) %>%
    group_map(function(rows, key) {
      list(
        # the key indicates the index of the pipe available_capacity variable
        column_indices = c(key[[1]], rows$variable_index.technology),
        values = c(1, -1 * rows$captured_CO2),
        direction = ">=",
        rhs = 0
      )
    })

  if (length(CO2_infrastructure_constraint) == 0) {
    return(NULL)
  } else {
    return(CO2_infrastructure_constraint)
  }

}

#-------------------------------------------------------------------------------
# CO2 cluster to storage transport

#' Constrain the capacity of CO2 transportation variables to meet the amount
#'  of CO2 captured within a cluster
#'
#' Sets the amount of CO2 transportation required to remove the captured CO2
#'  from each cluster to a storage site, often through means of another cluster
#'  (referred to as a terminal).
#'
#' @inheritParams comit_constraints
#'
#' @returns list of constraints. One constraint for each cluster with the potential
#'  to use CCS, in each time period. Each element contains a nested list
#'  with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
CO2_cluster_to_storage_transport <- function(data, decision_variables) {

  CO2_cluster_to_storage_variables <- get_CO2_cluster_to_storage_variables(
    data, decision_variables
    )

  CO2_cluster_to_storage_constraint <- formulate_CO2_cluster_to_storage_constraint(
    CO2_cluster_to_storage_variables
  )

  return(CO2_cluster_to_storage_constraint)

}



#' Get the data required to formulate the CO2 cluster to storage transport
#' constraint.
#'
#' Gets the decision variable data for both *CO2_transported* variables and
#'  technologies that capture CO2. Non industry demand for CO2 transport is also
#'  joined here to include in the constraint calculation. The two sets of decision
#'  variables are joined together to allow for the formulation of the cluster to
#'  storage site transport constraint.
#'
#' CO2_transported variables represent the possible connections of cluster to
#'  storage site for transporting CO2.
#'
#' @inheritParams comit_constraints
#'
#' @returns dataframe, with one row per feasible combination of decision variables
#'  with CCS and CO2_transported variables. Columns include:
#'   * variable_index.technology - the variable index for technologies.
#'   * year
#'   * site_ID
#'   * cluster
#'   * captured_CO2 - amount of CO2 captured within the cluster
#'   * demand - amount of demand for non-industry CO2 to be transported
#'   * variable_index.transport - the variable index for CO2_transported variable.
#' @export
get_CO2_cluster_to_storage_variables <- function(data, decision_variables) {

  CO2_transport_variables <- decision_variables %>%
    filter(variable_type == "CO2_transported") %>%
    select(variable_index, year, cluster, variable_name.transport = variable_name)

  # add non-industrial demand by cluster and year
  CO2_capture_variables <- get_CO2_capture_variables(data,
                                                     decision_variables) %>%
    left_join(data$Non_industry_CO2_demand,
              by = c('year', 'cluster'))

  CO2_cluster_to_storage_variables <- left_join(
    CO2_capture_variables,
    CO2_transport_variables,
    by = c('year', 'cluster'),
    suffix = c('.technology', '.transport'),
    relationship = 'many-to-many'
  )

  return(CO2_cluster_to_storage_variables)
}



#' Formulates the CO2 cluster to storage transport constraints
#'
#' @param CO2_cluster_to_storage_variables dataframe, produced by
#'  `get_CO2_cluster_to_storage_variables()`.
#'
#' @inherit CO2_cluster_to_storage_transport return
#'
#' @export
formulate_CO2_cluster_to_storage_constraint <- function(CO2_cluster_to_storage_variables) {

  CO2_cluster_to_storage_constraint <-  CO2_cluster_to_storage_variables %>%
    group_by(year, cluster) %>%
    group_map(function(rows, key) {

      ccs_technologies <- rows %>%
        distinct(variable_index.technology, captured_CO2)

      # the amount of CO2 captured at a cluster, must be equal to the amount of
      # CO2 transported away from the cluster
      list(
        column_indices = c(
          ccs_technologies$variable_index.technology,
          unique(rows$variable_index.transport)
        ),
        values = c(
          ccs_technologies$captured_CO2,
          rep(-1, length(unique(rows$variable_index.transport)))
        ),
        direction = "==",
        rhs = -rows$demand[1]
      )
    })

  # check if there is an empty list (e.g no CCS)
  if (length(CO2_cluster_to_storage_constraint) == 0) {
    return(NULL)
  } else {
    return(CO2_cluster_to_storage_constraint)
  }

}





#-------------------------------------------------------------------------------


# Injection constraint
# The sum of Co2 which ends up in a particular storage site cannot exceed the
# values specified in the inputs


#' Constrains the amount of CO2 that can be stored at the CO2 storage site
#'
#' Ensures that the maximum yearly amount CO2 that can be injected at each of the
#'  storage sites is not exceeded.
#'
#' @inheritParams comit_constraints
#'
#' @returns  list of constraints. One constraint for each storage site that is
#'  to be constrained, in each year. Each element contains a nested list
#'  with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
CO2_storage_injection <- function (data, decision_variables) {


  injection_constrained_variables <- get_injection_constrained_variables(
    data, decision_variables
  )

  injection_constraints <- formulate_injection_constraints(
    injection_constrained_variables
  )

  return(injection_constraints)

}


#' Get data for the decision variables to be constrained by CO2 storage injection
#' rates
#'
#' Get the *CO2_transported* variables which are the decision variables for
#'  cluster to storage site CO2 transport, along with the associated injection
#'  limits for the storage site recieving the CO2. Variables with limits of
#'  1,000,000 or over are removed and will not be constrained.
#'
#' @inheritParams comit_constraints
#'
#' @returns dataframe, with one row per *CO2_transported* variable, in each year
#'  to be constrained. Columns include:
#'  * variable_index
#'  * year
#'  * storage_site
#'  * max_injection
#' @export
get_injection_constrained_variables <- function(data, decision_variables) {

  CO2_transport_variables <- decision_variables %>%
    filter(variable_type == "CO2_transported") %>%
    select(variable_index, year, storage_site) %>%
    left_join(data$CO2_storage,
              by = c('storage_site', 'year'))

  # don't constrain those with arbitrarily large values
  injection_constrained_variables <- CO2_transport_variables %>%
    filter(max_injection < 1000000)

  return(injection_constrained_variables)
}



#' Formulate the constraint for CO2 storage injection
#'
#' @param injection_constrained_variables dataframe, produced by
#'  `CO2_storage_injection()`.
#'
#' @inherit CO2_storage_injection return
#' @export
formulate_injection_constraints <- function(injection_constrained_variables) {

  injection_constraints <- injection_constrained_variables %>%
    group_by(storage_site, year) %>%
    group_map(function(rows, key) {

      transport_index <- unique(rows$variable_index)
      injection <- unique(rows$max_injection) # this should be a single value

      list(
        column_indices = transport_index,
        values = rep(1, length(transport_index)),
        direction = "<=",
        rhs = injection
      )
    })

  # check if there is an empty list (e.g no CCS)
  if (length(injection_constraints) == 0) {
    return(NULL)
  } else {
    return(injection_constraints)
  }


}


#-------------------------------------------------------------------------------
# max_ccs

#' Constrain the amount of CCS that can be used in industry
#'
#' Limits the total amount of carbon that can be captured in a given year,
#'  across all of industry.
#'
#' @inheritParams comit_constraints
#'
#' @returns list of constraints. One constraint for each time period with a
#'  constraint on the maximum amount of carbon that can be captured.
#'  Each element contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
max_CCS <- function(data, decision_variables) {

  max_ccs_data <- get_max_CCS_data(data, decision_variables)

  max_ccs_constraint <- formulate_max_CCS_constraint(max_ccs_data)

  return(max_ccs_constraint)
}



#' Get decision variables to be constrained by the max_CCS constraint
#'
#' Get the data required to formulate the max_CCS constraint, by finding the
#'  relevant decision variables and joining the relevant CCS limit from the
#'  input data.
#'
#' @inheritParams comit_constraints
#'
#' @returns dataframe, with one row per decision variable to be constrained.
#'  Columns include:
#'  * variable_index
#'  * year
#'  * captured_CO2 - amount of CO2 captured per unit of capacity
#'  * max_CO2_captured - total amount of CO2 that can be captured in given year
#' @export
get_max_CCS_data <- function(data, decision_variables) {

  technology_sector_lookup <- select(data$Technologies, code, sector)

  ccs_variables <- get_CO2_capture_variables(data,
                                             decision_variables,
                                             biomass_setting = TRUE)

  # filter out hydrogen and refineries sector
  ccs_variables %<>%
    left_join(technology_sector_lookup, by = "code") %>%
    filter(!(sector %in% c("Refineries", "Hydrogen")))

  # join in maximum amount of emissions that can be captured annually
  ccs_variables %<>%
    left_join(data$max_CCS, by = "year") %>%
    filter(max_CO2_captured < 1000000) %>%
    select(variable_index, year, captured_CO2, max_CO2_captured)

  return(ccs_variables)
}



#' Formulate the max_CCS constraint
#'
#' @param max_ccs_data dataframe, produced by `get_max_CCS_data()` containing
#'   the data for the decision variables to be constrained.
#'
#' @inherit max_CCS return
#' @export
formulate_max_CCS_constraint <- function(max_ccs_data) {

  #### Maximum amount of CCS in industry constraint ####
  max_CCS_constraint <- max_ccs_data %>%
    group_by(year) %>%
    group_map(function(rows, key) {
      # The constraint is that sum(captured emissions) <= max captured emissions
      list(
        column_indices = rows$variable_index,
        values = rows$captured_CO2,
        direction = "<=",
        rhs = rows$max_CO2_captured[1]
      )
    })

  if (length(max_CCS_constraint) == 0) {
    return(NULL)
  } else {
    return(max_CCS_constraint)
  }


}


#===============================================================================
