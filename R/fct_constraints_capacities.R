

#' Constrain technology rollout to a given maximum proportion of commodity production
#'
#' Create constraints to set an upper limit for the proportion of outputs that
#'  can be produced by a given technology. For example, we may wish to set a limit
#'  in a given year for the proportion of all steel that can be produced by an
#'  electric arc furnace.
#'
#' This function is really just a wrapper for `capacity_constraints()` as the
#'  underlying code is essentially the same for `minimum_capacity()` - we just change
#'  the capacity type from 'max' to 'min'.
#'
#' @inheritParams comit_constraints
#'
#' @returns list of constraints. One constraint for each relevant technology, in
#'  each time point.
#'  Each element contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
maximum_capacity <- function(data, decision_variables) {


  max_capacity_constraint <- capacity_constraints(data,
                                                  decision_variables,
                                                  'max')

  return(max_capacity_constraint)

}



#' Constrain technology rollout to a given minimum proportion of commodity production
#'
#' Create constraints to set an upper limit for the proportion of outputs that
#'  can be produced by a given technology. For example, we may wish to set a limit
#'  in a given year for the proportion of all steel that can be produced by an
#'  electric arc furnace.
#'
#' This function is really just a wrapper for `capacity_constraints()` as the
#'  underlying code is essentially the same for `maximum_capacity()` - we just change
#'  the capacity type from 'min' to 'max'.
#'
#'
#' @inheritParams comit_constraints
#'
#' @returns list of constraints. One constraint for each relevant technology, in
#'  each time point.
#'  Each element contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
minimum_capacity <- function(data, decision_variables) {

  min_capacity_constraint <- capacity_constraints(data,
                                                  decision_variables,
                                                  'min')

  return(min_capacity_constraint)

}



#-------------------------------------------------------------------------------
# Functions used for both minimum and maximum constraint building



#' Create capacity limiting constraints (minimum or maximum capacities)
#'
#' This code contains all the steps required to build the minimum and maximum
#' capacity constraints, using the following functions:
#'  * `get_technology_capacity_tab` - gets the relevant input table containing
#'   capacity limits.
#'  * `get_capacity_variables_data` - returns the data from the relevant decision
#'   variables.
#'  * `add_alternative_technologies_data` - adds data for each variable of the
#'   alternative technologies avaialble.
#'  * `formulate_capacity_constraint` - formally sets the constraint.
#'
#' @inheritParams comit_constraints
#' @param capacity_type character, either 'max' or 'min' depending on whether
#'  the limit to be set is on maximum or minimum capacity.
#'
#' @returns list of constraints. One constraint for each relevant technology, in
#'  each time point.
#'  Each element contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
capacity_constraints <- function(data, decision_variables, capacity_type) {

  capacity_tab <- get_technology_capacity_tab(data, capacity_type)

  capacity_variables <- get_capacity_variables_data(data, decision_variables,
                                                    capacity_tab)

  capacity_variables <- add_alternative_technologies_data(capacity_variables,
                                                          data,
                                                          decision_variables,
                                                          capacity_tab)

  capacity_constraint <- formulate_capacity_constraint(capacity_variables,
                                                       capacity_type)

  return(capacity_constraint)
}



#' Get the relevant input table containing capacity limits
#'
#' @inheritParams capacity_constraints
#'
#' @returns dataframe containing one row for each year for each of the
#'  technologies to be restricted, and three columns:
#'   * year
#'   * code (technology code)
#'   * capacity_limit - either the minimum or the maximum capacity depending on
#'    the capacity_type.
#' @export
get_technology_capacity_tab <- function(data, capacity_type) {

  capacity_tab <- if(capacity_type == 'max') {
    data$maximum_capacity %>% rename(capacity_limit = max_capacity)
  } else if (capacity_type == 'min') {
    data$minimum_capacity %>% rename(capacity_limit = min_capacity)
  } else {
    stop('Incorrect capacity_type provided')
  }

  return(capacity_tab)

}



#' Get the relevant decision variables for the capacity constraint
#'
#' This function filters the decision_variables dataset down to only used_capacity
#' variables for the technologies that are present in the capacity_tab. It also
#' adds output commodity information from the technologies data.
#'
#' @inheritParams comit_constraints
#' @param capacity_tab dataframe, constaining data on the technologies to constrain
#'  as produced by `get_technology_capacity_tab`.
#'
#' @returns dataframe, with one row per decision variable for the relevant
#'  technologies. Columns are:
#'  * variable_index
#'  * year
#'  * site_ID
#'  * code - technology code
#'  * output_commodity
#' @export
get_capacity_variables_data <- function(data,
                                        decision_variables,
                                        capacity_tab) {

  constrained_technologies <- unique(capacity_tab$code)

  technologies <- data$Technologies %>%
    select(code, output_commodity)

  # Reduce decision variables to only those required and join in output commodity
  max_capacity_variables <- decision_variables %>%
    filter(variable_type == "used_capacity",
           code %in% constrained_technologies) %>%
    select(variable_index, year, site_ID, code) %>%
    left_join(technologies, by = "code")

}



#' Get all alternative technologies that can be deployed instead/as well as the
#' given technologies
#'
#' We need to constraint the % of a commodity that a technology produces in each
#' year, therefore we need to join on the alternative technologies so that they
#' can be used to formulate the constraint.
#'
#' @param capacity_variables dataframe for decision variables that are relevant
#'  to the capacity constraints, as produced by `get_capacity_variables_data`.
#' @inheritParams get_capacity_variables_data
#'
#' @returns dataframe for capacity variable, now with one row per feasible
#'  combination of decision variables with alternative technology decision
#'  variables.
#' @export
add_alternative_technologies_data <- function(capacity_variables,
                                              data,
                                              decision_variables,
                                              capacity_tab) {

  alt_technologies <- data$Technologies %>%
    select(alt_tech_code = code, output_commodity) %>%
    filter(alt_tech_code %in% decision_variables$code)
  # ^Filter for technologies which also have a decision variable because not all
  # technologies from the input template are necessarily used in the model

  # for each code and year, need to merge in other technologies which also
  #produce that commodity

  capacity_variables %<>%
    left_join(alt_technologies,
              by = "output_commodity",
              relationship = 'many-to-many') %>%
    filter(alt_tech_code != code)

  alt_technology_variables <-  decision_variables %>%
    filter(variable_type == "used_capacity") %>%
    select(alt_tech_variable_index = variable_index, year, site_ID, code)

  capacity_variables %<>%
    # join in the variable index of the process technologies
    left_join(alt_technology_variables,
              by = c("alt_tech_code" = "code", "year", "site_ID")) %>%
    filter(!is.na(alt_tech_variable_index)) %>%
    # add in maximum capacity percentage
    left_join(capacity_tab,
              by = c("year", "code"))

  return(capacity_variables)
}



#' Formulate the constraint for minimum and maximum capacities of technologies
#'
#' Sets up the list of constraints to be used in the model to provide either an
#' upper or lower limit to the share of a technology used to create a commodity
#' in each year.
#'
#' The formulation of this constraint is not immediately intuitive when looking
#' at the code, but it has been arrived at based on the following rearrangement
#' which will hopefully provide more understanding.
#'
#' Lets say we want to limit a technology (this_tech) to be less than 10% of a
#' commodities production, then:
#'
#'  this_tech <= 0.1(this_tech + all_other_techs)
#'
#'  this_tech <= 0.1(this_tech) + 0.1(all_other_techs)
#'
#'  this_tech - 0.1(this_tech) - 0.1(all_other_techs) <= 0
#'
#'  0.9(this_tech) - 0.1(all_other_techs) <= 0
#'
#' The formulation is the same for minimum and maximum technologies, with the slight
#' change from ">=" to "<=" from minimum to maximum.
#'
#' @param capacity_variables dataframe for decision variables that are relevant
#'  to the capacity constraints, as produced by `add_alternative_technologies_data`.
#' @param capacity_type character, either 'max' or 'min' used to determine whether
#'  the constraint is a minimum or maximum constraint.
#'
#' @returns list of constraints. One constraint for each relevant technology, in
#'  each year.
#'  Each element contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
formulate_capacity_constraint <- function(capacity_variables, capacity_type) {

  if(!capacity_type %in% c('max', 'min')) {
    stop('Incorrect capacity_type provided')
  }

  constraint_direction <- ifelse(capacity_type == 'max', '<=', '>=')

  capacity_constraints <- capacity_variables %>%
    group_by(year, code) %>%
    group_map(function(rows, key) {
      list(
        column_indices = c(unique(rows$variable_index), rows$alt_tech_variable_index),
        values = c(
          rep(1 - rows$capacity_limit[1], length(unique(rows$variable_index))),
          rep(-rows$capacity_limit[1], nrow(rows))
        ),
        direction = constraint_direction,
        rhs = 0
      )
    })

  return(capacity_constraints)

}





