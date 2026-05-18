

#' Constrain fuel consumption to remain under a maximum absolute value
#'
#' Create constraints that set upper limits for the absolute amount of a fuel
#'  that can be used, as specified in the input file. Fuels can be constrained
#'  either at the individual commodity code level (e.g. "INDMAINSHYG") or at
#'  the fuel category level (e.g. "Biomass and organic waste").
#'
#' This function is really just a wrapper for `absolute_fuel_constraints()` as the
#'  underlying code is essentially the same for `min_fuel_use()` - we just change
#'  the capacity type from 'max' to 'min'.
#'
#' @inheritParams comit_constraints
#'
#' @returns list of constraints. One constraint for each fuel specified to be
#'  constrainted in the input, in each time point. Each element contains a
#'  nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
max_fuel_use <- function(data, decision_variables) {

  max_fuel_constraints <- absolute_fuel_constraints(data, decision_variables, 'max')

  return(max_fuel_constraints)
}



#' Constrain fuel consumption to remain above a minimum absolute value
#'
#' Create constraints that set lower limits for the absolute amount of a fuel
#'  that has to be used, as specified in the input file. Fuels can be constrained
#'  either at the individual commodity code level (e.g. "INDMAINSHYG") or at
#'  the fuel category level (e.g. "Biomass and organic waste").
#'
#' This function is really just a wrapper for `absolute_fuel_constraints()` as the
#'  underlying code is essentially the same for `max_fuel_use()` - we just change
#'  the capacity type from 'min' to 'max'.
#'
#' @inheritParams comit_constraints
#'
#' @returns list of constraints. One constraint for each fuel specified to be
#'  constrained in the input, in each time point. Each element contains a
#'  nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
min_fuel_use <- function(data, decision_variables) {

  min_fuel_constraints <- absolute_fuel_constraints(data, decision_variables, 'min')

  return(min_fuel_constraints)
}




#' Create either a minimum or maximum fuel constraint
#'
#' Perform all manipulations required to the decision variables to get the data
#' required to set up the constraints. The final step then formulates the constraint
#' itself.
#'
#' @inheritParams comit_constraints
#' @param constraint_type character, either 'max' or 'min' depending on whether
#'  the limit to be set is on maximum or minimum fuel use.
#'
#' @returns list of constraints. One constraint for each fuel specified to be
#'  constrained in the input, in each time point. Each element contains a
#'  nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
absolute_fuel_constraints <- function(data, decision_variables, constraint_type) {

  consuming_variables <- get_consuming_variables(data, decision_variables)

  fuel_constrained_variables <- get_fuel_constrained_variables(data,
                                                               consuming_variables,
                                                               constraint_type)

  if(nrow(fuel_constrained_variables) == 0) {
    message('No ', constraint_type, ' fuel constraints provided.\n')
    return(NULL)
  }

  fuel_constrained_variables <- apply_sector_filter_to_relevant_fuel_variables(
    data, fuel_constrained_variables
  )


  fuel_constrained_variables <- aggregate_fuel_constrained_variables(
    fuel_constrained_variables
    )

  fuel_constraints <- formulate_fuel_constraints(fuel_constrained_variables,
                                                 constraint_type)

  return(fuel_constraints)

}




#' Get decision variables that use commodities
#'
#' Return all variables that are both of *used_capacity* type, and which are
#'  technologies that use inputs, along with the data on what commodities are
#'  used and in what volume.
#'
#' @inheritParams comit_constraints
#'
#' @returns dataframe, containing one row for each input used by a given variable.
#'  Columns are:
#'  * variable_index
#'  * year
#'  * site_ID
#'  * code - technology code.
#'  * commodity - this is the commodity that is consumed by the technology.
#'  * input - numeric, amount of consumption of commodity per unit of output.
#' @export
get_consuming_variables <- function(data, decision_variables) {

  technology_inputs <- data$technology_input_output %>%
    mutate(input = output * -1) %>%
    select(code = technology_code, commodity, input)

  # start with the amount of fuel used by each technology
  consuming_variables <- decision_variables %>%
    filter(variable_type == "used_capacity") %>%
    select(variable_index, year, site_ID, code) %>%
    left_join(technology_inputs, by = 'code', relationship = 'many-to-many') %>%
    filter(input > 0) # filter for inputs to a technology only

  return(consuming_variables)
}



#' Get the variables that are to be constrained by the fuel constraint
#'
#' Reduce the consuming variables down to only those which are to relevant to the
#'  constraint. Pulls the input tab relating to fuel constraints and then joins
#'  the relevant commodities for constraints specified at the fuel group rather
#'  than commodity level. This data is then joined back to the original
#'  consuming_variables data provided and reduced down to only the variables
#'  required.
#'
#' @inheritParams absolute_fuel_constraints
#' @param consuming_variables dataframe, all decision variables that consume
#'  commodities, as produced by `get_consuming_variables()`.
#'
#' @returns dataframe, with one row per variable and commodity combination for
#'  all of the variables involved in the specified fuel_constraint. Multiple
#'  occurances of the same variable index are permitted as some variables will
#'  be relevant to multiple fuel constraints where they have multiple fuel inputs.
#'  Note that 'fuel' and 'group' columns are maintained to later allow for grouping
#'  at the level at which the constraint was specified, however 'commodity'
#'  determines the fuel which is being constrained in the row itself.
#' @export
get_fuel_constrained_variables <- function(data, consuming_variables,
                                           constraint_type){

  if(!constraint_type %in% c('max', 'min')) {
    stop('Incorrect capacity_type provided')
  }

  commodities <- data$commodities %>%
    select(commodity, group = commodity_category)

  # now get fuel constraint information and get the relevant commodities for each group
  fuel_constraints_data <- get_fuel_contraint_tab(data, constraint_type) %>%
    left_join(commodities,
              by = 'group',
              relationship = 'many-to-many',
              na_matches = 'never') %>%
    mutate(commodity = if_else(is.na(group), fuel, commodity))


  # join in fuel constraints data and reduce to relevant variables
  fuel_constrained_variables <- consuming_variables %>%
    left_join(fuel_constraints_data, by = c('year', 'commodity'),
              relationship = 'many-to-many') %>%
    filter(!is.na(fuel_limit)) # filter out non-constrained technologies

  return(fuel_constrained_variables)
}


#' Fetch the fuel constraint input table from the data list
#'
#' @inheritParams get_fuel_constrained_variables
#'
#' @returns dataframe with one row per fuel commodity/group for each year, for
#'  all fuels/commodities specified to be constrained in the inputs. Columns are:
#'   * year
#'   * fuel - string, the commodity code of a fuel, will be NA if group is provided
#'      instead.
#'   * group - string, the fuel category. Used instead of a commodity code (fuel
#'      column), hence will be NA if commodity code provided.
#'   * apply_to_industry_only - boolean, whether fuel used by non-industry sectors
#'     (e.g. refineries and hydrogen) should be included in the constraint.
#'   * fuel_limit - the value to limit the amount of fuel that can be used.
#' @export
get_fuel_contraint_tab <- function(data, constraint_type) {

  constraint_tab <- if(constraint_type == 'max') {
    data$max_fuel_constraints %>%
      rename(fuel_limit = max) %>%
      filter(fuel_limit < 10000) # remove constraints that are set arbitrarily high
  } else if (constraint_type == 'min') {
    data$min_fuel_constraints %>%
      rename(fuel_limit = min) %>%
      filter(fuel_limit > 0) # remove constraints that are set arbitrarily low
  } else {
    stop('Incorrect constraint_type provided')
  }

  return(constraint_tab)
}



#' Filter out hydrogen and refinery variables when required
#'
#' Joins on the sector to the variables that are to be constrained, so that
#' when apply_to_industry only is TRUE, the hydrogen and refineries sector
#' variables can be removed. No filter is applied to rows where either the
#' sector is not hydrogen or refinery, or the apply_to_industry_only variable
#' is set to FALSE.
#'
#' @inheritParams comit_constraints
#' @param fuel_constrained_variables dataframe for the variables that are to be
#'  constrained, produced by `get_fuel_constrained_variables()`.
#'
#' @returns dataframe as provided with filter applied.
#' @export
apply_sector_filter_to_relevant_fuel_variables <- function(data,
                                                           fuel_constrained_variables) {

  # get sector so we can remove non-industry when required
  sector_info <- data$NAEI_clean %>%
    select(site_ID, IPM_sector)

  # filter out non-industry when necessary
  fuel_constrained_variables <- fuel_constrained_variables %>%
    left_join(sector_info, by = "site_ID") %>%
    filter(
      (apply_to_industry_only == FALSE) |
        (!IPM_sector %in% c("Hydrogen", "Refineries"))
      # the sector part of filter doesn't apply if apply_to_industry is already TRUE
    )

  return(fuel_constrained_variables)
}




#' Aggregate any fuel constraints which have more than one input from a commodity
#' group.
#'
#' Some commodity groups may mean that a technology has more than one input
#' from a commodity. This function ensures that there is only one row per
#' variable/type combination.
#'
#' @param fuel_constrained_variables dataframe containing all decision variables
#'  to be constrained. Produced by `get_fuel_constrained_variables()` and
#'  `apply_sector_filter_to_relevant_fuel_variables()`.
#'
#' @returns dataframe with one row per variable/fuel constraint type combination.
#'  There will often be no reduction in rows and often the input commodity
#'  to variable/type combination ratio is 1:1 for all variables. Columns are:
#'   * year
#'   * variable_index
#'   * type
#'   * input
#'   * fuel_limit
#' @export
aggregate_fuel_constrained_variables <- function(fuel_constrained_variables) {

  fuel_constrained_variables <- fuel_constrained_variables %>%
    # create a variable we can group by
    mutate(type = if_else(is.na(group), fuel, group)) %>%

    # some commodity groups may have more than one input from a commodity
    group_by(year, variable_index, type) %>%
    summarise(input = sum(input),
              fuel_limit = max(fuel_limit),
              .groups = 'drop')

  return(fuel_constrained_variables)
}




#' Create the list of constraints to limit the absolute amount of fuel to be used
#'
#' Generates the information required in the correct format to set constraints in
#' comit. This funciton can be used for both minimum and maximum fuel constraints,
#' switching the direction sign '>=' or '<=' based on the constraint type parameter.
#'
#' @param fuel_constrained_variables dataframe containing all decision variables
#'  to be constrained, as produced by `aggregate_fuel_constrained_variables()`.
#' @inheritParams absolute_fuel_constraints
#'
#' @returns list of constraints. One constraint for each fuel specified to be
#'  constrained in the input, in each time point. Each element contains a
#'  nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
formulate_fuel_constraints <- function(fuel_constrained_variables,
                                       constraint_type) {

  # error handling
  if(!constraint_type %in% c('max', 'min')) {
    stop('Incorrect capacity_type provided')
  }

  constraint_direction <- ifelse(constraint_type == 'max', '<=', '>=')

  fuel_constraints <- fuel_constrained_variables %>%
    group_by(year, type) %>%
    group_map(function(rows, key) {
      list(
        column_indices = rows$variable_index,
        values = rows$input,
        direction = constraint_direction,
        rhs = max(rows$fuel_limit)
      )
    })

  if (length(fuel_constraints) == 0) {
    return(NULL)
  }

  return(fuel_constraints)
}


