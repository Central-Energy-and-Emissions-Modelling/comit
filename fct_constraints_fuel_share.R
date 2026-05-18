

#' Constrain fuel use to remain under a specified share of the total fuel used
#' to produce a commodity
#'
#' Create constraints that set upper limits for the proportional amount of a fuel
#'  that can be used to produce a given commodity, as specified in the input file.
#'
#' Note that there are limits in the input dataframe for fuel categories for
#'  commodities that don't actually have inputs from that category - these are
#'  dropped when setting the constraint up.
#'
#' @inheritParams comit_constraints
#'
#' @returns list of constraints. One constraint for each commodity specified to
#'  be constrained in the input, in each time point and for each fuel type
#'  that is to be constrained. Each element contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
max_fuel_share <- function(data, decision_variables) {

  commodities <- data$commodities %>%
    select(commodity, group = commodity_category)

  fuel_commodities <- get_all_fuel_commodities(commodities)

  max_fuel_share_df <- get_raw_max_fuel_share_data(data, commodities)


  # we need another table with all fuel vars though, in order to calculate the share.
  fuel_consuming_variables <- get_fuel_consuming_variables(data,
                                                           decision_variables,
                                                           fuel_commodities,
                                                           max_fuel_share_df)

  fuel_constraint_combinations <- get_fuel_share_constraint_combinations(
    fuel_consuming_variables,
    max_fuel_share_df,
    data
  )


  fuel_share_constraints <- formulate_fuel_share_constraints(
    fuel_consuming_variables,
    fuel_constraint_combinations
  )

  return(fuel_share_constraints)

}


#' Return the fuel commodities from the commodity input data
#'
#' @param commodities dataframe for the commodities data.
#'
#' @returns dataframe for the fuel commodities. Will include the columns from
#'  the input parameter *commodities*.
#' @export
get_all_fuel_commodities <- function(commodities) {

  all_fuels <- c("Biomass and organic waste",
                 "Hydrogen",
                 "Coal",
                 "Oil",
                 "Gas",
                 "Electricity",
                 "Inorganic waste")

  fuel_commodities <- commodities %>%
    filter(group %in% all_fuels)

  return(fuel_commodities)
}



#' Get the max fuel share limit values from the input data
#'
#' Gets the max fuel share constraint data used to formulate the constraint.
#'  Reads in the table from the input, then tidies into a longer format and joins
#'  fuel category information from the commodities data.
#'
#' @inheritParams comit_constraints
#' @inheritParams get_all_fuel_commodities
#'
#' @returns dataframe, with one row per input for each possible input commodity,
#'  output_commodity and fuel type combination that is to be constrained. Columns
#'  include:
#'   * sector
#'   * output_commodity
#'   * group (fuel category)
#'   * share_limit
#'   * commodity (inputs)
#' @export
get_raw_max_fuel_share_data <- function(data, commodities) {

  max_fuel_share_df <- data$max_fuel_share %>%
    pivot_longer(cols = !c('sector', 'output_commodity'),
                 names_to = 'group',
                 values_to = 'share_limit') %>%
    filter(share_limit < 1)

  # Link the commodities to the fuel groups
  max_fuel_share_df %<>%
    left_join(commodities,
              by = 'group',
              relationship = 'many-to-many',
              na_matches = 'never')
  # these are the fuels to constrain!

  return(max_fuel_share_df)
}



#' Get decision variables that use fuel commodities
#'
#' Return data for all variables of *used_capacity* type which are technologies
#'  that use fuel commodities as inputs. Also joins on fuel share
#'  constraint input data.
#'
#' @inheritParams comit_constraints
#' @param fuel_commodities dataframe containing all fuel commodities. Contains
#'  two columns: commodity (commodity code) and group (fuel category).
#' @param max_fuel_share_df dataframe for the fuel share constraints information,
#'  produced by `get_raw_max_fuel_share_data()`.
#'
#' @returns dataframe, with one row for each fuel input for each decision variable.
#'  Columns include:
#'   * variable_index
#'   * year
#'   * site_ID
#'   * code - technology code
#'   * commodity - input commodity that is consumed by the technology
#'   * input - numeric, amount of consumption of commodity per unit of output
#'   * output_commodity - the commodity produced by the technology
#'   * sector
#'   * group - fuel category
#'   * share_limit - numeric, from the fuel share constraint inputs
#' @export
get_fuel_consuming_variables <- function(data,
                                         decision_variables,
                                         fuel_commodities,
                                         max_fuel_share_df) {

  # Link to relevant decision variables
  consuming_variables <- get_consuming_variables(data, decision_variables)

  fuel_consuming_variables <- consuming_variables %>%
    filter(commodity %in% fuel_commodities$commodity)


  # need to merge on outputs for grouping
  technology_output_group <- data$Technologies %>%
    select(code, output_commodity)

  fuel_consuming_variables <- left_join(fuel_consuming_variables,
                                        technology_output_group,
                                        by = c('code'))

  # get sector information for join and filtering
  sector_info <- data$NAEI_clean %>%
    select(site_ID, sector = IPM_sector)

  fuel_consuming_variables <- fuel_consuming_variables %>%
    left_join(sector_info, by = "site_ID")

  fuel_consuming_variables <- join_fuel_share_data(fuel_consuming_variables,
                                                   max_fuel_share_df)

  return(fuel_consuming_variables)
}




#' Gets the relevant fuel share constraints data for the fuel consuming
#' decision variables
#'
#' Joins the share limit information to the relevant decision variables to
#'  allow for the formulation of the fuel share constraints.
#'
#' @param fuel_consuming_variables dataframe for the fuel consuming
#'  decision variables.
#' @inheritParams get_fuel_consuming_variables
#'
#' @returns dataframe with same number of rows as input dataframe
#'  fuel_consuming_variables, but with an additional column for share_limit
#'  (numeric) and group (character for fuel category).
#' @export
join_fuel_share_data <- function(fuel_consuming_variables, max_fuel_share_df) {

  # Now join on constraint information
  fuel_consuming_variables <- fuel_consuming_variables %>%
    left_join(max_fuel_share_df,
              by = c("commodity",
                     "sector",
                     'output_commodity')) %>%
    mutate(group = ifelse(is.na(group), 'non_constrained_fuel', group)) %>%
    select(variable_index, year, commodity, input, sector,
           output_commodity, group, share_limit)

  return(fuel_consuming_variables)
}



#' Get the distinct groups for setting fuel share constraints.
#'
#' Create a dataframe of all distinct possible combinations of sector, group (fuel
#'  category), output_commodity and year for the fuel share constraints.
#'
#' The combinations are found by finding the sector, fuel categories and output
#'  commodity combinations that are present in both the fuel consuming variables
#'  data and the fuel share constraint input data.
#'
#' @inheritParams join_fuel_share_data
#' @inheritParams comit_constraints
#'
#' @returns dataframe, with as many rows as there are to be fuel share constraints.
#'  There is one constraint per possible combination of sector, fuel category,
#'  output_commodity and year. Columns are:
#'  * sector
#'  * group (fuel category)
#'  * output_commodity
#'  * year
#'
#' @export
get_fuel_share_constraint_combinations <- function(fuel_consuming_variables,
                                                   max_fuel_share_df,
                                                   data) {

  # Now get only the combinations we need, and iterate through these
  distinct_fuel_constraints_data_combos <- max_fuel_share_df %>%
    distinct(sector, group, output_commodity)

  distinct_fuel_consuming_variables_combos <- fuel_consuming_variables %>%
    distinct(sector, group, output_commodity, year)

  fuel_constraint_combinations <- inner_join(distinct_fuel_constraints_data_combos,
                                             distinct_fuel_consuming_variables_combos,
                                             by = c('sector',
                                                    'group',
                                                    'output_commodity')) %>%
    filter(year != data$model_parameters$start_year)

  return(fuel_constraint_combinations)
}




#' Create the list of constraints to limit the proportional amount of fuel to be
#' used
#'
#' Generates the information required in the correct format to set constraints
#'  in comit, allowing fuel shares to be constrained in the production of
#'  commodities.
#'
#' @param fuel_consuming_variables dataframe of the fuel consuming decision variables.
#'  Produced by `get_fuel_consuming_variables`.
#' @param fuel_constraint_combinations dataframe, produced by
#'  `get_fuel_share_constraint_combinations`.
#'
#' @returns list of constraint. One constraint for each fuel to be constrained,
#'  for each output commodity to be constrained, in each time point. Each element
#'  contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#'
#' @export
formulate_fuel_share_constraints <- function(fuel_consuming_variables,
                                             fuel_constraint_combinations) {

  # we can join to the combinations table which will give us our grouping
  # variables. Now decision variables and there input fuels are present multiple
  # times, appearing in each group to be constrained.
  fuel_consuming_variables_large_df <- left_join(
    fuel_constraint_combinations,
    fuel_consuming_variables,
    by = c('sector', 'output_commodity', 'year'),
    suffix = c('.combinations', '.var'),
    relationship = 'many-to-many'
  )

  fuel_share_constraints <- fuel_consuming_variables_large_df %>%
    group_by(sector, output_commodity, year, group.combinations) %>%
    group_map(function(rows, keys) {

      share <- rows %>%
        filter(group.var == keys$group.combinations) %>%
        distinct(share_limit) %>%
        pull()

      rows <- rows %>%
        mutate(coef = if_else(group.var == keys$group.combinations,
                              (1 - share) * input,
                              -1 * share * input)) %>%
        group_by(variable_index) %>%
        summarise(coef = sum(coef), .groups = 'drop')

      list(
        column_indices = rows$variable_index,
        values = rows$coef,
        direction = '<=',
        rhs = 0
      )
    })

  return(fuel_share_constraints)
}

