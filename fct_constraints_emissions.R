
#' Constrain the amount of emissions that can be released by industry in each
#' year
#'
#' This constraint states that the total amount of emissions released into
#'  the atmosphere by industry cannot exceed a certain amount in a certain year.
#'
#' @inheritParams comit_constraints
#'
#' @returns list of constraints. One constraint for each time point.
#'  Each element contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
emissions_limit <- function(data, decision_variables) {

  emissions_variables <- get_emissions_variables(data, decision_variables)

  emissions <- formulate_emissions_constraint(emissions_variables)

  return(emissions)
}




#' Get emissions data for used capacity variables to set up emissions constraint
#'
#' Gets the amount of emissions per used capacity of the used_capacity
#'  decision variables, as well at the emissions limit for the relevant year
#'  from the input data.
#'
#' @inheritParams comit_constraints
#'
#' @returns dataframe, with one row per used_capacity decision variable.
#'  Columns include:
#'  * emissions_per_used_capacity, numeric, kt of CO2e
#'  * emissions_limit, numeric, the annual maximum emissions/
#' @export
get_emissions_variables <- function(data, decision_variables) {

  used_capacity_variables <- decision_variables %>%
    filter(variable_type == "used_capacity") %>%
    left_join(select(data$Technologies, code, sector), by = "code") %>%
    filter(sector != 'hydrogen_conversion')

  # include direct and indirect emissions for the hydrogen sector
  # but only direct emissions for all other sectors
  used_capacity_variables %<>%
    mutate(
      emissions_per_used_capacity = if_else(
        sector == "Hydrogen",
        get_emissions(code, year, capture = FALSE, location = NULL,
                      .data = data),
        get_emissions(code, year, capture = FALSE, location = "direct",
                      .data = data)
      )
    )

  used_capacity_variables %<>%
    left_join(data$emissions_limit, by = c("year" = "year"))

  return(used_capacity_variables)

}



#' Formulate the emissions limit constraint
#'
#' @param emissions_variables
#'
#' @inherit emissions_limit return
#' @export
formulate_emissions_constraint <- function(emissions_variables) {

  emissions <- emissions_variables %>%
    group_by(year) %>%
    group_map(function(rows, key) {
      list(
        column_indices = rows$variable_index,
        values = rows$emissions_per_used_capacity,
        direction = "<=",
        rhs = rows$max_emissions[1]
      )
    })

  return(emissions)
}



