
#' Create a constraint to stop the building of new capacity in the first modeled
#' year
#'
#' Limit the building of new capacity for technology, hydrogen pipes and CO2
#'  pipes to 0 for the first period in the model. This ensures only the preset
#'  available capacities are used in the start year.
#'
#' @inheritParams comit_constraints
#'
#' @returns list of constraints. One constraint per variable for each new
#'  capacity variable in the start year. Each element contains a nested list
#'  with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
no_building_in_start_year <- function(data, decision_variables) {

  # define and filter on the new capacity variables
  new_capacity_vars <- c(
    "H2_national_pipe_new_capacity",
    "CO2_pipe_new_capacity",
    "H2_pipe_new_capacity",
    "new_capacity"
  )

  new_build_variables <- decision_variables %>%
    filter(variable_type %in% new_capacity_vars,
           year == data$model_parameters$start_year)

  # formulate the constraint
  build_start <- new_build_variables %>%
    group_by(variable_index) %>%
    group_map(function(rows, key) {
      list(
        column_indices = key[[1]],
        values = 1,
        direction = '==',
        rhs = 0
      )
    })

  return(build_start)
}









