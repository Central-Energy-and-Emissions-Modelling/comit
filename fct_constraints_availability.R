
#' Constrains the amount of used capacity based on the available capacity
#'
#' This constrains each technology, at each site, at each time point to only
#' use the capacity which is available to it. The available capacity is determined
#' by the available capacity variables multiplied by the availability factor of
#' the technology multiplied by the capacity to activity factor of the technology.
#'
#' used_capacity(t,s,tech) <= available_capacity(t,s,tech) \* availability_factor(tech)
#'  \* capacity_to_activity_factor(tech)
#'
#' @inheritParams comit_constraints
#'
#' @returns list of constraints. One constraint for each technology, at each site,
#'  at each time point.
#'  Each element contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
availability <- function(data, decision_variables) {

  # Some simple error handling -------------------------------------------------
  if(nrow(decision_variables) == 0) {
    stop('No decision variables present')
  }

  if(nrow(data$Technologies) == 0) {
    stop('No technologies data present')
  }
  #-----------------------------------------------------------------------------

  used_capacity_variables <- decision_variables %>%
    filter(variable_type == "used_capacity") %>%
    lazy_dt()

  available_capacity_variables <- decision_variables %>%
    filter(variable_type == "available_capacity") %>%
    lazy_dt()

  temp_techs <- data$Technologies %>%
    select(code, availability_factor, capacity_to_activity_factor) %>%
    lazy_dt()

  # get table where each row has the respective used and available capacities
  availability_constraint_data <- left_join(
      used_capacity_variables,
      available_capacity_variables,
      by = c("site_ID", "code", "year"),
      suffix = c(".used_capacity", ".available_capacity")
    )

  # join in availability factor and capacity to activity factor
  availability_constraint_data %<>%
    left_join(temp_techs, by = "code") %>%
    mutate(total_factor = availability_factor * capacity_to_activity_factor) %>%
    select(variable_index.used_capacity,
           variable_index.available_capacity,
           total_factor)

  # Generate the constraints
  availability_constraint <- formulate_availability_constraint(
    availability_constraint_data
    )

  return(availability_constraint)
}



#' Create the list of constraints for availability
#'
#' @param availability_constraint_data, data.table containing the data to set up
#'  the constraint. Requires the following columns:
#'  * variable_index.used_capacity - the index of the used capacity variable
#'  * variable_index.avaialable_capacity - the index of the available capacity variable
#'  * total_factor - the factor to times the available capacity by (available_factor \*
#'  capacity_to_activity_factor)
#'
#' @returns list of constraints. One constraint for each technology, at each site,
#'  at each time point.
#'  Each element contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
formulate_availability_constraint <- function(availability_constraint_data) {

  if(availability_constraint_data %>% count() %>% pull(n) == 0){
    stop('No availability constraints data present')
  }

  availability_constraint <-  availability_constraint_data %>%
    group_by(variable_index.used_capacity) %>%
    group_map(function(row, key) {
      list(
        column_indices = c(key[[1]], row$variable_index.available_capacity[1]),
        values = c(1,-row$total_factor[1]),
        direction = "<=",
        rhs = 0
      )
    })

  # Note, explaining the group_map above:
  # The key is the index of the used_capacity variable.
  # Column indicies is the used_Capacity and available_Capacity
  # The coefficient for the used_capacity variable is 1 and for the
  # available_capacity variable is the negative availability factor.

  return(availability_constraint)

}

