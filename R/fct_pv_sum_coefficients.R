
#' Function to combine multiple PV dataframes and do a rowwise sum
#' @param PV_dataframes A list of two column dataframes. The first column should be a variable index
#' The second column should be the coefficient
#' @param n_decision_varaibles The number of decision variables in the optimisation. This is usually
#' the number of rows of the decision variables object
sum_PV_coefficients <- function(PV_dataframes, n_decision_variables)
{
  # If there are any "null" PV dataframes, we need to set them to an empty dataframe
  PV_dataframes %<>% modify_if(is.null,
                               ~ data.frame(variable_index = integer(),
                                            coefficient = numeric()))

  # Join all PV_dataframes together
  x <- PV_dataframes %>%
    reduce(full_join, by = "variable_index", suffix = c("", ".new")) %>%
    set_names(c("variable_index", names(PV_dataframes))) %>%

    # We need to right join this dataframe to a column which has the index of every decision variable in the model
    right_join(data.frame(variable_index = 1:n_decision_variables), by = "variable_index") %>%

    # sum across all columns except the variable_index
    mutate(coefficient = rowSums(.[setdiff(names(.), "variable_index")], na.rm = TRUE)) %>%

    arrange(variable_index)

  return(x)
}
