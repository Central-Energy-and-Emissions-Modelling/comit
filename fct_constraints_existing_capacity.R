
# NOTE: this constraint is in development and is not currently used
# (although it is available to be switched on in the input spreadsheet)

existing_capacity <- function(data, decision_variables)
  # limits the retrofit available capacity to the (available) 2020 existing capacity of the base technology
{
  #	used_capacity(t,s,retrofit) <= 2020_existing_capacity(t,s,base_tech)
  if (data$model_parameters$use_retrofit == FALSE)
  {
    return(NULL)
  }

  base_capacity_indices = decision_variables %>%
    filter(variable_type == "available_capacity") %>%
    filter(code %in% data$Technologies$retrofit_to)

  # select retrofit decision variables
  retrofit_decision_variables <- decision_variables %>%
    filter(variable_type == "available_capacity") %>%
    left_join(select(data$Technologies, code, retrofit_to), by = "code") %>%
    filter(!is.na(retrofit_to))  %>%

    # join in base technology information
    left_join(
      base_capacity_indices,
      by = c("retrofit_to" = "code", "site_ID", "year"),
      suffix = c(".retro", ".base")
    ) %>%
    # select the columns needed
    select(variable_index.retro, variable_index.base, retrofit_to)

  #__________________________________________________________________________________________________________________________________________________
  # code adapted from production constraint - to calculate the (decreasing) 2020 existing capacity amount in each year

  # Filter decision variables to available capacity only
  available_existing_capacity <- decision_variables %>%
    filter(variable_type == "available_capacity") %>%
    left_join(data$Technologies, by = "code") %>%
    filter(code %in% retrofit_decision_variables$retrofit_to) %>%
    # Add residual capacity of each technology in each year after adjusting for site scaling factor
    left_join(data$site_demand %>% distinct(site_ID, scaling_factor_within_sector),
              by = "site_ID") %>%
    mutate(existing_capacity_2020 = existing_capacity_2020 * scaling_factor_within_sector) %>%
    mutate(residual_capacity = pmax(
      0 ,
      (-existing_capacity_2020 / lifetime) * (year - data$model_parameters$start_year) +
        existing_capacity_2020,
      na.rm = TRUE
    ))

  #__________________________________________________________________________________________________________________________________________________

  existing_capacity_constraint = available_existing_capacity %>%
    left_join(
      retrofit_decision_variables,
      by = c("variable_index" = "variable_index.base"),
      suffix = c(".B", ".R")
    ) %>%

    select(variable_index.retro, year, site_ID, residual_capacity) %>%
    unique() %>%
    filter(!is.na(variable_index.retro)) %>%

    group_by(variable_index.retro) %>%
    group_map(function(row, key) {
      # the key indicates the index of the used_capacity variable
      list(
        column_indices = key[[1]],
        values = 1,
        direction = "<=",
        rhs = row$residual_capacity
      )
    })


  #________________________________________________________________________

  return(existing_capacity_constraint)
}
