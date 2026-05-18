# This constraint is currently in development and is not in use
# The idea is to try and encorporate a constraint that is likely implemented
# in practice where technologies are used for a set period of time once built

tech_stability <- function(data, decision_variables) {

  tech_stability_data <- data$tech_stability

  # Need to get all new capacity decision variables for the techs in constraint
  new_capacity_variables <- decision_variables %>%
    filter(variable_type == 'new_capacity',
           code %in% tech_stability_data$code,
           year != data$model_parameters$start_year) %>%
    select(variable_index, year, site_ID, code)

  new_capacity_variables <- new_capacity_variables %>%
    left_join(tech_stability_data, by = c('code'))


  # Need to get all used capacity decision variables for the techs in constraint
  used_capacity_variables <- decision_variables %>%
    filter(variable_type == 'used_capacity',
           code %in% tech_stability_data$code) %>%
    select(variable_index, year, site_ID, code)

  # We have existing capacity in the first year. So use the used capacity as a
  # proxy for what already exists and needs to continue to be used.
  first_year_used_capacity_variables <- used_capacity_variables %>%
    filter(year == data$model_parameters$start_year) %>%
    left_join(tech_stability_data, by = c('code')) %>%
    mutate(stability_length = initial_stability_length,
           stability_factor = initial_stability_factor
           )

  new_capacity_variables <- rbind(new_capacity_variables, first_year_used_capacity_variables)

  used_capacity_variables <- used_capacity_variables %>%
    filter(year != data$model_parameters$start_year)


  # Link all used_capacity variables to all new_capacity variables on the
  # site_ID and tech code. Then filter out for only those within the tech consistency time.

  new_to_used_variables <- left_join(new_capacity_variables,
                                     used_capacity_variables,
                                     by = c('site_ID', 'code'),
                                     relationship = 'many-to-many',
                                     suffix = c('.new', '.used'))


  new_to_used_variables %<>%
    filter(between(year.used, year.new, year.new + stability_length),
           variable_index.new != variable_index.used)
  # don't want used variables pointing to themselves


  # Then formulate the constraints. For each variable index in the used_capacity
  # variables, used_capacity >= tech_consistency_factor * new_capacity(from base year)

  tech_stability_constraint <- new_to_used_variables %>%
    lazy_dt() %>% # for speed
    group_by(variable_index.used, variable_index.new) %>%
    group_map(function(rows, key) {
      list(
        column_indices = c(key$variable_index.used, key$variable_index.new),
        values = c(1, -1 * unique(rows$stability_factor)),
        direction = '>=',
        rhs = 0
      )

    })

  return(tech_stability_constraint)


}
