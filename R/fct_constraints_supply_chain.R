


# Supply chain constraints

supply_chain_constraints_by_tech <- function(data, decision_variables){

  tech <- data$Technologies %>%
    mutate(existing_capacity_2020 = existing_capacity_2020 * capacity_to_activity_factor)
  # the above adjustment is made to calculate the true capacity for CHP technologies


  first_year_capacity <- tech %>%
    group_by(sector, output_unit) %>%
    summarise(total_capacity = sum(existing_capacity_2020))

  existing_capacities <- tech %>%
    group_by(sector, output_unit, technology_category) %>%
    summarise(existing_capacity_2020 = sum(existing_capacity_2020)) %>%
    left_join(first_year_capacity, by = c('sector', 'output_unit')) %>%
    mutate(initial_capacity_prop = existing_capacity_2020/total_capacity) %>%
    ungroup()

  min_capacity_techs <- unique(data$minimum_capacity$code)

  new_sc <- data$supply_chain_constraints %>%
    pivot_longer(cols = !year,
                 names_to = 'category',
                 values_to = 'prop_of_initial_capacity_for_max_growth_allowance') %>%
    mutate(technology_category = str_extract(category, '.*(?=\\,.*,)'),
           output_unit = str_extract(category, '(?<=\\, ).*(?=\\, )'),
           sector = str_remove_all(category, technology_category),
           sector = str_remove_all(sector, output_unit),
           sector = str_remove_all(sector, '\\,'),
           sector = str_trim(sector)) %>%
    select(!category)


  new_sc %<>% right_join(tech %>%
                           select(technology_category, sector, code, output_unit),
                         by = c('technology_category', 'sector', 'output_unit'),
                         relationship = 'many-to-many')

  new_sc %<>% left_join(existing_capacities,
                        by = c('technology_category', 'sector', 'output_unit'),
                        relationship = 'many-to-many')



  new_sc %<>%
    mutate(prop_of_initial_capacity_for_max_growth_allowance = case_when(
      initial_capacity_prop >= 0.1 ~ 1000,
      code %in% min_capacity_techs ~ 1000,
      TRUE ~ prop_of_initial_capacity_for_max_growth_allowance
    ))


  # account for use of longer time periods
  if(data$model_parameters$timestep > 1) {
    new_sc$prop_of_initial_capacity_for_max_growth_allowance <- (
      new_sc$prop_of_initial_capacity_for_max_growth_allowance
      * data$model_parameters$timestep
    )
  }


  new_tech_vars <- decision_variables %>%
    filter(
      variable_type == 'new_capacity',
      year != data$model_parameters$start_year,  # don't constrain in yr 1
      !code %in% c(
        'INDMAINSHYGG_conversion',
        'INDMAINSHYGR_conversion',
        'INDMAINSHYGB_conversion'
      )
    ) %>% # can ignore 1st year
    left_join(new_sc, by = c('code', 'year')) %>%
    filter(!is.na(prop_of_initial_capacity_for_max_growth_allowance),
           prop_of_initial_capacity_for_max_growth_allowance < 10)
  # ^remove completely constraints that are set arbitrarily large


  new_tech_vars_constraints <- new_tech_vars %>%
    group_by(sector, output_unit, technology_category, year) %>%
    group_map(function(rows, key) {
      list(column_indices = rows$variable_index,
           values = rep(1, length(rows$variable_index)),
           direction = '<=',
           rhs = max(rows$prop_of_initial_capacity_for_max_growth_allowance
                     * rows$total_capacity)) # use max to return single value
    })

  return(new_tech_vars_constraints)

}
