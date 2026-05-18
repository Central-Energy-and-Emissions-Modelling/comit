
#
# raw_data <- read_excel_data_template(
#   "../comit_development/data_template_archive/comit_input_1_3_0_dev.xlsx"
# )
#
# in_app <- FALSE
#
# data <- comit_preprocess_inputs(raw_data, in_app)
#
# objective_function <- comit_objective_function(data, in_app)
#
# decision_variables <- objective_function[[1]]
# PV_coefficients <- objective_function[[2]]


supply_chain_constraints_new <- function(data, decision_variables) {


  decision_variables <- decision_variables %>%
    filter(variable_type == 'new_capacity')

  constraint_df <- data$new_supply_chains %>%
    pivot_longer(cols = starts_with('20'),
                 names_to = 'year',
                 values_to = 'capacity') %>%
    filter(capacity < 1000)

  constraint_df <- constraint_df %>%
    mutate(
      group = row_number(), # to retain group id
      all_codes = str_split(all_codes, ', ')) %>%
    unnest(all_codes)


  year_list = seq(
    data$model_parameters$start_year,
    data$model_parameters$end_year,
    data$model_parameters$timestep
  )



  # now apply to each year and group by tech and nearest year,
  # summing the additional capacity and restricting to only the modelled years.

  constraint_df <- constraint_df %>%
    mutate(year = as.numeric(year)) %>%
    rowwise() %>%
    mutate(next_modelled_year = find_next_modelled_year(year, year_list)) %>%
    ungroup() %>%
    select(all_codes, year = next_modelled_year, group, capacity)


  constraint_df <- constraint_df %>%
    group_by(all_codes, year, capacity) %>%
    summarise(capacity = sum(capacity), .groups = 'drop',
              group = max(group))

  sc_constraints <- list()

  for (i_group in unique(constraint_df$group)) {

    this_constraint_df <- constraint_df %>%
      filter(group == i_group)

    year_of_constraint <- unique(this_constraint_df$year) %>% as.numeric()

    techs <- unique(this_constraint_df$all_codes)

    additional_capacity <- unique(this_constraint_df$capacity)

    if(year_of_constraint == data$model_parameters$start_year) {
      next
    }

    this_years_vars <- decision_variables %>%
      filter(year == year_of_constraint,
             code %in% techs) %>%
      mutate(coef = 1)

    # last_years_vars <- decision_variables %>%
    #   filter(year == year_of_constraint - data$model_parameters$timestep,
    #          code %in% techs) %>%
    #   mutate(coef = -1)
    #
    # both_sets <- rbind(this_years_vars, last_years_vars)


    constraint <- list(
      column_indices = this_years_vars$variable_index,
      values = this_years_vars$coef,
      direction = '<=',
      rhs = additional_capacity
    )

    sc_constraints <- c(sc_constraints, list(constraint))

  }

  return(sc_constraints)

  ### TODO, WHEN USING A TIMESTEP, NEED TO ADD ALL OF THE YEARS IN BETWEEN!

}



find_next_modelled_year <- function(x, year_list) {

  years_dif <- x - year_list
  names(years_dif) <- year_list

  years_dif <- years_dif[years_dif <= 0]

  next_modelled_year <- years_dif[which.max(years_dif)] %>%
    names() %>%
    as.numeric()

  return(next_modelled_year)
}
