

known_changes <- function(data, decision_variables) {

  known_outcomes_tab <- data$known_changes %>%
    select(!notes)

  ## find what sites and technologies are effected

  known_outcomes_tab <- known_outcomes_tab %>%
    left_join(data$NAEI_clean %>% select(site_ID, PlantID), by = 'PlantID') %>%
    filter(!is.na(site_ID)) %>%
    rename(constrained_tech = technology)

  known_outcomes_data <- known_outcomes_tab %>%
    left_join(data$Technologies,
              by = c('output_commodity'),
              relationship = 'many-to-many') #%>%
    #mutate(existing_capacity_2020 = existing_capacity_2020 * capacity_to_activity_factor)


  output_info <- data$technology_input_output

  known_outcomes_data <- known_outcomes_data %>%
    left_join(output_info, by = c('code' = 'technology_code')) %>%
    filter(output_commodity == commodity) # only want the rows where the particular
  # output is the relevant one from the known outcomes input table

  known_outcomes_data <- known_outcomes_data %>%
    mutate(output_amount = output #* availability_factor * capacity_to_activity_factor # not sure on including these!
           )

  ## find the decision variablecommodity## find the decision variables for the technologies to be limited

  vars_to_constrain <- known_outcomes_data %>%
    left_join(decision_variables, by = c('site_ID', 'code'),
              relationship = 'many-to-many')


  vars_to_constrain <- vars_to_constrain %>%
    filter(between(year, year_from, year_to),
           variable_type == 'used_capacity')

  vars_to_constrain <- vars_to_constrain %>%
    mutate(
      # set arbirtrary limit so we can do both min and max for everything at once
      min_proportion = ifelse(is.na(min_proportion), 0, min_proportion),
      max_proportion = ifelse(is.na(max_proportion), 1, max_proportion),
      # apply capacity_to_activity_factor
      coef_min = case_when(
        code == constrained_tech ~ output_amount * (1 - min_proportion),
        TRUE ~ -1* output_amount * (min_proportion)
      ),
      coef_max = case_when(
        code == constrained_tech ~ output_amount * (1 - max_proportion),
        TRUE ~ -1 * output_amount * (max_proportion)
      )
    )

  ## Write the constraint for min and max prod

  # min constraints
  known_outcomes_constraints_min <- vars_to_constrain %>%
    group_by(site_ID, constrained_tech, year) %>%
    group_map(function(rows, key) {
      list(column_indices = rows$variable_index,
           values = rows$coef_min,
           direction = '>=',
           rhs = 0)
    })


  # max constraints
  known_outcomes_constraints_max <- vars_to_constrain %>%
    group_by(site_ID, constrained_tech, year) %>%
    group_map(function(rows, key) {
      list(column_indices = rows$variable_index,
           values = rows$coef_max,
           direction = '<=',
           rhs = 0)
    })




  ### The below should then constrain the output commodity from the technologies
  # to ensure that it is not eradicated to satisfy the above constraints

  commodities_constraints <- known_outcomes_tab %>%
    expand_df_by_model_years(data) %>%
    filter(between(year, year_from, year_to)) %>%
    select(year,
           in_commodity = output_commodity,
           out_commodity = final_output_commodity,
           limit = min_output_to_final_output_ratio,
           site_ID)


  known_outcomes_constraints_min_out <- site_level_intermediate_commodities(
    data,
    decision_variables,
    commodities_constraints)

  all_known_outcomes_constraints <- c(known_outcomes_constraints_min,
                                      known_outcomes_constraints_max,
                                      known_outcomes_constraints_min_out)

  return(all_known_outcomes_constraints)

}


### Need to fix intermediate commodity production at those sites to ensure
# production of commodity is not nullified
site_level_intermediate_commodities <- function(data,
                                                decision_variables,
                                                commodities_constraints) {



  #commodities_constraints <- #tidy_intermediate_commodities(data)

  commodities_constraints %<>%
    filter(limit > 0) # if 0, no constraint

  site_list <- unique(commodities_constraints$site_ID)

  all_int_constraints <- list()


  for(site in site_list) {

    intermediate_commodities_preprocessed <- intermediate_commodities_preprocessing(
      data,
      decision_variables %>% filter(site_ID == site),
      commodities_constraints %>% filter(site_ID == site)
    )

    decision_variables_to_constrain <- intermediate_commodities_preprocessed[[1]]
    commodities_constraints_df <- intermediate_commodities_preprocessed[[2]]

    int_constraints <- build_intermediate_commodities_constraints(
      data,
      decision_variables_to_constrain,
      commodities_constraints %>% filter(site_ID == site),
      commodities_constraints_df,
      dir = '>='
    )

    all_int_constraints <- c(all_int_constraints, int_constraints)
  }

  return(all_int_constraints)

}

