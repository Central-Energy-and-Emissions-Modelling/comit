



intermediate_commodities <- function(data, decision_variables) {

  commodities_constraints <- tidy_intermediate_commodities(data)

  commodities_constraints %<>%
    filter(limit > 0) # if 0, no constraint


  intermediate_commodities_preprocessed <- intermediate_commodities_preprocessing(
    data,
    decision_variables,
    commodities_constraints
  )

  decision_variables_to_constrain <- intermediate_commodities_preprocessed[[1]]
  commodities_constraints_df <- intermediate_commodities_preprocessed[[2]]

  all_int_constraints <- build_intermediate_commodities_constraints(data,
                                                                    decision_variables_to_constrain,
                                                                    commodities_constraints,
                                                                    commodities_constraints_df)


  return(all_int_constraints)
}




tidy_intermediate_commodities <- function(data) {

  intermediate_commodities <- data$intermediate_commodities %>%
    pivot_longer(cols = starts_with('20'),
                 names_to = 'year',
                 values_to = 'limit') %>%
    mutate(year = as.numeric(year)) %>%
    pivot_wider(id_cols = year,
                names_from = c('in_commodity', 'out_commodity'),
                names_sep = '_join_',
                values_from = limit)


  years_for_interpolation <- get_years_for_interpolation(data)
  all_years <- years_for_interpolation[[1]]
  modelled_years <- years_for_interpolation[[2]]

  intermediate_commodities <- interpolate_for_years(intermediate_commodities,
                                                    all_years,
                                                    modelled_years)

  intermediate_commodities <- pivot_longer(
    intermediate_commodities,
    cols = !year,
    values_to = 'limit',
    names_to = c('in_commodity', 'out_commodity'),
    names_sep = '_join_'
  )

  return(intermediate_commodities)
}





intermediate_commodities_preprocessing <- function(data,
                                                   decision_variables,
                                                   commodities_constraints) {

  in_commodity_producing_techs <- data$technology_input_output %>%
    filter(commodity %in% commodities_constraints$in_commodity,
           output < 0,
           end_commodity %in% commodities_constraints$out_commodity) %>%
    mutate(output = -1 * output) %>%
    select(producing_technology_code = technology_code,
           in_commodity = commodity,
           producing_end_commodity = end_commodity,
           output)

  out_commodity_producing_techs <- data$technology_input_output %>%
    filter(commodity %in% commodities_constraints$out_commodity,
           output > 0) %>%
    select(using_technology_code = technology_code,
           out_commodity = commodity,
           using_end_commodity = end_commodity,
           output)


  commodities_constraints_df <- commodities_constraints %>%
    left_join(in_commodity_producing_techs,
              by = c("in_commodity"),
              relationship = 'many-to-many') %>%
    left_join(out_commodity_producing_techs,
              by = c("out_commodity"),
              relationship = 'many-to-many')


  decision_variables_to_constrain <- decision_variables %>%
    filter(variable_type == 'used_capacity',
           code %in% c(in_commodity_producing_techs$producing_technology_code,
                       out_commodity_producing_techs$using_technology_code))

  decision_variables_to_constrain <- decision_variables_to_constrain %>%
    left_join(
      in_commodity_producing_techs %>% rename(producing_output = output),
      by = c('code' = 'producing_technology_code')
    ) %>%
    left_join(
      out_commodity_producing_techs %>% rename(used_output = output),
      by = c('code' = 'using_technology_code')
    )

  return(list(decision_variables_to_constrain, commodities_constraints_df))

}



build_intermediate_commodities_constraints <- function(data,
                                                       decision_variables_to_constrain,
                                                       commodities_constraints,
                                                       commodities_constraints_df,
                                                       dir = '==') {

  all_int_constraints <- list()

  for (i in 1:nrow(commodities_constraints)) {

    this_year <- commodities_constraints[i, 'year'] %>% as.numeric()
    this_in_commodity <- commodities_constraints[i, 'in_commodity'] %>% as.character()
    this_out_commodity <- commodities_constraints[i, 'out_commodity'] %>% as.character()

    prop_limit <- commodities_constraints[i, 'limit'] %>% as.numeric()

    producing_techs_to_constrain <- commodities_constraints_df[
      commodities_constraints_df$in_commodity == this_in_commodity, 'producing_technology_code'
    ] %>%
      unique() %>%
      pull()

    using_techs_to_constrain <- commodities_constraints_df[
      commodities_constraints_df$out_commodity == this_out_commodity, 'using_technology_code'
    ] %>%
      unique() %>%
      pull()

    in_decision_variables_to_constrain <- decision_variables_to_constrain %>%
      filter(code %in% producing_techs_to_constrain,
             year == this_year,
             producing_end_commodity == this_out_commodity) %>%
      mutate(coef = producing_output)

    out_decision_variables_to_constrain <- decision_variables_to_constrain %>%
      filter(code %in% using_techs_to_constrain,
             year == this_year,
             using_end_commodity == this_out_commodity) %>%
      mutate(coef = -1 * prop_limit * used_output)


    all_constraint_vars <- bind_rows(in_decision_variables_to_constrain,
                                     out_decision_variables_to_constrain) %>%
      group_by(variable_index) %>%
      summarise(coef = sum(coef))

    int_commodity_constraint <- list(
      column_indices = c(all_constraint_vars$variable_index),
      values = c(all_constraint_vars$coef),
      direction = dir,
      rhs = 0
    )

    all_int_constraints[[i]] <- int_commodity_constraint


  }

  return(all_int_constraints)
}




