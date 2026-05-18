
#### HEADROOM ####--------------------------------------------------------------

electricity_capacity <- function(data, decision_variables) {

  if(data$model_parameters$headroom_scenario == 'no_constraint') {
    return(NULL)
  }

  elec_capacity_df <- data$headroom_constraints %>%
    pivot_longer(cols = !c('year', 'cluster'),
                 names_to = 'scenario',
                 values_to = 'max_elec_gain') %>%
    filter(scenario == data$model_parameters$headroom_scenario) %>%
    select(!scenario)

  ## need to interpolate the data

  elec_capacity_df_wide <- elec_capacity_df %>%
    pivot_wider(id_cols = 'year',
                names_from = 'cluster',
                values_from = 'max_elec_gain')


  years <- data.frame(year = c(data$model_parameters$start_year : data$model_parameters$end_year))

  modelled_years <- data.frame(
    year =  seq(
      from = data$model_parameters$start_year,
      to = data$model_parameters$end_year,
      by = data$model_parameters$timestep
    )
  )

  elec_capacity_df_wide %<>%
    interpolate_for_years(., years, modelled_years)


  # get long again to form constraint
  elec_capacity_df_out <- elec_capacity_df_wide %>%
    pivot_longer(cols = !year,
                 names_to = 'cluster',
                 values_to = 'max_elec_gain')


  elec_techs <- decision_variables %>%
    left_join(data$technology_input_output,
              by = c('code' = 'technology_code'),
              relationship = 'many-to-many') %>%
    left_join(data$commodities,
              by = 'commodity',
              relationship = 'many-to-many') %>%
    left_join(data$Technologies,
              by = 'code') %>%
    filter(commodity == 'INDDISTELC',
           output < 0, # so we only get input commodities
           variable_type == 'used_capacity') %>%
    mutate(input = -1 * output)


  # # get cluster boundary
  # elec_techs %<>%
  #   left_join(data$NAEI_clean %>% select(site_ID, pipe_dist), by = 'site_ID') %>%
  #   mutate(cluster_category = case_when(pipe_dist <= 30 ~ 'Clustered',
  #                                       pipe_dist > 30 ~ 'Dispersed'))


  # need to keep comparing back to these values
  first_year_vals <- elec_techs %>%
    filter(year == data$model_parameters$start_year) %>%
    mutate(input = -1 * input,
           max_elec_gain = NA) %>%
    select(variable_index, code, year, cluster, input, max_elec_gain)



  elec_techs %<>%
    left_join(elec_capacity_df_out, by = c('cluster', 'year')) %>%
    filter(year != data$model_parameters$start_year, # don't want start year constraint
           !is.na(max_elec_gain)) %>% # remove years with no constraint
    select(variable_index, code, year, cluster, input, max_elec_gain)

  years_to_constrain <- unique(elec_techs$year) %>% sort()

  elec_constraint <- list()

  for(this_year in years_to_constrain) {

    this_year_elec <- elec_techs %>%
      filter(year == this_year) %>%
      rbind(first_year_vals) # to provide comparison

    this_year_elec_constraint <- this_year_elec %>%
      group_by(cluster) %>%
      group_map(function(rows, key) {
        list(
          column_indices = rows$variable_index,
          values = rows$input,
          direction = "<=",
          rhs = max(rows$max_elec_gain, na.rm = TRUE)
        )
      })

    elec_constraint <- append(elec_constraint, this_year_elec_constraint)

  }

  return(elec_constraint)
}


## a good test would be that we have n_years * n_clusters * 2 (n cluster_categories) constraints

