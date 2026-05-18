

#' Solve the comit Counterfactual
#'
#' Finds the solution to comit under counterfactual conditions. The counterfactual
#'  conditions are typically where no non-agreed future policies or interventions
#'  from the government take place (EEP) however users can create there own
#'  scenarios using the counterfactual_rollout tab of the input spreadsheet.
#'
#' This allows comparisons to be made to normal comit runs and is fundamental
#'  to producing attribution data.
#'
#' @param raw_data, a list of dataframes produced from `read_excel_data_template()`
#'  that contains all data for the modelling. Importantly this list should include
#'  the counterfactual rollout data named as 'counterfactual_rollout'.
#'
#' @returns list of length four. The first element is the solution as produced by
#'  [`ROI_solve()`].The second element is the list of preprocessed data used to
#'  find the solution. The third and fourth elements are the decision variables
#'  and coefficients respectively, as produced by [`comit_objective_function()`].
#'
#' @export
comit_counterfactual_solver <- function(raw_data){

  # Set resource and energy efficiency to 0
  raw_data$resource_efficiency %<>% set_all_vals_to_0()
  raw_data$energy_efficiency %<>% set_all_vals_to_0()

  # Preprocess the input data
  data <- comit_preprocess_inputs(raw_data, counterfactual = TRUE)

  # Define the objective function
  objective_function <- comit_objective_function(data)
  decision_variables <- objective_function[[1]]
  PV_coefficients <- objective_function[[2]]

  # List of constraint functions to apply
  constraint_functions <- c(
    "capacity_transfer",
    "availability",
    "hydrogen_production",
    "hydrogen_flows",
    "CO2_infrastructure_capacity",
    "CO2_cluster_to_storage_transport",
    "CO2_storage_injection",
    "H2_infrastructure_capacity",
    "minimum_hydrogen_plant_size",
    "non_industry_H2_demand",
    'baseline_constraint'
  )

  # Generate required constraints and put into matrix
  constraints <- lapply(constraint_functions,
                        run_constraint_function,
                        data,
                        decision_variables)

  constraints %<>% combine_matrix_constraints()

  # Solve the problem
  solved_baseline <- comit_problem_solver(data,
                                          decision_variables,
                                          PV_coefficients,
                                          constraints)

  return(solved_baseline)

}



#' Create Constraint To Set Used Capacity to EEP rollout
#'
#' Generate constraint for required used capacity of each technology for the
#'  counterfactual run.
#'
#' @param data, list of dataframes containing input data, produced by
#'  `comit_preprocess_inputs()`.
#' @param decision_variables dataframe for the decision variables. The first
#'  element of the list produced by `comit_objective_function()`.
#'
#' @returns list of lists, used to build constraints in the
#'  `combine_matrix_constraints()` function.
#'
#' @export
baseline_constraint <- function(data, decision_variables) {

  site_used_capacity <- get_yearly_site_used_capacity(data, decision_variables)

  # Now that we have the required used_capacity in every year, we can create a
  # constraint which sets the used_capacity variable in each year for each site
  # to the calculated value
  baseline_constraint <- decision_variables %>%
    filter(variable_type == "used_capacity") %>%
    left_join(site_used_capacity, by = c("year", "site_ID", "code")) %>%
    filter(!grepl("^PHYG", code))

  baseline_constraint %<>%
    group_by(variable_index) %>%
    group_map(function(row, key) { # key is index of the used_capacity variable
      list(
        column_indices = key[[1]],
        values = 1,
        direction = "==",
        rhs = row$site_required_capacity
      )
    })

  return(baseline_constraint)
}




#' Calculate the Yearly Required Used Capacity for Each Technology at Each Site
#'
#' Gets the amount of used capacity that each site needs for each technology in
#'  order to match EEP outputs. Used for setting the baseline constraint in the
#'  counterfactual model.
#'
#' @inheritParams baseline_constraint
#'
#' @returns dataframe containing the yearly required capacities for each
#' technology at each site.
#'
#' @export
get_yearly_site_used_capacity <- function(data, decision_variables) {

  modelled_years <- seq(data$model_parameters$start_year,
                        data$model_parameters$end_year,
                        by = data$model_parameters$timestep)

  used_capacity <- lapply(modelled_years, yearly_used_capacity, data = data)

  names(used_capacity) <- modelled_years

  used_capacity_df <- bind_rows(used_capacity, .id = "year") %>%
    mutate(year = as.numeric(year))

  site_demands <- data$site_demand %>%
    distinct(year, site_ID, scaling_factor_within_sector)

  # scale used_capacity for each site
  site_used_capacity <- decision_variables %>%
    filter(variable_type == "used_capacity") %>%
    left_join(used_capacity_df, by = c("code", "year")) %>%
    left_join(site_demands, by = c("year", "site_ID"))

  site_used_capacity %<>%
    mutate(site_required_capacity = required_capacity * scaling_factor_within_sector) %>%
    select(year, site_ID, code, site_required_capacity) %>%
    replace_na(replace = list(site_required_capacity = 0))

  return(site_used_capacity)

}


#' Calculate Used Capacity For Existing Technologies For a Given Year
#'
#' Uses EEP rollout to get the required yearly capacities for existing technologies.
#'
#' @param year, numeric
#' @inheritParams baseline_constraint
#'
#' @returns dataframe with the required capacity for each technology.
#'
#' @export
yearly_used_capacity <- function(year, data) {

  rollout <- get_eep_rollout(data)

  this_year_rollout <- rollout %>%
    filter(year == !!year) %>%
    select(code, scaled_rollout)

  techs_matrix <- get_existing_techs_matrix(data, this_year_rollout)

  # We also need to make a demand (output) matrix. In this case a 1-d matrix
  output <- tibble(commodity = rownames(techs_matrix)) %>%
    left_join(data$Demand_drivers %>% filter(year == !!year),
              by = "commodity") %>%
    replace_na(list(demand = 0)) %>%
    pull(demand)

  names(output) <- rownames(techs_matrix)

  # solve for amount of technology needed to be meet demand
  required_output <- solve(techs_matrix, output)

  capacity <- data$Technologies %>%
    left_join(enframe(required_output, name = "output_commodity", value = "required_output"),
      by = "output_commodity") %>%
    left_join(this_year_rollout, by = "code") %>%
    mutate(
      output_per_capacity = 1,
      required_capacity = required_output * scaled_rollout / output_per_capacity
    ) %>%
    select(code, required_capacity) %>%
    replace_na(list(required_capacity = 0))

  return(capacity)
}



#' Get EEP Rollout
#'
#' This function processes the counterfactual rollout data.
#'
#' @inheritParams baseline_constraint
#'
#' @returns dataframe with the scaled rollout for each technology.
#'
#' @export
get_eep_rollout <- function(data) {

  rollout <- data$counterfactual_rollout %>%
    pivot_longer(cols = !c('code', 'output_commodity'),
                 names_to = 'year',
                 values_to = 'scaled_rollout')

  return(rollout)
}



#' Set all values apart from year column to 0
#'
#' @param df, a single dataframe containing a year column
#'
#' @return dataframe with all values set to 0 for all columns apart from 'year'.
set_all_vals_to_0 <- function(df) {

  cols_to_mutate <- names(df)
  cols_to_mutate <- cols_to_mutate[cols_to_mutate != 'year']
  df[, cols_to_mutate] <- 0

  return(df)
}






#' Get Workbook for Counterfactual Results
#'
#' Produces a workbook object of the counterfactual model outputs. This function
#'  is not used by the comit app, but can be used for scripting another pipeline
#'  or for testing the counterfactual seperately from the app.
#'
#' @param solved_baseline, list provided by `comit_counterfactual_solver()`
#'  containing the problem solution and inputs.
#'
#' @returns workbook object of the model outputs. This can be written to excel
#'  using 'saveWorkbook()'.
#'
#' @export
get_counterfactual_outputs <- function(solved_baseline) {

  # get input data
  data <- solved_baseline[[2]]

  # ouptuts at cluster level
  tables <- create_output_tables(
    solved_baseline$solution,
    solved_baseline$data,
    solved_baseline$decision_variables,
    solved_baseline$PV_coefficients,
    "cluster"
  )

  # outputs at site level
  if (data$model_parameters$Output_site_data) {
    tables_sites <- create_output_tables(
      solved_baseline$solution,
      solved_baseline$data,
      solved_baseline$decision_variables,
      solved_baseline$PV_coefficients,
      "site_ID"
    )

    tables <- c(tables, tables_sites)

  }

  wb <- create_output_xlsx(tables, data)

}



