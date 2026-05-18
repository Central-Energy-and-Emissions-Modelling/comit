# This is used only for testing!

#' Run comit solver to a given stage
#'
#' This code is a copy of [comit_solver()], with checkpoints added to allow
#' for staged running of the code for testing. When [comit_solver()] is updated
#' this function should be updated in the exact same way.
#'
#' @inheritParams comit_solver
#' @param stage integer signifying which stage of the system to run up to.
#'   Default is 9.
#'
#' @return Output of comit_solver at specified stage, as follows:
#'   * 1 - list of dataframes returned after [process_sites()].
#'   * 2 - list of dataframes returned after additional data processing.
#'   * 3 - list of dataframes returned after further data processing, including
#'     calculation of site demand.
#'   * 4 - dataframe for decision variables
#'   * 5 - dataframe for coeffecients
#'   * 6 - list of matrices for constraints
#'   * 7 - large single matrix for constraints
#'   * 8 - variable_types
#'   * 9 - list for final solved problem, returns same as [comit_solver()].
comit_staged_solver <- function(raw_data, stage = 9) {

  message("Cleaning data")

  data <- raw_data %>%
    process_sites()

  #### Checkpoint ####
  if(stage == 1) {return(data)}

  data %<>%
    interpolate_data() %>%                  # expand data tables into yearly data if required
    tidy() %>%                              # turn data into tidy data
    round_years() %>%                       # round technology start year and lifetimes to nearest multiple of 5
    adjust_for_optimism() %>%               # uplift capex costs to account for optimism in prices
    apply_energy_efficiency() %>%           # reduce demand by energy efficiency values
    adjust_existing_capacity() %>%          # if required, increase existing capacity of technologies in the base year
    add_site_ID()                           # add unique id number to each site

  #### Checkpoint ####
  if(stage == 2) {return(data)}

  # add region variable
  data$NAEI_clean %<>% assign_site_region()

  # depending on whether H2 production is being explicitly modeled or not, we need to further adjust data
  data %<>%
    # add hydrogen plants to the center of each cluster
    add_H2_plants(data$model_parameters$model_H2_production) %>%
    # create H2 conversion technologies if in COMITmode
    create_H2_conversion(data$model_parameters$model_H2_production)

  # work out demand at each site, in each year
  data$site_demand <- site_demand(data)

  # work out cheapest transport option for each site
  data$site_H2C02_transport <- site_H2C02_transport(data)

  #### Checkpoint ####
  if(stage == 3) {return(data)}

  # create table of decision variables
  message("Creating decision variables")
  decision_variables <- create_decision_variables(data)

  #### Checkpoint ####
  if(stage == 4) {return(decision_variables)}

  # temp drop new description column until new refactored code implemented
  data$objective_function %<>% select(!description)

  # Get present value coefficients of decision variables for each linear term of the objective function
  message("Calculating costs")
  present_value_functions <- c("PV_fixed_opex",
                               "PV_fuel_cost",
                               "PV_carbon_cost",
                               "PV_technology_capex",
                               "PV_H2_pipe_cluster_to_site",
                               "PV_CO2_pipe_cluster_to_site",
                               "PV_CO2_national_transport",
                               "PV_H2_pipe_national")

  PV_coefficients <-
    lapply(present_value_functions, function(x) {
      do.call(get(x),
              list(data = data, decision_variables = decision_variables))
    }) %>%
    set_names(present_value_functions) %>%
    sum_PV_coefficients(n_decision_variables = nrow(decision_variables))

  #### Checkpoint ####
  if(stage == 5) {return(PV_coefficients)}

  # get constraints
  constraint_functions <- get_constraint_functions(data)

  constraints <- lapply(constraint_functions,
                        run_constraint_function,
                        data,
                        decision_variables)

  #### Checkpoint ####
  if(stage == 6) {return(constraints)}

  constraints %<>% combine_matrix_constraints()

  #### Checkpoint ####
  if(stage == 7) {return(constraints)}


  # define variable type (continuous, integer, or binary)
  if (data$H2_plant_size$minimum_available_capacity_EndYear > 0)
  {
    variable_types <-
      if_else(substr(decision_variables$variable_type, 1, 2) == "b_",
              "B",
              "C")

    solver <- "lpsolve"
  } else {
    variable_types <- NULL # defaults to all continuous
    solver <- "symphony"
  }

  #### Checkpoint ####
  if(stage == 8) {return(variable_types)}

  # Formulate problem and Solve
  problem <- OP(objective = L_objective(PV_coefficients$coefficient),
                constraints = L_constraint(L = constraints$matr,
                                           dir = constraints$directions,
                                           rhs = constraints$rhs),
                types = variable_types)

  message("Solving problem")
  solution <- ROI_solve(problem, solver = solver, control = list(amount = 2))

  print(solution)
  solved <- list(solution = solution,
                 data = data,
                 decision_variables = decision_variables,
                 PV_coefficients = PV_coefficients)

  #### Checkpoint ####
  if(stage == 9) {return(solved)} ## this is final (and also default) stage
}
