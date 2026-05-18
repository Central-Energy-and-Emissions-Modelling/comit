# Functions to adjust starting values

#' Applies resource efficiency and energy efficiency measures to demand figures
#'
#' Adjust the demand values by the estimates for resource efficiency and energy
#'  efficiency in each year, for each commodity.
#'
#' @param data list of all dataframes with some initial tidying having been
#'  completed.
#'
#' @return list of all dataframes as provided as inputs, but with adjustments
#'  for resource and energy efficiency made to demand in the Demand_drivers
#'  table.
#'
#' @export
apply_energy_efficiency <- function(data) {

  # join resource efficiency and energy efficiency amounts
  data$Demand_drivers %<>%
    left_join(data$resource_efficiency, by = c("commodity", "year"))  %>%
    left_join(data$energy_efficiency, by = c("commodity", "year"))

  # make the required adjustment to demand
  data$Demand_drivers %<>%
    replace_na(list(r_efficiency = 0,
                    efficiency = 0)) %>%
    mutate(demand = demand * (1-r_efficiency) * (1-efficiency)) %>%
    select(year, commodity, demand)

  return(data)
}


#' Amend the capacity in the start year for existing technologies to ensure that
#'  demand is met
#'
#' Finds the required amount of each technology already being deployed in 2020
#'  that is needed to meet the demand of each commodity, then updates the
#'  starting capacities accordingly. The necessary adjustments are made to
#'  the Technologies dataframe and a message is sent to the console to notify
#'  the user of any changes.
#'
#' The calculation is done using linear algebra to solve a system of linear
#'  equations.
#'
#' An error is raised if there are TRUE values for objective function parameters
#'  set in the 'objective_function' tab.
#'
#' @param data list of dataframes, the input data produced from
#'  `read_excel_data_template()` after some initial tidying, including by
#'  `interpolate_data()`, `tidy()` and `round_years()`.
#'
#' @return list of dataframes as provided in the input, but with the Technologies
#'  table updated with new `existing_capacity_2020()` values.
#' @export
adjust_existing_capacity <- function(data) {

  existing_techs_matrix <- get_existing_techs_matrix(data)

  required_output <- get_required_capacities(data, existing_techs_matrix)

  # compare what is required to produce demand specified in assumptions to the
  # existing capacity in 2020
  existing_capacity <- data$Technologies %>%
    left_join(required_output,
              by = "output_commodity") %>%
    mutate(existing_output = (existing_capacity_2020
                              * availability_factor
                              * capacity_to_activity_factor))

  existing_capacity %<>%
    group_by(output_commodity) %>%
    group_modify(~ adjust_capacity(.x, .y)) %>%
    ungroup()

  # return existing capacity in the same format as the original data
  data$Technologies <- existing_capacity %>%
    mutate(existing_capacity_2020 = if_else(is.na(new_existing_capacity_2020),
                                            existing_capacity_2020,
                                            new_existing_capacity_2020)) %>%
    select(names(data$Technologies))

  # Stop script if there are no terms in the objective function
  stopifnot(sum(data$objective_function$include) > 0)

  return(data)
}



#' Produce the matrix of coefficients required to solve the existing capacity
#'  system of linear equations
#'
#' Produces the coefficients matrix (A) used in the 'Ax = b' equation that solves
#'  a system of linear equations problem for finding the required output commodities
#'  required to meet demand in the start year.
#'
#' @param rollout, dataframe, default is NULL. Optional argument to provide a
#'  dataframe for the technology rollout in a given year, used for generating
#'  the counterfactual. The operations are very similar but change slightly
#'  when this dataframe is provided, to focus on capacity in a given year rather
#'  than just initial starting capacity. The rollout dataframe should be filtered
#'  to a single period and should contain a column for the technology code (code)
#'  and a columns for scaled_output.
#'
#' @inheritParams adjust_existing_capacity
#'
#' @return matrix, the coefficient values. Rows represent the input commodities used
#'  to produce the final output commodities. Columns represent the final output
#'  commodities. The coefficient values themselves are the average output of final
#'  commodities produced by the individual commodities.
#'
#' @export
get_existing_techs_matrix <- function(data, rollout = NULL) {

  existing_techs <- get_existing_techs(data, rollout)

  # We want to turn the technology recipes into a matrix with technologies along
  # the top, commodities along the side
  existing_techs_matrix <- slam::simple_triplet_matrix(
    i = as.numeric(existing_techs$commodity),
    j = as.numeric(existing_techs$output_commodity),
    v = existing_techs$average_output,
    dimnames = list(
      commodity = levels(existing_techs$commodity),
      code = levels(existing_techs$output_commodity)
    )
  ) %>% as.matrix()

  return(existing_techs_matrix)
}



#' Create table of starting commodity data
#'
#' Gets the technologies with existing capacitiy at the beginning of the modelled
#'  period and then joins on commodity variables to then reduce down to unique
#'  input commodity and output commodity pairs. The average and total capacity
#'  of each input_commodity and output_commodity pair across the different
#'  technologies is calculated.
#'
#' @inheritParams adjust_existing_capacity
#'
#' @return dataframe of the unique input and output commidity pairs. One row per
#'  pair and columns for:
#'   * output_commodity
#'   * commodity
#'   * average_output (average output across all technologies that produce the given
#'    commodity that is used to create the output_commodity).
#'   * existing (total output of all technologies that produce the given commodity
#'    that is used to create the output_commidity).
#' @export
get_existing_techs <- function(data, rollout = NULL) {

  # get existing techs and join the input_output data
  existing_techs <- data$Technologies %>%
    {if(is.null(rollout)) filter(., existing_capacity_2020 > 0) else .} #only filter when not using rollout data

  existing_techs %<>%
    left_join(data$technology_input_output,
              by = c("code" = "technology_code"))

  # make hydrogen before and after grid the same commodity and replace all fuels
  # with a generic commodity called "fuel"
  existing_techs %<>%
    mutate(commodity = replace(commodity,
                               output < 0 & commodity == "INDMAINSHYG",
                               "HYGEN"),
           commodity = if_else(commodity %in% commodity[primary_commodity == TRUE],
                               commodity,
                               "fuel"))

  # Let's apply availability factor to output commodities
  #mutate(output = if_else(output > 0, output * availability_factor * capacity_to_activity_factor, output)) %>%


  if(is.null(rollout)) {

    # group technologies by output commodity
    existing_techs %<>%
      group_by(output_commodity) %>%
      mutate(
        total_output_commodity_capacity = sum(
          existing_capacity_2020[match(unique(code), code)]),
        grouped_output = (
          output
          * existing_capacity_2020
          / total_output_commodity_capacity)
      )

  } else {

    existing_techs %<>%
      left_join(rollout,
                by = 'code') %>%
      replace_na(list(scaled_rollout = 0)) %>%
      mutate(grouped_output = output * scaled_rollout) # not really grouped, just for consistent names

  }

  # calculate aggregate values
  existing_techs %<>%
    group_by(output_commodity, commodity) %>%
    summarise(average_output = sum(grouped_output),
              existing = sum(existing_capacity_2020), .groups = "drop")

  # add a generic fuel technology
  existing_techs %<>%
    add_row(output_commodity = "fuel",
            commodity = "fuel",
            average_output = 1,
            existing = 0) %>%
    mutate(output_commodity = as.factor(output_commodity),
           commodity = as.factor(commodity))


  return(existing_techs)
}




#' Calculate the required amount of each commodity
#'
#' @param existing_techs_matrix matrix containing the average output values for
#'  each commodity. These are the coeffecients for the system of linear
#'  equations, produced by `get_existing_techs_matrix()`.
#' @inheritParams adjust_existing_capacity
#'
#' @return dataframe with one row per commodity and two columns:
#'  * output_commodity (name of the commodity)
#'  * required_commodity (calculated required demand)
#' @export
get_required_capacities <- function(data, existing_techs_matrix) {

  #### We also need to make a demand (output) matrix. In this case a 1-d matrix

  # get commodities
  required_commodities <- get_existing_techs(data)$commodity %>% levels()

  # firstly sort start year demand and add HYGEN
  total_non_industry_H2_demand <- data$Non_industry_H2_demand %>%
    filter(year == data$model_parameters$start_year) %>%
    pull(demand) %>%
    sum()

  start_year_demand <- data$Demand_drivers %>%
    filter(year == data$model_parameters$start_year) %>%
    add_row(year = data$model_parameters$start_year,
            commodity = "HYGEN",
            demand = total_non_industry_H2_demand) %>%
    select(!year)

  output <- tibble(commodity = required_commodities) %>%
    left_join(start_year_demand,
              by = "commodity") %>%
    replace_na(list(demand = 0)) %>%
    deframe() # convert to a named vector of demand values


  # solve for amount of technology needed to be meet demand
  required_output <- solve(existing_techs_matrix, output)

  # get output into dataframe format
  required_output %<>%
    enframe(name = 'output_commodity',
            value = 'required_output')

  return(required_output)

}


#' Amends the total capacity for technologies that produce a given commodity
#' when there isn't enough existing capacity
#'
#' Updates the total capacity for technologies when there isn't enough demand
#'  for a given commodity. If there is already enough capacity, the original
#'  values are returned. Otherwise the current capacities for the relevant
#'  technologies are increased proportionally to the amount which means that
#'  total demand is met.
#'
#' Used inside `group_modify()` of `adjust_existing_capacity()`.
#'
#' @param rows subset of the original dataframe which belong to a given group.
#' @param key a single row of the dataframe belonging to the group
#'
#' @return rows as input but with the capacity updated if so required. If an
#'  update is required a message will also be sent to the console to inform the
#'  user.
#' @export
adjust_capacity <- function(rows, key) {
  # the key contains the output commodity
  # check if existing capacity in 2020 is enough to meet required output
  required_output <- rows$required_output[1]
  existing_output <- sum(rows$existing_output)

  # If we have enough capacity, return as is, with new values as NA
  if(existing_output >= required_output)
  {
    return(rows %>% mutate(new_existing_capacity_2020 = NA))
  }

  # Else, we need to adjust existing_capacity
  extra_needed_output <- required_output - existing_output
  extra_needed_output_percent <- (required_output - existing_output) / existing_output

  if(extra_needed_output_percent > 0.01)
  {
    message(sprintf("%.1f%% extra capacity required for %s. Consider adding more existing capacity in the data assumptions",
                    extra_needed_output_percent * 100,
                    key[[1]]))
  }

  # get a vector of output from one unit of capacity
  output_per_capacity <- (rows$capacity_to_activity_factor
                          * rows$availability_factor
                          * (rows$existing_capacity_2020 > 0))

  extra_capacity <- (extra_needed_output
                     / sum(output_per_capacity)
                     / output_per_capacity)

  extra_capacity[is.infinite(extra_capacity)] <- 0

  final_capacity <- rows$existing_capacity_2020 + extra_capacity

  return(rows %>%
           mutate(new_existing_capacity_2020 = final_capacity)
  )
}



#### Hydrogen ####==============================================================

#' Creates blue and green hydrogen technologies if required
#' @param data data read in from excel template
#' @param model_H2_production logical TRUE/FALSE depending on whether H2 production is modeled explicitly
#' @return list of data
create_blue_and_green_hydrogen_technologies <- function(data, model_H2_production)
{
  # check input parameters
  stopifnot(is.logical(model_H2_production),
            length(model_H2_production) == 1,
            !is.na(model_H2_production))

  if(model_H2_production) {
    # If we are modelling hydrogen production technologies, we do not need to
    # create blue and green hydrogen technologies
    return(data)
  } else {
    # If we are not modeling hydrogen production sector, we need to create blue
    # and green hydrogen technologies

    # Get all H2 consuming technologies
    H2_technology_code <- data$technology_input_output %>%
      filter(commodity == "INDMAINSHYG" & output < 0) %>%
      distinct(technology_code) %>%
      pull(technology_code)

    # We need to make changes to the Technology table and technology_input_output
    # Start with the Technology table
    data$Technologies %<>%
      # make very row a group
      group_by(1:n()) %>%
      group_modify(~ {
        # The subset of the data for the group is exposed as .x.
        if(.x$code[[1]] %in% H2_technology_code)
        {
          # First, duplicate each row
          g_b_split <- rbind(.x, .x)

          # Call the the first technology blue and the second green
          # Append a B or G to the technology code
          g_b_split$code <- paste0(g_b_split$code, c("B", "G"))
          # change the description of the technology
          g_b_split$name[1] <- gsub("hydrogen", "blue hydrogen", g_b_split$name[1], ignore.case = TRUE)
          g_b_split$name[2] <- gsub("hydrogen", "green hydrogen", g_b_split$name[2], ignore.case = TRUE)

          return(g_b_split)
        } else {
          return(.x)
        }
      }) %>%
      ungroup() %>%
      select(-`1:n()`)

    # Next apply to the technology_input_output
    data$technology_input_output %<>%
      # duplicate the technology_code column so we can use it as a grouping variable
      mutate(technology_code_2 = technology_code) %>%
      group_by(technology_code_2) %>%
      group_modify(~ {
        # The subset of the data for the group is exposed as .x.
        # The key, a tibble with exactly one row and columns for each grouping variable is exposed as .y
        if(.y[[1]] %in% H2_technology_code)
        {
          # Duplicate the rows
          g_b_split <- bind_rows(list(B = .x, G = .x), .id = "hydrogen_fuel") %>%
            mutate(technology_code = paste0(technology_code, hydrogen_fuel),
                   commodity = if_else(commodity == "INDMAINSHYG" & output < 0, paste0("INDMAINSHYG", hydrogen_fuel), commodity)) %>%
            select(-hydrogen_fuel)

          return(g_b_split)
        } else {
          return(.x)
        }
      }) %>%
      ungroup() %>%
      select(-technology_code_2)

    return(data)
  }
}
