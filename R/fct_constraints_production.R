# Production constraints - making sure we make enough of each commodity

#' Formulate Production Constraints
#'
#' The production constraint ensures that the required amount of commodities are
#'  produced in each year and at each site. There are two types of constraints
#'  created here: one for demand commodities (final products) and another for
#'  process commodities (required as inputs in the process of creating final
#'  commodities). An example of a demand commodity would be *cement*, whilst an
#'  example of a process commodity would be *high temperature heat*.
#'
#' For demand commodities:
#'  ∑used_capacity(t,s,tech) = demand(t,s)
#'  for tech ∈ process<sub>d<sub>(s)
#'
#' For process commodities:
#'  ∑used_capacity(t,s,tech<sub>a</sub>) = ∑(used_capacity(t,s,tech<sub>b</sub>) *
#'  input_amount(tech<sub>b</sub>)),
#'  for any output commodity, for tech<sub>a</sub> ∈ process(s)
#'  which is not process<sub>d</sub>(s)
#'
#' @inheritParams comit_constraints
#'
#' @return list of constraints. One constraint for each commodity, at each site,
#'  at each time point.
#'  Each element contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
production <- function(data, decision_variables) {

  # Get all data required for the production constraints
  production_df <- initiate_production_constraint_data(data, decision_variables)

  production_df <- add_demand_to_production_data(production_df, data)

  production_df <- link_production_inputs_to_outputs(production_df)


  # Process demand
  process_demand_vars <- production_df %>%
    filter(!demand_technology.output)

  process_demand_vars <- formulate_process_demand_production_constraint(process_demand_vars)

  # End demand
  end_demand_vars <- production_df %>%
    filter(demand_technology.output)

  end_demand_vars <- formulate_end_demand_production_constraint(end_demand_vars)

  # Combine both demand types
  production_constraint <- c(process_demand_vars, end_demand_vars)

  return(production_constraint)
}




#' Create an initial table containing the data required for the production
#' constraints
#'
#' Links technology data to the decision variables in to create the table required
#'  to formulate the production constraints.
#'
#' @inheritParams comit_constraints
#'
#' @returns dataframe with one row per used_capacity decision variable and the
#'  following columns:
#'   * variable_index
#'   * year
#'   * site_ID
#'   * code
#'   * output_commodity
#'   * input_commodity
#'   * input_amount
#' @export
initiate_production_constraint_data <- function(data, decision_variables) {

  # get table for the output commodities of every technology
  technology_outputs <- data$Technologies %>%
    select(code, output_commodity)

  # get table for the technologies which use outputs from other technologies
  technology_inputs <- get_technologlies_that_use_outputs(data)

  # Join the technology variables onto the used_capacity decision variables
  production_df <- decision_variables %>%
    filter(variable_type == "used_capacity") %>%
    left_join(technology_outputs, by = "code", relationship = "many-to-many") %>%
    left_join(technology_inputs, by = "code", relationship = "many-to-many") %>%
    select(
      variable_index,
      year,
      site_ID,
      code,
      output_commodity,
      input_commodity,
      input_amount
    )

  return(production_df)

}



#' Get technologies that use commodities from other technologies
#'
#' Gets the technology data for technologies which use commodities produced by
#'  other technology processes. This is because we need to make sure that each
#'  technology is producing enough commodity to satisfy the inputs to other
#'  processes at the site.
#'
#' @inheritParams production
#'
#' @return dataframe with one row per technology/commodity combination for all
#'  of the technologies that use commodities produced by other technologies.
#'  There are three columns:
#'   * code - character, the technology code
#'   * input_commodity - character, the commodity used as an input
#'   * input_amount - numeric, the amount of input commodity used per unit of the
#'     technology.
#'
#' @export
get_technologlies_that_use_outputs <- function(data) {

  technology_inputs <- data$technology_input_output %>%
    filter(output < 0,
           commodity %in% data$Technologies$output_commodity) %>%
    mutate(output = -1 * output) %>%
    select(code = technology_code,
           input_commodity = commodity,
           input_amount = output)

  return(technology_inputs)
}



#' Add rows for demand data to the production data
#'
#' Add a row to the production data for the demand of each commodity at each
#'  site and in each year, taken from the site_demand data frame.
#'
#' Note that demand rows are distinguishable from decision variables by not
#'  having an variable_index number (the value will be NA instead).
#'
#' @param production_df dataframe of decision variables relevant to the production
#'  constraints as produced by `initiate_production_constraint_data()`.
#' @inheritParams production
#'
#' @returns dataframe as input as production_df, but with rows added for the
#'  demand of each commodity that each site is required to produce in each
#'  year. An additional column has also been introduced, flagging whether the
#'  output_commodity is a final (end demand) commodity or not.
#' @export
add_demand_to_production_data <- function(production_df, data) {

  # make sure there aren't any commodities with missing values
  site_demand <- data$site_demand %>%
    drop_na(output_commodity)

  # Create a dummy technology for each **end** demand process
  end_demand_rows <- site_demand %>%
    select(year,
           site_ID,
           input_commodity = output_commodity,
           input_amount = demand) %>%
    mutate(demand_technology = TRUE) # these are all end_demand commodities

  # Add the demand rows (dummy technologies) to the input df
  production_df <- production_df %>%
    mutate(
      demand_technology = output_commodity %in% site_demand$output_commodity
      # use demand_technology to indetify whether end_demand
      ) %>%
    bind_rows(end_demand_rows)

  return(production_df)
}




#' Join input commodities to output commodities
#'
#' Expand the production data so that all combinations of outputs and inputs
#' are accounted for, for each of the relevant decision variable (used_capacities).
#'
#' Columns from the output side (left) are suffixed with '.output' whilst columns from
#'  the input side (right) are suffixed with '.input'. Although note that determining
#'  each side as 'output' and 'input' is somewhat arbitrary as the data is identical
#'  - the join is to itself. What is different is the column used for joining:
#'  'output_commodity' for the left side and 'input_commodity' for the right.
#'
#' @param production_df
#'
#' @returns dataframe, with one row per combination of input and output commodities
#'  for each of the relevant decision variables (used capacities). This means
#'  that there will be more rows than there are decision variables. Contains
#'  the following columns:
#'   * variable_index.output - int, index of the output producing variable
#'   * variable_index.input - int, index of the variable providing the input
#'   * input_amount.input - numeric, amount of input commodity produced by the technology
#'   * output_commodity - character, the commodity that is output by the variable
#'   * demand_technology.output - boolean, whether the output is an end demand commodity.
#'   * site_ID
#'   * year
#' @export
link_production_inputs_to_outputs <- function(production_df) {

  # self join the input commodity to the output_commodity
  production_df <- production_df %>%
    left_join(
      x = .,
      y = .,
      by = c("output_commodity" = "input_commodity", "year", "site_ID"),
      na_matches = "never",
      relationship = "many-to-many",
      suffix = c('.output', '.input')
    )

  # remove the output of the dummy hygen technology, and reduce columns
  production_df %<>%
    drop_na(code.output) %>%
    filter(output_commodity != "HYGEN") %>%
    select(variable_index.output,
           variable_index.input,
           input_amount.input,
           output_commodity,
           demand_technology.output,
           site_ID,
           year)


  return(production_df)
}







#' Create the constraint which ensures end demand is met
#'
#' This constraint ensures each site produces the correct amount of its end
#' demand commodity (e.g. cement) in each year.
#'
#' The inequality states that in the case of an end demand process, the sum of
#' technologies has to equal the demand in that year. The demand is given in the
#' input_amount.input column, which is an incorrect name but used for consistency.
#' This column is actually taken from the site_demand data.
#'
#' @param end_demand_vars dataframe, production_df filtered to include only the
#'  technologies which produce end demand commodities directly.
#'
#' @return list of constraints. One constraint for each commodity, at each site,
#'  at each time point.
#'  Each element contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
formulate_end_demand_production_constraint <- function(end_demand_vars) {

  end_demand_production_constraint <- end_demand_vars %>%
    group_by(year, site_ID, output_commodity) %>%
    group_map(function(rows, key) {
        list(
          column_indices = unique(rows$variable_index.output),
          values = rep(1, length(unique(rows$variable_index.output))),
          direction = "==",
          rhs = unique(rows$input_amount.input) # note that this is actually demand
        )

    })

  return(end_demand_production_constraint)
}




#' Create the constraint that ensures enough process commodities are produced
#'
#' This constraint ensures that each process at a site produces the
#' correct amount of commodity for inputs to other processes, in each year.
#' This constraint requires that the output commodity produced by any process
#' at a site (other than the process producing the end demand commodity) is equal
#' to the amount that commodity is used by other technologies at a site.
#'
#' The sum of the output technologies minus the sum of technologies using that
#' output multiplied by the amount they use has to equal 0.
#'
#' @param process_demand_vars dataframe, production_df filtered to include only
#'  technologies which DO NOT produce end demand commodities directly.
#'
#' @return list of constraints. One constraint for each commodity, at each site,
#'  at each time point.
#'  Each element contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
formulate_process_demand_production_constraint <- function(process_demand_vars) {

  process_demand_production_constraint <- process_demand_vars %>%
    group_by(year, site_ID, output_commodity) %>%
    group_map(function(rows, key) {

      input_technologies <- rows %>%
        distinct(variable_index.input, input_amount.input)

      list(
        column_indices = c(
          unique(rows$variable_index.output),
          input_technologies[['variable_index.input']]
        ),
        values = c(rep(1, length(unique(rows$variable_index.output))),
                   -1 * input_technologies[["input_amount.input"]]),
        direction = "==",
        rhs = 0
      )

    })

  return(process_demand_production_constraint)
}

