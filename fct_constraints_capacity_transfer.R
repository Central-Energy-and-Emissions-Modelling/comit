# Formulating the capacity transfer constraint

#' Create the capacity transfer constraint
#'
#' This constrains the amount of availability of a given technology at a given
#' site, based on the new technologies added in previous years, plus the initial
#' capacity. In plain terms, it means that only the capacity that exists is available.
#'
#' available_capacity(t,s,tech) = (∑ new_capacity(t,s,tech)) for all periods
#'  t’ preceding t by the lifetime(tech) + residual_capacity(t,s,tech))
#'
#' @inheritParams comit_constraints
#'
#' @returns list of constraints. One constraint per variable for avaialable capacity.
#'  Each element contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
capacity_transfer <- function(data, decision_variables) {

  index_splits <- get_variable_index_splits(decision_variables, 600000)

  # set tables to use as lazy_dt for speed and memory savings

  decision_variables <- lazy_dt(decision_variables) %>%
    select(!c("terminal", "storage_site", "variable_name")) # drop vars to save memory

  pipes_lifetime <- data$Pipes_lifetime %>%
    pivot_longer(cols = everything(),
                 names_to = "code",
                 values_to = "lifetime")

  temp_techs <- bind_rows(data$Technologies, pipes_lifetime) %>%
    lazy_dt()

  technologies <- data$Technologies %>% lazy_dt()

  site_demand <- data$site_demand %>%
    distinct(site_ID, year, scaling_factor_within_sector) %>%
    lazy_dt()

  variable_types <- get_capacity_variable_types()

  #-----------------------------------------------------------------------------

  # initiate empty list before loop
  capacity_transfer_list <- list()

  # loop through the index splits, creating the constraint in sections
  for (indexes_to_keep in index_splits) {

    min_ind <- min(indexes_to_keep)
    max_ind <- max(indexes_to_keep)

    # Filter decision variables to available capacity only
    capacity_transfer_constraint <- initiate_capacity_transfer_dt(decision_variables,
                                                                  variable_types,
                                                                  min_ind,
                                                                  max_ind)


    capacity_transfer_constraint <- add_techs_to_capacity_transfer(
      capacity_transfer_constraint,
      temp_techs,
      site_demand,
      data
    )


    if(data$model_parameters$use_retrofit == TRUE) {

      capacity_transfer_constraint <- apply_retrofit_to_capacity_transfer(
        decision_variables,
        capacity_transfer_constraint,
        technologies,
        site_demand,
        variable_types,
        min_ind,
        max_ind
      )
    }

    capacity_transfer_constraint <- formulate_capacity_transfer_constraint(
      capacity_transfer_constraint
    )


    # combine lists so we get a single list of constraints
    capacity_transfer_list <- c(capacity_transfer_list,
                                capacity_transfer_constraint)

  }


  return(capacity_transfer_list)
}



#' Create (almost) even groups of index numbers to set constraints in groups,
#' reducing the amount of RAM required
#'
#' Allows for the setting of the constraint in n different splits in order to
#' reduce the load on RAM.
#'
#' @param max_indexes_per_group integer, the maximum number of indexes allowed
#'  in a single group. The default is 600000 which is just what worked well in
#'  development when splitting yearly data to reduce peak RAM. This can be
#'  changed to other values if more or less splits are required.
#' @inheritParams comit_constraints
#'
#' @returns list of integer vectors that are the indexes. There will be as many
#'  groups as required to ensure that max_indexes_per_group is not exceeded.
#' @export
get_variable_index_splits <- function(decision_variables,
                                      max_indexes_per_group = 600000) {

  n_decision_variables <- nrow(decision_variables)

  # calculate number of splits required
  n_splits <- ceiling(n_decision_variables / max_indexes_per_group)
  # 600000 is just what worked well in development when splitting yearly data.
  # Can be changed to whatever is required though

  index_splits <- split(decision_variables$variable_index,
                        sort(decision_variables$variable_index %% n_splits))

  return(index_splits)
}


#' Generate table of capacity variable types
#'
#' @returns data.table with the corresponding new and available variables for each
#'  capacity variable type.
#' @export
get_capacity_variable_types <- function() {
  variable_types <-
    tibble(
      new = c(
        "new_capacity",
        "H2_pipe_new_capacity",
        "H2_national_pipe_new_capacity",
        "CO2_pipe_new_capacity"
      ),
      available = c(
        "available_capacity",
        "H2_pipe_available_capacity",
        "H2_national_pipe_available_capacity",
        "CO2_pipe_available_capacity"
      )
    ) %>%
    lazy_dt()

  return(variable_types)
}


#' Initiate the data.table for the capacity transfer data
#'
#' Creates a data.table of variables for the subset of indexes of avaialble
#' variables, in order to create the capacity transfer constraint. Decision
#' variables for new capacities are joined to the decision variables for available
#' capacities, so that previous 'new' capacities can later be used to determine
#' constraint on 'available' capacity.
#'
#' @param min_ind integer, minimum of index values to include
#' @param max_ind integer, maximum of index values to include
#' @inheritParams comit_constraints
#'
#' @returns data.table containing the information required for a subset of the
#'  decision variables (those indexed between the min and max indexes).
#' @export
initiate_capacity_transfer_dt <- function(decision_variables,
                                          variable_types,
                                          min_ind,
                                          max_ind) {

  capacity_transfer_constraint <- decision_variables %>%
    filter(
      variable_index >= min_ind,
      variable_index <= max_ind,
      variable_type %in% pull(variable_types, 'available')) %>%

    # join new capacity variables for every year, we will then filter out the relevant years
    left_join(
      decision_variables %>% filter(variable_type %in% pull(variable_types, 'new')),
      by = c("site_ID", "code", "cluster", "pipe_cluster_end"),
      suffix = c(".available", ".new")
    )

  capacity_transfer_constraint %<>%
    # The available capacity variable type has to match the new capacity (i.e. CO2 pipe must match with CO2 pipe, H2 with H2 etc...)
    left_join(variable_types, by = c('variable_type.available' = 'available')) %>%
    filter(variable_type.new == new) %>%
    select(!new)


  return(capacity_transfer_constraint)

}


#' Add technology and site demand data to get the required data to set up
#' capacity transfer constraint
#'
#' Joins and processes data from technology and site_demand data.tables and
#' selects the variables required to set up the constraint.
#'
#' @param capacity_transfer_constraint - data.table created by `initiate_capacity_transfer_dt`
#' @param temp_techs - data.table for technology and pipe lifetime data
#' @param site_demand - data.table for site_demand
#' @inheritParams comit_constraints
#'
#' @returns data.table containing four columns:
#'  * variable_index.available - index of decision variables for available capacity
#'  * variable_index.new - index of decision variables for new capacity
#'  * residual_capacity.available - remaining initial capacity
#'  * int - multiplier to adjust retrofit technologies
#'
#' @export
add_techs_to_capacity_transfer <- function(capacity_transfer_constraint,
                                           temp_techs,
                                           site_demand,
                                           data) {
  # Add technology lifetimes to each new_capacity
  capacity_transfer_constraint %<>%
    mutate(
      code = case_when(
        variable_type.available == "available_capacity" ~ code,
        variable_type.available == "H2_pipe_available_capacity" |
          variable_type.available == "H2_national_pipe_available_capacity" ~ "H2Pipe_lifetime",
        variable_type.available == "CO2_pipe_available_capacity" ~ "CO2Pipe_lifetime"
      )
    ) %>%
    left_join(temp_techs, by = "code")


  # Filter new capacity variables so that the only remaining new_capacity for
  # each available_capacity is correct based on year and lifetime
  capacity_transfer_constraint %<>%
    filter(year.new <= year.available,
           year.new + lifetime > year.available) %>%

    # Add residual capacity of each technology in each year after adjusting for site scaling factor
    left_join(site_demand, by = c("site_ID", 'year.available' = 'year')) %>%
    mutate(existing_capacity_2020 = existing_capacity_2020 * scaling_factor_within_sector) %>%
    mutate(residual_capacity.available = pmax(
      0 ,
      (-existing_capacity_2020 / lifetime) * (year.available - data$model_parameters$start_year) +
        existing_capacity_2020,
      na.rm = TRUE
    ))


  # create multiplier to adjust retrofit technologies, so that their existing
  # capacities decreases at the same rate as the residual capacity
  capacity_transfer_constraint %<>%
    mutate(int = ifelse(str_detect(code, "_R"), pmax(0, (
      lifetime - (year.available - year.new)
    ) / lifetime), 1)) %>%
    # keep only variables which are needed to formulate the constraint
    select(variable_index.available,
           variable_index.new,
           residual_capacity.available,
           int)

  return(capacity_transfer_constraint)
}




apply_retrofit_to_capacity_transfer <- function(decision_variables,
                                                capacity_transfer_constraint,
                                                technologies,
                                                site_demand,
                                                variable_types,
                                                min_ind,
                                                max_ind) {

  # NEW RETROFIT CODE - subtract retrofit capacity from the existing technology capacity

  # Filter decision variables to available capacity only and select the Retrofit technologies
  capacity_transfer_constraint_R <- decision_variables %>%
    filter(variable_type %in% pull(variable_types, 'new')) %>%
    filter(str_detect(code, "_R")) %>%
    left_join(select(technologies, code, retrofit_to), by = c("code"))

  # select the corresponding Existing technologies that will be retrofitted
  capacity_transfer_constraint_E <- decision_variables %>%
    filter(variable_index >= min_ind,
           variable_index <= max_ind) %>% # filter for the index subset here too to avoid duplication!!
    filter(variable_type %in% pull(variable_types, 'available')) %>%
    filter(code %in% pull(technologies, retrofit_to)) %>%
    filter(!is.na(code)) %>%
    left_join(
      capacity_transfer_constraint_R,
      by = c("site_ID", "code" = "retrofit_to", "cluster", "pipe_cluster_end"),
      suffix = c(".available", ".new")
    ) %>%
    left_join(technologies, by = "code")


  capacity_transfer_constraint_E %<>%

    # Filter new capacity variables so that the only remaining new_capacity for each available_capacity is correct based on year and lifetime
    filter(year.new <= year.available,
           year.new + lifetime > year.available) %>%

    # Add residual capacity of each technology in each year after adjusting for site scaling factor
    left_join(site_demand, by = "site_ID") %>%
    mutate(existing_capacity_2020 = existing_capacity_2020 * scaling_factor_within_sector) %>%
    mutate(residual_capacity.available = pmax(
      0 ,
      (-existing_capacity_2020 / lifetime) * (year.available - data$model_parameters$start_year) +
        existing_capacity_2020,
      na.rm = TRUE
    )) %>%
    mutate(int = -1 * pmax(0, (lifetime - (year.available - year.new)) / lifetime)) %>%

    # keep only variables which are needed to formulate the constraint
    select(variable_index.available,
           variable_index.new,
           residual_capacity.available,
           int)

  capacity_transfer_constraint <- rbind(
    as.data.table(capacity_transfer_constraint),
    as.data.table(capacity_transfer_constraint_E)
  ) %>%
    lazy_dt()

  return(capacity_transfer_constraint)

}


#' Create the list of constraints for capacity transfer
#'
#' @param capacity_transfer_constraint data.table, containing the data required
#'  to generate the capacity transfer constraint (requires the following columns:
#'  variable_index.available, variable_index.new, int, residual_capacity.available).
#'
#' @returns list of constraints. One constraint per variable for available capacity.
#'  Each element contains a nested list with 4 elements:
#'  * column indices
#'  * values
#'  * direction
#'  * rhs
#' @export
formulate_capacity_transfer_constraint <- function(capacity_transfer_constraint) {

  these_constraints <- capacity_transfer_constraint %>%
    distinct() %>%
    group_by(variable_index.available) %>%
    group_map(function(rows, key) {
      list(
        # the key indicates the index of the available capacity variable
        column_indices = c(key[[1]], rows[["variable_index.new"]]),
        values = c(1, -1 * rows[["int"]]),
        direction = "==",
        rhs = rows[["residual_capacity.available"]][1]
      )
    })

  return(these_constraints)

}




