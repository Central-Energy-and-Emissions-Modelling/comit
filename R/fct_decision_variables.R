
# any useful objects used throughout -------------------------------------------

cols_to_keep <- c('variable_name', 'year', 'site_ID', 'cluster' = 'H2_point')

#-------------------------------------------------------------------------------


#' Generate table of decision variables
#'
#' Uses the data read in from excel spreadsheet to build a table of all decision
#' variables that form the objective function and are to be solved by the model.
#'
#' @param data list of data tables read in from excel data template, after some
#'  initial processing from preceding functions in [comit_solver()].
#'
#' @returns Table of decision variables, with one row per decision variable.
#'  Columns include:
#'
#'  * variable_index
#'  * variable_name
#'  * year
#'  * site_ID
#'  * code
#'  * cluster
#'  * variable_type (new, used or available capacity)
#'
#' @export
create_decision_variables <- function(data) {

  site_technologies_full <- get_site_technologies(data)

  # Apply filters
  site_technologies_filter <- get_site_technologies_filter(site_technologies_full)
  site_technologies <- site_technologies_full[site_technologies_filter, ]

  # Site capacity variables
  capacities <- get_capacity_variables(site_technologies)

  ## Hydrogen variables
  H2_parameters <- get_H2_parameters(data)
  model_H2_production <- H2_parameters[[1]]
  enforce_H2_plant_size <- H2_parameters[[2]]

  hydrogen_sites <- get_hydrogen_sites(data, site_technologies)

  hydrogen_sites_transport <- get_hydrogen_sites_transport(data, hydrogen_sites)

  # hydrogen variables based on conditions
  if (model_H2_production) {
    inter_cluster_hydrogen <-
      get_inter_cluster_hydrogen(data, hydrogen_sites)
  } else {
      non_industry_hydrogen <- get_non_industry_hydrogen(data)
  }

  if (model_H2_production & enforce_H2_plant_size) {
    min_H2 <- get_min_hydrogen_plant_size_variables(data)
  }

  # CCS variables
  CCS_sites <- get_CCS_sites(data, site_technologies)

  CCS_sites_transport <- get_CCS_sites_transport(data, CCS_sites)

  CCS_from_cluster_transport <-
    get_CCS_from_cluster_transport(data, CCS_sites_transport)


  # Combine all decision variables
  to_combine <-
    list(
      "capacities",
      "hydrogen_sites_transport",
      "CCS_sites_transport",
      "CCS_from_cluster_transport",
      "inter_cluster_hydrogen",
      "min_H2",
      "non_industry_hydrogen"
    )

  existing_tables <- lapply(to_combine, function(x) if(exists(x)) get(x))
  decision_variables <- combine_decision_variables(existing_tables)

  return(decision_variables)
}


#-------------------------------------------------------------------------------
## Helper functions

#' Set H2 parameters based on read data
#'
#' Also warns user when H2 parameters conflict.
#'
#' @inheritParams create_decision_variables
#'
#' @return list of Boolean values (length 2).
#'  First element contains parameter for whether to
#'  model H2 production, second contains model for whether to enforce H2 plant
#'  size.
#'  Prints warning when condition met.
#'
#' @export
get_H2_parameters <- function(data) {

  model_H2_production <- data$model_parameters$model_H2_production

  constrain_hydrogen_plant_size <- data$constraints_to_include$include[
    data$constraints_to_include$constraint == 'minimum_hydrogen_plant_size']

  enforce_H2_plant_size <- (
    constrain_hydrogen_plant_size
    & data$H2_plant_size$minimum_available_capacity_EndYear > 0
  )


  if (!model_H2_production & enforce_H2_plant_size) {
    warning("Not modeling H2 production explicitly, but minimum H2 plant size is specified")
  }

  return(list(model_H2_production, enforce_H2_plant_size))
}


#' Add additional rows to account for years in model
#'
#' @param dataframe to expand.
#' @param data list of data read in from model input file which contains model
#'  parameters.
#'
#' @return Dataframe with additional year column appended. Number of rows will be
#'  number of rows in input column * number of years to be included in the model.
#' @export
expand_df_by_model_years <- function(df, data) {

  year_list = seq(
    data$model_parameters$start_year,
    data$model_parameters$end_year,
    data$model_parameters$timestep
  )

  return(df %>% cross_join(data.frame(year = year_list)))
}


#' Produce table for site/technology combinations
#'
#' Get data for site/technology combinations as a prerequisite for creating
#' capacity variables.
#'
#' @inheritParams create_decision_variables
#'
#' @return data frame containing data for all possible combinations of sites and
#'  technologies.
#' @export
get_site_technologies <- function(data) {

  site_technologies_full <- data$NAEI_clean %>%
    left_join(
      data$Technologies,
      by = c("IPM_sector" = "sector"),
      relationship = 'many-to-many'
    ) %>%
    expand_df_by_model_years(data)

  return(site_technologies_full)
}


#' Produce filter to remove non-required rows from site_technologies table
#'
#' This is a helper function used for filtering in [get_site_technologies()].
#'
#' @param site_technologies_full dataframe containing the full set of combinations
#' of all sites, technologies and years.
#'
#' @return Large logical - TRUE when row is to be kept.
#' @export
get_site_technologies_filter <- function(site_technologies_full){

  # drop hydrogen tech vars not in cluster or pre start year
  hydrogen_rows_to_drop <- (
    site_technologies_full$technology_category == "Hydrogen"
    & (
      !site_technologies_full$in_cluster_H2
      | site_technologies_full$year < site_technologies_full$H2_first_year
    )
  )

  # drop ccs tech vars not in cluster or pre start year
  CCS_rows_to_drop <- (
    site_technologies_full$technology_category == "CCS"
    & (
      !site_technologies_full$in_cluster_CCS
      | site_technologies_full$year < site_technologies_full$CCS_first_year
    )
  )

  # combine conditions and add year filter.
  rows_to_keep <- (
    site_technologies_full$year >= site_technologies_full$start_year
    & !hydrogen_rows_to_drop
    & !CCS_rows_to_drop
    | is.na(hydrogen_rows_to_drop) # don't want to drop the NA values
    | is.na(CCS_rows_to_drop)
  )

  return(rows_to_keep)
}


#' Produce table for capacity variables
#'
#' Creates the table of all variables for capacities (new, available and used)
#'  for each site and technology combination.
#'
#' @param site_technologies data frame containing both site and technology data.
#'
#' @return data frame with `3 * nrow(site_technologies)` rows and 5 columns:
#'   * variable name
#'   * year
#'   * site ID
#'   * technology code
#'   * cluster
#'   @export
get_capacity_variables <- function(site_technologies){

  # Expand dataframe to get new, available and used capacities
  capacity_types <- c("new_capacity", "available_capacity", "used_capacity")

  capacities <- site_technologies %>%
    cross_join(data.frame(variable_name = capacity_types))

  # Get variable name in the format (t,s,tech) and reduce df
  capacities %<>%
    create_variable_name() %>%
    select(variable_name, year, site_ID, code, cluster = H2_point)

  return(capacities)
}



#' Produce hydrogen data for each site
#'
#' Generates hydrogen related variables for each combination of site and year for
#' all sites that have at least 1 year present that is after the hydrogen start
#' year at that site. This table can then be used to create the hydrogen related
#' decision variables.
#'
#' @inheritParams get_capacity_variables
#' @inheritParams create_decision_variables
#'
#' @return dataframe containing the following information for each site and year:
#'  * H2_point
#'  * hydrogen_start
#'  * lowest_cost_option.H2
#' @export
get_hydrogen_sites <- function(data, site_technologies){

  #### Hydrogen cluster to site pipe new capacity and available capacity ####

  # get hydrogen technologies with start years
  hydrogen_technologies <- data$Technologies %>%
    left_join(data$technology_input_output,
              by = c("code" = "technology_code")) %>%
    left_join(select(data$commodities, commodity, commodity_category),
              by = "commodity") %>%
    # filter for hydrogen consuming technologies
    filter(commodity_category == "Hydrogen" & output < 0) %>%
    group_by(code) %>%
    summarise(hydrogen_year = min(start_year),
              .groups = "drop")


  # Add this hydrogen information to the site data
  hydrogen_sites <- site_technologies %>%
    left_join(hydrogen_technologies, by = "code")

  # determine the earliest hydrogen date for each site
  hydrogen_sites %<>%
    replace_na(list(hydrogen_year = Inf)) %>%
    group_by(site_ID, year, H2_point) %>%
    summarise(hydrogen_start = min(hydrogen_year, na.rm = TRUE),
              .groups = "drop")

  # For each site, keep only years which are equal to or after the hydrogen start year
  hydrogen_sites %<>%
    group_by(site_ID, H2_point) %>%
    filter(year >= min(hydrogen_start)) %>%
    ungroup()

  hydrogen_sites %<>%
    left_join(select(data$site_H2C02_transport,
                     site_ID,
                     lowest_cost_option.H2),
              by = "site_ID")

  # Cheapest H2transport option: Trucking = 1, Pipeline = 2, Grid = 3

  return(hydrogen_sites)
}


#' Produce CCS data for relevant site
#'
#' Generates CCS related variables for each combination of site and year for
#' all sites that use CCS technologies. This table can then be used to create
#' the CCS related decision variables.
#'
#' @inheritParams get_capacity_variables
#' @inheritParams create_decision_variables
#'
#' @return dataframe containing the following information for each site and year for
#' sites which use CCS technologies:
#'  * H2_point
#'  * CCS_start
#'  * lowest_cost_option.C02
#' @export
get_CCS_sites <- function(data, site_technologies){

  CCS_sites <- site_technologies %>%
    filter(emissions_released < 1) %>% # remove all with no capture (1 = 100% emissions released)
    group_by(site_ID, year, H2_point) %>%
    summarise(CCS_start = min(start_year), .groups = "drop")  %>%
    left_join(select(data$site_H2C02_transport, site_ID, lowest_cost_option.CO2),
              by = "site_ID")

  return(CCS_sites)
}


#' Generate decision variable name
#'
#' Creates names for decision variables based on an original variable name indicating
#' new/used/available capacity (or technology specific), combined with year and site_ID.
#'
#' @param df dataframe containing original variable_name, year and site_ID variables.
#' @param location_var optional parameter (string) that can be used to provide the
#'  name of the variable to be used in place of site_ID. Defaul it site_ID.
#'
#' @return input df with variable_name updated to include all required info
#' @export
create_variable_name <- function(df, location_var = 'site_ID'){

  df$variable_name <- paste0(df[['variable_name']],
                             '(',
                             df[['year']],
                             ',',
                             df[[location_var]],
                             ')')

  return(df)
}




#' Create the decision variables for hydrogen transport types for sites
#'
#' Includes the variables for grid, pipes and trucks and combines them into a
#' single data frame.
#'
#' @inheritParams create_decision_variables
#' @param hydrogen_sites dataframe, output from [get_hydrogen_sites()].
#'
#' @return dataframe for decision variables regarding transport of hydrogen
#'   to specific sites.
#' @export
get_hydrogen_sites_transport <- function(data, hydrogen_sites) {
  # this version allows both pipe/trucking to persist, even when grid options are
  # selected. The model can then choose what it prefers
  # can use to see the impact this makes
  # create a boolean (TRUE/FALSE) to represent this
  use_H2_grid_sites <- data$model_parameters$use_H2_grid_site
  use_nps_for_H2_grid_sites <- data$model_parameters$use_nps_for_H2_grid_sites

  if( !use_nps_for_H2_grid_sites) {  #set to zero
    data$NAEI_clean %<>%  mutate(grid_connection_year = ifelse(grepl("npsg",data$NAEI_clean$site_name,fixed=TRUE),NA,grid_connection_year))
  }
  hydrogen_sites = hydrogen_sites %>% left_join(select(data$NAEI_clean,site_ID,grid_connection_year),by = "site_ID")  %>%
  mutate(grid_connection_year = replace_na(grid_connection_year,2060))


  if (data$model_parameters$full_transport_competition) {
    # Add variable names for Pipes
      # Add variable names for Pipes
     hydrogen_sites_pipes_and_trucks <- hydrogen_sites %>%
       cross_join(data.frame(variable_name = c("H2_pipe_new_capacity","H2_pipe_available_capacity","H2_truck_used_capacity"))
       ) %>%
       create_variable_name() %>%
       select(all_of(cols_to_keep))
   }
  else{
  hydrogen_sites_pipes_and_trucks <- hydrogen_sites %>%
    filter(lowest_cost_option.H2 == "2") %>%
    cross_join(data.frame(variable_name = c("H2_pipe_new_capacity","H2_pipe_available_capacity"))
    ) %>%
    create_variable_name() %>%
    select(all_of(cols_to_keep))


  # Add variable names for Trucking
  hydrogen_sites_truck <- hydrogen_sites  %>%
    filter(lowest_cost_option.H2 == "1") %>%
    cross_join(data.frame(variable_name = c("H2_truck_used_capacity"))) %>%
    create_variable_name() %>%
    select(all_of(cols_to_keep))

  hydrogen_sites_pipes_and_trucks = rbind(hydrogen_sites_pipes_and_trucks,hydrogen_sites_truck)
  }



  # Add variable names for Grid
  hydrogen_sites_grid <- hydrogen_sites %>%
    {if(use_H2_grid_sites) filter(.,  year >= grid_connection_year)
      else filter(., lowest_cost_option.H2 == "3")} %>%
    cross_join(data.frame(variable_name = c("H2_grid_used_capacity"))) %>%
    create_variable_name() %>%
    select(all_of(cols_to_keep))


  hydrogen_sites_transport <- rbind(hydrogen_sites_grid,
                                    hydrogen_sites_pipes_and_trucks)

  return(hydrogen_sites_transport)
}


#' Create the decision variables for CCS transport types for sites
#'
#' Includes the variables for pipes and trucks and combines them into a
#' single data frame.
#'
#' @inheritParams create_decision_variables
#' @param CCS_sites dataframe, output from [get_CCS_sites()].
#'
#' @return dataframe for decision variables regarding transport of CCS
#'   at specific sites.
#' @export
get_CCS_sites_transport <- function(data, CCS_sites) {

  CCS_sites_pipes <- CCS_sites %>%
    filter(lowest_cost_option.CO2 == "2")  %>%
    cross_join(data.frame(variable_name = c("CO2_pipe_new_capacity",
                                            "CO2_pipe_available_capacity"))) %>%
    create_variable_name() %>%
    select(all_of(cols_to_keep))

  CCS_sites_truck <- CCS_sites %>%
    filter(lowest_cost_option.CO2 == "1")  %>%
    cross_join(data.frame(variable_name = c("CO2_truck_used_capacity"))) %>%
    create_variable_name() %>%
    select(all_of(cols_to_keep))

  CCS_sites_transport <- rbind(CCS_sites_pipes, CCS_sites_truck)

  return(CCS_sites_transport)
}



#' Create the decision variables for hydrogen transport between clusters
#'
#' Includes the variables for both new and available capacity as well as H2
#' outflows.
#'
#' @inheritParams get_hydrogen_sites_transport
#'
#' @return dataframe for decision variables regarding transport of hydrogen
#'   between clusters.
#' @export
get_inter_cluster_hydrogen <- function(data, hydrogen_sites) {

  # pipes need to be available to build at earliest start year of hydrogen or non-industry hydrogen demand
  earliest_H2 <- min(
    hydrogen_sites$year,
    data$Non_industry_H2_demand %>% filter(demand > 0) %>% pull(year)
  )

  # create new capacity and available capacity variables.
  # filter unique combination of cluster connections
  H2_cluster_to_cluster_capacities <- data$Cluster_connections %>%
    filter(allowed_route == TRUE) %>%
    mutate(combination = paste0(
      pmin(cluster_1, cluster_2),
      ", ",
      pmax(cluster_1, cluster_2)
    )) %>%
    distinct(combination, .keep_all = TRUE)

  H2_cluster_to_cluster_capacities %<>%
    expand_df_by_model_years(data) %>%
    filter(year >= earliest_H2) %>%
    cross_join(data.frame(
      variable_name = c("H2_national_pipe_new_capacity",
                        "H2_national_pipe_available_capacity"))) %>%
    create_variable_name('combination') %>%
    select(
      variable_name,
      year,
      cluster = cluster_1,
      pipe_cluster_end = cluster_2)

  # Create outflows variables
  H2_outflows <- data$Cluster_connections %>%
    # These connections only go one way, i.e. from point A to B, we also want
    # to include B to A
    add_row(
      cluster_1 = .$cluster_2,
      cluster_2 = .$cluster_1,
      allowed_route = .$allowed_route
    ) %>%
    distinct() %>%
    filter(allowed_route == TRUE)

  # Create a new column which just repeats each model year, and then a column for new and available capacity
  H2_outflows %<>%
    expand_df_by_model_years(data) %>%
    filter(year >= earliest_H2) %>%
    mutate(variable_name = 'H2_outflow',
           location_var = paste0(cluster_1, ',', cluster_2)) %>%
    create_variable_name('location_var') %>%
    select(
      variable_name,
      year,
      cluster = cluster_1,
      pipe_cluster_end = cluster_2
    )


  inter_cluster_hydrogen <- rbind(H2_cluster_to_cluster_capacities,
                                  H2_outflows)

  return(inter_cluster_hydrogen)
}


#' Create the decision variables for non industrial hydrogen demand
#'
#' @inheritParams create_decision_variables
#'
#' @return dataframe for decision variables regarding transport of hydrogen
#'   between clusters.
#' @export
get_non_industry_hydrogen <- function(data) {

  non_industry_hydrogen <- data$Non_industry_H2_demand %>%
    # add a blue and green group to each row
    # duplicate each row twice
    slice(rep(1:n(), each = 2)) %>%
    group_by_all() %>%
    mutate(code = c("INDMAINSHYGB", "INDMAINSHYGG")) %>%
    ungroup() %>%
    mutate(variable_name = "non_industry_H2") %>%
    create_variable_name(location_var = 'cluster') %>%
    select(-demand)

  return(non_industry_hydrogen)
}


#### CO2 cluster to storage site transport variable ####
# To create national CO2 transport from cluster to storage site variables,
# determine the earliest CCS start date of each cluster

#' Create the decision variables for transport of CO2 from cluster to storage site
#'
#' @param CCS_sites_transport dataframe created by [`get_CCS_sites_transport()`]
#' @inheritParams create_decision_variables
#'
#' @return dataframe for decision variables regarding transport of C02 out of
#'   clusters to carbon storage sites.
#' @export
get_CCS_from_cluster_transport <- function(data, CCS_sites_transport) {

  earliest_CCS <- CCS_sites_transport %>%
    group_by(cluster) %>%
    summarise(earliest_start = min(year), .groups = "drop")

  CCS_from_cluster_transport <- data$`CO2_T&S_cost` %>%
    expand_df_by_model_years(data) %>%
    left_join(earliest_CCS, by = "cluster") %>%
    filter(year >= earliest_start) %>%
    # now get variable name
    mutate(variable_name = 'CO2_transported',
           location_var = paste0(cluster, ',', terminal, ',', storage_site)) %>%
    create_variable_name('location_var') %>%
    select(variable_name, year, cluster, terminal, storage_site)

  return(CCS_from_cluster_transport)
}


#### Minimum H2 plant size binary variables ####

#' Create binary variables to enforce minimum H2 plant size
#'
#' Creates a binary variable (0, 1) for each hydrogen producing
# technology at each site.
#'
#' @inheritParams create_decision_variables
#'
#' @return dataframe containing the decision variables for the minimum H2 plant
#' size.
#'
#' @export
get_min_hydrogen_plant_size_variables <- function(data) {

  # get hydrogen producing tech and join all to every site with hydrogen sector
  min_H2 <- data$technology_input_output %>%
    filter(primary_commodity == TRUE & commodity == "HYGEN") %>%
    cross_join(filter(data$NAEI_clean, IPM_sector == "Hydrogen"))

  # create variable names
  min_H2 %<>%
    mutate(year = data$model_parameters$end_year,
           variable_name = "b_H2_available_capacity",
           location_var = paste0(site_ID, ',', technology_code)) %>%
    create_variable_name('location_var') %>%
    select(all_of(cols_to_keep), code = technology_code)

  return(min_H2)
}



#' Append all decision variable tables to create a single final table
#'
#' @param existing_tables a list of all decision variable tables that exist in
#'  given scenario. Tables that don't exist in this scenario should be included
#'  as NULL values in the list.
#'
#' @return dataframe containing the final decision variables. See
#' [create_decision_variables()] for more information.
#' @export
combine_decision_variables <- function(existing_tables) {

  # remove any null elements
  existing_tables <- existing_tables[sapply(existing_tables, is.data.frame)]

  # bind and add additional columns
  decision_variables <- bind_rows(existing_tables) %>%
    mutate(variable_type = sub("\\(.*", "", variable_name)) %>%  # pull type from name
    rowid_to_column("variable_index") # Create a variable index column

  # add pipe_cluster_end column if it doesn't exist
  if(!("pipe_cluster_end" %in% colnames(decision_variables))) {
    decision_variables$pipe_cluster_end <- NA
  }

  return(decision_variables)
}
