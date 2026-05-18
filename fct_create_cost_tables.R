

#' Generate table of cost outputs
#'
#' Uses the model solution to construct a table of all costs incurred,
#' used to populate the 'Costs' tab of the Excel output workbook.
#'
#' @param solved a table containing the model solution, decision variables and
#' PV_coefficients
#' @param model_data list of data tables read in from excel data template
#' @param site_cluster variable indicating if site or cluster level data is to
#' be outputted.
#'
#' @returns table of cost outputs with one row per cost type
#'
#' @export
#'
create_cost_tables <- function(solved, model_data, site_cluster) {
  comit_tic(sprintf('Total for creating cost tables (%s)', site_cluster))

  H2_capacity_types = c(
    "H2_pipe_new_capacity",
    "H2_pipe_available_capacity",
    "H2_truck_used_capacity",
    "H2_grid_used_capacity"
  )

  CO2_capacity_types = c(
    "CO2_pipe_new_capacity",
    "CO2_pipe_available_capacity",
    "CO2_truck_used_capacity"
  )

  # add technology, commodity and site information to the solved data table
  solved_data  <- solved %>%
    left_join(model_data$Technologies, by = "code")  %>%
    left_join(
      select(model_data$commodities, commodity, description),
      by = c("output_commodity" = "commodity")
    ) %>%
    left_join(
      select(
        model_data$NAEI_clean_new,
        site_ID,
        cluster_rad,
        traded_site,
        Traded_NonTraded
      ),
      by = "site_ID"
    )

  costs_technology <- calculate_costs_technology(solved_data, model_data, site_cluster)

  costs_H2_site2cluster <- calculate_costs_site2cluster(
    solved_data,
    model_data,
    site_cluster,
    H2_capacity_types,
    "PV_H2_pipe_cluster_to_site",
    model_data$Pipes_lifetime$H2Pipe_lifetime,
    "H2_S2C"
  )

  costs_CO2_site2cluster = calculate_costs_site2cluster(
    solved_data,
    model_data,
    site_cluster,
    CO2_capacity_types,
    "PV_CO2_pipe_cluster_to_site",
    model_data$Pipes_lifetime$CO2Pipe_lifetime,
    "CO2_S2C"
  )

  costs_CO2_cluster2storage <- calculate_costs_CO2_cluster2storage(solved_data, model_data)

  ##H2 cluster to cluster pipe costs when there is H2 production
  if (model_data$model_parameters$model_H2_production)
  {
    costs_H2_cluster2cluster = calculate_costs_H2_cluster2cluster(solved_data, model_data, site_cluster)
  }
  else {
    costs_H2_cluster2cluster = NULL
  }


  # Combine all cost tables
  cost = bind_rows(
    costs_technology,
    costs_H2_site2cluster,
    costs_H2_cluster2cluster,
    costs_CO2_site2cluster,
    costs_CO2_cluster2storage
  )

  cost %<>%
    mutate(across(starts_with("2"), ~ replace_na(.x, 0))) %>%
    select(
      Sector_infrastructure,
      site_cluster,
      cluster_rad,
      Storage_site = storage_site,
      Primary_output,
      Output_description = description,
      Technology_code,
      Technology_description,
      Technology_category,
      Traded_NonTraded,
      Cost_type = PV_term,
      starts_with("2"),
      Sector_group
    )

  comit_toc()

  return(cost)

}




#' Calculates the technology costs incurred. Used to calculate the
#' non-infrastructure costs in the cost tabs of the Excel output workbook
#'
#' @param solved_data a table containing the model solution, decision variables and
#' PV_coefficients
#' @param model_data list of data tables read in from excel data template
#' @param site_cluster variable indicating if site or cluster level data is to
#' be outputted.
#'
#' @returns table of technology costs with one row per cost type
#'
#' @export
calculate_costs_technology <- function(solved_data, model_data, site_cluster) {

  comit_tic("calculate_cost_technology")

  english_names <- c(
    "PV_fixed_opex" = "Opex",
    "PV_fuel_cost" = "Fuel cost",
    "PV_carbon_cost" = "Carbon cost",
    "PV_technology_capex" = "Capex",
    "traded" = "Traded",
    "untraded" = "NonTraded"
  )

  group_cols <- c(
    "sector",
    "year",
    "output_commodity",
    "description",
    "code",
    "name",
    "technology_category",
    "PV_term",
    "cluster_rad",
    "Traded_NonTraded",
    site_cluster
  )

  capacity_types <- c("new_capacity", "available_capacity", "used_capacity")

  costs_technology <- solved_data %>%
    filter(variable_type %in% capacity_types) %>%
    filter(PV_term %in% names(english_names)) %>%
    mutate(PV_term = english_names[PV_term])

  ## Create Capex_lump costs (capex without borrowing, spreading,  discounting) - new capacity * capex
  costs_technology_capex_lump <- costs_technology %>%
    filter(PV_term == "Capex", variable_type == "new_capacity") %>%
    mutate(PV_term = "Capex_lump",
           cost = solution * capex)

  #and rbind into costs_technology dataframe
  costs_technology <- bind_rows(costs_technology, costs_technology_capex_lump)

  ## Capex adjustment - un-discounted yearly spread cost
  costs_capex  <- technology_cost_capex_adjustment(costs_technology, model_data)

  ## join back undiscounted  capex into main cost dataframe
  costs_technology <- costs_technology %>%
    left_join(costs_capex, by = c("site_ID", "year", "code")) %>%
    mutate(cost = ifelse(PV_term == "Capex" & variable_type == "new_capacity",
                         capex_costs,
                         cost))

  costs_technology %<>%
    undiscounted_yearly_cost(model_data, "new_capacity")

  costs_technology %<>%
    aggregate_cost_table(group_cols, "Sector")

  costs_technology %<>%
    rename(
      Primary_output = output_commodity,
      Technology_code = code,
      Technology_description = name,
      Technology_category = technology_category
    ) %>%
    mutate(
      Sector_group = case_when(
        Sector_infrastructure == "Refineries" ~ "Refineries",
        Sector_infrastructure == "Hydrogen" ~ "Hydrogen",
        TRUE ~ "Industry"
      )
    )

  comit_toc()

  return(costs_technology)
}
## _____________________________________________________________



#' Calculates the technology costs incurred. Used to calculate the
#' non-infrastructure costs in the cost tabs of the Excel output workbook
#'
#' @param cost_data a table containing the table of costs to aggregate over
#' @param group_cols  list of variables to aggregate (summarise) over
#' @param Sector_infrastructure_label string to label the type of cost
#' ("Sector" for technology costs or "H2_S2C","CO2_S2C", or "CO2_C2S" for infrastructure costs)
#'
#' @returns table of aggregated costs
#'
#' @export
aggregate_cost_table <- function(cost_data,
                                 group_cols,
                                 Sector_infrastructure_label) {

  pipes <- c("H2_S2C", "CO2_S2C")

  cost_data %<>%
    group_by(across(all_of(group_cols))) %>%
    summarise(cost = sum(cost), .groups = "drop") %>%
    pivot_wider(names_from = year, values_from = cost) %>%
    mutate(Sector_infrastructure = Sector_infrastructure_label)

  if (Sector_infrastructure_label == "Sector") {
    cost_data %<>%
      mutate(Sector_infrastructure = sector)  %>%
      select(-sector)
  }
  if (Sector_infrastructure_label %in% pipes) {
    cost_data %<>%
      select(-Sector_infrastructure) %>%
      rename(Sector_infrastructure = variable_type)

  }
  return(cost_data)
}

## _____________________________________________________________

#' Adjusts the capex costs for new technological capacity
#' to un-discounted yearly spread costs
#'
#' @param cost_data a table containing the technology costs
#' @param model_data list of data tables read in from excel data template
#'
#' @returns table of capex_costs for each site, technology and year
#' combination (4 columns)
#'
#' @export
technology_cost_capex_adjustment <- function(cost_data,
                                             model_data) {

  costs_capex <- cost_data %>%
    filter(PV_term == "Capex", variable_type == "new_capacity") %>%
    yearly_cost_calc(model_data)

  costs_capex %<>%
    add_capex_adjustment(model_data, "tech")

  return(costs_capex)
}


#' Converts technology capex costs into yearly payments to be made over the loan
#' period (technology lifetime) using the PMT function
#'
#' @param cost_data a table containing the capex costs to calculate the yearly
#' payments for
#' @param model_data list of data tables read in from excel data template
#'
#' @returns table of cost data with the following two columns added:
#'  * year_cost: the capex cost split into yearly payments over the loan period
#'  * end_year_loan: the final loan year
#'
#' @export
yearly_cost_calc <- function(cost_data,model_data) {

  cost_data %<>%
    mutate(year_cost = PMT(capex, model_data$rates$interest, lifetime), # calculate cost at every year
           end_year_loan = year + lifetime -1 )

  return(cost_data)

}


#' Un-discounts opex / carbon cost / fuel cost from 5 year aggregate into yearly
#' costs
#'
#' @param cost_data a table containing the cost data to be undiscounted
#' @param model_data list of data tables read in from excel data template
#' @param new_capacity_type the new_capacity variable type to filter (pipe or tech)
#' @returns table of cost data with the following two columns added:
#'  * year_cost: the capex cost split into yearly payments over the loan period
#'  * end_year_loan: the final loan year
#'
#' @export
undiscounted_yearly_cost <- function(cost_data, model_data, new_capacity_type) {

  cost_data %<>%
    mutate(PV = present_value_quick(1,
                                    rate = model_data$rates$discount,
                                    start_period = year - model_data$model_parameters$start_year,
                                    n_periods = model_data$model_parameters$timestep),
           cost = case_when(
             PV_term != "Capex" & variable_type != new_capacity_type ~ cost / PV,
             TRUE ~ cost
           )) %>%
    select(!PV)

  return(cost_data)
}




#' Generate table cost data for either H2 or CO2 pipes (and/or H2/CO2 trucking/grid transport)
#'
#' @param solved a table containing the model solution, decision variables and
#' PV_coefficients
#' @param model_data list of data tables read in from excel data template
#' @param site_cluster variable indicating if site or cluster level data is to
#' be outputted.
#' @param capacity_types the variable types to filter for from the model solution
#' @param pipe_PV_term the PV term to filter for from the model output (e.g.PV_H2_pipe_cluster_to_site
#' for hydrogen pipes or PV_CO2_pipe_cluster_to_site for CO2 pipes)
#' @param pipes_lifetime the pipe lifetime (years)
#' @param cost_type_name string used to label the cost rows as site to cluster costs (e.g. H2_SCS or CO2_SCS)
#'
#' @returns table of cost data for either H2 or CO2 pipes (and/or H2/CO2 trucking/grid transport)
#'
#' @export
calculate_costs_site2cluster <- function(solved,
                                         model_data,
                                         site_cluster,
                                         capacity_types,
                                         pipe_PV_term,
                                         pipes_lifetime,
                                         cost_type_name) {
  comit_tic('calculate_costs_site2cluster')

  group_cols_pipes <- c(site_cluster,
                        "PV_term",
                        "year",
                        "cluster_rad",
                        "variable_type")
  costs_site2cluster <- solved %>%
    filter(variable_type %in% capacity_types) %>%
    filter(PV_term == pipe_PV_term)

  ## Append capex_lump (capex without borrowing, spreading,  discounting)
  costs_site2cluster  = add_pipe_capex_lump(capacity_types[1],
                                            costs_site2cluster,
                                            model_data,
                                            pipes_lifetime)

  ## CAPEX ADJUSTMENT
  costs_site2cluster = capex_adjustment_pipe(capacity_types[1],
                                             costs_site2cluster,
                                             model_data,
                                             pipes_lifetime) %>%
    # un-discount opex from 5 year aggregate into yearly cost
    undiscounted_yearly_cost(model_data, capacity_types[1])  %>%
    add_PVterm_labels(capacity_types[1]) %>% # rename PV_term labels for grouping
    aggregate_cost_table(group_cols_pipes, cost_type_name)

  comit_toc()

  return(costs_site2cluster)

}


## _____________________________________________________________

#' Produces the "CO2_C2S" rows in the cost tab of the Excel output workbook
#'  (costs for transporting CO2 from clusters to terminals)
#'
#' @param solved a table containing the model solution, decision variables and
#' PV_coefficients
#' @param model_data list of data tables read in from excel data template

#' @returns table of cost data for transport of CO2 from clusters to storage sites
#'
#' @export
calculate_costs_CO2_cluster2storage <- function(solved, model_data) {

  comit_tic("calculate_costs_CO2_cluster2storage")

  group_cols <- c("cluster", "storage_site", "year")

  costs_CO2_cluster2storage <- solved %>%
    filter(variable_type == "CO2_transported") %>%
    filter(PV_term == "PV_CO2_national_transport")

  costs_CO2_cluster2storage = costs_CO2_cluster2storage %>%
    # un-discount opex from 5 year aggregate into yearly cost
    undiscounted_yearly_cost(model_data,"CO2_transported")  %>%
    aggregate_cost_table(group_cols,"CO2_C2S")   %>%
    replace(is.na(.), 0)

  comit_toc()

  return(costs_CO2_cluster2storage)
}


#' CAPEX ADJUSTMENT - undiscounted yearly spread cost
#'
#' @param capacity_variable variable type to filter for (H2 or CO2 pipes)
#' @param cost_data a table containing the pipe costs
#' @param model_data list of data tables read in from excel data template
#' @param pipes_lifetime lifetime for either H2 or CO2 pipes.
#'
#' @returns table of costs with capex costs adjusted
capex_adjustment_pipe <- function(capacity_variable,
                                  cost_data,
                                  model_data,
                                  pipes_lifetime) {
  costs_site2cluster_capex = cost_data %>%
    filter(PV_term != "Capex_lump" & variable_type == capacity_variable)

  # temporary fix to ensure code works with no H2 pipe costs. Will need to improve later.
  if(nrow(costs_site2cluster_capex) >0) {
    # the number of payments that the system actually has to pay is only up to 2050
    costs_site2cluster_capex = costs_site2cluster_capex %>% mutate(loan_periods = pmin(year + pipes_lifetime - 1, model_data$model_parameters$end_year) - year + 1) %>%

      # will need to adjust the coefficient to remove the discounting and aggregation part used for calculating present value
      mutate(year_cost = cost /
               present_value(1,
                             rate = model_data$rates$discount,
                             start_period = year - model_data$model_parameters$start_year,
                             n_periods = loan_periods)
             # dividing the discounting aggregates to calculate yearly cost before discounting
             ,  end_year_loan = year + pipes_lifetime - 1) %>%

      add_capex_adjustment(model_data, "pipe")

    # incorporate into the cost data table
    cost_data <- cost_data %>%
      left_join(costs_site2cluster_capex, by = c("site_ID", "year"))

    cost_data = cost_data %>%
      mutate(
        cost = ifelse(
          PV_term != "Capex_lump" &
            variable_type == capacity_variable,
          capex_costs,
          cost
        )
      )
  }
  return(cost_data)
}



#' Adds rows for capex_lump costs ('lumpy' yearly investment cost figures  without
#'  borrowing, spreading or  discounting) for pipes to a table of pipe costs
#'
#' @param capacity_variable variable type to filter for (H2 or CO2 pipes)
#' @param cost_data a table containing the pipe cost data
#' @param model_data list of data tables read in from excel data template
#' @param pipes_lifetime pipe lifetime
#'
#' @returns table of cost data with capex_lump rows added
#'
#' @export
add_pipe_capex_lump <- function(capacity_variable,cost_data,model_data,pipes_lifetime) {

  ## Append capex_lump (capex without borrowing, spreading,  discounting)
  if (nrow(cost_data)>0) {
    cost_data_capex_lump <- cost_data %>%
      filter(variable_type == capacity_variable)
    if (nrow(cost_data_capex_lump)>0) {

      cost_data_capex_lump <- cost_data_capex_lump %>%
        mutate(PV_term = "Capex_lump",
               cost = cost / present_value(1,
                                           rate = model_data$rates$discount,
                                           start_period = year - model_data$model_parameters$start_year,
                                           n_periods = pmin(year + pipes_lifetime - 1, model_data$model_parameters$end_year) - year + 1)
               / PMT(1, model_data$rates$interest, pipes_lifetime))


      #and rbind into costs_technology dataframe
      cost_data <- bind_rows (cost_data, cost_data_capex_lump)

    }

  }
  return(cost_data)

}


#' Renames the PV_term label: capex for new_capacity, opex for
#'  available_capacity variables, capex_lump for capex_lump
#'
#' @param cost_data a table containing the pipe cost data
#' @returns table of cost data with updated PV_term labels
#'
#' @export
add_PVterm_labels <- function(cost_data,capacity_type) {

  cost_data %<>%
    mutate(PV_term = ifelse(
      PV_term == "Capex_lump",
      "Capex_lump",
      if_else(variable_type == capacity_type, "Capex", "Opex")
    ))

}

#' Renames the PV_term label: capex for new_capacity, opex for
#'  available_capacity variables, capex_lump for capex_lump
#'
#' @param costs_capex a table containing the capex cost data
#' @param model_data list of data tables read in from excel data template
#' @param capex_type string indicating if this is a technology or pipe cost dataset
#'
#' @returns table of capex cost data with adjusted (spread) capex costs incorporated
#'
#' @export
add_capex_adjustment <- function(costs_capex,
                                 model_data,
                                 capex_type) {

  year_list <- seq(
    from = model_data$model_parameters$start_year,
    to = model_data$model_parameters$end_year,
    by = model_data$model_parameters$timestep
  )

  years_df <- matrix(ncol = length(year_list)) %>%
    as.data.frame() %>%
    setNames(year_list)


  # add in columns for every year
  costs_capex <- costs_capex %>%
    cbind(years_df) %>%
    pivot_longer(
      cols = as.character(model_data$model_parameters$start_year):as.character(model_data$model_parameters$end_year),
      names_to = "loan_year",
      values_to = "costs_incured"
    )

  #check if loan is to be applied to year
  costs_capex %<>%
    mutate(
      loan_year = as.numeric(loan_year),
      loan_year_bool = (loan_year <= end_year_loan) & (loan_year >= year))


  # check whether to apply technology or infrastructure adjustment
  if(capex_type == 'tech') {

    costs_capex %<>%
      mutate(
        costs_incured = ifelse(loan_year_bool, year_cost * solution, 0)
      )

  } else {

    costs_capex %<>%
      mutate(
        costs_incured = ifelse(loan_year_bool, year_cost, 0)
      )

  }

  costs_capex %<>%
    group_by(site_ID, code, loan_year) %>% # group costs by loan year
    summarise(capex_costs = sum(costs_incured), .groups = 'drop') %>%
    rename(year = loan_year) %>%
    mutate(year = as.numeric(year))

  return(costs_capex)
}



## Below is not refactored yet -------------------------------------------------

# H2 production functions


calculate_costs_H2_cluster2cluster <- function(solved, model_data) {
  group_cols <- c("cluster", "PV_term", "year")
  costs_H2_cluster2cluster <- solved %>%
    filter(variable_type %in% c("H2_national_pipe_new_capacity", "H2_national_pipe_available_capacity")) %>%
    filter(PV_term == "PV_H2_pipe_national")

  ## Append capex_lump (capex without borrowing, spreading,  discounting)

  costs_H2_cluster2cluster_capex_lump <- costs_H2_cluster2cluster %>%
    filter(variable_type == "H2_national_pipe_new_capacity") %>%
    mutate(PV_term = "Capex_lump",
           cost = cost / present_value(1,  rate = model_data$rates$discount,  start_period = year - model_data$model_parameters$start_year, n_periods = pmin(year + model_data$Pipes_lifetime$H2Pipe_lifetime - 1, model_data$model_parameters$end_year) - year + 1)
           / PMT(1, model_data$rates$interest, model_data$Pipes_lifetime$H2Pipe_lifetime))

  #and rbind into costs_technology dataframe
  costs_H2_cluster2cluster <- bind_rows(costs_H2_cluster2cluster, costs_H2_cluster2cluster_capex_lump)



  ## CAPEX ADJUSTMENT
  costs_H2_cluster2cluster_capex <-  costs_H2_cluster2cluster %>%
    filter(PV_term != "Capex_lump" & variable_type == "H2_national_pipe_new_capacity") %>%
    # the number of payments that the system actually has to pay is only up to 2050
    mutate(loan_periods = pmin(year + model_data$Pipes_lifetime$H2Pipe_lifetime - 1, model_data$model_parameters$end_year) - year + 1) %>%

    # will need to adjust the coefficient to remove the discounting and aggregation part used for calculating present value

    mutate(year_cost = cost /
             present_value(1, rate = model_data$rates$discount, start_period = year - model_data$model_parameters$start_year, n_periods = loan_periods)
           # dividing the discounting factor for the time interval period to calculate yearly cost before discounting
           ,  end_year_loan = year + model_data$Pipes_lifetime$H2Pipe_lifetime - 1) %>%

    # add in columns for every year
    cbind(setNames(data.frame(matrix(ncol = length(seq(model_data$model_parameters$start_year, model_data$model_parameters$end_year, model_data$model_parameters$timestep)))),
                   as.character(seq(model_data$model_parameters$start_year, model_data$model_parameters$end_year, model_data$model_parameters$timestep)))) %>%

    pivot_longer(cols = as.character(model_data$model_parameters$start_year): as.character(model_data$model_parameters$end_year),
                 names_to = "loan_year",
                 values_to = "cost_incured") %>%

    #check if loan is to be applied to year
    mutate(costs_incured = ifelse(loan_year <= end_year_loan & loan_year >=year , year_cost, 0)) %>%
    group_by(variable_index, loan_year) %>%  # group costs by loan year
    summarise(capex_costs = sum(costs_incured))%>%
    rename(year = loan_year) %>%
    mutate( year = as.numeric(year))


  # join back into data
  costs_H2_cluster2cluster <- costs_H2_cluster2cluster %>%
    left_join(costs_H2_cluster2cluster_capex, by= c("variable_index", "year")) %>%
    mutate(cost = ifelse(PV_term != "Capex_lump" & variable_type == "H2_national_pipe_new_capacity", capex_costs, cost)) %>%

    # un-discount opex from 5 year aggregate into yearly cost
    mutate(cost = ifelse(variable_type != "H2_pipe_new_capacity"
                         , cost /present_value(1,  rate = model_data$rates$discount,  start_period = year - model_data$model_parameters$start_year, n_periods = model_data$model_parameters$timestep)
                         , cost)) %>%

    # we know that H2_site_new_capacity is related only to capex and H2_site_available_capacity to opex
    mutate(PV_term = ifelse(PV_term == "Capex_lump", "Capex_lump", if_else(variable_type == "H2_national_pipe_new_capacity", "Capex", "Opex"))) %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(cost = sum(cost), .groups = "drop") %>%
    pivot_wider(names_from = year, values_from = cost) %>%
    mutate(Sector_infrastructure = "H2_C2C")

  return(costs_H2_cluster2cluster)
}


#### Re-adjust Costs tab ####==================================================



adjust_cost_tables <- function(cost, energy, site_cluster,
                               total_H2_energy, model_data) {

  H2_extra_unit_cost <- cost %>%
    filter(Sector_infrastructure == "hydrogen_conversion")

  # only perform these calculations if there is hydrogen, to avoid an error
  # If no hydrogen conversion, there is no extra cost that needs to be added
  if (nrow(H2_extra_unit_cost) > 0) {

    H2_extra_cost <- calculate_H2_extra_cost(H2_extra_unit_cost,
                                             total_H2_energy,
                                             site_cluster)

    cost <- adjust_H2_cost(cost, H2_extra_cost, site_cluster)

  }

  cost = adjust_price_year(cost, model_data)

  return(cost)

}



adjust_price_year <- function(cost, model_data) {
  # Set prices to base year price
  price_cols <- colnames(cost)[colnames(cost) %in% as.character(2000:2200)]
  cost[price_cols] <- lapply(cost[price_cols],
                             base_year_adjustment,
                             parameter_data = model_data)
  return(cost)
}


#' Adds rows for capex_lump costs ('lumpy' yearly investment cost figures  without
#'  borrowing, spreading or  discounting) for pipes to a table of pipe costs
#'
#' @param H2_extra_unit_cost a table containing the hydrogen_conversion rows
#' from the cost table data (i.e. output from [create_cost_tables])
#' @param total_H2_energy list of data tables read in from excel data template
#' @param site_cluster pipe lifetime
#'
#' @returns table of cost data with H2 extra cost for each combination of
#' for each site, year, cost_type and technology
#'
#' year
#' TWh
#' sector
#'
#' H2_extra_unit_cost
#' Cost_type
#' 2xxx
#'
#'
#' @export
calculate_H2_extra_cost <- function(H2_extra_unit_cost,total_H2_energy,site_cluster) {

  total_H2_energy_per_year <- total_H2_energy %>%
    group_by(year) %>%
    summarise(h2_energy = sum(TWh))

  H2_extra_unit_cost %<>%
    pivot_longer(cols = starts_with("2"),
                 names_to = "year",
                 values_to = "costs")%>%
    group_by(Cost_type, year) %>%
    summarise(cost = sum(costs)) %>%
    left_join(total_H2_energy_per_year, by = "year") %>%
    mutate(unit_cost = (cost/h2_energy)) %>%
    select(c(Cost_type, year, unit_cost)) %>%
    replace(is.na(.), 0)%>%
    pivot_wider(names_from = Cost_type,
                values_from = unit_cost )

  H2_extra_cost <- total_H2_energy %>%
    filter(sector != "hydrogen_conversion") %>%
    left_join(H2_extra_unit_cost, by = "year")  %>%
    mutate(Capex = Capex * TWh,
           `Carbon cost` = `Carbon cost`* TWh,
           `Fuel cost` = `Fuel cost` * TWh,
           Opex = Opex * TWh) %>%
    pivot_longer(cols = Capex:Opex,
                 names_to = "Cost_type",
                 values_to = "Extra_cost") %>%
    rename(Sector_infrastructure = sector) %>%
    select(
      "Sector_infrastructure",
      all_of(site_cluster),
      "cluster_rad",
      "Technology_code" = "code",
      Traded_NonTraded = "traded_site",
      "Cost_type",
      "year",
      "Technology_category" = "technology_category",
      "Extra_cost"
    )

  # Note: we need to join by traded/non-traded to avoid double counting.
  # For costs, Traded_NonTraded is the same as traded_site as is just a flag
  # for whether the site is in the ETS or not.

  return(H2_extra_cost)
}



adjust_H2_cost <- function(cost, H2_extra_cost, site_cluster) {

  cost_long <- cost %>%
    pivot_longer(cols = starts_with("2"),
                 names_to = "year",
                 values_to = "costs")


  cost_long %<>%
    left_join(H2_extra_cost, by = c("Sector_infrastructure",
                                    site_cluster,
                                    "cluster_rad",
                                    "Technology_code",
                                    "Traded_NonTraded",
                                    "Cost_type",
                                    "year",
                                    "Technology_category"),
              relationship = 'many-to-many') %>%
    mutate(Extra_cost = replace_na(Extra_cost, 0)) %>%
    mutate(Extra_cost = replace(Extra_cost, which (Extra_cost<0), 0)) %>%
    mutate(costs = costs + Extra_cost) %>%
    select(-Extra_cost)


  year_cols <- unique(cost_long$year)

  # Below deals with duplicates, then makes back to wider form
  cost_final <- cost_long %>%
    group_by(across(c(!costs))) %>%
    mutate(row_number = row_number()) %>% # this ensures duplicate rows of groups are kept rather than nested
    ungroup() %>%
    pivot_wider(names_from = year, values_from = costs) %>%
    select(!row_number)

  return(cost_final)
}






