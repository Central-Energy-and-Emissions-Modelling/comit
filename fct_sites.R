# Functions to do various things with sites

#' Add a unique id number for each site in the site data table
#'
#' @param data list of data tables as read in from excel template and having
#'  been passed to `process_sites()`
#'
#' @return list of data tables as input but with site_ID column added to the
#'  NAEI_clean data frame.
add_site_ID <- function(data) {

  data$NAEI_clean <- data$NAEI_clean %>%
    rowid_to_column("site_ID")

  return(data)
}



#' Calculate annual demand for each site
#'
#' @param data list of data tables as read in from excel template and having
#'  gone through a series of early preprocessing steps as follows:
#'  ```
#'  data <- raw_data %>%
#'    process_sites()
#'
#'  data %<>%
#'    interpolate_data() %>%
#'    tidy() %>%
#'    round_years() %>%
#'    adjust_for_optimism() %>%
#'    apply_energy_efficiency() %>%
#'    adjust_existing_capacity() %>%
#'    add_site_ID()
#'
#'  # add region variable
#'  data$NAEI_clean %<>% assign_site_region()
#' ```
#' @param counterfactual, boolean. TRUE when the site demand is run as part
#'  of a counterfactual run. This removes site closures.
#'
#' @return dataframe for demand at each site. One row for each commodity
#'  produced at each site in each year. Contains the following columns:
#'   * year
#'   * site_ID
#'   * output_commodity
#'   * scaling_factor_within_sector - factor used to multiply national demand
#'     to produce site level demand
#'   * demand - site level demand of output commodity
site_demand <- function(data, counterfactual = FALSE) {

  # deal with hydrogen conversion seperately - don't expand years
  hydrogen_conversion <- data$NAEI_clean %>%
    filter(IPM_sector == 'hydrogen_conversion') %>%
    mutate(year = NA)

  # And same for non industry hydrogen demand
  non_industry_h2_start <- data$Non_industry_H2_demand %>%
    filter(year == data$model_parameters$start_year) %>%
    select(!year)

  non_industry_h2 <- data$NAEI_clean %>%
    filter(IPM_sector == 'Hydrogen') %>%
    left_join(non_industry_h2_start, by = c("H2_point" = "cluster")) %>%
    mutate(total_MtCO2 = if_else(IPM_sector == "Hydrogen", demand, total_MtCO2),
           year = NA) %>%
    select(-demand)

  # Now everything else
  site_demand <- data$NAEI_clean %>%
    filter(IPM_sector != "hydrogen_conversion",
           IPM_sector != 'Hydrogen') %>%
    expand_df_by_model_years(data)

  # Bring everything together
  site_demand <- site_demand %>%
    rbind(hydrogen_conversion) %>%
    rbind(non_industry_h2)

  if(data$model_parameters$include_site_closures) {

    # Here we want to remove the sites whose demand we expect to be picked up elsewhere.
    # if not counterfactual run, always remove these sites. If it is a counterfactual
    # run, only keep sites which are set to be removed from the counterfactual.
    pre_demand_plant_closures <- data$plant_closures %>%
      filter(redistribute_demand == TRUE
             & (!counterfactual | (counterfactual & remove_from_counterfactual)))

    if(nrow(pre_demand_plant_closures) > 0) {

      original_names <- names(site_demand)

      # Set emissions to 0 in closed sites, in turn resulting in 0 demand
      site_demand <- site_demand %>%
        left_join(pre_demand_plant_closures, by = 'PlantID') %>%
        mutate(total_MtCO2 = case_when(
          !is.na(closure_date) & (year > closure_date) ~ 0,
          TRUE ~ total_MtCO2
        )) %>%
        select(all_of(original_names)) # revert to the og columns
    }

  }


  # Get total sector level emissions, then work out ratio of site emissions to
  # total emissions in sector
  site_demand <- site_demand %>%
    group_by(IPM_sector, year) %>%
    mutate(total_sector_emissions = sum(total_MtCO2)) %>%
    ungroup() %>%
    mutate(scaling_factor_within_sector = total_MtCO2 / total_sector_emissions)


  if(data$model_parameters$include_site_closures) {

    # Here we want to remove sites who already have demand attributed.
    # if not counterfactual run, always remove these sites. If it is a counterfactual
    # run, only keep sites which are set to be removed from the counterfactual.
    post_demand_plant_closures <- data$plant_closures %>%
      filter(redistribute_demand == FALSE
             & (!counterfactual | (counterfactual & remove_from_counterfactual)))

    if(nrow(post_demand_plant_closures) > 0) {

      original_names <- names(site_demand)

      # Set scaling factor to 0 in closed sites, in turn resulting in 0 demand
      site_demand <- site_demand %>%
        left_join(post_demand_plant_closures, by = 'PlantID') %>%
        mutate(scaling_factor_within_sector = case_when(
          !is.na(closure_date) & (year > closure_date) ~ 0,
          TRUE ~ scaling_factor_within_sector
        )) %>%
        select(all_of(original_names)) # revert to the og columns
    }


  }




  # add output demand commodity for each sector
  output_commodities <- data$Technologies %>%
    filter(output_commodity %in% data$Demand_drivers$commodity) %>%
    distinct(sector, output_commodity)

  site_demand <- site_demand %>%
    left_join(output_commodities,
              by = c("IPM_sector" = "sector"),
              relationship = 'many-to-many') %>%
    left_join(data$Demand_drivers,
              by = c("output_commodity" = "commodity", 'year'),
              relationship = 'many-to-many')


  # Again, treat hydrogen separately
  site_demand <- site_demand %>%
    mutate(
      demand = if_else(
        IPM_sector == "Hydrogen",
        sum(non_industry_h2_start$demand),
        demand
      ),
      year = if_else(
        IPM_sector == "Hydrogen",
        data$model_parameters$start_year,
        year
      ),
      # site demand is national demand * scaling factor
      demand = demand * scaling_factor_within_sector) %>%
    select(year,
           site_ID,
           output_commodity,
           scaling_factor_within_sector,
           demand) %>%
    arrange(site_ID, output_commodity, year)


  return(site_demand)
}




# work out whether to use grid/truck or pipes to transport H2/Co2 from each site
site_H2C02_transport <- function(data)
{
  capacity_PJ <- 0.03154   # pJ to seconds in year

  max_H2 <- data$site_demand %>%
    left_join(data$sector_max_H2_CO2, by = "output_commodity") %>%
    group_by(site_ID, output_commodity) %>%
    summarise(max_H2 = max(demand * max_H2_per_output), .groups = "drop") %>%
    group_by(site_ID) %>%
    summarise(max_H2 = sum(max_H2) * !!capacity_PJ, .groups = "drop") %>%
    replace_na(list(max_H2 = 0)) %>%

    # Add distance from site to cluster
    left_join(select(data$NAEI_clean, site_ID, pipe_dist), by = "site_ID") %>%

    # Assume that a pipe can only be built in certain discrete capacities.
    # For each site, choose the smallest pipe that will meet the max_H2 requirement.
    mutate(nearest_flow_rate = cut(max_H2,
                                   c(0, unique(data$H2_transport_cost$capacity_MW)),
                                   include.lowest = TRUE,
                                   right = FALSE,
                                   labels = FALSE
    )) %>%
    mutate(nearest_flow_rate = c(0, unique(data$H2_transport_cost$capacity_MW))[nearest_flow_rate + 1])
    # ensure that sites are allocated to the nearest smaller bin, as this has a higher cost / capacity

  max_H2 %<>% mutate(nearest_distance = cut(
    pipe_dist,
    c(unique(data$H2_transport_cost$distance)),
    include.lowest = TRUE,
    right = FALSE,
    labels = FALSE
  )) %>%

    mutate(nearest_distance = c(0,unique(data$H2_transport_cost$distance))[nearest_distance + 1])
    max_H2 = max_H2 %>% left_join(data$H2_transport_cost,
                                  by = c("nearest_distance" = "distance", "nearest_flow_rate" = "capacity_MW"))

    # repeat for CO2
    # First work out maximum potential CO2 captured at each site

    if (data$model_parameters$use_CCS_Spur==TRUE) {
      data$NAEI_clean <- naei_pipe_to_spur_adjustment(data)
    }

    max_CO2 <- get_site_max_CO2(data) %>%
      select(!pipe_dist) # revert back to unadjusted now we have corrected the CO2 features

    site_H2C02_transport = max_H2 %>%
      left_join(max_CO2,
                by = c("site_ID"),
                suffix = c(".H2", ".CO2"))

return(site_H2C02_transport)
}




get_site_max_CO2 <- function(data) {

  max_CO2 <- data$site_demand %>%
    left_join(data$sector_max_H2_CO2, by = "output_commodity") %>%
    group_by(site_ID, output_commodity) %>%
    summarise(max_CO2 = max(demand * max_CO2_per_output), .groups = "drop") %>%
    group_by(site_ID) %>%
    summarise(max_CO2 = sum(max_CO2), .groups = "drop") %>%
    replace_na(list(max_CO2 = 0))


  max_CO2 %<>%
    # Add distance from site to cluster
    left_join(select(data$NAEI_clean, site_ID, pipe_dist), by = "site_ID") %>%

    # Assume that a pipe can only be built in certain discrete capacities.
    # For each site, choose the smallest pipe that will meet the max_CO2 requirement.
    mutate(nearest_flow_rate = cut(max_CO2,
                                   c(0, unique(data$CO2_transport_cost$flow_rate_kt)),
                                   include.lowest = TRUE,
                                   right = FALSE,
                                   labels = FALSE
    )) %>%
    mutate(nearest_flow_rate = c(0, unique(data$CO2_transport_cost$flow_rate_kt))[nearest_flow_rate + 1])

  max_CO2 %<>% mutate(nearest_distance = cut(
    pipe_dist,
    c(unique(data$CO2_transport_cost$distance_km)),
    include.lowest = TRUE,
    right = FALSE,
    labels = FALSE
  ))

  max_CO2$nearest_distance[is.na(max_CO2$nearest_distance)] <- min(data$CO2_transport_cost$distance_km)

  max_CO2 %<>%
    mutate(nearest_distance = c(unique(data$CO2_transport_cost$distance_km))[nearest_distance])  %>%
    left_join(data$CO2_transport_cost,
              by = c("nearest_distance" = "distance_km",
                     "nearest_flow_rate" = "flow_rate_kt"))

  return(max_CO2)

}



#' Adjust the NAEI data's pipe_dist column to account for any cluster
#'  spurs.
#'
#' @param data list of input dataframes after initial preprocessing steps.
#'
#' @returns dataframe of the NAEI_data as provided in the data input list,
#'  but with the pipe_dist for any sites specified in the input spreadsheet
#'  'spur_sites' tab to be effected by spurs to be adjusted to the specified
#'  values.
naei_pipe_to_spur_adjustment <- function(data) {

  spur_sites <- data$CCS_spur_sites %>%
    select(PlantID, spur_pipe_dist = pipe_dist)

  NAEI_clean_adjusted <- data$NAEI_clean %>%
    left_join(spur_sites, by = 'PlantID') %>%
    mutate(pipe_dist = case_when(!is.na(spur_pipe_dist) ~ spur_pipe_dist,
                                 TRUE ~ pipe_dist)) %>%
    select(!spur_pipe_dist)

  return(NAEI_clean_adjusted)
}

