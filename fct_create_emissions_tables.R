

#### Emissions tab ####==================================================

#' Generate table of emissions outputs
#'
#' Uses the model solution to construct a table of all emissions through time
#' that will be used to populate the 'Emissions' tab of the Excel output workbook
#'
#' @param solved a table containing the model solution, decision variables and
#' PV_coefficients
#' @param model_data list of data tables read in from excel data template
#' @param site_cluster variable indicating if site or cluster level data is to
#' be outputted.
#'
#' @returns table of emissions outputs with 1 row per emissions type
#'  Columns include:
#'  * Emissions_category (e.g. direct / indirect)
#'  * Technology
#'  * GHG type
#'  * Emissions_type (energy/process)
#'  * years - one column for each timestep in the model
#'
#' @export
create_emissions_tables <- function(output_data,
                                    energy,
                                    model_data,
                                    site_cluster) {

  comit_tic(sprintf('Total for creating emissions tables (%s)', site_cluster))

  # add site information and commodity information to the output_data table
  expanded_output_data <- expand_output_data(output_data, model_data)

  # calculate each of the different sorts of emissions
  emissions_combined <- calculate_emissions_types(expanded_output_data,
                                                  model_data,
                                                  energy,
                                                  site_cluster)

  # reformat and tidy up
  emissions_combined <- tidy_up_emissions_table(emissions_combined,site_cluster)

  comit_toc()


  return(emissions_combined)
}


#' Calculates  total emissions through time across all combinations of
#' traded and non-traded sites, fuel/process and CO2/nonCO2 emissions using
#' [get_emissions()].
#'
#' @param emissions_data a table containing the emissions data
#' @param captured boolean TRUE, FALSE. When TRUE, returns only emissions that are captured. When FALSE, returns
#' only emissions that are released. If NULL, returns both captured and emitted emissions
#' @param model_data list of data tables read in from excel data template
#' @param site_cluster variable indicating if site or cluster level data is to
#' be outputted.
#' @param location_type string of either "direct" or "indirect". Default NULL returns both direct and indirect emissions
#'
#' @returns table of emissions through time for each site, technology and output combination
emissions_combinations <- function(emissions_data,
                                   captured,
                                   model_data,
                                   site_cluster,
                                   location_type = NULL) {

  comit_tic('Total for emissions_combinations')

  group_cols <- c(
    site_cluster,
    "sector",
    "year",
    "CO2_NonCO2",
    "Emission_type",
    "Traded_NonTraded",
    "output_commodity",
    "description",
    "code",
    "name",
    "technology_category",
    "cluster_rad"
  )

  english_names <- c(
    "CO2" = "CO2",
    "nonCO2" = "NonCO2",
    "fuel" = "Energy",
    "process" = "Process",
    "traded" = "Traded",
    "untraded" = "NonTraded"
  )

  # get emissions per unit of used capacity for each permutation of traded, process, and gas
  emissions_data <- emissions_data %>%
    mutate(
      CO2_process_traded = traded_site * get_emissions(
        code,
        year = year,
        gas = "CO2",
        source = "process",
        capture = captured,
        .data = model_data,
        location = location_type
      ),
      CO2_process_untraded = (!traded_site) * get_emissions(
        code,
        year = year,
        gas = "CO2",
        source = "process",
        capture = captured,
        .data = model_data,
        location = location_type
      ),
      CO2_fuel_traded = traded_site * get_emissions(
        code,
        year = year,
        gas = "CO2",
        source = "fuel",
        capture = captured,
        .data = model_data,
        location = location_type
      ),
      CO2_fuel_untraded = (!traded_site) * get_emissions(
        code,
        year = year,
        gas = "CO2",
        source = "fuel",
        capture = captured,
        .data = model_data,
        location = location_type
      ),
      nonCO2_process_traded = 0,
      nonCO2_process_untraded = get_emissions(
        code,
        year = year,
        gas = "nonCO2",
        source = "process",
        capture = captured,
        .data = model_data,
        location = location_type
      ),
      nonCO2_fuel_traded = 0,
      nonCO2_fuel_untraded = get_emissions(
        code,
        year = year,
        gas = "nonCO2",
        source = "fuel",
        capture = captured,
        .data = model_data,
        location = location_type
      )
    )

  emissions_data %<>%
    # multiply by used_capacity to get actual emissions
    mutate(across(CO2_process_traded:nonCO2_fuel_untraded, ~ .x * solution)) %>%
    pivot_longer(
      cols = CO2_process_traded:nonCO2_fuel_untraded,
      names_to = c("CO2_NonCO2", "Emission_type", "Traded_NonTraded"),
      names_pattern = "(.*)_(.*)_(.*)",
      values_to = "emissions"
    )

  emissions_data %<>%
    group_by(across(all_of(group_cols))) %>%
    summarise(emissions = sum(emissions, na.rm = TRUE),
              .groups = "drop") %>%
    pivot_wider(names_from = year, values_from = emissions) %>%
    mutate(across(CO2_NonCO2:Traded_NonTraded, ~ english_names[.x]), unit = "kt") %>%
    mutate(across(starts_with("2"),  ~ replace_na(.x, 0)))

  comit_toc()

  return(emissions_data)
}



#' Calculates total emissions across both CO2 and nonCO2 and
#' pivots the table from wide to long format for years
#'
#' @param emissions_data a table containing the (direct) emissions data
#'
#' @returns table of (direct) emissions with a total emissions column added
calculate_total_emissions <- function(emissions_data) {

  comit_tic("Total for calculate total emissions")

  # group by all characteristics bar CO2_nonCO2 to get total emissions per year
  emissions_data %<>%
    pivot_longer(starts_with('2'),
                 names_to = 'year',
                 values_to = 'emissions') %>%
    group_by(across(c(-CO2_NonCO2, -emissions))) %>% # group by all apart from CO2_NonCO2
    summarise(total_emissions = sum(emissions)) %>% # get total direct emissions
    ungroup()

  comit_toc()

  return(emissions_data)
}


#' Adds the Greenhouse gas splits to the table of direct emissions
#' using the information in the ghg_splits tab of the Excel input workbook
#' Only the splits for CO2, CH4 and N20 are used
#'
#' @param emissions_data a table containing the (direct) emissions data
#' @param model_data list of data tables read in from excel data template
#'
#' @returns table of (direct) emissions with the following columns added:
#'  * ghg_type (either CO2, CH4, N20 or total_CO2e)
#'  * CO2_NonCO2 (either CO2 or NonCO2)
#'  * years - one column for each timestep in the model
add_ghg_splits <- function(emissions_data,model_data){

  comit_tic("Total for add_ghg_splits")

  emissions_data %<>%
    left_join(model_data$ghg_splits %>% select(Sector, CO2, CH4, N2O),
              by = c('sector' = 'Sector')) %>%
    mutate(total_CO2e = 1) %>% # factor is for total is just 1
    pivot_longer(c(total_CO2e, CO2, CH4, N2O),
                 names_to = 'ghg_type',
                 values_to = 'ghg_factor') %>%
    mutate(emissions = ghg_factor * total_emissions) %>% # need to aggregate first
    select(!c(ghg_factor, total_emissions)) %>%
    pivot_wider(names_from = year, values_from = emissions)

  emissions_data %<>%
    mutate(
      Emissions_category = case_when(
        ghg_type == 'total_CO2e' ~ 'Direct (total CO2e)',
        TRUE ~ Emissions_category
      )
    ) %>%
    mutate(
      CO2_NonCO2 = case_when(
        Emissions_category == "Direct (split by ghg type)" &
          ghg_type == "CO2"  ~ "CO2",
        Emissions_category == "Direct (split by ghg type)" &
          ghg_type == "CH4"  ~ "NonCO2",
        Emissions_category == "Direct (split by ghg type)" &
          ghg_type == "N2O"  ~ "NonCO2"
      )
    )

  comit_toc()

  return(emissions_data)
}





#' Calculates the fuel emissions for hydrogen use in the model, according to
#' the mix of blue/green/grey hydrogen that are deployed in the optimal solution
#'
#' @param output_data a table containing a subset of the model solution outputs
#' @param model_data list of data tables read in from excel data template
#'
#' @returns table of fuel emissions factors with rows for hydrogen use in each
#' year of the model added
add_fuel_emissions_for_H2 <- function(output_data, model_data) {

  comit_tic("Total for add_fuel_emissions_for_H2")

    fuel_emissions_withH2 <- output_data %>%

    filter(grepl("conversion", code, fixed = TRUE) == TRUE )%>%
    mutate(commodity = substr(code, 1, str_length(code) - str_length("_conversion"))) %>%
    left_join(model_data$Fuel_emissions, by = c("year" = "year", "commodity" = "commodity")) %>%
    mutate(emission = solution * CO2e, fuel_use = solution ) %>%
    group_by(year) %>%
    summarise(fuel_use = sum(fuel_use), emission = sum(emission)) %>%
    mutate(year = as.numeric(year), commodity = "INDMAINSHYG", "CO2e" = emission / fuel_use) %>%
    select(year, commodity,  CO2e) %>%
    rbind(model_data$Fuel_emissions)

  comit_toc()

  return(fuel_emissions_withH2)
}


#' Calculates the indirect emissions for the model solution
#'
#' @param output_data a table containing a subset of the model solution outputs
#' @param energy_data a table of energy (fuel) use through time
#'
#' @returns table of indirect emissions through time
#'
calculate_indirect_emissions <- function(fuel_emissions_with_H2, energy_data) {
  comit_tic("calculate_indirect_emissions")

  # Indirect emissions - for separate H2 and Electricity; assume all are CO2 fuel emissions
  emissions_indirect = energy_data %>% select(-c(ends_with("ktCO2e"))) %>%
    pivot_longer(
      cols = (ends_with("_TWh")),
      names_to = "year",
      values_to = "fuel_use"
    ) %>%
    mutate(year = str_sub(year, end = 4)) %>% mutate(fuel_use = fuel_use * 3.6) %>%  #fuel use unit in TWh, but emissions factors in PJ
    filter(Input_commodity %in% c("INDDISTELC" , "INDMAINSHYG")) %>%
    mutate(year = as.numeric(year)) %>%
    left_join(fuel_emissions_with_H2,
              by = c("year" = "year", "Input_commodity" = "commodity")) %>%
    mutate(emission = fuel_use * CO2e) %>%
    select(-c(fuel_use, CO2e)) %>%
    pivot_wider(names_from = year,
                values_from = emission,
                values_fn = list)

  emissions_indirect <- emissions_indirect %>%
    # 'values_fn = list' allows to deal with non-unique rows that are then unnested below
    unnest(cols =  c(names(emissions_indirect)[grepl("2", names(emissions_indirect))])) %>%

    mutate(across(starts_with("2"),  ~ replace_na(.x, 0)))   %>%

    # need to add code and commodity descriptions
    mutate(Emissions_category = Fuel_category, unit = "kt") %>%
    rename(sector = Sector)

  comit_toc()

  return(emissions_indirect)

}


#' Calculates the negative emissions for the model solution
#'
#' @param output_data a table containing a subset of the model solution outputs
#' @param model_data list of data tables read in from excel data template
#' @param site_cluster variable indicating if site or cluster level data is to
#' be outputted.
#' @returns table of negative emissions through time
calculate_negative_emissions <- function(output_data,model_data,site_cluster) {

  comit_tic("calculate_negative_emissions")

  group_cols <- c(site_cluster, "sector", "year", "Traded_NonTraded", "output_commodity", "description","code", "name", "technology_category", "cluster_rad")

  emissions_negative <- output_data %>%
  mutate(net_emissions_traded =   traded_site * solution * net_emissions(code,  year = year, .data = model_data),
         net_emissions_nontraded =  (!traded_site) * solution * net_emissions(code,  year = year, .data = model_data)) %>%

  pivot_longer(cols = (net_emissions_traded:net_emissions_nontraded), names_to = "Traded_NonTraded", values_to = "emissions") %>%
  group_by(across(all_of(group_cols))) %>%
  summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = emissions) %>%
  mutate(unit = "kt") %>%   #keep negative emissions only
  rowwise() %>%
  # 2020 values are often NA as techs are deployed after 2020
  filter ( sum(c_across(starts_with("2")),na.rm=TRUE)<0)  %>%
  # need to add code and commodity descriptions
  mutate(Emissions_category = "Negative") %>%
  select(Emissions_category, Sector = sector, site_cluster, cluster_rad, emissions_traded_status = Traded_NonTraded,
         Primary_output = output_commodity, Output_description = description, Technology_code = code, Technology_description = name,  Technology_category = technology_category,  unit, starts_with("2") )

  comit_toc()

  return(emissions_negative)
}


#' Adds additional required columns to the output_data table from the model_data list
#' Done upfront to avoid repetition
#'
#' @param output_data a table containing a subset of the model solution outputs
#' @param model_data list of data tables read in from excel data template
#' be outputted.
#' @returns table of output_data with the following columns added:
#'  *cluster_rad
#'  *traded_site
#'  *(commodity) description
#'
expand_output_data <- function(output_data, model_data) {

  comit_tic("expand_output_data")

  output_data %<>%
    left_join(select(
      model_data$NAEI_clean_new,
      site_ID,
      cluster_rad,
      traded_site
    ),
    by = "site_ID") %>%
    left_join(
      select(model_data$commodities, commodity, description),
      by = c("output_commodity" = "commodity")
    )

  comit_toc()

  return(output_data)
}



calculate_emissions_types <- function(expanded_output_data,
                                      model_data,
                                      energy,
                                      site_cluster) {

  comit_tic("calculate_emissions_types")

  # captured emissions
  emissions_captured <-  emissions_combinations( expanded_output_data,TRUE, model_data,site_cluster) %>%
    mutate(Emissions_category = "Captured")

  # direct and indirect emissions
  emissions = emissions_combinations( expanded_output_data,FALSE, model_data,site_cluster) %>%
    mutate(Emissions_category = "Direct_and_Indirect")

  ## direct emissions
  emissions_direct <- emissions_combinations( expanded_output_data,FALSE, model_data,site_cluster,location = "direct") %>%
    calculate_total_emissions() %>%
    mutate(Emissions_category = "Direct (split by ghg type)")

  # join ghg factors on, make calculations and then put back in wide format
  emissions_direct = add_ghg_splits(emissions_direct,model_data)

  ## calculate emissions intensity for H2 based on which types of H2 are deployed
  fuel_emissions_with_H2 = add_fuel_emissions_for_H2(expanded_output_data,model_data)

  # Indirect emissions - for separate H2 and Electricity; assume all are CO2 fuel emissions
  emissions_indirect <- calculate_indirect_emissions(fuel_emissions_with_H2, energy)

  ## Negative emissions
  emissions_negative  = calculate_negative_emissions(expanded_output_data,model_data, site_cluster)

  # combine these three emissions tables with the same format (to minimize code repetition)
  emissions_combined = bind_rows(emissions,emissions_direct, emissions_captured, emissions_indirect) %>%
    select(Emissions_category, Sector = sector, site_cluster, cluster_rad, emissions_traded_status = Traded_NonTraded,
           Primary_output = output_commodity, Output_description=description, Technology_code = code, Technology_description = name,  Technology_category = technology_category, CO2_NonCO2, Emission_type, ghg_type,unit,starts_with("2") )

  # combine remaining emissions tables
  emissions_combined <- bind_rows(emissions_combined, emissions_negative)

  comit_toc()

  return(emissions_combined)
}


tidy_up_emissions_table <- function(emissions_combined,site_cluster) {

  comit_tic("Total for tidy_up_emissions_table")

  emissions_combined %<>%
    mutate(across(starts_with("2"), ~ replace_na(.x, 0))) %>%
    mutate(
      Sector_group = case_when(
        Sector == "Refineries" ~ "Refineries",
        Sector == "Hydrogen" ~ "Hydrogen",
        TRUE ~ "Industry"
      )
    ) %>%
    select(
      Emissions_category,
      Sector,
      site_cluster,
      cluster_rad,
      Primary_output,
      Output_description,
      Technology_code,
      Technology_description,
      Technology_category,
      CO2_NonCO2,
      Emission_type,
      ghg_type,
      Sector_group,
      starts_with("2")
    ) %>%
    filter(rowSums(abs(across(starts_with(
      "2"
    )))) != 0) # remove rows that contain all zeros

  comit_toc()

  return(emissions_combined)

}


