
#### Infrastructure tab ####==================================================

#' Generate table of H2 and CO2 transport infrastructure outputs
#'
#' Uses the model solution to construct a table of all H2 and CO2 transport
#' infrastructure built and used through time that will be used to populate the
#' 'Infrastructure' and 'Infrastructure_sites' tabs of the Excel output workbook
#'
#' @param solved a table containing the model solution, decision variables and
#' PV_coefficients
#' @param model_data list of data tables read in from excel data template
#' @param site_cluster variable indicating if site or cluster level data is to
#' be outputted.
#'
#' @returns table of infrastructure outputs with 1 row per variable_type for
#' each site or cluster.
#'  Columns include:
#'  * site_ID
#'  * variable_type (new/used/available H2/CO2 pipe/grid/truck capacity)
#'  * unit
#'  * terminal
#'  * storage_site
#'  * pipe_cluster_end
#'  * years - one column for each timestep in the model
#'
#' @export
create_infrastructure_tables <- function(solved,model_data,site_cluster){

  comit_tic(sprintf('Total for creating infrastructure tables (%s)', site_cluster))

    H2_pipe_capacity_types <- c("H2_pipe_new_capacity","H2_pipe_available_capacity","H2_truck_used_capacity","H2_grid_used_capacity")
    Co2_pipe_capacity_types <- c("CO2_pipe_new_capacity", "CO2_pipe_available_capacity","CO2_truck_used_capacity")

    group_cols_pipes <- c(site_cluster, "year", "variable_type")
    group_cols_Co2_transport <- c("year", "variable_type", site_cluster, "terminal", "storage_site")

     #filter out h2 conversion "site"
    solved %<>% filter(!grepl("conversion",code))

    pipes <- solved %>%
      filter(variable_type %in% c(H2_pipe_capacity_types,Co2_pipe_capacity_types)) %>%
      filter(PV_term == "PV_fixed_opex") %>%
      group_by(across(all_of(group_cols_pipes))) %>%
      summarise(solution = sum(solution)) %>%
      mutate(unit = ifelse(variable_type %in% H2_pipe_capacity_types, "PJ_a", "kt_a"))

    CO2_transported <- solved %>%
      filter(variable_type == "CO2_transported", PV_term == "PV_CO2_national_transport") %>%
      group_by(across(all_of(group_cols_Co2_transport))) %>%
      summarise(solution = sum(solution)) %>%
      mutate(unit = "kt")

    if(model_data$model_parameters$model_H2_production) {
      H2_transported = H2_production_infrastructure(solved)
      infrastructure <- bind_rows(pipes, CO2_transported, H2_transported)
    }
    else {
      infrastructure <- bind_rows(pipes, CO2_transported)
    }
    # add the pipe_cluster_end column if required
    infrastructure  = infrastructure  %>%
      {if("pipe_cluster_end" %in% names(.)) . else add_column(., pipe_cluster_end = NA)} %>%
      pivot_wider(names_from = year, values_from = solution)


    comit_toc()

    return(infrastructure)
  }


#' Generate table of H2 production infrastructure outputs. Only used in when
#' the model is run in H2_production mode
#'
#' @param solved a table containing the model solution, decision variables and
#' PV_coefficients
#'
#' @returns table of H2 production infrastructure outputs with 1 row per
#' variable_type for each site or cluster.
#'
#' @export
H2_production_infrastructure <- function(solved){

  group_cols_H2_production <- c("year", "variable_type", site_cluster, "pipe_cluster_end")
  H2_transported <- solved %>%
    filter(variable_type == "H2_outflow",PV_term == "PV_fixed_opex") %>%
    group_by(across(all_of(group_cols_H2_production))) %>%
    summarise(solution = sum(solution)) %>%
    mutate(solution = solution/3.6,unit = "TWh")

  return(H2_transported)
}


#__________________________________________________________________

#### Outputs tab ####==================================================

#' Generates table of 'Outputs', i.e. the deployment of industrial technologies
#' in terms of used capacity through time.
#'
#' Uses the model solution to construct a table showing the deployment of
#' industrial technologies through time that will be used to populate the
#' 'Outputs' tabs of the Excel output workbook
#'
#' @param solved a table containing the model solution, decision variables and
#' PV_coefficients
#' @param model_data list of data tables read in from excel data template
#' @param site_cluster variable indicating if site or cluster level data is to
#' be outputted.
#'
#' @returns table of Outputs with 1 row per technology for
#' each site or cluster.
#'  Columns include:
#'  * sector
#'  * cluster_rad
#'  * Primary_output
#'  * Output_description
#'  * Technology_code
#'  * Technology_category
#'  * variable_type (used capacity)
#'  * years - one column for each timestep in the model
#'
#' @export
create_outputs_table <- function(output_data,model_data,site_cluster){

  comit_tic(sprintf('Total for creating production outputs tables (%s)', site_cluster))

  group_cols <- c("sector", "site_ID", site_cluster,"cluster_rad", "output_commodity", "code", "name", "description", "technology_category", "commodity_category", "year", "variable_type")

  outputs <- output_data %>%
    left_join(model_data$commodities, by = c("output_commodity" = "commodity")) %>%
    left_join(select(model_data$NAEI_clean_new, site_ID, cluster_rad), by = "site_ID") %>%

    group_by(across(all_of(group_cols))) %>%
    summarise(solution = sum(solution), .groups = "drop") %>%

    pivot_wider(names_from = year, values_from = solution)     %>%
    mutate(across(starts_with("2"),  ~ replace_na(.x, 0))) %>%

    left_join(select(model_data$Technologies, code, output_unit),by = "code") %>%
    select( Sector = sector, site_cluster, cluster_rad, Primary_output = output_commodity,Output_description = description,Technology_code = code,Technology_description = name,
      Technology_category = technology_category,Capacity = variable_type,Unit = output_unit,starts_with("2"))

  comit_toc()

  return(outputs)
}


#### Energy tab #### ==========================================================
#' Generates table of fuel use through time
#'
#' Uses the model solution to construct a table showing fuel use through time
#' that will be used to populate the 'Energy' tabs of the Excel output workbook
#'
#' @param solved a table containing the model solution, decision variables and
#' PV_coefficients
#' @param model_data list of data tables read in from excel data template
#' @param site_cluster variable indicating if site or cluster level data is to
#' be outputted.
#'
#' @returns list containing a table of energy use and a table of
#' #' H2 energy use, to use for the cost calculation H2 sector adjustment
#'
#' @export
create_energy_tables <- function(output_data,model_data,site_cluster){

  comit_tic(sprintf('Total for creating energy tables (%s)', site_cluster))

    group_cols <- c("sector", site_cluster, "cluster_rad", "traded_site", "output_commodity", "code", "name", "description", "technology_category", "commodity", "commodity_category", "commodity_produces_emissions", "year")

    # calculate fuel use for each commodity, technology and site_cluster combination
    energy <- calculate_fuel_use(output_data,model_data,group_cols)

    # calculate the H2 energy use, that we will need later for the cost adjustment for the H2 sector
    total_H2_energy <- calculate_H2_energy_use(energy)

    # convert to wide format and remove NAs and unneeded commodities
    energy %<>% pivot_wider(names_from = year, values_from = c(TWh, ktCO2e), names_glue = "{year}_{.value}") %>%
      mutate(across(starts_with("2"), ~replace_na(.x, 0))) %>%
      filter(!(commodity_category %in% c("Miscellaneous", "Heat", "Steam") )) %>%

    # select and format into the columns required for the Energy tab
     left_join(select(model_data$commodities, commodity, Output_description = description),
               by = c("output_commodity" = "commodity")) %>%
     select(Sector = sector, site_cluster, cluster_rad, Primary_output = output_commodity,
            Output_description, Technology_code = code, Technology_description = name,
            Technology_category = technology_category, Input_commodity = commodity,
            Fuel_category = commodity_category, Generate_emissions = commodity_produces_emissions,
            starts_with("2"))

    # create a list of outputs to return
    energy_list = list(energy,total_H2_energy)

    comit_toc()

  return(energy_list)

}



#### helper functions #### ==========================================================


# calculate the H2 energy use, that we will need later for the cost calculation
# adjustment for the H2 sector
calculate_H2_energy_use <- function(energy){

  total_H2_energy <- energy %>%
  filter(commodity_category == "Hydrogen")

  total_H2_energy$year  = as.character(total_H2_energy$year)

return(total_H2_energy)
}


#' Calculate fuel used in the optimal model solution
#'
#' @param output_data a table containing the used technology capacities
#' @param model_data list of data tables read in from excel data template
#' @param group_cols a vector of the column names to be aggregated over
#'
#' @returns a table with columns for Twh and ktCO2e added for each technology,
#' commodity, site/cluster-type and year combination
calculate_fuel_use <- function(output_data,model_data,group_cols) {

   fuel_use <- output_data %>%
    filter(!sector== "hydrogen_conversion") %>% #remove hydrogen_conversion sector
    # add technology fuel inputs
    left_join(model_data$technology_input_output, by = c("code" = "technology_code")) %>%
    filter(output < 0) %>%
    mutate(fuel_use = solution * -1 * output) %>%
    left_join(model_data$commodities, by = "commodity") %>%
    left_join(model_data$NAEI_clean_new%>% select(site_ID,traded_site, cluster_rad),by = "site_ID") %>%

    group_by(across(all_of(group_cols))) %>%
    summarise(fuel_use_Pj = sum(fuel_use), .groups = "drop") %>%

    left_join(model_data$Fuel_emissions, by = c("year" = "year", "commodity")) %>%
    mutate(ktCO2e = if_else(commodity_produces_emissions, fuel_use_Pj*CO2e, 0),
         TWh = fuel_use_Pj/3.6) %>%
    select(-c(fuel_use_Pj, CO2e))

    return(fuel_use)
}

