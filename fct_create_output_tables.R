
# Function to turn solution into Costs, Outputs, Emissions, Emissions captured, and Energy tables
# in the format of the output Excel template

#  * solution - a list containing the model solution, produced by [ROI_solve()]
#  * data - a list of data tables that is used to create the model
#  * decision_variables - a sparse matrix of the decision variables used by the model
#  * PV_coefficients - list of model coefficients

#a table containing the model solution, decision variables and PV_coefficients

create_output_tables <- function(ROI_solution,
                                 model_data,
                                 decision_variables,
                                 PV_coefficients,
                                 site_cluster) {

  comit_tic('total for create_output_tables')

  #create a table containing the model solution, decision variables and PV_coefficients
  solved <- create_solution_table(ROI_solution,
                                  decision_variables,
                                  PV_coefficients)

  #create a table of the used technology capacity in the optimal model solution
  output_data <- create_output_data(solved,
                                    model_data)

  #post-processing of the NAEI data table to add cluster radius and traded_site fields
  model_data$NAEI_clean_new <- adjust_NAEI_data(model_data$NAEI_clean)

  #### Energy tab #### ==========================================================
  energy_list <- create_energy_tables(output_data, model_data, site_cluster)
  energy <- energy_list[[1]]
  total_H2_energy <- energy_list[[2]]

  ####  Costs tab #### =========================================================

  cost <- create_cost_tables(solved, model_data, site_cluster)

  cost <- adjust_cost_tables(cost, energy, site_cluster, total_H2_energy,
                             model_data)

  #### Outputs tab ####=========================================================
  outputs = create_outputs_table(output_data,
                                 model_data,
                                 site_cluster)

  #### Emissions tab ####=======================================================
  emissions_combined = create_emissions_tables(output_data,
                                               energy,
                                               model_data,
                                               site_cluster)

  #### Infrastructure tab ####==================================================
  infrastructure = create_infrastructure_tables(solved,
                                                model_data,
                                                site_cluster)

  ### Sites tab ####==================================================
  sites =  create_sites_tables(model_data)
  sites_info =  create_sites_info_tables(model_data)


  #### Combine all tables ####==================================================
  tables <- combine_tables(
    cost,
    model_data,
    outputs,
    emissions_combined,
    sites,
    sites_info,
    energy,
    infrastructure,
    site_cluster
  )

  comit_toc()

  return(tables)

}



create_output_xlsx <- function(tables,
                               model_data,
                               comit_package_version = NULL) {

  comit_tic('total for create_output_xlsx')

    # read existing output template into R
    output_template_file <- "output_template.xlsx"


    output_template_location <- system.file(
      'output_template',
      output_template_file,
      package='comit')

    wb <- loadWorkbook(output_template_location)

    # write info to title tab
    openxlsx::writeData(wb,
                        sheet = 'Title',
                        x = paste0('Package version: ', comit_package_version),
                        xy = c(2, 8))

    openxlsx::writeData(wb,
                        sheet = 'Title',
                        x = paste0('Run date: ', Sys.Date()),
                        xy = c(2, 9))

    openxlsx::writeData(wb,
                        sheet = 'Title',
                        x = paste0('Completion time: ', format(Sys.time(), '%H:%M')),
                        xy = c(2, 10))



    # add cluster level tabs when required
    if(model_data$model_parameters$output_cluster_level) {

      cluster_tabs <- c('Costs_cluster',
                        'Outputs_cluster',
                        'Emissions_cluster',
                        'Energy_cluster',
                        'Infrastructure_cluster')

      for(tab in cluster_tabs) {
        openxlsx::addWorksheet(wb, tab, tabColour = '#F8CBAD')
      }

    }

    walk(names(tables), function(x){

      # first delete the table if it exists (it should for sites level outputs)
      if(!is_empty(openxlsx::getTables(wb, x))){
        removeTable(wb, x, x)
      }

      # write the table
      writeDataTable(wb,
                     x,
                     tables[[x]],
                     tableName = x,
                     tableStyle = 'TableStyleLight18')
    })

    comit_toc()

    return(wb)
  }


#-------------------------------------------------------------------------------
## Helper functions

#' Creates a table containing the model solution, decision variables and PV_coefficients
#
#' @param ROI_solution table containing the model solution
#' @param decision_variables  a sparse matrix of the decision variables used by the model
#' @param PV_coefficients list of model coefficients

#' @return a table containing the model solution, decision variables and PV_coefficients
#'
#' @export
create_solution_table <- function (ROI_solution,decision_variables,PV_coefficients) {

  solved <- decision_variables %>%
    mutate(solution = ROI_solution$solution) %>%
    left_join(PV_coefficients, by = "variable_index") %>%
    select(-coefficient) %>%
    pivot_longer(cols = PV_fuel_cost:PV_H2_pipe_national, names_to = "PV_term",
                 values_to = "coefficient") %>%
    replace_na(list(coefficient = 0)) %>%
    mutate(cost = solution * coefficient)
  return(solved)
  }

#' A  table of the used technology capacity in the optimal model solution

#' @param solved table containing the model solution
#' @param model_data  list of data tables read in from excel template
#' @return a data frame listing the used technology capacity
#'
#' @export

create_output_data <- function(solved,model_data){
  output_data <- solved %>%
    filter(variable_type == "used_capacity") %>%
    # use any old PV type
    filter(PV_term == "PV_fixed_opex") %>%
    # need to add sector by code
    left_join(model_data$Technologies, by = "code")

  return(output_data)
}


#' Adds cluster radius to the NAEI data table, and creates the traded_site field
#'
#' @param NAEI_data  one of the data tables read in from excel template
#'
#' @return the NAEI_clean data as provided as in the input, with the following updates:
#'
#'  * A column has been added to indicate if each site is within a 25km and 30km cluster radius
#'  * the traded_flag column is renamed to traded_site
#'
#' @export
adjust_NAEI_data <- function(NAEI_data){
  NAEI_data = NAEI_data %>%
    mutate(cluster_rad = ifelse(pipe_dist <= 25, "<25km" , ifelse(pipe_dist <= 30, "25-30km", ">30km"))) %>%
    mutate(Traded_NonTraded = case_when(traded_flag == 'Traded' ~ TRUE,
                                        traded_flag != 'Traded' ~ FALSE)) %>%
    mutate(traded_site = case_when(traded_flag == 'Traded' ~ TRUE,
                                   traded_flag %in% c('Non-traded-non-point',
                                                      'Non-traded') ~ FALSE))
  return(NAEI_data)
}



#### Sites tab ####===========================================================

#' Creates the Sites tab in the Output data workbook
#'
#' @param model_data  list of data tables read in from excel template
#' @return a data frame listing the number of sites for each sector, cluster,
#' traded status and cluster radius combination
#'
#' @export
create_sites_tables <- function(model_data){
  sites <- model_data$NAEI_clean_new %>%
    group_by(Sector = IPM_sector,  cluster = H2_point, cluster_rad,  traded_site) %>%
    summarise(Site_count = n(), .groups = "drop") %>%
    arrange(Sector, cluster)

  return(sites)
}


#' Creates the Sites_info tab in the Output data workbook
#'
#' @param model_data  list of data tables read in from excel template
#' @return a data frame listing information about each site in the model: name,
#' latitude, longitude, region, traded_status, cluster and NAEI plantID
#'
#' @export
create_sites_info_tables <- function(model_data){

  site_info_columns <- c('site_ID', 'site_name', 'Latitude', 'Longitude',
                         'region', 'PlantID')

  sites_info = model_data$NAEI_clean %>%
    select(all_of(site_info_columns))

  return(sites_info)
}



combine_tables <- function(cost, model_data, outputs, emissions_combined, sites,
                           sites_info, energy, infrastructure, site_cluster) {

  #combine sites or clusters tabs
  if (site_cluster == "cluster") {

    # add regions to clusters
    cluster_regions <- region_lookup(model_data$Cluster_location,
                                     link_to_cluster = TRUE)

    tables <- list('Costs_cluster' = cost,
                    'Outputs_cluster' = outputs,
                    'Emissions_cluster' = emissions_combined,
                    'Energy_cluster' = energy,
                    'Infrastructure_cluster' = infrastructure)

    tables <- lapply(tables, function(x){
      x %>%
        left_join(cluster_regions, by = c('cluster' = 'Cluster')) %>%
        relocate(region, .after = 'cluster') %>%
        rename(region_of_cluster_centre = 'region') # for clarity of region var
    })
    # sort outputs
    tables[2:4] <- lapply(tables[2:4], function(x){
      x  %<>%   arrange(Sector, cluster)})

    tables[['Costs_cluster']] <- tables[['Costs_cluster']] %>%
      arrange(Sector_infrastructure,cluster)

    tables[['Infrastructure_cluster']]  <- tables[['Infrastructure_cluster']] %>%
      arrange(cluster,variable_type)


  } else {

  # combine for site_cluster = 'site_ID' option

    cols_to_keep <- c('site_ID', 'site_traded_status' = 'traded_flag',
                      'cluster' = 'H2_point' )
    cols_to_move <- c('site_traded_status', 'cluster')

    tables <- list('Sites_info' = sites_info,
                   'Costs' = cost,
                   'Outputs' = outputs,
                   'Emissions' = emissions_combined,
                   'Energy' = energy,
                   'Infrastructure' = infrastructure)

    tables <- lapply(tables, function(x){
      x %<>% left_join(model_data$NAEI_clean %>%
                         select(all_of(cols_to_keep)),
                       by = "site_ID")
      x %<>% relocate(all_of(cols_to_move), .after = site_ID)
    })
    tables[3:5] <- lapply(tables[3:5], function(x){
    x  %<>%   arrange(Sector, site_ID)})

    tables[['Costs']] <- tables[['Costs']] %>% arrange(Sector_infrastructure,site_ID)
    tables[['Infrastructure']]  <- tables[['Infrastructure']] %>% arrange(site_ID,variable_type)

  }

  return(tables)
}



#_____________________________________________________________________________
### NOT CURRENTLY USED ###

pivot_wider_years <- function(data) {
  data = data %>%
    pivot_wider(names_from = year, values_from = solution) %>%
    mutate(across(starts_with("2"),  ~ replace_na(.x, 0)))
  return(data)
}


group_summarise <- function(data,group_cols,variable_to_sum){
  data = data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(solution = sum(data[[variable_to_sum]]))

  return(data)
}
#_____________________________________________________________________________


