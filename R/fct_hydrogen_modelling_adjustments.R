
# Adds H2 conversion technologies
create_H2_conversion <- function(data, model_H2_production)
{
  if(model_H2_production == FALSE) {
    H2_fuels <- c("INDMAINSHYGG", "INDMAINSHYGB", "INDMAINSHYGR")
    H2_production_commodity <- "HYGEN"

    # Just check that H2 fuels is a complete list of the hydrogen fuels available
    all_H2 <- data$commodities %>%
      filter(commodity_category == "Hydrogen", !(commodity %in% c("HYGEN", "INDMAINSHYG"))) %>%
      pull(commodity)

    if(!all(all_H2 %in% H2_fuels)){
      missing_fuels <- all_H2[!(all_H2 %in% H2_fuels)]
      warning(paste0(paste(missing_fuels, collapse = ", "),
                     " possibly missing from list of hydrogen fuels in sites.R/create_H2_conversion"))
    }


    # First add the technologies into the technology table
    data$Technologies %<>%
      add_row(code = paste0(H2_fuels, "_conversion"),
              name = code,
              sector = "hydrogen_conversion",
              output_commodity = H2_production_commodity,
              emissions_released = 1,
              existing_capacity_2020 = 999,
              capacity_to_activity_factor = 1,
              availability_factor = 1,
              capex = 0,
              fixed_opex = 0,
              lifetime = 999,
              start_year = data$model_parameters$start_year,
              technology_category = NA)

    # Add technology input/output
    data$technology_input_output %<>%
      add_row(technology_code = rep(paste0(H2_fuels, "_conversion"), each = 2),
              commodity = c(rbind(H2_fuels, rep(H2_production_commodity, length(H2_fuels)))),
              output = rep(c(-1, 1), length(H2_fuels)),
              primary_commodity = rep(c(FALSE, TRUE), length(H2_fuels)),
              commodity_produces_emissions = rep(c(TRUE, FALSE), length(H2_fuels)))



  }
  return(data)

}



#' Adds H2 production plants at clusters if required
#' @param data data read in from excel template
#' @param model_H2_production logical TRUE/FALSE depending on whether H2 production is modeled explicitly
#' @return list of data
add_H2_plants <- function(data, model_H2_production)
{
  # check input parameters
  stopifnot(is.logical(model_H2_production), length(model_H2_production) == 1, !is.na(model_H2_production))

  if(model_H2_production == FALSE) {
    # If we are modelling hydrogen in "comit" mode, we want to create national technologies which
    # convert hydrogen to HYGEN

    # To do this we need to create the site as a "hydrogen_conversion" sector
    data$NAEI_clean %<>%
      # H2 sites can only be built at certain clusters
      add_row(site_name = "H2 conversion site",
              IPM_sector = "hydrogen_conversion",
              H2_point = NA,
              total_MtCO2 = 0,
              pipe_dist = 0,
              num_sites = 1) %>%

      # reapply the site ID column
      select(-site_ID) %>%
      rowid_to_column("site_ID")

    return(data)

  } else {

    data$NAEI_clean %<>%
      # H2 sites can only be built at certain clusters
      add_row(site_name = paste(data$Cluster_location$Cluster[data$Cluster_location$H2_production], "central H2 site"),
              IPM_sector = "Hydrogen",
              H2_point = data$Cluster_location$Cluster[data$Cluster_location$H2_production],
              total_MtCO2 = 0,
              pipe_dist = 0,
              num_sites = 1) %>%

      # reapply the site ID column
      select(-site_ID) %>%
      rowid_to_column("site_ID")

    return(data)
  }
}

