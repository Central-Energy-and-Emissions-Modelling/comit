
#===============================================================================

# Some parameters/objects used throughout

point_site_emissions_cut_off <- 0.01 # decides whether sites are treated as small or large

MtC_to_MtCO2_conversion <- 3.664099575


regions_lookup <- c(North_East = "E12000001 : North East",
                    North_West = "E12000002 : North West",
                    Yorkshire_Humber = "E12000003 : Yorkshire and The Humber",
                    East_Midlands = "E12000004 : East Midlands",
                    West_Midlands = "E12000005 : West Midlands",
                    East = "E12000006 : East",
                    London = "E12000007 : London",
                    South_East = "E12000008 : South East",
                    South_West = "E12000009 : South West",
                    Wales = "W12000001 : Wales",
                    Scotland = "S12000001 : Scotland",
                    Northern_Ireland = "N12000001 : Northern Ireland")

regions <- names(regions_lookup)

#===============================================================================


#' Cleans site data and prepares for use in model
#'
#' Tidies and prepares the site data ready for use in the models, including
#' adding imputed aggregate sites for non-point sources and aggregating small
#' point sources to reduce computational load.
#'
#' @param input_data data read in from excel template, same as what is passed to
#'  [comit_solver()].
#'
#' @return list of data as provided as input_data, with the following updates:
#'
#'  * A data frame NAEI_clean appended to be used as the source for site data
#'  * NAEI_df_clean_2023_revised has a row appended to flag traded status
#'
#' @export
process_sites <- function(input_data){

  NAEI_data_2023 <- append_information_to_sites(input_data)

  # ---- Point source sites ----
  large_point_sites <- get_large_point_sites(NAEI_data_2023, input_data)

  aggregated_small_point_sites <-
    get_aggregated_small_point_sites(NAEI_data_2023,
                                     input_data)

  # ---- Non point source sites ----
  if (input_data$model_parameters$Use_IDBR) {
    aggregated_non_point_sites <-
      non_point_sites_from_ratios(NAEI_data_2023,
                                  input_data)
  } else {
    aggregated_non_point_sites <-
      non_point_sites_from_site_data(NAEI_data_2023,
                                     large_point_sites,
                                     input_data)
  }

  # ---- Combine all sites to get the main df ----
  NAEI_clean <-
    bind_rows(large_point_sites,
              aggregated_small_point_sites,
              aggregated_non_point_sites)


  # sort final data
  input_data$NAEI_clean <- tidy_sites_data(NAEI_clean, input_data)

  return(input_data)

}


#' Add required variables to site data
#'
#' Appends columns to the NAEI data, including new sector mapping, longitude
#'  and latitude, pipe distances and emissions variables.
#'
#' @inheritParams process_sites
#'
#' @return dataframe of NAEI data with additional columns added including
#'  * H2 grid connection year
#'  * Longitude
#'  * Latitude
#'  * H2_point (cluster)
#'  * Lat_clust (cluster center coordinates)
#'  * Lon_clust (cluster center coordinates)
#'  * pipe_dist (pipe distance to cluster center in kilometers)
#'  * emissions_MtCO2 (site emissions in Megatons)
#'  * sector_share_by_traded (proportion of sector by traded_status emissions, see
#'  [`calculate_share_of_sector_emissions`] for more information).
#'
#' @export
append_information_to_sites <- function(input_data){
  NAEI_data_2023 <- left_join(input_data$NAEI_df_clean_2023_revised,
                              input_data$new_sector_mapping,
                              by = c("IPM_sector" = "sector_NAEI")) %>%
    select(!IPM_sector) %>% # drop the original vers for new mapping vers
    rename(IPM_sector = 'IPM_sector.y',
           emissions_tons_CO2 = Emissions_tco2e) %>%  # renaming for consistency and readability
    mutate(grid_connection_year = 10000) # set to very high value if not in use (>>2050 )
    if(input_data$model_parameters$use_H2_grid_site) {
      # add in the grid availability year data if this functionality is in use
        NAEI_data_2023 <- NAEI_data_2023 %>% left_join(input_data$H2_grid_start,by = c( "PlantID" = "naei_plant_id")) %>%
        select(-grid_connection_year) %>% rename(grid_connection_year = 'availability_final_year')
    }

  NAEI_data_2023 %<>%
    calculate_coordinates() %>%
    allocate_cluster_points(input_data) %>%
    calculate_pipe_distances() %>%
    mutate(emissions_MtCO2 = emissions_tons_CO2/1000000) %>% # convert to mega tons
    do_if_using_traded_share_calc(., calculate_share_of_sector_emissions,
                                  input_data, pass_input_data = TRUE)

  return(NAEI_data_2023)
}



#---- Get aggregated non point sites ----

non_point_sites_from_site_data <- function(df, large_point_sites, input_data) {

  aggregated_non_point_sites <-
    get_non_point_sites_by_sector(df, input_data) %>%
    allocate_emissions_to_non_point_sites_by_sector()

  aggregated_non_point_sites_long <-
    lengthen_aggregated_non_point_sites(aggregated_non_point_sites, input_data)

  number_of_non_point_sites <- get_number_of_non_point_sites(df, input_data)

  # get the non_point_sites
  in_cluster_non_point_sites <-
    generate_non_point_sites(large_point_sites,
                             aggregated_non_point_sites_long,
                             number_of_non_point_sites,
                             input_data,
                             inside = TRUE)

  out_cluster_non_point_sites <-
    generate_non_point_sites(large_point_sites,
                             aggregated_non_point_sites_long,
                             number_of_non_point_sites,
                             input_data,
                             inside = FALSE)

  non_point_sites_combined <- rbind(in_cluster_non_point_sites,
                                    out_cluster_non_point_sites)

  single_non_point_sites <- get_single_non_point_sites(aggregated_non_point_sites,
                                                       number_of_non_point_sites,
                                                       input_data)

  # use parameter settings to choose which non_point_sites to use
  if (input_data$model_parameters$Two_nps_sites) {
    aggregated_non_point_sites_final = non_point_sites_combined
  } else {
    aggregated_non_point_sites_final = single_non_point_sites
  }

  return(aggregated_non_point_sites_final)
}


#' Produce a table for artificial sites that represent the non-point sources
#'
#' Performs a series of calculations to impute the relevant details so that
#'  the sites are represented as accurately as possible.
#'
#' @inheritParams get_large_point_sites
#'
#' @return dataframe with one row per artificial site, representing the
#'  non-point source sites for modelling purposes. There should be as many sites
#'  as sectors * used clusters when Two_nps_sites is set to FALSE, and twice this
#'  amount otherwise. Columns are:
#'  * site_name
#'  * IPM_sector
#'  * H2_point
#'  * total_MtCO2 (emissions)
#'  * pipe_dist
#'  * num_sites
#'  * Latitude
#'  * Longitude
#'  * traded_flag
#'
#' @export
non_point_sites_from_ratios <- function(df, input_data) {

  aggregated_non_point_sites <-
    get_sector_emission_totals(df, input_data) %>%
    filter(traded_flag == "Non-traded") %>% # remove duplicates
    calculate_non_point_site_emissions(input_data) %>%
    get_non_point_site_values_for_ratios(., input_data)

  return(aggregated_non_point_sites)
}


#' Convert easting and northing values into longitude and latitude
#'
#' @param df dataframe with columns 'Easting' and 'Northing' to be converted.
#'
#' @return dataframe as input with Longitude and Latitude columns output.
#' @export
calculate_coordinates <- function(df){

  ps_coords <- df %>%
    st_as_sf(coords = c("Easting", "Northing"), crs = 27700) %>%
    st_transform(4326) %>%
    st_coordinates() %>%
    as_tibble() %>%
    rename(Longitude = "X", Latitude = "Y")

  # adding coordinates back to original df
  df <- mutate(df, ps_coords)

  return(df)
}



# ---- Allocate cluster point

#' Classify coordinates to closest cluster point
#'
#' For any set of coordinates find the closest cluster and it's respective
#' center point coordinates.
#'
#' @param df dataframe containing the Longitude and Latitude for a site.
#' @inheritParams process_sites
#'
#' @return original dataframe with columns appended for cluster classification
#' and the coordinates of the cluster center.
#'
#' @export
allocate_cluster_points <- function(df, input_data) {

  # Get df of just lat and long of sites
  ps_sites_coord <- df %>%
    select(Longitude, Latitude)

  cluster_location <- get_cluster_locations(input_data)

  for (i in 1:nrow(ps_sites_coord)){

    # find the nearest cluster point
    new_clust <- which.min(distHaversine(ps_sites_coord[i, ],
                                         select(cluster_location,
                                                "Lon_clust",
                                                "Lat_clust")))

    # add cluster point into data frame
    df[i, "H2_point"] <- cluster_location[new_clust,"Cluster"]
    df[i, "Lat_clust"] <- cluster_location[new_clust,"Lat_clust"]
    df[i, "Lon_clust"] <- cluster_location[new_clust,"Lon_clust"]

  }
  return(df)

}



#' Helper function to call and tidy cluster locations
#'
#' @inheritParams process_sites
#'
#' @return dataframe with one row per cluster, with variables 'Cluster',
#'  'Lat_clust' (cluster latitude) and 'Lon_clust' (cluster longitude).
#' @export
get_cluster_locations <- function(input_data) {

  cluster_location <- input_data$Cluster_location %>%
    filter(use_cluster == "TRUE") %>%
    rename(Lat_clust = "Latitude", Lon_clust = "Longitude")

  return(cluster_location)
}


#' Get the pipe distance from site to cluster center
#'
#' Calculates the havershine distance between two set of coordinates - the sites
#'   coordinates and the coordinates for the center of the nearest cluster.
#'
#' @param df dataframe containing coordinates of point (Longitude and Latidude)
#'   as well as coordinates of closest cluster center (Lon_clust and Lat_clust).
#'
#' @return input dataframe with pipe_dist column appended which contains the
#'  pipe distances in kilometers.
#'
#' @export
calculate_pipe_distances <- function(df) {

  df <- df %>%
    mutate(pipe_dist = distHaversine(
      select(df, Longitude, Latitude),
      select(df, Lon_clust, Lat_clust)
    ) / 1000)

  return(df)
}


#' Perform a given function only when traded share calculation parameter is true
#'
#' This function takes a function as an argument and runs it on a df when
#'  the Traded_share_calc is set to TRUE, otherwise it just returns the df with
#'  no change. This allows for a more succint implementation of the conditions
#'  into the workflow.
#'
#' @param df dataframe to perform function upon
#' @param f function to be carries out if traded_share_calc is TRUE
#' @param pass_input_data boolean, whether input_data has been passed.
#' @inheritParams process_sites
#'
#' @return dataframe, after function applied when traded_share_calc is TRUE, or
#'  identical to input otherwise.
#' @export
do_if_using_traded_share_calc <- function(df, f, input_data,
                                          pass_input_data = FALSE) {
  if (input_data$model_parameters$Traded_share_calc) {
    df <- {if(pass_input_data) f(df, input_data) else f(df)}
  }
  return(df)
}


#' Get the proportion of emissions contributed by a site to its sector and
#' traded status combination
#'
#' @param df dataframe of the site (NAEI) data, containing sector, traded_status
#'  emissions variables.
#' @inheritParams process_sites
#'
#' @return input data with sector_share_by_traded (as well as some columns used
#'  to calculate this) appended, which is the proportion of emissions from the
#'  site to the sum of emissions from the sites sector split and traded status.
#' @export
calculate_share_of_sector_emissions <- function(df, input_data){

  # calculate emissions by sector
  point_sites_emissions <- get_sector_emission_totals(df, input_data)

  # Now append to df and do required calculation
  df %<>%
    left_join(point_sites_emissions, by = c('IPM_sector', 'traded_flag')) %>%
    mutate(sector_share_by_traded = emissions_MtCO2 / sector_emissions_from_point_sites)

  return(df)
}


#' Produce emissions figures by sector and traded status
#'
#' Get total emissions by sector and traded/non-traded split, in order to allow
#' allocation of emissions later.
#'
#' @inheritParams sum_emissions_by_sector
#' @inheritParams process_sites
#'
#' @return dataframe, with two rows per sector (one for each traded_status).
#'  The output columns are the same as that from [`sum_emissions_by_sector`]
#'  with additional columns added, including point_sites_emissions_from_share
#'  which gives the total emissions for point sites in the respective sector and
#'  traded status combination.
#'
#' @export
get_sector_emission_totals <- function(df, input_data) {
  # From point sites only
  point_sites_emissions <- sum_emissions_by_sector(df) %>%
    rename(sector_emissions_from_point_sites = sector_emissions)

  point_sites_emissions %<>%
    left_join(input_data$traded_share, by = 'IPM_sector') %>%
    left_join(input_data$Emissions, by = 'IPM_sector')

  point_sites_emissions %<>%
    mutate(
      point_sites_emissions_from_share = case_when(
        traded_flag == 'Traded' ~
          traded_share * Total_emissions,
        traded_flag == 'Non-traded' ~
          non_traded_point_share * Total_emissions
      )
   )

  grid_year = df %>% group_by(IPM_sector, traded_flag) %>%
    summarise(grid_connection_year_grouped = mean(grid_connection_year, na.rm = T))

  point_sites_emissions %<>%
    left_join(grid_year, by = c('IPM_sector','traded_flag')) %>%
    mutate(grid_connection_year_grouped = round(grid_connection_year_grouped))


   return(point_sites_emissions)
}



#' Get total traded and non-traded emissions in each sector
#'
#' Helper function to [get_sector_emission_totals()]
#'
#' @param df dataframe containing columns for IPM_sector, traded_flag and
#'  emissions_MtCO2.
#'
#' @return dataframe with one row per sector and traded status combination and
#'  the following additional columns:
#'
#'  * sector_emissions - total amount of emissions for each sector by traded and
#'    non-traded status (emissions per row)
#'
#'  * total_point_sites_emissions_in_sector -  the total amount of emissions for
#'    the sector overall (not split  by traded and non-traded).
#'
#' @export
sum_emissions_by_sector <- function(df){

  emissions_by_sector <- df %>%
    group_by(IPM_sector, traded_flag) %>% # also group by traded/non-traded
    summarise(sector_emissions = sum(emissions_MtCO2)) %>%
    group_by(IPM_sector) %>%
    mutate(total_point_sites_emissions_in_sector = sum(sector_emissions) # total within group
  ) %>%
    ungroup()

  return(emissions_by_sector)
}


#' Create table for large point sites
#'
#' Filter for the large point sites and then get emissions and drop columns that
#'  are no longer required.
#'
#' @param dataframe containing the NAEI data after processed by
#'  [`append_information_to_sites()`]
#' @inheritParams process_sites
#'
#' @return input dataframe now manipulated and reduced to the following columns,
#'  for large point sites only:
#'  * site_name
#'  * IPM_sector
#'  * H2_point
#'  * total_MtCO2 (emissions)
#'  * pipe_dist
#'  * num_sites
#'  * Latitude
#'  * Longitude
#'  * traded_flag
#' @export
get_large_point_sites <- function(df, input_data) {

  large_point_sites <- df %>%
    filter(!get_small_point_sites_filter(df)) %>%
    do_if_using_traded_share_calc(., get_emissions_from_share,
                                  input_data) %>%
    mutate(num_sites = 1) %>%
    rename(site_name = Operator,
           total_MtCO2 = emissions_MtCO2) %>%
    select_cols_for_sites_data()

  return(large_point_sites)
}



#' Create table for small point sites
#'
#' Aggregate all small point sites that are in the same cluster, distance from
#'  the center of the cluster (in or out of range) and sector. Note that these
#'  are point sites (from NAEI data source) but are non-traded and have
#'  smaller amounts of emissions. Therefore, we group them into single points
#'  to reduce the computational burden of the model. This function does such
#'  grouping, including required imputation of coordinates and returns the table
#'  of the artificial aggregate sites.
#'
#' @inheritParams process_sites
#'
#' @return input dataframe now manipulated and reduced to the following columns,
#'  for small point sites only:
#'  * site_name
#'  * IPM_sector
#'  * H2_point
#'  * total_MtCO2 (emissions)
#'  * pipe_dist
#'  * num_sites
#'  * Latitude
#'  * Longitude
#'  * traded_flag
#' @export
get_aggregated_small_point_sites <- function(df, input_data) {

  small_point_sites_grouped <- get_small_point_site_data(df, input_data) %>%
    aggregate_small_point_sites(., input_data)

  new_small_loc <- impute_site_location(small_point_sites_grouped, 'pipe_dist')

  # Some last tidying up
  small_point_sites_grouped %<>%
    unite("site_name",
          H2_point, IPM_sector, pipe_dist_category,
          remove = FALSE) %>%
    mutate(site_name = paste0(site_name, "_psg"), # meaning point_source_grouped
           Longitude = new_small_loc$lon,
           Latitude = new_small_loc$lat) %>%
    mutate(PlantID = NA) %>%
    select_cols_for_sites_data()

  return(small_point_sites_grouped)
}


#' Retrieve small point sites from NAEI data and tidy
#'
#' Filters for the small point sites from the NAEI data and then does some
#'  manipulation to get the required variables, inlcuding calculating emissions
#'  from share and determining whether a site is within the cluster radius.
#'
#' @inheritParams process_sites
#'
#' @return input dataframe filtered for small sites and with additional columns
#'  for emission and cluster data.
#' @export
get_small_point_site_data <- function(df, input_data) {

  small_point_sites <- df %>%
    filter(get_small_point_sites_filter(df)) %>%
    left_join(input_data$cluster_radius, by = c("H2_point" = "cluster")) %>%
    mutate(in_cluster_H2 = pipe_dist <= cluster_radius_H2) %>%
    do_if_using_traded_share_calc(., get_emissions_from_share, input_data) %>%
    mutate(pipe_dist_category = cut(pipe_dist,
                                    breaks=seq(0,500,by=500),
                                    right = F))

  return(small_point_sites)
}


#' Combines small point sites based on there characteristics and calculates
#'  information for the sites at the aggregate level
#'
#' The sites are grouped by [`group_small_point_sites()`]. Calculations are then
#'  made to combine the site level data into the aggregate level.
#'
#' @param df dataframe for small point sites produced by
#'   [`get_small_point_site_data()`]
#' @inheritParams process_sites
#'
#' @return dataframe of the aggregated sites, with the following columns:
#'  * num_sites - the number of sites that were aggregated to form the new row
#'  * pipe_dist - the mean pipe distances from the aggregated sites
#'  * total_MtCO2 - the sum of the emissions from the aggregated sites
#'  * pipe_dist_category - the category for the pipe distance that the aggregated
#'   site lies within.
#' @export
aggregate_small_point_sites <- function(df, input_data) {
  # get cluster location
  cluster_location <- get_cluster_locations(input_data)

  small_point_sites_grouped <- df %>%
    group_small_point_sites(input_data) %>%
    summarise(
      num_sites = n(),
      pipe_dist = mean(pipe_dist),
      total_MtCO2 = sum(emissions_MtCO2),
      grid_connection_year = ifelse(all(is.na(grid_connection_year)),NA,mean(grid_connection_year, na.rm=T))) %>%
    mutate(pipe_dist_category = str_replace_all(
      as.character(pipe_dist_category),
      c("\\[" = "",
        "\\)" = "",
        "," = "_")
    )) %>%
    mutate(grid_connection_year = round(grid_connection_year)) %>%
    left_join(cluster_location, by = c("H2_point" = "Cluster"))  %>%
    ungroup()

  return(small_point_sites_grouped)

}


#' Generate a new set of coordinates for the aggregate sites
#'
#' Imputes the locations for the artificial sites, by taking the mean pipe
#'   distance as input as a radius and finding a location randomly on the circle
#'   formed by the radius around the coordinates of the cluster center.
#'
#' @param df dataframe with as many rows as aggregate sites to impute locations
#'  for. Needs to include variables for the coordinates of the cluster center
#'  (Lon_clust & Lat_clust) as well as a variable provided as the other parameter
#'  for pipe distance.
#' @param var string for the name of the column which is to be used as the
#'  distance from the cluster centers. Normally this will be the mean pipe
#'  distance of the sites that formed the aggregate.
#'
#' @return a dataframe with as many rows as aggregate sites in the input data,
#'  with a column each for the new longitudes (lon) and latitudes (lat).
#' @export
impute_site_location <- function(df, var) {

  set.seed(423)

  new_locations <-
    as.data.frame(
      destPoint(
        df %>% select(Lon_clust, Lat_clust),
        runif(1, min = 0, max = 360),
        df[[var]]
      )
    )

  return(new_locations)
}



#' Produce variable to flag small point sites
#'
#' Sites are determined as small if they are both non-traded and their emissions
#'  are below a threshold specified elsewhere. This is done so that small sites
#'  can be aggregated to reduce the complexity of the model and increase its speed.
#'
#' @inheritParams get_large_point_sites
#'
#' @return list of logical, TRUE when sites should be considered as small. Should
#'  be same length as input dataframe.
#' @export
get_small_point_sites_filter <- function(df) {

  small_point_site_filter <- (
    (df$emissions_MtCO2 <= point_site_emissions_cut_off)
    & (df$traded_flag == 'Non-traded')
  )

  return(small_point_site_filter)
}


#' Groups the small point sites data conditionally on Two_nps_sites parameter
#'
#' When Two_nps_sites parameter is FALSE, the data is grouped by sector, cluster
#'  and pipe distance category (in or out of cluster). If the parameter is TRUE
#'  then the data is also grouped by in_cluster_H2.
#'
#' @inheritParams aggregate_small_point_sites
#'
#' @return input dataframe now grouped as required
#' @export
group_small_point_sites <- function(df, input_data){

  if (input_data$model_parameters$Two_nps_sites){
    df %<>% group_by(IPM_sector, H2_point, pipe_dist_category, traded_flag,
                     in_cluster_H2) # include traded flag just to keep label, not for aggregation
  } else {
    df %<>% group_by(IPM_sector, H2_point, pipe_dist_category, traded_flag)
  }
  return(df)
}



#' Apportion emissions to traded and non-traded point sites
#'
#' The emissions apportioned are based on the site's proportion contributed to the
#'  sector and traded_status combination overall multiplied by the total emissions
#'  for the given sector and traded status.
#'
#' @param df dataframe containing columns sector_share_by_traded and
#'  point_sites_emissions_from_share.
#'
#' @return The input dataframe with the column emissions_MtCO2 appended.
#' @export
get_emissions_from_share <- function(df) {
  df %<>%
    mutate(emissions_MtCO2 =
             sector_share_by_traded * point_sites_emissions_from_share)

  return(df)
}



#' Helper function - avoids repeating the same column select for small and large
#' point sites.
select_cols_for_sites_data <- function(df) {
  df %<>% select(
    site_name, IPM_sector, H2_point, total_MtCO2, pipe_dist,
    num_sites, Latitude, Longitude, traded_flag,grid_connection_year,PlantID)
}


# ---- Non point source sites ----

get_non_point_sites_by_sector <- function(df, input_data) {

  non_point_sites <- get_non_point_sites(input_data)

  sector_emissions_totals <- get_sector_emission_totals(df, input_data) %>%
    filter(traded_flag == "Non-traded") # removes duplicates

  non_point_sites_per_sector <- non_point_sites %>%
    group_by(IPM_sector) %>%
    select(all_of(regions)) %>%
    summarise_all(sum) %>% # add number of businesses per sector for all regions
    mutate(num_non_point_sites = rowSums(.[2:13])) %>% # add up all nps sites
    left_join(sector_emissions_totals, by = "IPM_sector")

  # Calculate emissions using appropriate calculation based on parameter settings
  non_point_sites_per_sector %<>%
    calculate_non_point_site_emissions(input_data) %>%
    filter(!is.na(IPM_sector), IPM_sector != 'Not applicable')


  return(non_point_sites_per_sector)
}


#' Produce the initial table for non point source sites
#'
#' Finds the number of non point source sites (i.e. sites not in NAEI data) that
#'  are in each combination of region and sic codes.
#'
#' @inheritParams process_sites
#'
#' @return dataframe containing 1 row per sector. Columns are sic code, sector
#'  classification and then a column for each region. Values are the number
#'  sites in each region of the given sic code.
#' @export
get_non_point_sites <- function(input_data) {

  # rename regions
  df_non_point_sites <- input_data[['nps_sites']] %>%
    rename(all_of(regions_lookup)) %>%
    mutate(sic_code_4_digit = as.numeric(str_sub(SIC, 1, 4))) %>%
    select(all_of(regions), sic_code_4_digit)

  # map IPM sectors to the ONS data
  ONS_sector_mapping <- left_join(input_data[['ONS_sector_mapping']],
                                  input_data[['GHGI_sector_mapping']],
                                  by = c("SIC" = "SIC code")) %>%
    select(sic_code_4_digit = "4_digit_SIC_code", IPM_sector = 'COMIT Sectors')

  df_non_point_sites %<>%
    left_join(ONS_sector_mapping, by = 'sic_code_4_digit') %>%
    select(sic_code_4_digit, IPM_sector, all_of(regions))

  return(df_non_point_sites)
}



#' Helper function to calculate non_point_site_emissions using traded_share
#' only when parameter allows.
calculate_non_point_site_emissions <- function(df, input_data) {

  df %<>%
    mutate(non_point_site_emissions_MtCO2 = {
      if (input_data$model_parameters$Traded_share_calc) {
        (non_point_share
         * Total_emissions)
      } else {
        Total_emissions - sector_emissions_from_all_sites #Note is this worth keeping?
      }
    })

  return(df)

}


allocate_emissions_to_non_point_sites_by_sector <- function(df) {

  non_point_site_emissions_by_sector <-
    map(regions, regional_non_point_site_shares, df) %>%
    set_names(paste0(regions, '_emissions_MtCO2')) %>%
    bind_cols(df, .) %>%
    select(IPM_sector, all_of(paste0(regions, '_emissions_MtCO2'))) %>%
    filter(IPM_sector != "not included")

  return(non_point_site_emissions_by_sector)
}


#' Helper function to calculate the amount of emissions from non-point-sites
#' for each sector in each region
regional_non_point_site_shares <- function(region, df) {
  return((df[[region]] / df[["num_non_point_sites"]]) *
           df[["non_point_site_emissions_MtCO2"]])
}


# Non-point sites continued ----------------------------------------------------

get_number_of_non_point_sites <- function(df, input_data){

  number_of_non_point_sites <-
    get_non_point_sites_by_sector(df, input_data) %>%
    select(IPM_sector, all_of(regions)) %>%
    filter(IPM_sector != 'not included') %>%
    pivot_longer(names_to = 'site_region',
                 values_to = 'number_of_sites',
                 all_of(regions)) %>%
    unite('non_point_site_name', site_region, IPM_sector, remove = TRUE)

  return(number_of_non_point_sites)
}


# NOTE: think about renaming
lengthen_aggregated_non_point_sites <- function(df, input_data) {

  aggregated_non_point_sites_long <- df %>%
    pivot_longer(names_to = 'site_region', values_to = 'Emissions_MtCO2',
                 all_of(paste0(regions, '_emissions_MtCO2'))) %>%
    mutate(site_region = str_remove(site_region, '_emissions_MtCO2')) %>%
    unite('non_point_site_name', site_region, IPM_sector, remove = FALSE) %>%
    filter(Emissions_MtCO2 != 0) %>%
    left_join(input_data$nps_loc_mapping,
              by = c('site_region' = 'nps_site_region')) %>%
    left_join(get_cluster_locations(input_data), by = c('H2_point' = 'Cluster')) %>%
    select(-Latitude, -Longitude)

  return(aggregated_non_point_sites_long)
}


# Use info from large point sites to get info for small ones -------------------

# boundaried refers to the fact we are spiltting by inside/outside a cluster
# zone.

# Now use info from large point sites to get info for small ones.
generate_non_point_sites <- function(df,
                                     long_non_point_sites_df,
                                     number_of_non_point_sites,
                                     input_data,
                                     inside = TRUE) {

  # check we have at least some sites inside/outside cluster, if not return empty
  # and skip the rest
  df_sites_by_cluster_and_boundary <- summarise_sites_in_boundary(df,
                                                                  input_data,
                                                                  inside)
  sites_available <- nrow(df_sites_by_cluster_and_boundary) > 0

  if(sites_available == FALSE) {
    return(data.frame())
  }

  boundaried_cluster_non_point_site_details <-
    impute_site_details_from(df, input_data, inside)

  boundaried_cluster_non_point_sites <-
    left_join(
      long_non_point_sites_df,
      inside_cluster_proportions(df, input_data, inside), # props by sector
      by = c("IPM_sector")
    ) %>%
    select(-boundaried_mean_pipe_dist) %>%
    left_join(boundaried_cluster_non_point_site_details,
              by = c('H2_point'))

  boundaried_cluster_non_point_sites %<>%
    mutate(Emissions_MtCO2 = Emissions_MtCO2 * boundaried_sites_proportion) %>%
    left_join(number_of_non_point_sites, by = 'non_point_site_name') %>%
    mutate(number_of_sites = round(number_of_sites * boundaried_sites_proportion)) %>%
    select(-boundaried_emissions_proportion,
           -boundaried_sites_proportion) %>%
    rename(pipe_dist = mean_pipe_dist) %>%
    tidy_non_point_sites()

  return(boundaried_cluster_non_point_sites)
}


impute_site_details_from <- function(df, input_data, inside = TRUE) {

  df_sites_by_cluster_and_boundary <- summarise_sites_in_boundary(df, input_data, inside)

  # Generate new point source site locations
  location_for_non_point_sites <-
    impute_site_location(df_sites_by_cluster_and_boundary, 'mean_pipe_dist')

  imputed_non_point_site_details <- df_sites_by_cluster_and_boundary %>%
    mutate(Longitude = location_for_non_point_sites$lon,
           Latitude = location_for_non_point_sites$lat) %>%
    select(H2_point, mean_pipe_dist, Latitude, Longitude)

  return(imputed_non_point_site_details)
}


summarise_sites_in_boundary <- function(df, input_data, inside = TRUE){

  df_sites_by_cluster_and_boundary <- df %>%
    filter_on_cluster_boundary(., input_data, inside) %>%
    summarise_sites_by('H2_point', ., include_pipe_dist = TRUE) %>%
    left_join(get_cluster_locations(input_data), by = c('H2_point' = 'Cluster'))

  return(df_sites_by_cluster_and_boundary)
}


# get props for each inside/outside cluster by sector
inside_cluster_proportions <- function(df, input_data, inside = TRUE){

  # first get overall site info
  large_point_sites_by_sector <- summarise_sites_by('IPM_sector', df)


  # get proportion of sites inside and outside of the boundary
  inside_boundary <- filter_on_cluster_boundary(df, input_data, TRUE) %>%
    mutate(boundary = 'in')
  outside_boundary <- filter_on_cluster_boundary(df, input_data, FALSE) %>%
    mutate(boundary = 'out')

  in_and_out <- rbind(inside_boundary, outside_boundary)

  in_or_out <- in_and_out %>%
    group_by(boundary) %>% summarise(
    num_sites = sum(num_sites),
    sum_emissions = sum(total_MtCO2),
    mean_pipe_dist = mean(pipe_dist)
    ) %>%
    mutate(prop_sites = num_sites/sum(num_sites),
           prop_emissions = sum_emissions/sum(sum_emissions)) %>%
    filter(boundary == {if (inside) 'in' else 'out'})

  # get the sectors that need imputation
  large_point_sites_to_impute <-
    filter(large_point_sites_by_sector, number_of_sites < 10) %>%
    pull(IPM_sector)

  # impute info for sites without large point sites
  sectors <- input_data$traded_share$IPM_sector

  imputed_sectors_inside <- data.frame(
    IPM_sector = sectors,
    boundaried_emissions_proportion =
      in_or_out$prop_emissions,
    boundaried_sites_proportion =
      in_or_out$prop_sites,
    boundaried_mean_pipe_dist = in_or_out$mean_pipe_dist
  )


  # now breakdown based on whether inside/outside site
  cluster_large_point_sites_by_sector <-
    filter_on_cluster_boundary(df, input_data, inside) %>%
    summarise_sites_by('IPM_sector', ., include_pipe_dist = TRUE)

  cluster_large_point_sites_by_sector %<>%
    filter(!IPM_sector %in% large_point_sites_to_impute) %>%
    left_join(large_point_sites_by_sector, by = c('IPM_sector')) %>%
    mutate(boundaried_emissions_proportion = emissions.x/emissions.y,
           boundaried_sites_proportion = number_of_sites.x/number_of_sites.y) %>%
    select(IPM_sector,
           boundaried_mean_pipe_dist = mean_pipe_dist,
           boundaried_emissions_proportion,
           boundaried_sites_proportion)

  # join the imputed sectors
  cluster_large_point_sites_by_sector %<>%
    rbind(
      filter(imputed_sectors_inside, IPM_sector %in% large_point_sites_to_impute)
    )

  return(cluster_large_point_sites_by_sector)
}



#' Get total number of emissions and total number of sites by a given grouping
#' variable. E.g. by passing the data for large point sites, and a sector
#' variable for grouping, the total emissions and a total number of sites by
#' sector are calculated.
summarise_sites_by <- function(group, df, include_pipe_dist = FALSE) {

  summary_table <- df %>%
    group_by(df[[group]]) %>%
    summarise(
      emissions = sum(total_MtCO2),
      number_of_sites = n(),
      mean_pipe_dist = mean(pipe_dist)
    ) %>%
    ungroup() %>%
    {if (include_pipe_dist) . else select(., -mean_pipe_dist)}


  colnames(summary_table)[1] <- group

  return(summary_table)
}


#' Joins cluster radius parameter for given cluster and then filters the df
#' based on whether inside or outside of cluster is required.
filter_on_cluster_boundary <- function(df, input_data, inside = TRUE) {

  df %<>% left_join(input_data$cluster_radius, by = c('H2_point' = 'cluster')) %>%
    {
      if (inside)
        filter(., pipe_dist < cluster_radius_H2)
      else
        filter(., pipe_dist >= cluster_radius_H2)
    }

  return(df)
}


# Get single non_point_sites - don't split by in/out of cluster ----------------

get_single_non_point_sites <- function(df,
                                       number_of_non_point_sites,
                                       input_data) {

  single_non_point_sites <- df

  # rename to get region only
  names(single_non_point_sites) <- str_remove(names(single_non_point_sites),
                                              '_emissions_MtCO2')

  single_non_point_sites %<>%
    pivot_longer(names_to = 'non_point_site_region',
                 values_to = 'Emissions_MtCO2',
                 cols = all_of(regions)) %>%
    unite(col = 'non_point_site_name',
          non_point_site_region,
          IPM_sector,
          remove = FALSE)

  single_non_point_sites %<>%
    filter(Emissions_MtCO2 != 0) %>%
    left_join(input_data$nps_loc_mapping,
              by = c('non_point_site_region' = 'nps_site_region')) %>%
    left_join(get_cluster_locations(input_data), by = c('H2_point' = 'Cluster')) %>%
    calculate_pipe_distances() %>%
    left_join(number_of_non_point_sites, by = 'non_point_site_name') %>%
    tidy_non_point_sites()

  return(single_non_point_sites)

}


#' Helper function to get non-point sites data in the correct format
tidy_non_point_sites <- function(df) {

  tidied_non_point_sites <- df %>%
    mutate(site_name = paste0(non_point_site_name, '_npsg'),
           traded_flag = 'Non-traded-non-point') %>%
    rename(total_MtCO2 = Emissions_MtCO2, num_sites = number_of_sites) %>%
    mutate(PlantID = NA) %>%
    select_cols_for_sites_data()

  return(tidied_non_point_sites)
}


#### For when using IDBR ratios instead ----------------------------------------

# Used in process_site_ratios
#' Produce a table containing imputed site data for non-point sites
#'
#' Creates the artificial sites used to model the non-point sites data,
#'  imputing the site characteristics from the data we have available from
#'  point sites and IDBR.
#'
#' @param df dataframe produced in [non_point_sites_from_ratios()] after
#'  running [get_sector_emission_totals()] and filtering for non-traded sites.
#' @inheritParams process_sites
#'
#' @return dataframe with a row for each of the artificial sites that represent
#'  the aggregated non-point source sites. There should be as many sites as there
#'  are combinations of sectors and clusters (minus the ones not used) - multiplied
#'  by two if the Two_nps_sites parameter is set to TRUE. Columns include:
#'  * total_MtCO2 (imputed emissions)
#'  * pipe_dist (imputed pipe distance)
#'  * Latitude and Longitude (imputed site coordinates)
#' @export
get_non_point_site_values_for_ratios <- function(df, input_data) {

  cluster_ratios <- get_inside_vs_outside_cluster_ratios(input_data)

  cluster_sector_share <- get_proportion_of_emissions_from_sector(input_data)

  emissions_by_sector <- df %>%
    select(IPM_sector, non_point_site_emissions_MtCO2,grid_connection_year_grouped) %>%
    rename(grid_connection_year = grid_connection_year_grouped)

  non_point_site_sector_share <- cluster_sector_share %>%
    left_join(emissions_by_sector, by = "IPM_sector") %>%
    left_join(cluster_ratios, by = c("Cluster", "IPM_sector")) %>%
    left_join(input_data$NPS_pipe_dist, by = "Cluster") %>%
    left_join(get_cluster_locations(input_data), by = c("Cluster")) %>%
    mutate(non_point_site_total_cluster_sector_emissions =
             sector_share * non_point_site_emissions_MtCO2) %>%
    filter(use_cluster == TRUE) # filter out non used clusters

  non_point_site_sector_share %<>%
    impute_site_details_for_ratios(input_data)

  return(non_point_site_sector_share)
}


#' Produce a table for the proportion of sites inside the cluster boundary for
#'  each sector in each cluster
#'
#' This table can then be used to impute how many sites for each sector should
#'  be found within the cluster for non-point source sites.
#'
#' @inheritParams process_sites
#'
#' @return dataframe with columns for sector, cluster an in_ratio which gives
#'  the proportion of sites inside the cluster boundary for the given sector
#'  and cluster combination. There is one row for every cluster and sector
#'  combination.
#' @export
get_inside_vs_outside_cluster_ratios <- function(input_data) {

  cluster_ratios <- input_data$Cluster_ratios %>%
    pivot_longer(cols = -Cluster,
                 names_to = "IPM_sector",
                 values_to = "in_ratio")

  return(cluster_ratios)
}


#' Produce a table for the proportion of sites in each cluster which belong to
#'  the different sectors
#'
#' @inheritParams process_sites
#'
#' @return dataframe with columns for cluster, sector and sector_share which is
#'  the proportion of emissions belonging to the sector in each cluster. Should
#'  have one row per sector per cluster.
#' @export
get_proportion_of_emissions_from_sector <- function(input_data) {

  cluster_sector_share <- input_data$Cluster_sector_share %>%
    pivot_longer(cols = -Cluster,
                 names_to = "IPM_sector",
                 values_to = "sector_share")

  return(cluster_sector_share)
}



#' Assign values to aggregated non point source sites
#'
#' Gets the emissions and imputed locations of the artificial sites for the
#' non-point sources.
#'
#' @param df dataframe produce inside of [get_non_point_site_values_for_ratios()]
#'  function, that contains artificial sites for the non-point sources. One row
#'  per sector and cluster combination.
#' @inheritParams process_sites
#'
#' @return dataframe with all columns required for the final sites dataframe,
#'  including total_MtCO2 and pipe_dist. Returns twice as many rows as input
#'  df when Two_nps_sites is set to TRUE, else returns same number of rows as
#'  input df.
#' @export
impute_site_details_for_ratios <- function(df, input_data) {

  df %<>%
    {
      if (input_data$model_parameters$Two_nps_sites == TRUE)
        get_values_for_two_non_point_sites(.)
      else
        rename(., total_MtCO2 = non_point_site_total_cluster_sector_emissions) %>%
        mutate(., pipe_dist = out_cluster_distance)
    }


  site_loc <- impute_site_location(df, 'pipe_dist')

  df %<>%
    mutate(Longitude = site_loc$lon,
           Latitude = site_loc$lat,
           site_name = paste(Cluster, IPM_sector, "npsg", sep = "_"),
           num_sites = 1,
           traded_flag = 'Non-traded-non-point') %>%
    rename(H2_point = Cluster) %>%
    mutate(PlantID = NA)%>%
    select_cols_for_sites_data()

  return(df)
}


#' Transform the dataframe to account for two non-point sites in each cluster
#'  and sector combination
#'
#' Doubles the number of rows so that there are now two sites per sector and
#'  cluster combination - one for inside cluster sites and another for outside
#'  of cluster sites. The emissions and pipe distances are also set at that point
#'  based on whether the site is inside or outside of the cluster.
#'
#' @inheritParams impute_site_details_for_ratios
#'
#' @return dataframe with twice as many rows (sites) as the input dataframe.
#'  Columns 'total_MtCO2' (emissions), 'cluster_in_out' and 'pipe_dist' added.
#' @export
get_values_for_two_non_point_sites <- function(df) {

  df %<>%
    mutate(non_point_site_in_cluster_emissions =
             non_point_site_total_cluster_sector_emissions * in_ratio,
           non_point_site_out_cluster_emissions =
             non_point_site_total_cluster_sector_emissions * (1 - in_ratio))

  df %<>%
    pivot_longer(cols = c(non_point_site_in_cluster_emissions,
                          non_point_site_out_cluster_emissions),
                 names_to = 'cluster_in_out',
                 values_to = 'total_MtCO2') %>%
    mutate(pipe_dist = case_when(
      cluster_in_out == "non_point_site_in_cluster_emissions" ~ in_cluster_distance,
      cluster_in_out == "non_point_site_out_cluster_emissions" ~ out_cluster_distance))

  return(df)
}



#' Final tidying of the site data
#'
#' Adds variables for whether a site is within the H2 or CCS clusters, and
#' arranges the sites in alphabetical order and then by Latitude.
#'
#' @param df dataframe containing all sites to be modeled (usually the combination
#'  of large point source, small point source and non-point source sites), with
#'  the following columns:
#'  * site_name
#'  * IPM_sector
#'  * H2_point
#'  * total_MtCO2 (emissions)
#'  * pipe_dist
#'  * num_sites
#'  * Latitude
#'  * Longitude
#'  * traded_flag
#' @inheritParams process_sites
#'
#' @return reordered input dataframe with four new columns:
#'  * H2_first_year
#'  * CCS_first_year
#'  * in_cluster_H2 (whether site is within the H2 cluster radius)
#'  * in_cluster_CCS (whether site is within the CCS cluster radius)
#' @export
tidy_sites_data <- function(df, input_data) {

  df %<>%
    left_join(input_data$cluster_radius, by = c("H2_point" = "cluster")) %>%
    mutate(in_cluster_H2 = pipe_dist <= cluster_radius_H2,
           in_cluster_CCS = pipe_dist <= cluster_radius_CCS) %>%
    select(-c(cluster_radius_H2, cluster_radius_CCS)) %>%
    arrange(site_name, Latitude)

  return(df)
}
