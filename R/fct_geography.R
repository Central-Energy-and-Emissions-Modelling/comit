# Geography functions

#' Distance in kilometers between two points
#'
#' Uses the formula for the havershine distance to calculate the distance between
#' two points on the surface of earth. Earth's diameter is included as 12742km.
#' Note that the entries of arguments is backwards to the typical entries of
#' latitude and longitude, i.e. here longitude comes before latitude.
#'
#' Future development could remove the need for this function by instead using
#' `geosphere::distHaversine()` - this should be looked at.
#'
#' @param long1 numeric, longitude of point 1.
#' @param lat1 numeric, latitude of point 1.
#' @param long2 numeric, longitude of point 2.
#' @param lat2 numeric, latitude of point 2.
#'
#' @return numeric, distance between the input coordinates in kilometers.
hav.dist <- function(long1, lat1, long2, lat2) {

  p <- pi/180

  a <- (0.5 - cos((lat2 - lat1)*p)/2
        + cos(lat1*p) * cos(lat2*p) * (1-cos((long2 - long1)*p))/2)

  dist <- 12742 * asin(sqrt(a)) # 12742 is the diameter of the earth

  return(dist)
}



#' Get site regions
#'
#' Assign region based on site long and lat. This allows for dis-aggregation of
#'  regions for the sites in the outputs.
#'
#' @param data dataframe of site of data, containing columns: site_ID, site_name,
#' Latitude and Longitude.
#'
#' @return The original dataframe with region column appended.
assign_site_region <- function (data){

  # Add region variable
  site_data <- data %>%
    select(site_ID, Latitude, Longitude) %>%
    filter(!is.na(Latitude))

  out_data <- data %>%
    left_join(region_lookup(site_data), by = c('site_ID')) %>%
    # Some Peterhead sites not assigned (just outside boundary) - assign manually
    mutate(region = case_when(
      !is.na(region) ~ region,
      str_detect(site_name, '^Peterhead_') ~ 'Scotland'
    ))

  return(out_data)
}



#' Classify region of sites/clusters
#'
#' Assigns the GOR9D region to either sites or clusters of a dataframe based on
#'  its co-ordinates. This is done so that outputs can be dis-aggrageted by region.
#'
#' @param coord_data dataframe to classify, must include Longitude and Latitude.
#' @param link_to_cluster logical, use TRUE if linking to clusters rather than sites.
#'
#' @return A dataframe with classified regions, to be used as a lookup. This can
#'  then be joined on site_ID (or 'Cluster' if link_to_cluster = TRUE).
region_lookup <- function(coord_data, link_to_cluster = FALSE) {

  # Get shapefile for region boundaries - dowloaded from ONS website
  shape_file_path <- system.file(
    'extdata',
    'geography/NUTS_Level_1_January_2018_FEB_in_the_United_Kingdom.shp',
    package='comit')

  boundary <- st_read(
    shape_file_path,
    quiet = TRUE
  ) %>%
    st_transform(4326) # set coord reference system

  # convert site_data to sf object
  df_sf <- st_as_sf(coord_data, coords = c('Longitude', 'Latitude')) %>%
    st_set_crs(4326)

  # assign region if site lies within region polygon
  regions_assigned <- as.data.frame(st_contains(boundary, df_sf))

  # get region labels and create index to link on
  regions <- boundary$nuts118nm %>%
    as.data.frame() %>%
    rename(region = '.')

  regions$index <- as.numeric(rownames(regions))

  # produce look up
  regions_assigned %<>%
    left_join(regions, by = c('row.id' = 'index')) %>%
    select(site_ID = col.id, region)

  # account for different id when using cluster
  if (link_to_cluster) {
    regions_assigned %<>% arrange(site_ID)
    regions_assigned$Cluster <- df_sf$Cluster
    regions_assigned %<>% select(!site_ID)
  }

  return(regions_assigned)
}

