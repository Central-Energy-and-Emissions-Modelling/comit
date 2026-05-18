
#' Get names of time series tables to interpolate
#'
#' If any timeseries tables are added to the input spreadsheet, add the tab name
#'  to the list below to allow for additional interpolation.
#'
#' @return vector of characters, the names of tables that are to be interpolated.
tables_to_interpolate <- function() {

  # hardcoded list of the data tables that have yearly data that may require interpolation
  yearly_data_tables <- c(
    "Demand_drivers",
    "Non_industry_H2_demand",
    "Non_industry_CO2_demand",
    "resource_efficiency",
    "energy_efficiency",
    "Fuel_costs",
    "Fuel_emissions",
    "max_CCS",
    "emissions_limit",
    "Carbon_price",
    "max_fuel_constraints",
    "min_fuel_constraints",
    "CO2_storage",
    "maximum_capacity",
    "minimum_capacity",
    "H2_availability",
    'supply_chain_constraints'
  )

  return(yearly_data_tables)
}


#' Ammend time series data tables to include all modelled years
#'
#' Reduce or expand specified timeseries input datatables to ensure values are
#'  present for every modelled year. This allows for different timesteps to be
#'  used in the model without having to specify different years in the input
#'  spreadsheet.
#'
#' @param data list of dataframes containing the input data for the model.
#'
#' @return data list of dataframes. Tables specified by `tables_to_interpolate()`
#'  are interpolated whilst all other tables are left unchanged.
#'
#' @export
interpolate_data <- function(data) {

  years_for_interpolation <- get_years_for_interpolation(data)
  all_years <- years_for_interpolation[[1]]
  modelled_years <- years_for_interpolation[[2]]

  # get the names of tables to interpolate
  yearly_data_tables <- tables_to_interpolate()

  #raise message if any of the tables are not present
  missing_tables <- yearly_data_tables[!yearly_data_tables %in% names(data)]

  if (length(missing_tables) > 0) {
    message(paste(missing_tables, 'contraint table not present', collapse = '\n'))
  }

  # interpolate
  data <- mapply(
    FUN = interpolate_relevant_tabs,
    data, # arg to iterate 1
    names(data), # arg to iterate 2
    MoreArgs = list(
      yearly_data_tables,
      c('max_fuel_constraints', 'min_fuel_constraints'),
      all_years,
      modelled_years
    ),
    SIMPLIFY = FALSE # to return list
  )
  return(data)

}



#' Get the year vectors required for interpolation
#'
#' @inheritParams interpolate_data
#'
#' @returns list of two vectors, first element is all years in period,
#'  second element is the years to actually be modelled.
get_years_for_interpolation <- function(data) {

  start_year <- data$model_parameters$start_year
  end_year <- data$model_parameters$end_year

  # create a df with every year in order to use later when interpolating
  all_years <- data.frame(year = start_year:end_year)

  # create a df with the years we actually want to include in the model
  modelled_years <- data.frame(
    year = seq(from = start_year,
               to = end_year,
               by = data$model_parameters$timestep)
  )

  return(list(all_years, modelled_years))
}



#' Perform interpolation on a specified table
#'
#' Get the required values for each modeled year for a specific table from the
#'  input data. This function calls `interpolate_for_years()` and allows for
#'  the additional processing required for some of the fuel constraints.
#'
#' @param table dataframe, a specific table from the input data.
#' @param name character, the name of the table to be interpolated.
#' @param yearly_data_tables vector of characters
#' @param fuel_constraints vector of characters, the fuel constraints which
#'  require additional processing due to containing an initial row with a
#'  boolean setting.
#' @param all_years dataframe with a single column containing every year between
#'  the start and end years.
#' @param modelled_years dataframe with a single column containing only the years
#'  to be modelled.
#'
#' @return dataframe, the table with interpolation having been completed. All of
#'  the original columns remain and there will be one row per modelled year.
#'
#' @export
interpolate_relevant_tabs <- function(table,
                                      name,
                                      yearly_data_tables,
                                      fuel_constraints,
                                      all_years,
                                      modelled_years) {

  # deal with fuel constraints seperately first as these need a little work
  if(name %in% fuel_constraints) {

    first_row_fuel_constraint <- table[1, ]

    table <- table[-1,] %>%
      mutate(year = as.numeric(year))
    table <- interpolate_for_years(table, all_years, modelled_years)
    table <- rbind(first_row_fuel_constraint, table)
  } else if(name %in% yearly_data_tables) {

    table <- interpolate_for_years(table, all_years, modelled_years)

    }
  return(table)

}



#' Interpolate a dataframe to get values for all years to be modelled
#'
#' This function takes all years, makes values in between the years specified NA,
#'  interpolates and then returns a dataframe with only the modelled years present.
#'
#' @param df dataframe to be interpolated.
#' @inheritParams interpolate_relevant_tabs
#'
#' @return dataframe, with the same columns as input dataframe but now with
#'  one row per year to be modelled and interpolation carried out to fill all
#'  missing values.
#'
#' @export
interpolate_for_years <- function(df, all_years, modelled_years) {
  # extrapolate start or end year points if needed
  df <- extrapolate_for_years(df, all_years)

  # expand df to get every possible year and then interpolate
  df %<>%
    right_join(all_years, by = "year") %>%
    arrange(year) %>%
    interpolate_all_columns()

  # now reduce back to only the years we require
  df %<>% right_join(modelled_years, by = 'year')

  return(df)
}



#' Impute missing values of time series data
#'
#' Interpolate the missing values between known points of time series data. This
#'  is used to provide datapoints for years between the years specified in the
#'  inputs. For example, input data is often specified at 5 year intervals and
#'  this function imputes the years between those specified to provide
#'  yearly estimates.
#'
#' All interpolation is linear. Only missing values are interpolated, if there
#'  is no missing data then the input data will remain unchanged.
#'
#' @param df dataframe for the table to be interpolated, with one row per year
#'  for every year between the start and end years. The first column is the
#'  year column and all following columns are interpolated. This df is created
#'  inside the `interpolate_for_years()` function.
#'
#' @return dataframe as provided as df argument, but with all missing values
#'  filled by interpolation.
interpolate_all_columns <- function(df) {

  n_timesteps  <- nrow(df)

  for (i in seq_along(df)) {

    # trigger warnings if there is a missing value at start or end
    if(is.na(df[1, i])) {
      warning('Missing start value when interpolating table. \nThis will spread ',
              'other values and gives an unexpected start value.')
    }

    if(is.na(df[n_timesteps, i])) {
      warning('Missing end value when interpolating table. \nThis will spread ',
              'other values and gives an unexpected end value.')
    }

    df[[i]] <- approx(df[[1]], df[[i]], n = n_timesteps)$y
  }

  return (df)
}





#' Extrapolate missing start and/or end values of time series data
#'
#' Extrapolates the missing end values outside of known points of time series
#'  data. If either the model start year and/or end year are missing from
#'  the time series, this function extrapolates these values.
#'  For example, input data is often specified only to 2050, and
#'  this function extrapolates to 2051 values to allow the model to run
#'  with a 2051 end year. It also covers the case where the model start year
#'  is 2021 and the time series includes values for 2020 but not 2021,
#'
#' All extrapolation is linear. Only missing start/end year values are
#'  extrapolated. Separate interpolation functions are used to fill other
#'  missing values in the time series data.
#'
#'
#' @param df dataframe for the table to be extrapolated, with one row per year
#'  for every year between the start and end years. The first column is the
#'  year column and all following columns are extrapolated.
#'
#' @param extrapolated_years a list of the years included in the time series
#' data plus the additional years that need to be extrapolated
#'
#' @param new_years_to_add either 1 or 2, depending on whether either start year
#' and/or end year need to extrapolated
#'
#' @return dataframe as provided as df argument, but with missing end values
#'  filled by extrapolation.
extrapolate_all_columns <- function(df,extrapolated_years,new_years_to_add) {
    # increase table size to allow adding extrapolated values
    # values will be replaced in for-loop so doesn't matter what they are
  extrapolated_df = add_row(df,year = 1:new_years_to_add)

  for (i in seq_along(df)) {
    extrapolated_df[[i]] = approxExtrap(df[[1]],df[[i]],extrapolated_years)$y
  }
  extrapolated_df[extrapolated_df <0] = 0 #set any negative extrapolated values to 0
  df = extrapolated_df

  return (df)
}


#' Function to check if extrapolation of start and/or end points is needed for
#' a dataframe. If so, it calls the 'extrapolate_all_columns()' function to
#' perform the extrapolation
#'
#' @param df dataframe for the table to be extrapolated, with one row per year
#'  for every year between the start and end years. The first column is the
#'  year column and all following columns are to be extrapolated (if needed).
#'
#' @param all_years dataframe with a single column containing every year between
#'  the start and end years.
#'
#' @return dataframe as provided as df argument, but with missing end values
#'  filled by extrapolation if needed. Otherwise, returns the df unchanged.
extrapolate_for_years <- function(df, all_years) {

  extrapolated_years = sort(unique( c(min(all_years),df$year,max(all_years))))
  new_years_to_add = length(extrapolated_years )  - length(df$year)

  if (new_years_to_add >0) {
    df <- extrapolate_all_columns(df,extrapolated_years,new_years_to_add)
  }
  return (df)
}

