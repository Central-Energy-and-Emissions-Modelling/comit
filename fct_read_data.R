# Functions to read and clean data

#### Main read of data ####-----------------------------------------------------

#' Read in the input template workbook
#'
#' Read in the relevant sheets of the excel spreadsheet that provides all
#'  input assumptions, data and model parameters for a comit scenario to be
#'  ran.
#'
#' @param path character for the file location of the workbook
#'
#' @return list of dataframes. One dataframe per sheet from the input data
#'  workbook. Note that only the sheets in the workbook between the section
#'  headers (with titles ending '==>') are read in, all other sheets are
#'  ignored.
#' @export
read_excel_data_template <- function(path) {

  comit_tic('total read_excel_data_template')

  sections <- c("Data",
                "Parameters",
                "Assumptions",
                "Constraints",
                "Attribution") # section names ending in "==>"

  # get names of all sheets
  sheets_to_read <- get_template_sheet_names(path, sections)

  # Read data
  sheets_data <- lapply(sheets_to_read, read_input_sheet, path = path)

  names(sheets_data) <- sheets_to_read

  sheets_data <- tidy_parameters(sheets_data)

  comit_toc()

  return(sheets_data)
}


#' Get the names of relevant input template sheets
#'
#' Returns the names of all input workbook sheets that are required. Section
#' headers (suffixed with '==>') are removed as well as the sheets before the
#' first header and the sheets after the last header, as these are not required.
#'
#' @param sections vector of characters for the section headers
#' @inheritParams read_excel_data_template
#'
#' @return a vector of the sheet names to be read in for comit.
#'
get_template_sheet_names <- function(path, sections){

  sheet_names <- excel_sheets(path) # get all names from input

  # create df of sheet names to filter down to what we need
  sheet_names_df <- data.frame(index = c(1:length(sheet_names)),
                               name = sheet_names)
  # flag the section holders
  sheet_names_df %<>%
    mutate(section_holder = str_detect(name,
                                       paste(sections, collapse = ' *==>|')))

  # get index of section holders
  section_holders <- which(sheet_names_df$section_holder)

  if(length(section_holders) < 4) {
    stop('Not enough sections found. Do you have assumptions, constraints, data and attribution sections?')
  }

  # retain only the sheets after the first and not holders themselves
  sheet_names <- sheet_names_df %>%
    filter(index > min(section_holders),
           !index %in% section_holders) %>%
    pull(name)

  return(sheet_names)
}


#' Read in an individual sheet from the input workbook
#'
#' Reads in the table from the relevant sheet. The read in starts from the 7th
#' row and 1st column, and reads until the first completely empty row and first
#' completely empty column.
#'
#' @inheritParams read_excel_data_template
#' @param sheet string, the name of the sheet to bread
#'
#' @return dataframe version of the input table present in the sheet named from
#'  the input workbook.
read_input_sheet <- function(path, sheet){

  #print(sheet)

  df <- read_xlsx_with_warning_handler(path, sheet)

  # Read only the rectangle surrounded by empty cells (read in as NAs in readxl)
  # Get the last row of the rectangle (ie the first row where all values are NA)
  i_row <- which(apply(df, 1, function(x) all(is.na(x)))) - 1
  i_row <- ifelse(length(i_row) == 0, nrow(df), min(i_row))

  # Then get the last column of the rectangle, which is the first column to have
  # all NA above the i_row row
  i_col <- which(apply(df, 2, function(x){all(is.na(x[1:i_row]))})) - 1
  i_col <- ifelse(length(i_col) == 0, ncol(df), min(i_col))

  # Trim the dataframe and return
  df <- df[1:i_row, 1:i_col]

  # re-guess column variable types when there are character columns present
  if ('character' %in% sapply(df, class)){
    df <- readr::type_convert(df, col_types = cols())
  }

  return(df)
}



#' Read in a sheet from the xlsx worksheet in the standard way, but suppress
#' a particular type of warning that does not need to be flagged.
#'
#' @inheritParams read_input_sheet
#'
#' @return dataframe version of the specified xlsx sheet.
read_xlsx_with_warning_handler <- function(path, sheet){

  df <- withCallingHandlers(
    read_xlsx(path, sheet = sheet, skip = 6),
    warning = function(w) custom_warning_handler(w, sheet)
  )

  return(df)
}



#' This funciton allows for the specific warning raised when reading in
#'  'min_fuel_constraints' or 'fuel_constraints' sheet through the
#   `read_xlsx_with_warning_handler` function to be ignored and the function to
#'  be ran as usual. This is to avoid the expected warning be raised, meaning
#'  it's easier to spot other warnings.
#'
#' @param w condition passed from withCallingHandlers
#' @inheritParams read_input_sheet
#'
#' @return NULL, restart of initial function is invoked
custom_warning_handler <- function(w, sheet){

  if(sheet %in% c('min_fuel_constraints', 'max_fuel_constraints') &
     str_detect(w$message, 'Coercing boolean to numeric')){
    invokeRestart('muffleWarning')
  }

}


#### Basic tidy of data ####----------------------------------------------------

#' Tidy the input data into a more usable format
#'
#' Basic wrangling of the input dataframes to facilitate further processing of
#'  the data later. This function pivots many of the dataframes into longer
#'  tables, as well as doing some specific transformations such as tidying the
#'  cluster connection tables and making required adjustments to retrofit data.
#'
#' @param data list of dataframes read in by `read_excel_data_template()`
#'
#' @return list of dataframes as provided as input, but with basic tidying
#'  completed on some dataframes.
#'
#' @export
tidy <- function(data) {

  # firstly make sure any required dataframes are present
  if(!'Technologies' %in% names(data)) {
    stop('"Technologies" data is missing from input data.')
  }

  # these are all dataframes that need to be pivoted. Each df to be pivoted
  # has a vector in the list in the following format:
  # c(data_frame_name, new_column_name1, new_column_name2)
  pivot_info <- list(
    c('Demand_drivers', 'commodity', 'demand'),
    c('Fuel_emissions', 'commodity', 'CO2e'),
    c('Fuel_costs', 'commodity', 'cost'),
    c('Non_industry_H2_demand', 'cluster', 'demand'),
    c('Non_industry_CO2_demand', 'cluster', 'demand'),
    c('resource_efficiency', 'commodity', 'r_efficiency'),
    c('energy_efficiency', 'commodity', 'efficiency'),
    c('CO2_storage', 'storage_site', 'max_injection'),
    c('max_fuel_constraints', 'fuel', 'max'),
    c('min_fuel_constraints', 'fuel', 'min'),
    c('maximum_capacity', 'code', 'max_capacity'),
    c('minimum_capacity', 'code', 'min_capacity')
  )

  # pivot the required dfs
  for(i in 1:length(pivot_info)){
    data <- pivot_specified_table(data, pivot_info[[i]])
    }

  # Tidy a few other tables with specific requirements
  data$max_fuel_constraints <- tidy_fuel_constraints(data,
                                                     'max_fuel_constraints',
                                                     'max')

  data$min_fuel_constraints <- tidy_fuel_constraints(data,
                                                     'min_fuel_constraints',
                                                     'min')

  data$Cluster_connections <- tidy_cluster_connections(data)

  data <- sort_retrofit(data)

  return(data)
}


#' Pivot a specified dataframe
#'
#' This function carries out common pivoting, whereby tables are made longer,
#'  pivoting all columns apart from a year column.
#'
#' @inheritParams tidy
#' @param pivot_details vector of length 3 containing the information required
#'  for pivoting of input data. The first element is the name of the table to be
#'  pivoted. The second element provides the name of the new naming column (as per
#'  the 'names_to' argument in pivot longer) and the third element provides the
#'  name of the new values column (as per the 'values_to' argument of pivot longer).
#'
#' @return data, the entire list of dataframes provided but with the named table
#'  pivoted.
pivot_specified_table <- function(data, pivot_details){

  this_sheet_name <- pivot_details[[1]]

  if(this_sheet_name %in% names(data)) {

    data[[this_sheet_name]] %<>%
      pivot_longer(cols = -year,
                   names_to = pivot_details[[2]],
                   values_to = pivot_details[[3]])
  }

  return(data)
}


#' Performing some additional specific tidying on fuel constraints
#'
#' @inheritParams tidy
#' @param table, string for the name of the sheet that is to be tidied
#' @param min_or_max, string for whether the constraint uses 'min' or 'max' values
#'
#' @return dataframe, the tidied table for the fuel constraint, with one row per
#'  year and fuel combination from the inputs. Columns for:
#'   * year (numeric)
#'   * fuel (character)
#'   * group (character)
#'   * apply_to_industry_only (boolean)
#'   * 'min' or 'max' depending on value of 'min_or_max' argument (numeric)
tidy_fuel_constraints <- function(data, table, min_or_max){

  if(!table %in% names(data) | is.null(data[[table]])){
    return(NULL)
  }

  fuel_constraint <- data[[table]] %>%
    rename(val = min_or_max) %>% # temporarily give general name
    mutate(apply_to_industry_only = case_when(
      year == 'apply_to_industry_only' ~ val,
      TRUE ~ 0
    )) %>%
    group_by(fuel) %>%
    mutate(apply_to_industry_only = max(apply_to_industry_only)) %>%
    ungroup()

  # set correct types and get commodity/group info
  fuel_constraint %<>%
    filter(year != 'apply_to_industry_only') %>%
    arrange(fuel, year) %>%
    mutate(apply_to_industry_only = as.logical(apply_to_industry_only),
           year = as.numeric(year),
           group = case_when(
             !fuel %in% data$commodities$commodity ~ fuel,
             TRUE ~ NA_character_),
           fuel = case_when(
             fuel %in% data$commodities$commodity ~ fuel,
             TRUE ~ NA_character_)
    ) %>%
    select(year, fuel, group, apply_to_industry_only, val)

  names(fuel_constraint)[5] <- min_or_max # revert to correct name

  return(fuel_constraint)

}


#' Modify the data table for cluster connection availability
#'
#' @inheritParams tidy
#'
#' @return dataframe with row per possible cluster connection. Columns for:
#'  * cluster_1 - character
#'  * cluster_2 - character
#'  * allowed_route - boolean
tidy_cluster_connections <- function(data){

  if(!'Cluster_connections' %in% names(data)) {
    return(NULL)
  }

  cluster_connections <- data$Cluster_connections %>%
    pivot_longer(cols = -Cluster,
                 names_to = "cluster_2",
                 values_to = "allowed_route") %>%
    rename(cluster_1 = Cluster) %>%
    drop_na(allowed_route) %>%
    mutate(allowed_route = as.logical(allowed_route)) %>%
    mutate(combination = paste0(pmin(cluster_1, cluster_2), ", ", pmax(cluster_1, cluster_2))) %>%
    distinct(combination, .keep_all = TRUE) %>%
    select(-combination)

  return(cluster_connections)

}



#' Make adjustments to input data based on retrofit parameters
#'
#' @inheritParams tidy
#'
#' @return data, list of input data tables as provided but with minor adjustments
#'  made based on retrofit parameters.
sort_retrofit <- function(data){

  if (!'retrofit_to' %in% names(data$Technologies)) {
    # if no retrofit information add a NULL column placeholder
    data$Technologies %<>%
      mutate(retrofit_to = NA_character_)

    # if no retrofit techs, this parameter should always be FALSE
    data$model_parameters$use_retrofit <- FALSE

  } else if (data$model_parameters$use_retrofit == FALSE) {
    #remove any retrofit data if retrofits are not to be included in the model
    data$Technologies %<>%
      filter(is.na(retrofit_to))

    data$technology_input_output %<>%
      filter(!str_detect(technology_code, "_R"))

  }

  return(data)
}


#' Get a single wide dataframe for main model parameters
#'
#' If two model parameter tables exist (model_parameters_a and
#' model_parameters_b), combine them into a single wide dataframe for easier
#' calling of parameters.
#'
#' @inheritParams read_excel_data_template
#'
#' @return list of dataframes, as input but with a single parameter tab called
#'  'model_parameters' instead of the previous two tabs.
#' @export
tidy_parameters <- function(data) {

  if(('model_parameters_a' %in% names(data))
     & ('model_parameters_b' %in% names(data))) {

    data$model_parameters_a %<>%
      select(!c('type', 'description')) %>%
      pivot_wider(names_from = parameter, values_from = value)

    data$model_parameters_b %<>%
      select(!c('description')) %>%
      pivot_wider(names_from = parameter, values_from = value)

    data$model_parameters <- cbind(data$model_parameters_a, data$model_parameters_b)

    data <- data[!names(data) %in% c('model_parameters_a', 'model_parameters_b')]

  # there is also a third param tab, add that too if exists.
    if('model_parameters_c' %in% names(data)) {

      data$model_parameters_c %<>%
        select(!c('description')) %>%
        pivot_wider(names_from = parameter, values_from = value)

      data$model_parameters <- cbind(data$model_parameters, data$model_parameters_c)

      data <- data[!names(data) == 'model_parameters_c']
   }

  }

  return(data)
}


#### Other basic transformations to inputs ####---------------------------------

#' Rounds year columns from the Technologies table to nearest multiple of the
#'  timestep used (set in the model parameters)
#'
#' Rounds lifetime and technology start years columns to the nearest timestep year
#'  (e.g. nearest 5 year period when timestep is 5).
#'
#' @inheritParams tidy
#'
#' @return list of data tables as provided in inputs, with the changes outlined
#'  made to the Technologies table.
#'
#' @export
round_years <- function(data) {

  timestep <- data$model_parameters$timestep

  data$Technologies %<>%
    mutate(lifetime = mround(lifetime, timestep),
           start_year = mround(start_year, timestep))

  return(data)
}


#' Helper function to round a number x to the nearest multiple of base
mround <- function(x, base) {
  base*round(x/base)
}



#' Apply a factor to all capex prices to account for underestimation in prices
#'
#' Accounts for optimism bias by multiplying capex prices by a set factor. The
#'  factor is set in the input spreadsheet model parameter tabs. Both technology
#'  and infrastructure prices are adjusted.
#'
#' @inheritParams tidy
#'
#' @return list of data tables as provided in inputs, with capex adjusted in the
#'  relevant dataframes.
#'
#' @export
adjust_for_optimism <- function(data){

  # make small func for friendlier code
  tech_uplifter <- data$model_parameters$tech_optimism_adjustment
  pipes_uplifter <- data$model_parameters$pipes_optimism_adjustment

  data$Technologies$capex %<>% multiply_by(tech_uplifter)
  data$CO2_transport_cost$Capex %<>% multiply_by(pipes_uplifter)
  data$H2_transport_cost$capex %<>% multiply_by(pipes_uplifter)

  return(data)
}



