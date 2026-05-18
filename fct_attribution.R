# This is the code for emissions attributions calculation


#' Attribute Emissions Savings From Technology Categories
#'
#' Calculates the amount of emissions abated from each technology category, by
#'  comparing the amount of emissions from a scenario with a counterfactual run.
#'
#' @param scenario_emissions dataframe, emissions outputs from a scenario run.
#'  Can be generated from an output file by using `read_outputs()`.
#' @param scenario_energy dataframe, same as scenario_emissions but for energy.
#' @param cf_emissions dataframe, same as scenario_emissions, but for the
#'  counterfactual.
#' @param cf_energy dataframe, same as scenario_emissions, but for the counterfactual's
#'  energy outputs.
#' @param data list of dataframes, generated from passing the filepath for an
#'  input spreadsheet to `read_excel_data_template()`.
#' @param filter_type character, the type of emissions to attribute (either
#'  'Direct' or 'Direct_and_Indirect").
#'
#' @returns dataframe, containing one row for each combination of year, sector and
#'  abatement type. Columns produced are:
#'  * year - numeric
#'  * sector - character
#'  * savings - numeric, total amount of emissions saved in the scenario.
#'  * type - character, abatement type.
#'  * scaling_factor - numeric, the factor used in the calculation of savings.
#'
#' @export
emissions_attribution <- function(scenario_emissions,
                                  scenario_energy,
                                  cf_emissions,
                                  cf_energy,
                                  data,
                                  filter_type) {

  # do standard COMIT tidy on input file first
  data %<>% tidy()

  sectors_vec <- get_sector_commodity_lookup(data) %>%
    distinct(sector) %>%
    filter(sector != 'Hydrogen') %>%
    pull(sector)

  sector_outputs <- get_sector_outputs(data)

  REEE_df <- get_efficiencies(data)

  scenario_emissions %<>% filter(sector %in% sectors_vec)
  scenario_energy %<>% filter(sector %in% sectors_vec)
  cf_emissions %<>% filter(sector %in% sectors_vec)
  cf_energy %<>% filter(sector %in% sectors_vec)

  # load the default assumptions data
  attr_params_list <- get_attribution_parameters(data)

  attr_params <- attr_params_list[[1]]
  BECCS_share <- attr_params_list[[2]]


  # Now on to the actual calculations

  cf_energy_aggregated <- get_emissions_from_fuel_use(cf_energy, attr_params)

  cf_energy_sector_outputs <- attr_aggregations_by_type(cf_energy_aggregated,
                                                        sector_outputs)

  emissions_attr <- get_emissions_attr_df(scenario_emissions,
                                          cf_emissions,
                                          filter_type,
                                          data,
                                          REEE_df)

  ccs_captured <- get_ccs_captured(scenario_emissions)

  hydrogen_savings <- get_hydrogen_savings(scenario_energy, cf_energy_sector_outputs, REEE_df)

  total_electricity_savings <- get_electricity_savings(
    scenario_energy,
    cf_energy,
    REEE_df,
    attr_params,
    emissions_attr,
    cf_energy_aggregated,
    cf_energy_sector_outputs,
    sector_outputs
  )


  biomass_savings <- get_biomass_savings(
    scenario_emissions,
    sector_outputs,
    scenario_energy,
    cf_energy,
    REEE_df,
    cf_energy_aggregated,
    attr_params,
    data
  )


  # combine the emissions savings
  emissions_reductions <- bind_rows(
    ccs_captured,
    hydrogen_savings,
    total_electricity_savings,
    biomass_savings
  )

  scaled_emissions <- get_scaled_emissions(emissions_attr,
                                           emissions_reductions,
                                           data)

  emissions_reductions %<>%
    left_join(scaled_emissions, by = c('year', 'sector')) %>%
    mutate(savings = savings * scaling_factor)

  beccs_and_ccs_savings <- get_beccs_and_css_savings(ccs_captured,
                                                     scaled_emissions,
                                                     BECCS_share)

  residual_emissions <- get_residual_emissions(emissions_attr)

  # final emissions tables
  emissions_total <- get_emissions_total(residual_emissions,
                                         emissions_reductions,
                                         beccs_and_ccs_savings)

  return(emissions_total)

}



#' Read an output file and complete some basic preprocessing
#'
#' @param file (character), file path for the output file to be read, or alternatively a wb object.
#' @param type (character), name of the tab to be read - either 'Emissions' or 'Energy'
#'
#' @return dataframe, the output file in longer format with years as a variable.
#' @export
read_outputs <- function(file, type) {

  if(!type %in% c('Emissions', 'Energy')){
    stop('Incorrect type supplied to outputs.')
  }

  if(is.character(file)) {

    if(!type %in% excel_sheets(file)){stop('Sheet not present.')}
    output_df <- read_xlsx(file, sheet = type) # this is quicker for saved outputs

  } else {
    output_df <- openxlsx::readWorkbook(file, sheet = type)
  }


  if (type == 'Emissions') {

    output_df %<>%
      correct_legacy_emissions_outputs() %>%
      filter(!Emissions_category %in% c("Electricity", "Negative", "Hydrogen"))
  }

  output_df %<>%
    select(primary_output = Primary_output,
           any_of(c('Emissions_category', 'Fuel_category')), # only selects if present
           Sector,
           Technology_code,
           Technology_category,
           starts_with('20')) %>%
    filter(Sector != 'Refineries') %>%
    pivot_longer(cols = starts_with('20'),
                 names_to = 'year',
                 values_to = type)

  if(type == 'Energy') {
    output_df %<>%
      filter(!str_detect(year, 'ktCO2e')) %>%
      mutate(year = str_remove_all(year, '_TWh'))
  }

  output_df %<>%
    mutate(year = as.numeric(year))

  colnames(output_df) <- tolower(colnames(output_df))

  return(output_df)

}



#' Produce table resource and energy efficency factors
#'
#' Get the RE and EE factors for each sector in each year.
#'
#' @inheritParams emissions_attribution
#'
#' @returns dataframe, containing one row for each combination of sector and year,
#'  with the following columns:
#'  * efficiency_RE
#'  * efficiency_EE
#'  * REEE - combined RE and EE
#'  * REEE_adjust (1 - REEE, the factor to multiply by to get REEE adjusted emissions)
#'
#' @export
get_efficiencies <- function(data) {

  RE_df <- sort_efficiencies(data, efficiency_type = 'resource')
  EE_df <- sort_efficiencies(data, efficiency_type = 'energy')

  REEE_df <- left_join(RE_df, EE_df,
                       by = c('sector', 'year'),
                       suffix = c('_RE', '_EE'))

  REEE_df %<>%
    mutate(REEE = 1 - (1- efficiency_EE) * (1 - efficiency_RE),
           REEE_adjust = 1 - REEE)

  return(REEE_df)
}



#' Tidy a resource or energy efficiency dataframe
#'
#' This function sorts efficiencie dataframes based on the specified efficiency
#'  type, providing the mean efficiency for each sector in each year.
#'
#' @param efficiency_type A string specifying the type of efficiency ('resource'
#'  or 'energy').
#'
#' @inheritParams emissions_attribution
#'
#' @return A dataframe with sorted efficiencies. One row for each sector and year
#'  combination. The column 'efficiency' provides the mean efficiency for the
#'  given sector and year.
#'
#' @export
sort_efficiencies <- function(data, efficiency_type) {

  sector_commodity_lookup <- get_sector_commodity_lookup(data)

  eff_df <- if (efficiency_type == 'resource') {
    data$resource_efficiency %>%
      rename(efficiency = r_efficiency)
  } else if (efficiency_type == 'energy') {
    data$energy_efficiency
  } else {
    stop("Invalid efficiency type. Choose 'resource' or 'energy'.")
  }

  eff_df %<>%
    left_join(sector_commodity_lookup, by = c('commodity' = 'output_commodity'),
              relationship = "many-to-many") %>%
    select(!commodity) %>%
    group_by(sector, year) %>%
    summarise(efficiency = mean(efficiency, na.rm = TRUE),
              .groups = 'drop')

  # set na vals to 0
  eff_df[is.na(eff_df$efficiency), 'efficiency'] <- 0

  return(eff_df)

}


#' Get a Commodity to Sector Lookup
#'
#' This function retrieves a lookup table for sectors and commodities.
#'
#' @inheritParams emissions_attribution
#'
#' @return A dataframe with all distinct sector and commodity pairs.
#'
#' @export
get_sector_commodity_lookup <- function(data) {

  sector_commodity_lookup <- data$Technologies %>%
    select(output_commodity, sector) %>%
    distinct()

  return(sector_commodity_lookup)

}



#' Get Attribution Parameters
#'
#' This function loads the default assumptions data and extracts attribution-specific
#' parameters and BECCS share data.
#'
#' @inheritParams emissions_attribution
#'
#' @return A list containing two data frames: `attr_params` and `BECCS_share`.
#' @export
get_attribution_parameters <- function(data){

  if('description' %in% colnames(data$attribution_parameters)) {
    data$attribution_parameters %<>% select(!description)
  }

  # Extract attribution-specific parameters
  attr_params <- data$attribution_parameters %>%
    pivot_wider(names_from = 'parameter', values_from = 'value')

  # Extract BECCS share data
  BECCS_share <- data$CCS_BECCS_Split %>%
    rename(sector = Sector)

  return(list(attr_params, BECCS_share))

}


#' Get Sector Outputs Lookup
#'
#' This function provides all unique combinations of sector and primary outputs
#'  from the input technologies data.
#'
#' @inheritParams emissions_attribution
#'
#' @return Data frame of the unique sector and outputs combinations/
#' @export
get_sector_outputs <- function(data) {

  sector_outputs <- data$Technologies %>%
    select(sector, primary_output = output_commodity) %>%
    filter(sector != 'Hydrogen') %>%
    distinct() %>%
    arrange(sector, primary_output)

  return(sector_outputs)
}




#' Get Sector Level Aggregate Energy and Emissions Data by Type
#'
#' This function aggregates energy and emissions data by sector, excluding
#'  specified fuel categories.
#'
#' @param cf_energy_aggregated A data frame containing aggregated emissions
#'  and energy outputs. This is produced by `get_emissions_from_fuel_use()`.
#' @param sector_outputs A data frame containing all disctinct combinations of
#'  sectors and primary outputs, produced by `get_sector_outputs()`.
#' @param exclusion_type A character vector of fuel categories to exclude from
#'  the aggregation. Default is an empty character vector - i.e. no exclusion.
#'
#' @return A data frame with aggregated energy and emissions data, including
#'  emissions intensity. There is one row per year and primary output.
#'
#' @export
attr_aggregations_by_type <- function(cf_energy_aggregated,
                                      sector_outputs,
                                      exclusion_type = c()) {

  if(length(exclusion_type) > 0) {
    cf_energy_aggregated <- cf_energy_aggregated %>%
      filter(!fuel_category %in% exclusion_type)
  }

  cf_energy_sector_totals <- cf_energy_aggregated %>%
    group_by(year, primary_output) %>%
    summarise(total_energy = sum(total_energy),
              total_emissions = sum(total_emissions),
              .groups = 'drop')

  cf_energy_sector_outputs <- cf_energy_sector_totals %>%
    left_join(sector_outputs, by = 'primary_output')

  cf_energy_sector_outputs %<>%
    mutate(emissions_intensity = total_emissions/total_energy,
           emissions_intensity = case_when(is.infinite(emissions_intensity) ~ 0,
                                           is.nan(emissions_intensity) ~ 0,
                                           TRUE ~ emissions_intensity))

  return(cf_energy_sector_outputs)
}


#' Filter and Aggregate Energy Output by Technology
#'
#' This function filters and summarizes energy output by technology, sector, and
#'  primary output.
#'
#' @param df A data frame containing energy data.
#' @param this_fuel_category A string specifying the fuel category to filter by.
#' @param tech A string specifying the technology code to filter by.
#' @param negate_tech A boolean indicating whether to exclude the specified technology code
#'  (TRUE) or include only the named technology code (FALSE). Default is FALSE.
#' @param new_col_name A string specifying the new column name for the summarized energy output.
#'
#' @return A data frame with total energy output by year, sector, and primary output.
#' @export
energy_output_by_tech <- function(df, this_fuel_category, tech,
                                  negate_tech = FALSE, new_col_name) {

  # Filter by technology code
  df_out <- if (negate_tech) {
    df %>% filter(!str_detect(technology_code, tech))
  } else {
    df %>% filter(str_detect(technology_code, tech))
  }

  df_out %<>%
    filter(fuel_category == this_fuel_category) %>%
    group_by(year, sector, primary_output) %>%
    summarise(energy = sum(energy), .groups = 'drop')

  colnames(df_out)[colnames(df_out) == 'energy'] <- new_col_name

  return(df_out)

}


#' Calculate Emissions from Fuel Use
#'
#' This function calculates emissions from fuel use based on energy consumption
#'  and emission factors.
#'
#' @param cf_energy A dataframe containing energy consumption data from a
#'  counterfactual run.
#' @param attr_params A dataframe of parameters for the attribution anaylsis,
#'  containing emission factors for different fuel categories.
#'
#' @return A data frame with aggregated energy consumption and calculated
#'  emissions by year, primary output, and fuel category.
#' @export
get_emissions_from_fuel_use <- function(cf_energy, attr_params) {

  ff_factors <- data.frame(
    fuel_category = c('Gas', 'Coal', 'Oil'),
    factor = c(attr_params$gas_factor,
               attr_params$coal_factor,
               attr_params$oil_factor)
  )

  cf_energy_aggregated <- cf_energy %>%
    filter(!is.na(fuel_category),
           fuel_category != "NonEnergyUse") %>%
    group_by(year, primary_output, fuel_category) %>%
    summarise(total_energy = sum(energy), .groups = "drop") %>%
    left_join(ff_factors,
              by = 'fuel_category',
              relationship = "many-to-many")

  # Replace NA values in the factor column with 0
  cf_energy_aggregated$factor[is.na(cf_energy_aggregated$factor)] <- 0

  cf_energy_aggregated %<>%
    mutate(total_emissions = factor * total_energy)

  return(cf_energy_aggregated)

}


#' Get Total Emissions by Year and Sector
#'
#' @param df A data frame containing emissions data.
#' @param filter_type A value specifying the type of emissions to filter by.
#' @param new_name A string specifying the new column name for the summarised
#'  emissions.
#'
#' @return A data frame with summarised emissions by year and sector.
#'
#' @export
summarise_emissions_attr <- function(df, filter_type, new_name) {

  df_attr <- df %>%
    filter(emissions_category == filter_type) %>%
    group_by(year, sector) %>%
    summarise(!!new_name := sum(emissions), .groups = 'drop')
  # Using bangbang ('!!') with ':=' to dynamically name new vars

  return(df_attr)

}


#' Get Emissions Attribution Data Frame
#'
#' This function calculates the emissions attribution data frame by summarizing
#' scenario and counterfactual emissions, and then joining them with the REEE data frame.
#'
#' @param scenario_emissions Data frame containing scenario emissions.
#' @param cf_emissions Data frame containing counterfactual emissions.
#' @param REEE_df Data frame containing REEE data.
#' @inheritParams emissions_attribution
#'
#' @return Data frame with emissions attribution data, for each sector and year.
#'  Columns include:
#'  * scenario_emissions - total emissions from scenario
#'  * cf_emissions - total emissions from emissions run
#'  * emissions_reductions - the amount of emissions reduced when comparing the
#'    counterfactual to the scenario.
#'  * cf_emissions_re - total emissions reductions due to re
#'  * cf_emissions_ee - total emissions reductions due to ee
#'  * emissions_reductions_post_reee - total emissions reductions after
#'    accounting for re and ee.
#' @export
get_emissions_attr_df <- function(scenario_emissions,
                                  cf_emissions,
                                  filter_type,
                                  data,
                                  REEE_df) {

  # Summarize scenario emissions
  scenario_emissions_attr <- summarise_emissions_attr(scenario_emissions,
                                                      filter_type,
                                                      'scenario_emissions')

  # Summarize counterfactual emissions
  cf_emissions_attr <- summarise_emissions_attr(cf_emissions,
                                                filter_type,
                                                'cf_emissions')

  # Join scenario and counterfactual emissions and calculate emissions reduction
  emissions_attr <- left_join(scenario_emissions_attr,
                              cf_emissions_attr,
                              by = c('sector', 'year')) %>%
    mutate(emissions_reduction = case_when(
      year == data$model_parameters$start_year ~ 0,
      scenario_emissions > cf_emissions ~ 0,
      TRUE ~ cf_emissions - scenario_emissions))

  # Join with REEE data and calculate post-REEE emissions reduction
  emissions_attr %<>%
    left_join(REEE_df, by = c('sector', 'year'),
              relationship = "many-to-many") %>%
    mutate(cf_emissions_ee = pmax(cf_emissions * efficiency_EE, 0), # set neg vals to 0
           cf_emissions_re = pmax(cf_emissions * efficiency_RE, 0),
           emissions_reduction_post_reee = pmax(
             emissions_reduction - cf_emissions_re - cf_emissions_ee,
             0))

  return(emissions_attr)
}



#' Get CCS Captured Emissions
#'
#' This function calculates the total captured emissions for Carbon Capture and
#' Storage (CCS) technology by year and sector.
#'
#' @param scenario_emissions Data frame containing scenario emissions.
#'
#' @return Data frame with total captured emissions by year and sector.
#' @export
get_ccs_captured <- function(scenario_emissions){

  # Filter for captured emissions in CCS technology
  ccs_captured <- scenario_emissions %>%
    filter(emissions_category == 'Captured',
           technology_category == 'CCS') %>%
    group_by(year, sector) %>%
    summarise(total_captured = sum(emissions), .groups = 'drop')

  # Replace NA values with 0
  ccs_captured$total_captured[is.na(ccs_captured$total_captured)] <- 0

  # Select relevant columns and add type column
  ccs_captured %<>%
    select(year, sector, savings = total_captured) %>%
    mutate(type = 'CCS')

  return(ccs_captured)
}



#' Calculate Emissions Savings From Hydrogen
#'
#' This function calculates the hydrogen savings by summarizing the energy data
#' for hydrogen, joining it with counterfactual energy sector outputs, and then
#' calculating the savings based on emissions intensity.
#'
#' @param scenario_energy Data frame containing scenario energy data.
#' @param cf_energy_sector_outputs Data frame containing counterfactual energy
#'  sector outputs.
#' @param REEE_df Data frame containing REEE data.
#'
#' @return Data frame with hydrogen savings by year and sector.
#' @export
get_hydrogen_savings <- function(scenario_energy, cf_energy_sector_outputs, REEE_df) {

  # Summarize energy data for hydrogen
  hydrogen_dep <- scenario_energy %>%
    filter(fuel_category == 'Hydrogen') %>%
    group_by(year, sector, primary_output) %>%
    summarise(energy = sum(energy), .groups = 'drop')

  # Join with counterfactual energy sector outputs and calculate hydrogen savings
  hydrogen_dep <- left_join(cf_energy_sector_outputs,
                            hydrogen_dep,
                            by = c('sector', 'primary_output', 'year')) %>%
    mutate(hydrogen_savings = energy * emissions_intensity)

  # Replace NA values with 0
  hydrogen_dep[is.na(hydrogen_dep$hydrogen_savings), "hydrogen_savings"] <- 0

  # Summarize hydrogen savings by year and sector and join with REEE data
  hydrogen_savings <- hydrogen_dep %>%
    group_by(year, sector) %>%
    summarise(hydrogen_savings = sum(hydrogen_savings), .groups = 'drop') %>%
    left_join(REEE_df, by = c('year', 'sector'), relationship = "many-to-many") %>%
    drop_na()

  # Select relevant columns and add type column
  hydrogen_savings %<>%
    select(year, sector, savings = hydrogen_savings) %>%
    mutate(type = 'Hydrogen')

  return(hydrogen_savings)
}


#' Calculate Emissions Savings From Electricity
#'
#' This function calculates the electricity savings by comparing scenario energy data
#' with counterfactual energy data, adjusting for REEE factors, and summarizing the results.
#'
#' @param scenario_energy Data frame containing scenario energy data.
#' @param cf_energy Data frame containing counterfactual energy data.
#' @param REEE_df Data frame containing REEE data.
#' @param attr_params Data frame containing attribute parameters values.
#' @param emissions_attr Data frame containing emissions attribution data.
#' @param cf_energy_aggregated Data frame containing aggregated counterfactual energy data.
#' @param cf_energy_sector_outputs Data frame containing counterfactual energy sector outputs.
#' @param sector_outputs Data frame containing sector outputs.
#'
#' @return Data frame with total electricity savings by year and sector.
#' @export
get_electricity_savings <- function(scenario_energy,
                                    cf_energy,
                                    REEE_df,
                                    attr_params,
                                    emissions_attr,
                                    cf_energy_aggregated,
                                    cf_energy_sector_outputs,
                                    sector_outputs){

  # Aggregate counterfactual energy excluding electricity
  cf_energy_no_elec <- attr_aggregations_by_type(cf_energy_aggregated,
                                                 sector_outputs,
                                                 exclusion_type = c('Electricity'))

  # Define columns for joining
  join_cols <- c('year', 'sector', 'primary_output')

  # Get scenario electricity data excluding ELCHP technology
  scenario_elec <- energy_output_by_tech(scenario_energy,
                                         this_fuel_category = 'Electricity',
                                         tech = 'ELCHP',
                                         negate = TRUE,
                                         new_col_name = 'scenario_elec')

  # Get counterfactual electricity data
  cf_elec <- energy_output_by_tech(cf_energy,
                                   this_fuel_category = 'Electricity',
                                   tech = 'ignorable_string', # this just means no filtering
                                   negate = TRUE,
                                   new_col_name = 'cf_elec')

  # Adjust counterfactual electricity data with REEE factors
  cf_elec %<>%
    left_join(REEE_df, by = c('year', 'sector'),
              relationship = "many-to-many") %>%
    mutate(adj_cf_elec = cf_elec * REEE_adjust)

  # note filter out for required primary outputs at end

  # Calculate electricity increase
  elec_increase <- full_join(cf_elec,
                             scenario_elec,
                             by = join_cols) %>%
    mutate(
      scenario_elec = ifelse(is.na(scenario_elec), 0, scenario_elec),
      elec_increase_twh = scenario_elec - adj_cf_elec,
      elec_increase_twh = pmax(elec_increase_twh, 0)
    ) # make 0 if neg

  # NOTE - there are primary_outputs present in scenario not present in cf.
  # at the moment these are just set to 0 in the above, but should they be
  # accounted for differently??

  # Get scenario electricity data for heat pumps
  scenario_elec_hp <- energy_output_by_tech(
    scenario_energy,
    this_fuel_category = 'Electricity',
    tech = 'ELCHP',
    new_col_name = 'scenario_elec'
  ) %>%
    mutate(scenario_elec = scenario_elec * attr_params$Non_CHP_factor)


  # Get scenario electricity data for CHP
  scenario_elec_chp <- energy_output_by_tech(
    scenario_energy,
    this_fuel_category = 'Electricity',
    tech = 'STMHP',
    new_col_name = 'scenario_elec'
  ) %>%
    mutate(scenario_elec = scenario_elec * attr_params$CHP_factor)



  # Calculate savings from electric arc furnace
  eaf_savings <- emissions_attr %>%
    filter(sector == 'Iron & steel') %>%
    mutate(savings = emissions_reduction_post_reee,
           savings = pmax(savings, 0),
           type = 'EAF') %>%
    select(year, sector, savings)



  elec_increase_twh <- elec_increase %>%
    select(all_of(join_cols), elec_increase_twh)

  emissions_intensity_elec <- cf_energy_no_elec %>%
    select(all_of(join_cols), emissions_intensity)

  emissions_intensity_other <- cf_energy_sector_outputs %>%
    select(all_of(join_cols), emissions_intensity)

  # Calculate total electricity savings
  electricity_savings_all <- left_join(elec_increase_twh,
                                       emissions_intensity_elec,
                                       by = join_cols) %>%
    mutate(savings = elec_increase_twh * emissions_intensity,
           type = 'all')


  electricity_savings_hp <- left_join(scenario_elec_hp,
                                      emissions_intensity_other,
                                      by = join_cols) %>%
    mutate(savings = scenario_elec * emissions_intensity,
           type = 'heat_pump')


  electricity_savings_chp <- left_join(scenario_elec_chp,
                                       emissions_intensity_other,
                                       by = join_cols) %>%
    mutate(savings = scenario_elec * emissions_intensity,
           type = 'CHP')

  total_electricity_savings <- bind_rows(electricity_savings_all,
                                         electricity_savings_hp,
                                         electricity_savings_chp)

  # Replace NA values with 0
  total_electricity_savings[is.na(total_electricity_savings$savings), 'savings'] <- 0

  # Summarize total electricity savings and add EAF savings
  total_electricity_savings %<>%
    filter(sector != 'Iron & steel') %>%
    group_by(year, sector) %>%
    summarise(savings = sum(savings), .groups = 'drop') %>%
    bind_rows(eaf_savings)

  total_electricity_savings %<>%
    mutate(type = 'Electrification')

  return(total_electricity_savings)
}



#' Calculate Emission Savings From Biomass
#'
#' This function calculates the biomass savings based on scenario energy and
#' emissions compared to counterfactual energy use.
#'
#' @param scenario_emissions Data frame containing scenario emissions data.
#' @param sector_outputs Data frame containing sector outputs data.
#' @param scenario_energy Data frame containing scenario energy data.
#' @param cf_energy Data frame containing counterfactual energy data.
#' @param REEE_df Data frame containing REEE data.
#' @param cf_energy_aggregated Data frame containing aggregated counterfactual energy data.
#' @param attr_params Dataframe of attribute parameters.
#' @inheritParams emissions_attribution
#'
#' @return Data frame containing biomass savings for each year and sector.
#'
#' @export
get_biomass_savings <- function(scenario_emissions,
                                sector_outputs,
                                scenario_energy,
                                cf_energy,
                                REEE_df,
                                cf_energy_aggregated,
                                attr_params,
                                data) {

  # make vector of required columns that is repeated alot
  join_cols <- c('year', 'sector', 'primary_output')

  # Aggregate counterfactual energy excluding biomass and inorganic waste
  cf_energy_no_biomass <- attr_aggregations_by_type(
    cf_energy_aggregated,
    sector_outputs,
    exclusion_type = c('Biomass and organic waste', 'Inorganic waste')
  )

  # Filter and summarize BECCS biomass emissions
  BECCS_biomass_emissions <- scenario_emissions %>%
    filter(emissions_category == 'Captured',
           technology_category == 'BECCS') %>%
    group_by(across(all_of(join_cols))) %>%
    summarise(emissions = sum(emissions), .groups = 'drop') %>%
    mutate(BECCS_biomass_filter = ifelse(emissions > 0.01, 0, 1)) %>%
    select(!emissions)

  # Create BECCS biomass filter
  BECCS_biomass_filter <- sector_outputs %>%
    filter(!primary_output %in% unique(BECCS_biomass_emissions$primary_output)) %>%
    mutate(BECCS_biomass_filter = 1) %>%
    expand_df_by_model_years(., data) %>%
    bind_rows(BECCS_biomass_emissions)

  # NOTE: the below can be used as a test to make sure all cases accounter for
  # nrow(BECCS_biomass_filter) == (length(unique(BECCS_biomass_filter$year))
  #   * length(unique(BECCS_biomass_filter$primary_output)))

  # Summarize biomass scenario energy
  biomass_scenario <- scenario_energy %>%
    filter(fuel_category %in% c('Biomass and organic waste',
                                'Inorganic waste')) %>%
    group_by(across(all_of(join_cols))) %>%
    summarise(scenario_energy = sum(energy), .groups = 'drop')

  # Summarize counterfactual biomass energy and adjust with REEE
  biomass_cf <- cf_energy %>%
    filter(fuel_category %in%
             c('Biomass and organic waste', 'Inorganic waste')) %>%
    group_by(across(all_of(join_cols))) %>%
    summarise(cf_energy = sum(energy), .groups = 'drop') %>%
    left_join(REEE_df,
              by = c('sector', 'year'),
              relationship = "many-to-many") %>%
    mutate(adj_cf_energy = cf_energy * REEE_adjust)

  # Calculate biomass increase
  biomass_cf %<>%
    left_join(biomass_scenario, by = join_cols) %>%
    mutate(biomass_increase = scenario_energy - adj_cf_energy,
           biomass_increase = pmax(biomass_increase, 0))

  # Calculate biomass savings
  biomass_savings <- biomass_cf %>%
    left_join(BECCS_biomass_filter, by = join_cols) %>%
    left_join(cf_energy_no_biomass, by = join_cols) %>%
    mutate(savings = biomass_increase
           * emissions_intensity
           * BECCS_biomass_filter
           * attr_params$Biomass_Switch)

  biomass_savings %<>%
    group_by(sector, year) %>%
    summarise(savings = sum(savings), .groups = 'drop') %>%
    mutate(savings = ifelse(year == data$model_parameters$start_year, 0, savings),
           type = 'Biomass')

  return(biomass_savings)

}


#' Calculate Scaled Emissions
#'
#' This function calculates the scaling factor for emissions reductions based on
#' attribute emissions and total emissions reductions.
#'
#' @param emissions_attr Data frame containing attribution data.
#' @param emissions_reductions Data frame containing emissions reductions data.
#' @inheritParams emissions_attribution
#'
#' @return Data frame containing the scaling factor for emissions reductions in
#'  each year and sector.
#' @export
get_scaled_emissions <- function(emissions_attr, emissions_reductions, data){

  # Summarize total emissions reductions by year and sector
  total_emissions_reductions <- emissions_reductions %>%
    mutate(savings = ifelse(
      year == data$model_parameters$start_year, 0, savings)) %>%
    group_by(year, sector) %>%
    summarise(savings = sum(savings), .groups = 'drop')

  # Join attribute emissions with total emissions reductions
  scaled_emissions <- emissions_attr %>%
    select(year, sector, emissions_reduction_post_reee) %>%
    left_join(total_emissions_reductions, by = c('year', 'sector'))

  # Calculate scaling factor and handle infinite or NaN values
  scaled_emissions %<>%
    mutate(scaling_factor = emissions_reduction_post_reee / savings,
           scaling_factor = ifelse(
             is.infinite(scaling_factor) | is.nan(scaling_factor),
             0, scaling_factor)) %>%
    select(year, sector, scaling_factor)

  return(scaled_emissions)
}


#' Calculate Emission Savings From BECCS and CCS
#'
#' This function calculates the savings from BECCS (Bioenergy with Carbon
#' Capture and Storage) and CCS (Carbon Capture and Storage) based on captured
#' emissions, scaled emissions, and BECCS share.
#'
#' @param ccs_captured Data frame containing captured emissions data.
#' @param scaled_emissions Data frame containing emissions scaling factors.
#' @param BECCS_share Data frame containing the share of BECCS in each sector.
#'
#' @return Data frame containing the savings from BECCS and CCS.
#' @export
get_beccs_and_css_savings <- function(ccs_captured,
                                      scaled_emissions,
                                      BECCS_share) {

  ccs_emissions_savings_total <- ccs_captured %>%
    left_join(scaled_emissions, by = c('year', 'sector')) %>%
    left_join(BECCS_share, by = 'sector') %>%
    mutate(savings = savings * scaling_factor,
           BECCS_emissions_savings = savings * `BECCS %`,
           ccs_emissions_savings = savings - BECCS_emissions_savings)

  BECCS_emissions_savings_total <- ccs_emissions_savings_total %>%
    select(year, sector, savings = BECCS_emissions_savings) %>%
    mutate(type = 'BECCS')

  ccs_emissions_savings_total %<>%
    select(year, sector, savings = ccs_emissions_savings) %>%
    mutate(type = 'CCS')

  beccs_and_ccs_savings <- bind_rows(BECCS_emissions_savings_total,
                                     ccs_emissions_savings_total)

  return(beccs_and_ccs_savings)

}


#' Calculate Total Emissions
#'
#' This function calculates the total emissions by combining residual emissions,
#' emissions reductions, and savings from BECCS and CCS.
#'
#' @param residual_emissions Data frame containing residual emissions data.
#' @param emissions_reductions Data frame containing emissions reductions data.
#' @param beccs_and_ccs_savings Data frame containing savings from BECCS and CCS.
#'
#' @return Data frame containing the total emissions savings for each sector, year
#'  and attribution type.
#' @export
get_emissions_total <- function(residual_emissions,
                                emissions_reductions,
                                beccs_and_ccs_savings) {

  # Extract final energy efficiency emissions
  final_ee_emissions <- residual_emissions %>%
    select(year, sector, savings = cf_emissions_ee) %>%
    mutate(type = 'EE')

  # Extract final renewable energy emissions
  final_re_emissions <- residual_emissions %>%
    select(year, sector, savings = cf_emissions_re) %>%
    mutate(type = 'RE')

  # Extract residual emissions
  residual_emissions %<>%
    select(year, sector, savings = residual_emissions) %>% # savings is the wrong word here, but need to be consistent
    mutate(type = 'Residual')

  # Combine all emissions data
  emissions_total <- bind_rows(
    residual_emissions,
    emissions_reductions %>% filter(type != 'CCS'), # get ccs from ccs_emissions_savings_total instead
    final_re_emissions,
    final_ee_emissions,
    beccs_and_ccs_savings
  )

  return(emissions_total)
}




#' Calculate Residual Emissions
#'
#' This function calculates the residual emissions based on attribute emissions
#' data. This is the amount of emissions remaining (i.e. emissions not saved).
#'
#' @param emissions_attr Data frame containing emissions attribution data.
#'
#' @return Data frame containing the residual emissions.
#' @export
get_residual_emissions <- function(emissions_attr){

  residual_emissions <- emissions_attr %>%
    mutate(
      cf_emissions_residual = cf_emissions - cf_emissions_re - cf_emissions_ee,
      residual_emissions = pmin(scenario_emissions, cf_emissions_residual,
                                na.rm = TRUE)
    )

  return(residual_emissions)
}





################################################################################

# Functions for processing the outputs

#' Process Emissions Attribution Table
#'
#' This function processes the emissions attribution table providing savings for
#'  a specified sector or all sectors.
#'
#' @param emissions_total Data frame containing total emissions data.
#' @param this_sector Character string specifying the sector to filter by.
#'  Use 'All' for no filter.
#'
#' @return Data frame containing emissions savings, in each year
#'  and for each attribution type. Filtered by sector (and therefore not aggregated)
#'  if a specific sector is provided as `this_sector`. Otherwise the outputs will
#'  be aggregated to provide a total for all sectors.
#' @export
emissions_attribution_table <- function(emissions_total, this_sector) {

  emissions_total_agg <- emissions_total %>%
    filter(sector == this_sector | this_sector == 'All') %>% # this means no filter if this_sector set to 'all'
    group_by(year, type) %>%
    summarise(savings = sum(savings), .groups = 'drop') %>%
    mutate(savings = savings/1000,
           sector = this_sector)

  return(emissions_total_agg)
}


#' Plot Emissions Attribution
#'
#' This function creates a plot of emissions attribution for a specified sector
#' or all sectors and visualizing the data using a ribbon plot.
#'
#' @param emissions_total Data frame containing total emissions data.
#' @param this_sector Character string specifying the sector to filter by.
#'  Use 'All' for no filter.
#'
#' @return ggplot object
#' @export
plot_attribution <- function(emissions_total, this_sector) {

  emissions_total_agg <- emissions_attribution_table(emissions_total, this_sector)


  my_colours <- c('grey70',
                  '#ffeb00',
                  '#44B0E2',
                  '#F28705',
                  'burlywood4',
                  'forestgreen',
                  'purple4',
                  'purple')

  abatement_types <- c('Residual',
                       'Electrification',
                       'Hydrogen',
                       'Biomass',
                       'BECCS',
                       'CCS',
                       'EE',
                       'RE')

  names(my_colours) <- abatement_types

  emissions_total_plot <- emissions_total_agg %>%
    mutate(type = factor(type, levels = abatement_types)) %>%
    group_by(year) %>%
    arrange(type) %>%
    mutate(prev_val = lag(savings, default = 0),
           ymin = cumsum(prev_val),
           ymax = cumsum(savings))

  start_year <- min(emissions_total_plot$year)
  end_year <- max(emissions_total_plot$year)

  p <- ggplot(emissions_total_plot, aes(x = year, group = type)) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = type), colour = 'black') +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(limits = c(start_year,
                                  end_year),
                       breaks = c(start_year,
                                  start_year + 10,
                                  start_year + 20,
                                  end_year)) +
    scale_fill_manual(values = my_colours) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.line.x = element_line()) +
    ylab('MtCO2e') +
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(title = 'Emissions Attribution',
         subtitle = '')

  return(p)

}
