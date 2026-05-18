
## This code is currently in development - to visualise the modelled processes


processes_sankey <- function(data,
                             output_data,
                             year,
                             sector,
                             type = c('fuel', 'commodity')) {

  technologies <- data$Technologies

  technology_input_output <- data$technology_input_output

  technologies <- technologies %>%
    select(code, sector, output_commodity, technology_category)

  technology_input_output <- left_join(
    technologies,
    technology_input_output,
    by = c('code' = 'technology_code')
  )

  model_outputs <- sankey_get_model_outputs(output_data, year)

  technology_inputs <- technology_input_output %>%
    filter(output < 0)

  technology_outputs <- technology_input_output %>%
    filter(output > 0)

  inputs_outputs <- get_sankey_node_links_data(data,
                                               technologies,
                                               technology_inputs,
                                               technology_outputs,
                                               model_outputs,
                                               sector,
                                               type)

  all_inputs <- inputs_outputs[[1]]
  all_outputs <- inputs_outputs[[2]]

  sankey_plotly(all_inputs, all_outputs, data, model_outputs, year, sector)

}




sankey_get_model_outputs <- function(model_outputs_raw, year_to_show) {

  model_outputs <- model_outputs_raw %>%
    select(sector = Sector, commodity = Primary_output, code = Technology_code,
           technology_category = Technology_category,
           technology_description = Technology_description,
           unit = Unit, run = run, starts_with('20')) %>%
    pivot_longer(cols = starts_with('20'),
                 names_to = 'year',
                 values_to = 'output') %>%
    mutate(year = as.numeric(year))


  # get single year
  model_outputs <- model_outputs %>%
    filter(year == year_to_show)

  # summarise at the tech level
  model_outputs <- model_outputs %>%
    group_by(code, commodity,
             unit, technology_description) %>% # these are just for labels later
    summarise(output = sum(output), .groups = 'drop') %>%
    ungroup()

  return(model_outputs)

}


# may be able to remove 'layer' parameter, just using for testing if it can help
get_tech_inputs <- function(technology_inputs, output_code, layer,
                            model_output_df, data, type = 'commodity') {

  fuels <- unique(data$Fuel_costs$commodity)

  technology_inputs <- technology_inputs %>%
    filter(output_commodity == output_code) %>%
    mutate(input = -1 * output,
           input_type = case_when(commodity %in% fuels ~ 'fuel',
                                  TRUE ~ 'commodity'),
           commodity = paste0(commodity, layer)) %>%
    select(code, sector, input_commodity = commodity, output_commodity, input,
           input_type, technology_category)

  technology_inputs <-  technology_inputs %>%
    left_join(model_output_df, by = c('code',
                                      'output_commodity' = 'commodity')) %>%
    replace_na(list(output = 0)) %>%
    mutate(input = input * output) # scale the amount of input based on how much output is produced


  technology_inputs <- technology_inputs %>%
    filter(input > 0,
           input_type %in% type)


  return(technology_inputs)

}


get_tech_outputs <- function(technology_outputs, output_code, layer,
                             model_output_df) {

  technology_outputs <- technology_outputs %>%
    filter(commodity == output_code) %>%
    select(code, sector, commodity, output_factor = output, technology_category) %>%
    mutate(commodity = paste0(commodity, layer)) %>%
    left_join(model_output_df, by = c('code', 'commodity')) %>%
    replace_na(list(output = 0)) %>%
    mutate(output = output_factor * output) %>%
    select(!output_factor)


  technology_outputs <- technology_outputs %>%
    filter(output > 0)

  return(technology_outputs)
}



get_sankey_data <- function(inputs, outputs, data, model_outputs) {

  fuels <- unique(data$Fuel_costs$commodity)

  fuel_categories <- data$commodities %>%
    filter(commodity %in% fuels) %>%
    select(commodity, commodity_category, description) %>%
    mutate(unit = 'PJ')


  links <- data.frame(
    source = c(inputs$input_commodity, outputs$code),
    target = c(inputs$code, outputs$commodity),
    value = c(inputs$input, outputs$output),
    units = c(inputs$unit, outputs$unit)
  )

  nodes <- data.frame(
    name = c(as.character(links$source),
             as.character(links$target)) %>% unique()
  )


  tech_categories <- inputs %>%
    distinct(code, technology_category, unit, description = technology_description) %>%
    rename(name = code, group = technology_category)

  # as some techs have no commodity inputs and are only shown through the outputs
  out_tech_categories <- outputs %>%
    distinct(code, technology_category, unit, description = technology_description) %>%
    rename(name = code, group = technology_category)

  ## todo add technology_description here using commodities data info
  input_commodities <- inputs %>%
    distinct(input_commodity) %>%
    left_join(fuel_categories, by = c('input_commodity' = 'commodity')) %>%
    mutate(group = case_when(!is.na(commodity_category) ~ commodity_category,
                             TRUE ~ 'commodity')) %>%
    select(name = 'input_commodity', group, unit, description)

  ## todo add description here using commodities data info
  commodity_info <- data$commodities %>%
    select(commodity, description)

  commodities <- outputs %>%
    distinct(commodity, unit) %>%
    left_join(commodity_info, by = c('commodity')) %>%
    select(name = commodity, unit, description) %>%
    mutate(group = 'commodity')

  node_groups <- rbind(tech_categories, out_tech_categories, commodities,
                       input_commodities) %>%
    group_by(name) %>%
    tidyr::fill(unit, description, .direction = 'downup') %>%
    ungroup() %>%
    distinct()

  nodes <- left_join(nodes, node_groups, by = 'name')

  ##### Getting node values ######

  model_outputs <- model_outputs %>%
    select(code, commodity, output, unit)

  model_technologies_pj <- model_outputs %>%
    filter(unit == 'PJ') %>%
    select(name = code, output_PJ = output)

  model_technologies_mt <- model_outputs %>%
    filter(unit == 'Mt') %>%
    select(name = code, output_Mt = output)

  model_commodities_pj <- model_outputs %>%
    filter(unit == 'PJ') %>%
    group_by(commodity) %>%
    summarise(output_PJ = sum(output), .groups = 'drop') %>%
    ungroup() %>%
    rename(name = commodity)

  model_commodities_mt <- model_outputs %>%
    filter(unit == 'Mt') %>%
    group_by(commodity) %>%
    summarise(output_Mt = sum(output), .groups = 'drop') %>%
    ungroup() %>%
    rename(name = commodity)

  model_input_commodities <- inputs %>%
    select(code, commodity = input_commodity, output = input) %>%
    left_join(data$commodities %>% select(commodity, unit = commodity_unit),
              by = 'commodity')

  model_input_commodities_mt <- model_input_commodities %>%
    group_by(commodity) %>%
    filter(unit == 'Mt') %>%
    summarise(output_Mt = sum(output), .groups = 'drop') %>%
    ungroup() %>%
    rename(name = commodity)

  model_input_commodities_pj <- model_input_commodities %>%
    group_by(commodity) %>%
    filter(unit == 'PJ') %>%
    summarise(output_PJ = sum(output), .groups = 'drop') %>%
    ungroup() %>%
    rename(name = commodity)


  all_model_output_info <- bind_rows(model_technologies_pj,
                                     model_technologies_mt,
                                     model_commodities_pj,
                                     model_commodities_mt,
                                     model_input_commodities_mt,
                                     model_input_commodities_pj) %>%
    replace_na(list(output_PJ = 0, output_Mt = 0))

  nodes <- left_join(nodes,
                      all_model_output_info,
                      by = c('name'))


  ##############################################################################

  links <- links %>%
    left_join(fuel_categories, by = c('source' = 'commodity')) %>%
    select(!unit)

  links <- links %>%
    mutate(commodity_category = case_when(is.na(commodity_category) ~ 'output',
                                          TRUE ~ commodity_category))

  # correct commodity units
  links <- links %>%
    left_join(data$commodities %>% select(commodity, commodity_unit),
              by = c('source' = 'commodity')) %>%
    mutate(units = case_when(!is.na(commodity_unit) ~ commodity_unit,
                             TRUE ~ units))


  ##############################################################################



  links$IDsource <- match(links$source, nodes$name) - 1
  links$IDtarget <- match(links$targe, nodes$name) -1

  return(list(nodes, links))
}




get_sankey_node_links_data <- function(data,
                                       technologies,
                                       technology_inputs,
                                       technology_outputs,
                                       model_outputs,
                                       sector_to_show, type = c('fuel', 'commodity')) {

  sector_outputs <- technologies %>%
    filter(sector == sector_to_show) %>%
    distinct(output_commodity) %>%
    pull()

  all_inputs <- tibble()
  all_outputs <- tibble()

  for(output_commoditiy in sector_outputs) {

    inputs <- get_tech_inputs(technology_inputs, output_commoditiy, '',
                              model_outputs, data, type)
    outputs <- get_tech_outputs(technology_outputs, output_commoditiy, '', model_outputs)


    all_inputs <- rbind(all_inputs, inputs)
    all_outputs <- rbind(all_outputs, outputs)

  }

  return(list(all_inputs, all_outputs))

}



sankey_plotly <- function(inputs, outputs, data, model_outputs, year, sector) {

  colour_mapping <- list("Biomass" = "#804941",
                         "Biomass and organic waste" = "#804941",
                         "Inorganic waste" = "#57F2AE",
                         "NonEnergyUse" = "#B0CFF5",
                         "CCS" = "#9B34F5",
                         "Coal" = "#191919",
                         "Dry kiln" = "#553D3A",
                         "Electricity" = "#ffeb00",
                         "Heat pump" = "#E33BFF",
                         "Hydrogen" = "#44B0E2",
                         "Natural gas" = "#FF6F00",
                         "Gas" = "#FF6F00",
                         "Oil" = "#323232",
                         "Standard_FF" = "#646464",
                         "Steam" = "#DCDCDC",
                         "commodity" = "#0367A6",
                         "output" = "#BFBFBF")

  colour_mapping <- as.data.frame(colour_mapping) %>%
    pivot_longer(cols = everything(),
                 names_to = 'group',
                 values_to = 'colour') %>%
    mutate(group = str_replace_all(group, '\\.', ' '))


  sankey_data <- get_sankey_data(inputs, outputs, data, model_outputs)

  nodes <- sankey_data[[1]]
  links <- sankey_data[[2]]

  nodes <- nodes %>%
    left_join(colour_mapping, by = c('group'))

  links <- links %>%
    left_join(colour_mapping, by = c('commodity_category' = 'group'))

  fig <- plot_ly(
    type = "sankey",
    domain = list(
      x =  c(0,1),
      y =  c(0,1)
    ),
    orientation = "h",
    valueformat = ".2f",

    node = list(
      label = nodes$name,
      color = nodes$colour,
      customdata = paste0('\ncategory: ', nodes$group,
                          ',\ndescription: ', nodes$description,
                          '\n<i>output: ', signif(nodes$output_PJ, 3), ' PJ, ',
                          signif(nodes$output_Mt, 3), ' Mt</i>'),
      hovertemplate = paste0('<b>%{label}</b>', '%{customdata}', '<extra></extra>'),
      pad = 15,
      thickness = 15,
      line = list(
        color = "black",
        width = 0.5
      )
    ),

    link = list(
      source = links$IDsource,
      target = links$IDtarget,
      value =  links$value,
      color = links$colour,
      customdata = paste0('\nsource: ', links$source,
                          ',\ntarget: ', links$target,
                          '\n<i>value: ', signif(links$value, 3), ' ', links$units, '</i>'),
      hovertemplate = '%{customdata}<extra></extra>'
    )
  )

  fig <- fig %>% layout(
    title = paste0("Industry Processes in ", sector, ' Sector, <b>', year, '</b>'),
    font = list(
      size = 10
    ),
    xaxis = list(showgrid = F, zeroline = F),
    yaxis = list(showgrid = F, zeroline = F)
  )

  fig <- config(fig,
                displaylogo = FALSE,
                modeBarButtonsToRemove = list('select2d', 'lasso2d',
                                              'hoverClosestCartesian',
                                              'hoverCompareCartesian')#,
                #modeBarButtonsToAdd = list('zoom2d', 'zoomIn', 'zoomOut')
                )

  return(fig)
}


# # For testing:
#
# model_outputs <- sankey_get_model_outputs(model_outputs_raw, 2050)
#
# inputs_outputs <- get_sankey_node_links_data(technologies,
#                                              technology_inputs,
#                                              technology_outputs,
#                                              model_outputs,
#                                              'Cement')
#
# all_inputs <- inputs_outputs[[1]]
# all_outputs <- inputs_outputs[[2]]
#
# sankey_plotly(all_inputs, all_outputs, data)

