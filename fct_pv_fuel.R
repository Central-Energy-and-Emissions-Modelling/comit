
#' Function which gets coefficient associated with fuel use of each used technology
#' @param data list of data tables read in from excel data template
#' @param decision_variables dataframe of decision variables
#' @return A two column dataframe. The first column specifies the index of the decision variable.
#' The second column gives the the coefficient associated with fuel use
PV_fuel_cost <- function(data, decision_variables) {

  # start by calculating fuel use and costs, ignoring the fact that some electricity is generated on site
  fuel_costs <- decision_variables %>%
    filter(variable_type %in% c("used_capacity", "non_industry_H2")) %>%

    # join in each technology's input amount
    left_join(
      data$technology_input_output %>%
        filter(output < 0 | commodity == "ELCGEN") %>%
        select(technology_code, commodity, output) %>%
        mutate(output = -output) %>%
        rename(input = output),
      by = c("code" = "technology_code")
    ) %>%

    # add input amount and commodity for green and blue hydrogen used by non-industry
    mutate(
      input = if_else(variable_type == "non_industry_H2", 1, input),
      commodity = if_else(variable_type == "non_industry_H2", code, commodity)
    ) %>%

    # before merging in each commodity's price, change the "generated electricity" commodity name to that of
    # grid purchased electricity. This ensures that the price of generated and grid purchased electricity is the same
    mutate(commodity = if_else(commodity == "ELCGEN", "INDDISTELC", commodity)) %>%

    # join in each commodity's price
    left_join(data$Fuel_costs, by = c("commodity", "year" = "year")) %>%

    # replace any missing cost data (NAs) with zero to allow fuel cost calculations
    mutate(cost = ifelse(is.na(cost), 0, cost)) %>%


    # RETROFIT CODE_________________________________________________________________________________________________________

    # join in whether each technology is a retrofit or not
    left_join(select(data$Technologies, code, retrofit_to), by = "code")


  #create a conversion matrix for corresponding existing tech and retrofit used capacity decision variables
  #-------------------------------
  base_capacity = decision_variables %>%
    filter(variable_type == "used_capacity") %>%
    filter(code %in% data$Technologies$retrofit_to)

  # select retrofit decision variables
  link_matrix <- decision_variables %>%
    filter(variable_type == "used_capacity") %>%
    left_join(select(data$Technologies, code, retrofit_to), by = "code") %>%
    filter(!is.na(retrofit_to))  %>%

    # join in base technology information
    left_join(
      base_capacity,
      by = c("retrofit_to" = "code", "site_ID", "year"),
      suffix = c(".retro", ".base")
    ) %>%
    # select the columns needed
    select(
      variable_index.retro,
      variable_index.base,
      year,
      code,
      site_ID,
      variable_name.retro,
      variable_name.base,
      retrofit_to
    )
  #-------------------------------


  # identify the corresponding base (existing) technologies
  base_fuel_costs = fuel_costs  %>% filter(code %in% data$Technologies$retrofit_to)

  # link the base technology fuel cost to the corresponding retrofit decision variable so it can be subtracted
  retrofit_fuel_costs =  fuel_costs  %>%   filter(!is.na(retrofit_to)) %>%
    left_join(
      select(base_fuel_costs, year, site_ID, code, commodity, input, cost),
      by = c("retrofit_to" = "code", "year", "site_ID", "commodity"),
      suffix = c(".retrofit", ".base")
    )


  # now handle two cases: where the existing technologies have commodities that
  # don't match and that do match with the retrofit technologies.
  # Need to adjust both types.

  adjusted_fuel_costs_1 = anti_join(
    base_fuel_costs,
    retrofit_fuel_costs,
    by = c("site_ID", "year", "commodity", "code" = "retrofit_to")
  ) %>%
    left_join(
      select(
        link_matrix,
        variable_index.retro,
        variable_index.base,
        code
      ),
      by = c("variable_index" = "variable_index.base")
    ) %>%
    mutate(variable_index = variable_index.retro, code = code.y) %>%
    select(-code.y, -code.x, -variable_index.retro) %>% mutate(input = -input)

  adjusted_fuel_costs_2 = inner_join(
    retrofit_fuel_costs,
    select(
      base_fuel_costs,
      -variable_name,
      -variable_index,
      -retrofit_to,
      -pipe_cluster_end
    ),
    by = c(
      "site_ID",
      "year",
      "commodity",
      "retrofit_to" = "code",
      "terminal",
      "cluster",
      "variable_type",
      "storage_site"
    )
  ) %>%
    mutate(input = -input.base, cost = cost.base) %>% select(-input.base, -cost.base, -input.retrofit, -cost.retrofit)
  # add the 'subtracted retrofit fuel cost' into the main fuel cost (dataframe will be NULL if no retrofit techs)

  fuel_costs = rbind(fuel_costs, adjusted_fuel_costs_1, adjusted_fuel_costs_2) %>%

    # ___________________________________________________________________________________________________________________________

    # sum all fuel costs per used_capacity variable
    group_by(variable_index, year) %>%
    summarise(fuel_cost = sum(cost * input), .groups = "drop") %>%

    # get present value of costs over timestep because fuel needs to be paid annually
    mutate(
      coefficient = present_value(
        FV = fuel_cost,
        rate = data$rates$discount,
        start_period = year - data$model_parameters$start_year,
        n_periods = data$model_parameters$timestep
      )
    ) %>%

    select(variable_index, coefficient)

  return(fuel_costs)
}
