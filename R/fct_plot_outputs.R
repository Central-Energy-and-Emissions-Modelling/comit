
#### Costs ---------------------------------------------------------------------

#' Summarise total costs by year (optionally by type)
#'
#' Harmonises cost labels, applies filters, pivots year columns and aggregates.
#' Optionally includes `Cost_type` in the grouping when `breakdown = TRUE`.
#'
#' @inheritParams summary_filters
#' @param capex_selection Which capex to keep ("Capex" or "Capex_lump")
#' @param breakdown Logical; include cost type in grouping
#' @return Tibble with columns `run`, `year`, optional `Cost_type`, meta fields,
#'   and `total_cost`
#' @seealso [plot_costs()], [plot_costs_by_type()], [plot_cost_shares_by_type()]
#'
#' @export
summarise_costs <- function(data,
                            filter_options,
                            capex_selection = "Capex",
                            breakdown = FALSE) {

  # Harmonise cost types + sector labels
  out_data <- data %>%
    mutate(
      Cost_type = case_when(Sector_infrastructure == "CO2_C2S" ~ "CO2_C2S",
                            TRUE ~ .data$Cost_type),
      cluster_rad = case_when(Sector_infrastructure == "CO2_C2S" ~ "CO2_C2S",
                              TRUE ~ .data$cluster_rad),
      Sector_infrastructure = case_when(
        str_detect(.data$Sector_infrastructure, "CO2") ~ "CO2 Infrastructure",
        str_detect(.data$Sector_infrastructure, "H2")  ~ "H2 Infrastructure",
        TRUE ~ .data$Sector_infrastructure
      )
    )

  out_data %<>%
    mutate(Cost_type = factor(
      .data$Cost_type,
      levels = c(
        "Opex",
        "Capex",
        "Capex_lump",
        "Fuel cost",
        "Carbon cost",
        "CO2_C2S"
      )
    ))

  out_data <- summary_filters(out_data,
                              filter_options)

  # Avoid capex double counting
  capex_options <- c("Capex", "Capex_lump")
  dropped_capex <- capex_options[capex_options != capex_selection]

  cost_outputs <- out_data %>%
    filter(.data$Cost_type != dropped_capex) %>%
    .pivot_year("cost", cols_pattern = "^2", cast_year = TRUE)

  # Grouping: add Cost_type only when breakdown = TRUE
  cost_groups <- c("year", "run", if (breakdown) "Cost_type")

  cost_out <- cost_outputs %>%
    group_by(across(all_of(cost_groups))) %>%
    summarise(total_cost = sum(.data$cost), .groups = "drop") %>%
    get_table_cols(filter_options, total_cost)

  return(cost_out)
}

#' Plot total costs by year
#'
#' @param data Output of [summarise_costs()] with `total_cost`
#' @param my_colours Named vector of colours keyed by run
#' @return A ggplot object
#'
#' @export
plot_costs <- function(data, my_colours) {
  .line_plot(data, y_col = total_cost, y_label = "Costs (£m)",
             title = "Total Costs by Year", my_colours = my_colours)
}

#' Plot total costs by year, faceted by cost type
#'
#' @inheritParams plot_costs
#' @return A ggplot object
#'
#' @export
plot_costs_by_type <- function(data, my_colours) {
  .line_plot(data, y_col = total_cost, y_label = "Cost (£m)",
             title = "Total Cost by Type", my_colours = my_colours,
             facet_var = "Cost_type")
}

#' Compute cost shares and cumulative ribbons
#'
#' Converts `total_cost` into shares within each (year, run) and computes
#' cumulative ymin/ymax for stacked ribbon plots.
#'
#' @param data Tibble from [summarise_costs()] grouped by type
#' @return Tibble with `share`, `ymin`, `ymax`
#' @keywords internal
#'
#' @export
get_cost_shares <- function(data) {
  .compute_shares(data, total_col = total_cost, by = c("year", "run"),
                  category_col = "Cost_type")
}

#' Plot stacked cost shares by year (per run)
#'
#' @param data Output of [get_cost_shares()]
#' @return A ggplot object
#'
#' @export
plot_cost_shares_by_type <- function(data) {
  data <- get_cost_shares(data)

  palette <- c(
    "CO2_C2S"     = "#54FFD5",
    "Carbon cost" = "grey70",
    "Fuel cost"   = "#F7C821",
    "Capex"       = "#97b7d3",
    "Capex_lump"  = "#97b7d3",
    "Opex"        = "#CC314B"
  )

  .ribbon_share_plot(
    data, ymin_col = ymin, ymax_col = ymax, fill_col = "Cost_type",
    title = "Cost Shares", ylab = "Share of Total Cost",
    palette = palette
  )
}


#### Emissions -----------------------------------------------------------------


#' Backward-compatibility for legacy emissions outputs
#'
#' Normalises historical files that used `Emissions_total` by creating or
#' renaming to `Emissions_category` and aligning labels.
#'
#' @param data Data frame of emissions outputs
#' @return Data frame with consistent `Emissions_category`
#' @keywords internal
correct_legacy_emissions_outputs <- function(data) {
  if ("Emissions_total" %in% names(data) && "Emissions_category" %in% names(data)) {
    data  %>%
      mutate(
        Emissions_category = case_when(
          !is.na(.data$Emissions_category) ~ .data$Emissions_category,
          .data$Emissions_total == "Direct (total)" ~ "Direct (total CO2e)",
          TRUE ~ .data$Emissions_total
        )
      ) %>%
      select(!.data$Emissions_total)
  } else if ("Emissions_total" %in% names(data)) {
    data %>%
      rename(Emissions_category = .data$Emissions_total) %>%
      mutate(
        Emissions_category = case_when(
          .data$Emissions_category == "Direct (total)" ~ "Direct (total CO2e)",
          TRUE ~ .data$Emissions_category
        )
      )
  } else {
    data
  }
}

.summarise_emissions_core <- function(data, filter_categories) {
  data %>%
    filter(.data$Emissions_category %in% filter_categories) %>%
    .pivot_year("emissions", cols_pattern = "^2", cast_year = TRUE) %>%
    group_by(.data$year, .data$run) %>%
    summarise(total_emissions = sum(.data$emissions) / 1000, .groups = "drop")
}

#' Summarise total direct emissions (MtCO2e) by year
#'
#' Applies filters, normalises legacy formats, pivots years and aggregates to
#' total direct emissions in MtCO2e.
#'
#' @inheritParams summary_filters
#' @return Tibble with `total_emissions`
#'
#' @export
summarise_emissions <- function(data, filter_options) {
  data %>%
    summary_filters(filter_options) %>%
    correct_legacy_emissions_outputs() %>%
    .summarise_emissions_core(c("Direct (total CO2e)", 'Negative')) %>%
    get_table_cols(filter_options,
                   total_emissions,
                   round_digits = 5)
}

#' Plot total direct emissions by year
#'
#' @param data Output of [summarise_emissions()]
#' @param my_colours Named vector of colours keyed by run
#' @return A ggplot object
#'
#' @export
plot_emissions <- function(data, my_colours) {
  .line_plot(data, y_col = total_emissions, y_label = "Emissions (MtCO2e)",
             title = "Total Emissions by Year", my_colours = my_colours)
}

#### Carbon capture ------------------------------------------------------------

#' Summarise CO2 captured (MtCO2) by year
#'
#' As per [summarise_emissions()] but for the `Captured` emissions category.
#'
#' @inheritParams summary_filters
#' @return Tibble with `total_emissions` (captured)
#'
#' @export
summarise_capture <- function(data, filter_options) {
  data %>%
    summary_filters(filter_options) %>%
    correct_legacy_emissions_outputs() %>%
    .summarise_emissions_core("Captured") %>%
    get_table_cols(filter_options,
                   total_emissions, round_digits = 5)
}

#' Plot CO2 captured by year
#'
#' @param data Output of [summarise_capture()]
#' @param my_colours Named vector of colours keyed by run
#' @return A ggplot object
#'
#' @export
plot_capture <- function(data, my_colours) {
  .line_plot(data, y_col = total_emissions, y_label = "Emissions captured (MtCO2)",
             title = "Total Emissions Captured by Year", my_colours = my_colours)
}


#### Fuel use ------------------------------------------------------------------

#' Summarise total fuel use by year and fuel category
#'
#' Pivots only `*_TWh` columns, strips the suffix to produce numeric years, and
#' aggregates to `total_fuel`.
#'
#' @inheritParams summary_filters
#' @return Tibble with `total_fuel`
#'
#' @export
summarise_fuel <- function(data, filter_options) {

  data <- summary_filters(data, filter_options)

  # Pivot only *_TWh columns; strip suffix to year
  outputs <- data %>%
    select(-matches('_ktCO2e$')) %>%
    .pivot_year("emissions", cols_pattern = "_TWh$", cast_year = FALSE,
                transform_year = function(x) as.integer(str_remove_all(x, "_TWh"))) %>%
    group_by(.data$year, .data$Fuel_category, .data$run) %>%
    summarise(total_fuel = sum(.data$emissions), .groups = "drop")

  outputs %>%
    get_table_cols(filter_options,
                   total_fuel, round_digits = 4)
}


#' Plot total fuel use by year, faceted by fuel category
#'
#' @param data Output of [summarise_fuel()]
#' @param my_colours Named vector of colours keyed by run
#' @return A ggplot object
#'
#' @export
plot_fuel <- function(data, my_colours) {
  .line_plot(data, y_col = total_fuel, y_label = "Fuel used (TWh)",
             title = "Total Fuel Use by Year", my_colours = my_colours,
             facet_var = "Fuel_category")
}


#' Compute fuel shares and cumulative ribbons
#'
#' Drops `NonEnergyUse`, orders fuel categories, computes shares within
#' (year, run) and cumulative bounds for ribbons.
#'
#' @param data Tibble from [summarise_fuel()]
#' @return Tibble with `share`, `ymin`, `ymax`
#' @keywords internal
#'
#' @export
get_fuel_shares <- function(data) {
  data %>%
    filter(.data$Fuel_category != "NonEnergyUse") %>%
    mutate(Fuel_category = factor(.data$Fuel_category,
                                  levels = c("Electricity", "Coal", "Oil", "Gas",
                                             "Hydrogen", "Biomass and organic waste",
                                             "Inorganic waste"))) %>%
    .compute_shares(total_col = total_fuel, by = c("year", "run"),
                    category_col = "Fuel_category")
}


#' Plot fuel shares over time, faceted by fuel
#'
#' @param data Output of [get_fuel_shares()]
#' @param my_colours Named vector of colours keyed by run
#' @return A ggplot object
#'
#' @export
plot_fuel_shares <- function(data, my_colours) {
  data <- data %>%
    drop_na(.data$share) %>%
    mutate(Fuel_category = as.character(.data$Fuel_category))

  # suppress messages because of two scale_y_continuous calls
  suppressMessages(
    .line_plot(data, y_col = share, y_label = "Fuel share (%)",
               title = "Total Fuel Share by Year", my_colours = my_colours,
               facet_var = "Fuel_category") +
      scale_y_continuous(limits = c(0, NA), expand = c(0, 0),
                         labels = percent_format())
  )

}

#' Plot stacked fuel shares by year (per run)
#'
#' @param data Output with `share`, `ymin`, `ymax` from [get_fuel_shares()]
#' @return A ggplot object
#'
#' @export
plot_fuel_shares_stacked <- function(data) {
  palette <- c(
    "Oil"                       = "grey40",
    "Electricity"               = "#ffeb00",
    "Hydrogen"                  = "#44B0E2",
    "Gas"                       = "#F28705",
    "Biomass and organic waste" = "forestgreen",
    "Inorganic waste"           = "purple4",
    "Coal"                      = "grey20"
  )

  .ribbon_share_plot(
    data %>% drop_na(.data$share),
    ymin_col = ymin, ymax_col = ymax, fill_col = "Fuel_category",
    title = "Fuel shares", ylab = "Share of total fuel use",
    palette = palette
  )
}


#### Deployment -----------------------------------------------------------------

#' Summarise technology deployment by year and tech category
#'
#' Filters by `Unit`, pivots years (stripping suffixes like `_TWh`), and
#' aggregates to `total_deployment`.
#'
#' @inheritParams summary_filters
#' @param unit Character; the unit (as in the data) to filter by
#' @return Tibble with `total_deployment`
#'
#' @export
summarise_deployment <- function(data, filter_options, unit) {

  data <- summary_filters(data, filter_options)

  outputs <- data %>%
    filter(.data$Unit == unit) %>%
    .pivot_year("deployment", cols_pattern = "^2", cast_year = TRUE) %>%
    group_by(.data$year, .data$Technology_category, .data$run, .data$Unit) %>%
    summarise(total_deployment = sum(.data$deployment), .groups = "drop")

  outputs %>%
    get_table_cols(filter_options, total_deployment, round_digits = 4) %>%
    mutate(Unit = unit) %>%
    select(!.data$Technology) # got Technology_category instead
}

#' Plot technology deployment by year, faceted by technology category
#'
#' @param data Output of [summarise_deployment()]
#' @param my_colours Named vector of colours keyed by run
#' @return A ggplot object
#'
#' @export
plot_deployment <- function(data, my_colours) {
  .line_plot(data, y_col = total_deployment,
             y_label = paste0("Output (", unique(data$Unit), ")"),
             title = "Total Output by Year", my_colours = my_colours,
             facet_var = "Technology_category")
}


