
#### Utilities -----------------------------------------------------------------

# Helper to avoid warnings when calculating min/max on length 0.
#' Robust min/max helper
#'
#' Avoids warnings on zero-length vectors by returning -Inf/Inf.
#'
#' @param x Numeric vector
#' @param min_or_max One of "min" or "max"
#' @return A single numeric value (-Inf/Inf on empty input)
#' @keywords internal
robust_min_max <- function(x, min_or_max) {
  if (min_or_max == "min") {
    return({if (length(x) > 0) min(x) else -Inf})
  } else {
    return({if (length(x) > 0) max(x) else Inf})
  }
}

# Compute decade-ish breaks including start/end years.
.year_breaks <- function(start_year, end_year) {

  if (!is.finite(start_year) || !is.finite(end_year)) return(NULL)
  if (start_year > end_year) return(NULL)

  brks <- seq(start_year, end_year, by = 10)
  brks <- unique(c(start_year, brks, end_year))

  return(brks)
}

# Common minimal theme
.base_theme <- function() {
  theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line.x = element_line(),
      text = element_text(size = 14)
    )
}

# X scale using computed limits/breaks
.scale_x_year <- function(start_year, end_year) {
  scale_x_continuous(
    limits = c(start_year, end_year),
    breaks = .year_breaks(start_year, end_year)
  )
}

# Guard: return placeholder if data is empty or no finite years
.return_placeholder_if_empty <- function(data, year_col = "year") {
  if (nrow(data) == 0 || !any(is.finite(data[[year_col]]))) {
    return(plot_placeholder())
  }
  return(NULL)
}

# Generic line plot builder
.line_plot <- function(data, y_col, y_label, title, subtitle = " ",
                       my_colours, facet_var = NULL, panel_spacing = 1.5,
                       year_col = "year", group_col = "run", colour_col = "run",
                       y_limits_min_zero = TRUE) {
  # Early exit if empty
  ph <- .return_placeholder_if_empty(data, year_col)
  if (!is.null(ph)) return(ph)

  y_sym <- ensym(y_col)
  year_sym <- ensym(year_col)

  start_year <- robust_min_max(data[[year_col]], "min")
  end_year   <- robust_min_max(data[[year_col]], "max")

  y_limits <- if (y_limits_min_zero) c(0, NA) else c(NA, NA)

  p <- ggplot(
    data %>% drop_na(!!y_sym),
    aes(x = !!year_sym, y = !!y_sym, group = .data[[group_col]])
  ) +
    geom_line(aes(colour = .data[[colour_col]]), linewidth = 1) +
    scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
    .scale_x_year(start_year, end_year) +
    scale_color_manual(values = my_colours, name = "Run:") +
    .base_theme() +
    labs(title = title, subtitle = subtitle, y = y_label, x = NULL)

  if (!is.null(facet_var)) {
    p <- p + theme(panel.spacing.x = unit(panel_spacing, "lines")) +
      facet_wrap(vars(.data[[facet_var]]))
  }

  p
}

# Generic stacked ribbon share plot
.ribbon_share_plot <- function(data, ymin_col, ymax_col, fill_col, title, ylab,
                               facet_var = "run", subtitle = " ",
                               palette = NULL, year_col = "year") {
  # Early exit if empty
  ph <- .return_placeholder_if_empty(data, year_col)
  if (!is.null(ph)) return(ph)

  ymin_sym <- ensym(ymin_col)
  ymax_sym <- ensym(ymax_col)
  year_sym <- ensym(year_col)

  start_year <- robust_min_max(data[[year_col]], "min")
  end_year   <- robust_min_max(data[[year_col]], "max")

  p <- ggplot(
    data %>% drop_na(!!ymin_sym, !!ymax_sym),
    aes(x = !!year_sym, group = .data[[fill_col]])
  ) +
    geom_ribbon(
      aes(ymin = !!ymin_sym, ymax = !!ymax_sym, fill = .data[[fill_col]]),
      colour = "black"
    ) +
    scale_y_continuous(expand = c(0, 0), labels = percent_format()) +
    .scale_x_year(start_year, end_year) +
    .base_theme() +
    labs(title = title, subtitle = subtitle, y = ylab, x = NULL) +
    facet_wrap(vars(.data[[facet_var]])) +
    guides(fill = guide_legend(reverse = TRUE))

  if (!is.null(palette)) p <- p + scale_fill_manual(values = palette)

  return(p)
}

# Compute shares + cumulative ribbons (generic)
.compute_shares <- function(data, total_col, by = c("year", "run"),
                            category_col) {
  total_sym <- ensym(total_col)

  data %<>%
    arrange(.data[[category_col]]) %>%
    group_by(across(all_of(by))) %>%
    mutate(
      share   = (!!total_sym) / sum(!!total_sym),
      prev_val = lag(share, default = 0),
      ymin    = cumsum(prev_val),
      ymax    = cumsum(share)
    ) %>%
    ungroup()

  return(data)
}

# Build a tidy year/value from wide columns that start with a pattern (default '2')
.pivot_year <- function(data, value_name, cols_pattern = "^2", cast_year = TRUE,
                        transform_year = NULL) {
  out <- data %>%
    pivot_longer(cols = matches(cols_pattern),
                 names_to = "year", values_to = value_name)
  if (!is.null(transform_year)) {
    out <- out %>%
      mutate(year = transform_year(.data$year))
  } else if (cast_year) {
    out <- out %>%
      mutate(year = as.integer(.data$year))
  }

  return(out)
}

# Compose a consistent run factor order (appearance order)
.order_runs <- function(data) {
  data %>%
    mutate(run = factor(.data$run, levels = unique(.data$run)))
}

# Smart selection of optional columns for tables
.select_table_cols <- function(data) {
  optional <- c("Fuel_category", "Primary_output", "Technology_category",
                "Cost_type")
  keep <- c("run", "year", intersect(optional, names(data)),
            "Sector", "Technology", "Cluster", "Distance")
  keep <- unique(keep[keep %in% names(data)])

  data %<>% select(all_of(keep))

  return(data)
}

# Standardise the meta columns to attach to exported tables
.attach_table_meta <- function(data, filter_options) {
  data %>%
    mutate(
      Sector = filter_options$sector,
      Technology = filter_options$technology,
      Cluster = filter_options$this_cluster,
      Distance = ifelse(length(filter_options$cluster_category) == 4,
                        "All",
                        list(filter_options$cluster_category))
    )
}

# Round a column by tidy-eval name
.round_col <- function(data, col, digits = 2) {
  col_sym <- ensym(col)
  mutate(data, !!col_sym := round(!!col_sym, digits))
}


#### IO + naming ----------------------------------------------------------------

# wb can be an actual wb file or a filepath to an xlsx sheet to be read.
#' Read workbooks and attach run names
#'
#' Reads a list of workbook objects or file paths and binds them into a single
#' tibble, tagging each row with a derived `run` name.
#'
#' @param wb List of workbook objects **or** file paths to `.xlsx` files.
#' @param wb_names Character vector of names corresponding to `wb`.
#' @param tab Sheet name to read from each workbook.
#' @return A tibble with all rows bound and a `run` column.
get_wbs_values <- function(wb, wb_names, tab) {
  map2_dfr(wb, wb_names, ~{
    run_name <- get_run_name(.y)
    outputs <- if (is.character(.x)) {
      read_xlsx(.x, sheet = tab)
    } else {
      readWorkbook(.x, sheet = tab)
    }
    as_tibble(outputs) %>%
      mutate(run = run_name)
  })
}

#' Normalise a workbook file name into a run name
#'
#' Strips common prefixes/suffixes to produce a clean `run` label.
#'
#' @param file_name Character file name
#' @return Character run name
#' @keywords internal
get_run_name <- function(file_name) {
  str_remove_all(file_name, "\\.xlsx") %>%
    str_remove_all("Scenario_") %>%
    str_remove_all("output_") %>%
    str_remove_all("Data_") %>%
    str_remove_all("template_") %>%
    str_remove_all("complete_") %>%
    str_remove_all("dev_") %>%
    str_remove_all("comit_") %>%
    str_remove_all("input_")
}


#### Filtering + table shaping ---------------------------------------------------

#' Apply common filters for summary tables
#'
#' Filters by selected sector/technology/cluster and limits to specified models
#' and cluster categories. Also standardises run ordering and renames
#' `Sector_infrastructure` to `Sector` when present.
#'
#' @param data Data frame of outputs
#' @param filter_options named list for the values to keep for different variables.
#'  Should contain the following elements:
#'  * "sector" Character, selected sector or 'All'
#'  * "technology" Character, selected technology category or 'All'
#'  * "this_cluster" Character, selected cluster or 'All'
#'  * "cluster_category" Character vector of cluster radii/categories to keep
#'  * "models_to_present" Character vector of run names to include
#' @return Filtered data frame
summary_filters <- function(data, filter_options) {

  data <- .order_runs(data)

  data <- filter(
    data,
    .data$run %in% filter_options$models_to_present,
    .data$cluster_rad %in% filter_options$cluster_category
  )

  # rename sector col name when costs is used
  if ("Sector_infrastructure" %in% names(data)) {
    data <- rename(data, Sector = Sector_infrastructure)
  }

  if (filter_options$sector != "All") {
    data <- filter(data, .data$Sector == filter_options$sector)
  }

  if (filter_options$technology != "All") {
    data <- filter(data, .data$Technology_category == filter_options$technology)
  }

  if (filter_options$this_cluster != "All") {
    data <- filter(data, .data$cluster == filter_options$this_cluster)
  }

  return(data)
}

#' Attach meta columns and keep tidy table fields
#'
#' Adds `Sector/Technology/Cluster/Distance` metadata, keeps relevant optional
#' fields when present, and rounds the requested outcome column.
#'
#' @param data Data frame containing a computed outcome column
#' @param filter_options See [summary_filters()]
#' @param outcome_var Tidy-eval column specifying the numeric outcome to keep
#' @param round_digits Integer, number of digits to round to
#' @return A tibble ready for saving/export
get_table_cols <- function(data, filter_options, outcome_var, round_digits = 2) {
  # Attach meta columns
  base <- data %>%
    .attach_table_meta(filter_options)

  # Preferred core columns (only those present will be kept)
  core <- c("run", "year", "Fuel_category", "Primary_output",
            "Technology_category", "Cost_type",
            "Sector", "Technology", "Cluster", "Distance")

  out <- base %>%
    select(any_of(core), {{ outcome_var }}) %>%
    mutate({{ outcome_var }} := round({{ outcome_var }}, round_digits))

  return(out)
}





#### Colours, placeholders, spinners -------------------------------------------

#' Default colour palette for runs
#'
#' Returns a named vector mapping `run` to colours.
#'
#' @param data Data frame that includes a `run` column
#' @return Named character vector of colours
get_my_colours <- function(data) {
  my_colours <- c(
    "#001a2b", "#97b7d3", "#54FFD5", "#CC314B", "grey50",
    "#F7C821", "#9EAD39", "#463018", "#AB4C11", "#191919"
  )
  setNames(my_colours, unique(data$run))
}

#' Placeholder plot for empty/missing data
#'
#' @return A ggplot object with an instructional message
#' @keywords internal
plot_placeholder <- function() {
  ggplot(data = data.frame(), aes(x = 1, y = 1)) +
    geom_text(aes(label = "Run a model or provide previous\noutputs to visualise results"),
              size = 5, colour = "grey35") +
    theme_void() +
    theme(panel.background = element_rect(fill = "#f8f8f8", colour = "#f8f8f8"))
}

# to show all plot spinners (assumes UI framework provides showSpinner)
#' Show all plot spinners in the UI
#'
#' Convenience wrapper for app's spinner function.
#'
#' @return Invisibly, after calling UI spinners
#' @keywords internal
show_spinners <- function() {
  showSpinner("plot1")
  showSpinner("plot2")
  showSpinner("plot3")
  showSpinner("plot4")
}

