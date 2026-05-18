#' outputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_outputs_ui <- function(id,
                           sectors,
                           technology_category,
                           clusters_vec,
                           cluster_category) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(tags$div(h4('Filters',
                               style = 'font-weight: bold;')),
                   uiOutput(ns('run_selection')),
                   selectInput(ns('sector_selection'),
                               'Sector:',
                               sectors),
                   selectInput(ns('technology_selection'),
                               'Technology type:',
                               technology_category),
                   selectInput(ns('cluster_selection'),
                               'Cluster:',
                               clusters_vec),
                   checkboxGroupInput(inputId = ns('cluster_category_selection'),
                                      'Cluster distance:',
                                      cluster_category,
                                      selected = cluster_category),
                   width = 3,
                   class = 'fixed-sidebar',
                   id = ns('filter_sidebar')),
      mainPanel(
        tags$br(), # add some space before the plot
        tabsetPanel(
          id = ns('tabset'),
          tabPanel('Emissions',
                   tabsetPanel(
                     tabPanel(
                       'Direct Emissions',
                       tags$br(),
                       # add some space before the plot,
                       plotOutput(ns('plot1')) %>% withSpinner(type = 6),
                       tags$h3('Data Table'),
                       DT::dataTableOutput(ns('table1'))
                     ),

                     tabPanel('Capture',
                              tags$br(),
                              plotOutput(ns('plot2')) %>% withSpinner(type = 6),
                              tags$h3('Data Table'),
                              DT::dataTableOutput(ns('table2')))
                   )),
          tabPanel('Fuel Use',
                   tabsetPanel(
                     tabPanel('Fuel Use',
                              tags$br(),
                              plotOutput(ns('plot3')) %>% withSpinner(type = 6),
                              tags$h3('Data Table'),
                              DT::dataTableOutput(ns('table3'))),
                     tabPanel('Fuel Share',
                              tags$br(),
                              plotOutput(ns('plot3b')) %>% withSpinner(type = 6),
                              tags$h3('Data Table'),
                              DT::dataTableOutput(ns('table3b'))),
                     tabPanel('Fuel Share (stacked)',
                              tags$br(),
                              plotOutput(ns('plot3c')) %>% withSpinner(type = 6),
                              tags$h3('Data Table'),
                              DT::dataTableOutput(ns('table3c'))))
          ),
          tabPanel('Costs',
                   radioButtons(ns('capex_selection'),
                                label = 'Capex method:',
                                choices = c('Smooth' = 'Capex',
                                            'Lumpy' = 'Capex_lump')),
                   tabsetPanel(
                     tabPanel(
                       'Total',
                       tags$br(),
                       plotOutput(ns('plot4')) %>% withSpinner(type = 6),
                       tags$h3('Data Table'),
                       DT::dataTableOutput(ns('table4'))
                     ),
                     tabPanel(
                       'By type',
                       tags$br(),
                       plotOutput(ns('plot4b')) %>% withSpinner(type = 6),
                       tags$h3('Data Table'),
                       DT::dataTableOutput(ns('table4b'))
                     ),
                     tabPanel(
                       'Share by type',
                       tags$br(),
                       plotOutput(ns('plot4c')) %>% withSpinner(type = 6),
                       tags$h3('Data Table'),
                       DT::dataTableOutput(ns('table4c'))
                     )

                   )),
          tabPanel('Deployment',
                   tabsetPanel(
                     tabPanel('Mt deployed',
                              tags$br(),
                              plotOutput(ns('plot5')) %>% withSpinner(type = 6),
                              tags$h3('Data Table'),
                              DT::dataTableOutput(ns('table5'))),
                     tabPanel('PJ deployed',
                              tags$br(),
                              plotOutput(ns('plot6')) %>% withSpinner(type = 6),
                              tags$h3('Data Table'),
                              DT::dataTableOutput(ns('table6')))
                   )

        ), type = 'pills'),
        br()
      )
    )

  )
}

#' outputs Server Functions
#'
#' @noRd
mod_outputs_server <- function(id,
                               plot_names,
                               plot_data,
                               this_set_of_runs,
                               reactive_cost_plot_data,
                               reactive_fuel_plot_data,
                               reactive_emissions_plot_data,
                               reactive_deployment_plot_data,
                               my_cols,
                               datatable_options) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # add run selector to user interface and update when runs are added
    output$run_selection <- renderUI({
      checkboxGroupInput(inputId = ns('model_selection'),
                         'Run:',
                         plot_names(),
                         selected = plot_names())
    })


    # Update when new data is found
    observeEvent(this_set_of_runs(), ignoreInit = TRUE, {

      tryCatch({ # using try so we don't loose new outputs if plot fails

        id5 <- showNotification('Gathering outputs...',
                                duration = NULL, closeButton = FALSE)
        on.exit(removeNotification(id5), add = TRUE)

        runs_to_plot <- plot_data()[this_set_of_runs()]

        emissions_plot_data <- get_wbs_values(wb = runs_to_plot,
                                              wb_names = this_set_of_runs(),
                                              'Emissions')

        fuel_plot_data <- get_wbs_values(wb = runs_to_plot,
                                         wb_names = this_set_of_runs(),
                                         'Energy')

        costs_plot_data <- get_wbs_values(wb = runs_to_plot,
                                          wb_names = this_set_of_runs(),
                                          'Costs')

        deployment_plot_data <- get_wbs_values(wb = runs_to_plot,
                                               wb_names = this_set_of_runs(),
                                               'Outputs')


        if(isTruthy(reactive_cost_plot_data())) { #check if we already have some tables

          reactive_emissions_plot_data(bind_rows(reactive_emissions_plot_data(),
                                                 emissions_plot_data))

          reactive_fuel_plot_data(bind_rows(reactive_fuel_plot_data(),
                                            fuel_plot_data))

          reactive_cost_plot_data(bind_rows(reactive_cost_plot_data(),
                                            costs_plot_data))

          reactive_deployment_plot_data(bind_rows(reactive_deployment_plot_data(),
                                                  deployment_plot_data))

        } else {
          reactive_emissions_plot_data(emissions_plot_data)
          reactive_fuel_plot_data(fuel_plot_data)
          reactive_cost_plot_data(costs_plot_data)
          reactive_deployment_plot_data(deployment_plot_data)
        }

        # set colours on full dataset
        my_cols(get_my_colours(reactive_cost_plot_data()))

      },
      error = function(err) {

        showNotification(sprintf("Error whilst getting plot values: %s", err),
                         type = 'error',
                         duration = NULL)

      })

  })


    # create reactive for common filters
    active_filter_options <- reactive({
      list(
        "sector" = input$sector_selection,
        "technology" = input$technology_selection,
        "this_cluster" = input$cluster_selection,
        "cluster_category" = input$cluster_category_selection,
        "models_to_present" = input$model_selection
      )
    }) %>% debounce(150)

    # create reactives for summary data for use in plots and tables
    emissions_summary <- reactive({
      req(reactive_emissions_plot_data())
      summarise_emissions(reactive_emissions_plot_data(), active_filter_options())
    })

    capture_summary <- reactive({
      req(reactive_emissions_plot_data())
      summarise_capture(reactive_emissions_plot_data(), active_filter_options())
    })

    cost_summary <- reactive({
      req(reactive_cost_plot_data())
      summarise_costs(
        reactive_cost_plot_data(),
        active_filter_options(),
        capex_selection = input$capex_selection
      )
    })

    cost_type_summary <- reactive({
      req(reactive_cost_plot_data())
      summarise_costs(
        reactive_cost_plot_data(),
        active_filter_options(),
        capex_selection = input$capex_selection,
        breakdown = TRUE
      )
    })

    fuel_summary <- reactive({
      req(reactive_fuel_plot_data())
      summarise_fuel(reactive_fuel_plot_data(), active_filter_options())
    })


    mt_deployment_summary <- reactive({
      req(reactive_deployment_plot_data())
      summarise_deployment(reactive_deployment_plot_data(),
                           active_filter_options(),
                           unit = 'Mt')
    })

    pj_deployment_summary <- reactive({
      req(reactive_deployment_plot_data())
      summarise_deployment(reactive_deployment_plot_data(),
                           active_filter_options(),
                           unit = 'PJ')
    })


    ### Make plots from the outputs

    # Plot --------------------------------------------------------

    output$plot1 <- renderPlot({

      if (!isTruthy(reactive_emissions_plot_data())) {
        plot_placeholder()
      } else {
        plot_emissions(emissions_summary(), my_colours = my_cols())
      }
    })


    output$plot2 <- renderPlot({
      if(!isTruthy(reactive_emissions_plot_data())) {
        plot_placeholder()
      } else {
        plot_capture(capture_summary(), my_colours = my_cols())
      }
    })


    output$plot3 <- renderPlot({
      if(!isTruthy(reactive_fuel_plot_data())) {
        plot_placeholder()
      } else {
        plot_fuel(fuel_summary(), my_colours = my_cols())
      }
    })


    output$plot3b <- renderPlot({
      if(!isTruthy(reactive_fuel_plot_data())) {
        plot_placeholder()
      } else {
        plot_fuel_shares(fuel_summary() %>% get_fuel_shares(),
                         my_colours = my_cols())
      }
    })


    output$plot3c <- renderPlot({
      if(!isTruthy(reactive_fuel_plot_data())) {
        plot_placeholder()
      } else {
        plot_fuel_shares_stacked(fuel_summary() %>% get_fuel_shares())
      }
    })


    output$plot4 <- renderPlot({
      if(!isTruthy(reactive_cost_plot_data())) {
        plot_placeholder()
      } else {
        plot_costs(cost_summary(), my_colours = my_cols())
      }
    })

    output$plot4b <- renderPlot({
      if(!isTruthy(reactive_cost_plot_data())) {
        plot_placeholder()
      } else {
        plot_costs_by_type(cost_type_summary(), my_colours = my_cols())
      }
    })

    output$plot4c <- renderPlot({
      if(!isTruthy(reactive_cost_plot_data())) {
        plot_placeholder()
      } else {
        plot_cost_shares_by_type(cost_type_summary())
      }
    })

    output$plot5 <- renderPlot({
      if(!isTruthy(reactive_deployment_plot_data())) {
        plot_placeholder()
      } else {
        plot_deployment(mt_deployment_summary(), my_colours = my_cols())
      }
    })

    output$plot6 <- renderPlot({
      if(!isTruthy(reactive_deployment_plot_data())) {
        plot_placeholder()
      } else {
        plot_deployment(pj_deployment_summary(), my_colours = my_cols())
      }
    })

    # data tables #-------------------------------------------------------------

    observeEvent(reactive_emissions_plot_data(), {

      output$table1 <- DT::renderDataTable({
        DT::datatable(
          emissions_summary(),
          options = datatable_options,
          extensions = 'Buttons',
          selection = 'multiple',
          rownames = FALSE
        )
      })

      output$table2 <- DT::renderDataTable({
        DT::datatable(
          capture_summary(),
          options = datatable_options,
          extensions = 'Buttons',
          selection = 'multiple',
          rownames = FALSE
        )
      })


    })


    observeEvent(reactive_fuel_plot_data(), {

      output$table3 <- DT::renderDataTable({
        DT::datatable(
          fuel_summary(),
          options = datatable_options,
          extensions = 'Buttons',
          selection = 'multiple',
          rownames = FALSE
        )
      })

        output$table3b <- DT::renderDataTable({
          DT::datatable(
            fuel_summary() %>%
              get_fuel_shares() %>%
              select(!c(total_fuel, ymin, ymax, prev_val)) %>%
              mutate(share = round(share, 2)),
            options = datatable_options,
            extensions = 'Buttons',
            selection = 'multiple',
            rownames = FALSE
          )
        })

      output$table3c <- DT::renderDataTable({
        DT::datatable(
          fuel_summary() %>%
            get_fuel_shares() %>%
            select(!c(total_fuel, ymin, ymax, prev_val)) %>%
            mutate(share = round(share, 2)),
          options = datatable_options,
          extensions = 'Buttons',
          selection = 'multiple',
          rownames = FALSE
        )
      })

    })



    observeEvent(reactive_cost_plot_data(), {

      output$table4 <- DT::renderDataTable({
        DT::datatable(
          cost_summary(),
          options = datatable_options,
          extensions = 'Buttons',
          selection = 'multiple',
          rownames = FALSE
        )
      })

      output$table4b <- DT::renderDataTable({
        DT::datatable(
          cost_type_summary(),
          options = datatable_options,
          extensions = 'Buttons',
          selection = 'multiple',
          rownames = FALSE
        )
      })

      output$table4c <- DT::renderDataTable({
        DT::datatable(
          cost_type_summary() %>%
            get_cost_shares(.) %>%
            mutate(share = round(share, digits = 2)) %>%
            select(!c('prev_val', 'ymin', 'ymax')),
          options = datatable_options,
          extensions = 'Buttons',
          selection = 'multiple',
          rownames = FALSE
        )
      })


    })



    observeEvent(reactive_deployment_plot_data(), {

      output$table5 <- DT::renderDataTable({
        DT::datatable(
          mt_deployment_summary(),
          options = datatable_options,
          extensions = 'Buttons',
          selection = 'multiple',
          rownames = FALSE
        )
      })

      output$table6 <- DT::renderDataTable({
        DT::datatable(
          pj_deployment_summary(),
          options = datatable_options,
          extensions = 'Buttons',
          selection = 'multiple',
          rownames = FALSE
        )
      })


    })

  })

  list(
    id = id,
    reactive_cost_plot_data = reactive_cost_plot_data,
    reactive_fuel_plot_data = reactive_fuel_plot_data,
    reactive_emissions_plot_data = reactive_emissions_plot_data,
    reactive_deployment_plot_data = reactive_deployment_plot_data
  )

}

# To be copied in the UI
# mod_outputs_ui("outputs_1",
#               sectors,
#               technology_category,
#               clusters_vec,
#               cluster_category)

## To be copied in the server
# mod_ouputs_server(
#   "outputs_1",
#   plot_names,
#   plot_data,
#   this_set_of_runs,
#   reactive_cost_plot_data,
#   reactive_fuel_plot_data,
#   reactive_emissions_plot_data,
#   reactive_deployment_plot_data,
#   my_cols,
#   datatable_options
# )



### For dev testing ####--------------------------------------------------------
#
# sectors <- c('All',
#              "Cement",
#              "Ceramics",
#              "Chemicals",
#              "Construction",
#              "Electrical engineering",
#              "Food & drink",
#              "Glass",
#              "Iron & steel",
#              "Lime",
#              "Mechanical engineering",
#              "Non-ferrous metals",
#              "Other",
#              "Paper",
#              "Refineries",
#              "Textiles",
#              "Vehicles",
#              "CO2 Infrastructure",
#              "H2 Infrastructure")
#
# clusters_vec <- c('All',
#                   'Grangemouth',
#                   'Humberside',
#                   'Humberside2',
#                   'Londonderry',
#                   'Medway',
#                   'Merseyside',
#                   'Peterhead',
#                   'South Wales',
#                   'Southampton',
#                   'Teeside')
#
# cluster_category <- c('< 25km' = '<25km',
#                       '25 to 30km' = '25-30km',
#                       '> 30km' = '>30km',
#                       'CO2_C2S' = 'CO2_C2S')
#
# flow_category <- c('fuel', 'commodity')
#
# technology_category <- c('All',
#                          'BECCS',
#                          'CCS',
#                          'Coal',
#                          'Dry kiln',
#                          'Electricity',
#                          'Heat pump',
#                          'Hydrogen',
#                          'Natural gas',
#                          'Oil',
#                          'Standard_FF',
#                          'Steam')
#
# outputs_ui <- bslib::page_fluid(
#
#   tabsetPanel(
#     tabPanel('Upload', mod_upload_ui("upload_1"),
#              style = "margin: auto; width: 40%;"),
#     tabPanel('Outputs',
#              mod_outputs_ui("outputs_1",
#                             sectors,
#                             technology_category,
#                             clusters_vec,
#                             cluster_category)
#     ))
# )
#
# outputs_server <- function(input, output, session) {
#
#   # get all reactives and common objects #------------------------------------
#   options(shiny.maxRequestSize = 8000*1024^2) # Increase max file size to ~8GB
#
#   comit_package_version <- desc_get_version()
#
#   out_files <- reactiveVal()
#   output_workbooks <- reactiveVal()
#
#   input_templates <- reactiveVal()
#
#   plot_names <- reactiveVal()
#   plot_data <- reactiveVal()
#   this_set_of_runs <- reactiveVal()
#
#   reactive_cost_plot_data <- reactiveVal()
#   reactive_emissions_plot_data <- reactiveVal()
#   reactive_fuel_plot_data <- reactiveVal()
#   reactive_deployment_plot_data <- reactiveVal()
#
#   cf_workbooks <- reactiveVal()
#   scenario_workbooks <- reactiveVal()
#
#   emissions_attr <- reactiveVal()
#   energy_attr <- reactiveVal()
#
#
#   my_cols <- reactiveVal()
#
#   files_to_remove <- list()
#
#   mod_upload_server("upload_1",
#                     plot_names,
#                     plot_data,
#                     this_set_of_runs,
#                     emissions_attr,
#                     energy_attr,
#                     input_templates)
#
#   datatable_options <- list(paging = FALSE,
#                             scrollX = TRUE,
#                             scrollY = '50vh', #this means the table will be 50% of viewport height
#                             server = FALSE,
#                             dom = 'Brtip',
#                             buttons = list('copy',
#                                            list(extend = 'csv',
#                                                 title = 'Download',
#                                                 text = 'Download')),
#                             search = NULL)
#
#   mod_outputs_server("outputs_1",
#                      plot_names,
#                      plot_data,
#                      this_set_of_runs,
#                      reactive_cost_plot_data,
#                      reactive_fuel_plot_data,
#                      reactive_emissions_plot_data,
#                      reactive_deployment_plot_data,
#                      my_cols,
#                      datatable_options)
#
# }
#
# shinyApp(outputs_ui, outputs_server)

