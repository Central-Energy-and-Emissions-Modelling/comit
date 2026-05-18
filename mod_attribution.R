#' attribution UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_attribution_ui <- function(id, sectors) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        radioButtons(inputId = ns('attr_type'),
                     label = 'Type:',
                     choices = c('Direct' = 'Direct (total CO2e)',
                                 'Direct and Indirect' = 'Direct_and_Indirect')),
        selectInput(ns('attr_sector_selection'),
                    'Sector:',
                    sectors),
        ### TODO - here need to add filter type and country/region if relevant
        width = 3,
        class = 'fixed-sidebar' # class is actually passed up a level!
        ),
      mainPanel(
        fluidRow(
          column(6,
                 uiOutput(ns('prev_input_selection')),
                 uiOutput(ns('cf_run_selection'))),
          column(6,
                 uiOutput(ns('scenario_run_selection')))
        ),
        tags$br(),
        # add some space before the plot
        plotOutput(ns('attr_plot')) %>% withSpinner(type = 6),
        tags$h3('Data Table'),
        DT::dataTableOutput(ns('attr_table')),
        br()
      )
    )

  )
}

#' attribution Server Functions
#'
#' @noRd
mod_attribution_server <- function(id,
                                   input_templates,
                                   plot_names,
                                   energy_attr,
                                   emissions_attr,
                                   datatable_options) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # files for attribution
    output$prev_input_selection <- renderUI({

      available_choices <- c('None selected', names(input_templates()))

      radioButtons(
        inputId = ns('input_selection'),
        label = 'Input template:',
        choices = available_choices,
        selected = tail(available_choices, n = 1) # always select last element
      )
    })


    output$cf_run_selection <- renderUI({

      available_choices <- c('None selected',
                             plot_names()[str_detect(plot_names(), '[Cc]ounterfactual')])

      radioButtons(
        inputId = ns('cf_model_selection'),
        label = 'Counterfactual:',
        choices = available_choices,
        selected = tail(available_choices, n = 1)
      )
    })

    output$scenario_run_selection <- renderUI({

      available_choices <- c('None selected',
                             plot_names()[!str_detect(plot_names(), '[Cc]ounterfactual')])

      radioButtons(
        inputId = ns('scenario_model_selection'),
        label = 'Scenario:',
        choices = available_choices,
        selected = tail(available_choices, n = 1)
      ) # always select last element
    })


    attribution_data <- reactive({
      req(input$input_selection)
      if (input$input_selection != 'None selected') {
        input_templates()[[input$input_selection]]
      }
    })

    attr_results <- reactive({

      req(input$cf_model_selection,
          input$scenario_model_selection,
          input$input_selection)

      validate(
        need(input$cf_model_selection        != 'None selected', "Pick a counterfactual run"),
        need(input$scenario_model_selection  != 'None selected', "Pick a scenario run"),
        need(input$input_selection           != 'None selected', "Pick an input template")
      )

      id4 <- showNotification(
        'Calculating attribution...',
        duration = NULL,
        closeButton = FALSE
      )
      on.exit(removeNotification(id4), add = TRUE)

      filter_type <- input$attr_type

      this_cf_energy <- energy_attr()[[input$cf_model_selection]]
      this_cf_emissions <- emissions_attr()[[input$cf_model_selection]]
      this_scenario_energy <- energy_attr()[[input$scenario_model_selection]]
      this_scenario_emissions <- emissions_attr()[[input$scenario_model_selection]]

      return(
        emissions_attribution(
          this_scenario_emissions,
          this_scenario_energy,
          this_cf_emissions,
          this_cf_energy,
          attribution_data(),
          filter_type
        )
      )

    })


    # --- Plot -----------------------------------------------------------------

    output$attr_plot <- renderPlot({
      req(attr_results())
      plot_attribution(attr_results(), input$attr_sector_selection)
    })

    # --- Data table -----------------------------------------------------------

    # Data table
    output$attr_table <- DT::renderDataTable({
      req(attr_results)

      d <- emissions_attribution_table(attr_results(),
                                       input$attr_sector_selection)  %>%
        mutate(emissions = input$attr_type,
               savings = round(savings, 5)) %>%
        select(year, type, sector, emissions, `Savings (MtCO2e)` = savings)

      DT::datatable(
        d,
        options = datatable_options,
        extensions = 'Buttons',
        selection = 'multiple',
        rownames = FALSE
      )

      })
  })
}

## To be copied in the UI
# mod_attribution_ui("attribution_1",
#                    sectors)

## To be copied in the server
# mod_attribution_server("attribution_1",
#                        input_templates,
#                        plot_names,
#                        energy_attr,
#                        emissions_attr,
#                        datatable_options)
