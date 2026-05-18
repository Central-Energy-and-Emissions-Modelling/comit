#' processes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_processes_ui <- function(id, flow_category, sectors) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(inputId = ns('flow_type'),
                           'Flow type:',
                           flow_category,
                           selected = flow_category),
        selectInput(ns('processes_sector_selection'),
                    'Sector:',
                    sectors[sectors != 'All']),
        sliderInput(ns('processes_year'), 'Year', 2021, 2051,
                    value = 2021, sep = '', animate = TRUE),
        ### TODO - here need to add filter type and country/region if relevant
        width = 3,
        class = 'fixed-sidebar',
        id = 'processes_filter_sidebar'),
      mainPanel(
        div(tags$p('All work in this tab is currently in development and needs QA.',
                   style = 'margin: auto; color: #D96704; text-align: center; line-height: 40px'),
            style = 'height: 40px; background-color: #F2DA91;'),
        br(),
        fluidRow(
          column(6,
                 uiOutput(ns('processes_input_selection'))),
          column(6,
                 uiOutput(ns('processes_run_selection')))
        ),
        tags$br(),
        # add some space before the plot
        plotlyOutput(ns('processes_plot')),
      )
    )

  )
}

#' processes Server Functions
#'
#' @noRd
mod_processes_server <- function(id,
                                 plot_names,
                                 input_templates,
                                 reactive_deployment_plot_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns



    # add run selector to user interface and update when runs are added - for sankey
    output$processes_run_selection <- renderUI({
      radioButtons(inputId = ns('processes_run_selection'),
                   'Run:',
                   c('None selected', plot_names()),
                   selected = tail(plot_names(), 1))
    })


    # input files for sankeys
    output$processes_input_selection <- renderUI({
      radioButtons(inputId = ns('processes_input_selection'),
                   label = 'Input template:',
                   choices = c('None selected',
                               names(input_templates())
                   ),
                   selected = tail(
                     c('None selected',
                       names(input_templates())
                     ),
                     n = 1)) # always select last element
    })



    sankey_results <- reactive({

      validate(
        need(input$processes_input_selection != 'None selected', "Pick an input template"),
        need(input$processes_run_selection != 'None selected', "Pick a scenario run")
      )

      req(reactive_deployment_plot_data())

      inputs <- input_templates()[[input$processes_input_selection]] %>% tidy()
      outputs <- reactive_deployment_plot_data() %>% filter(run == input$processes_run_selection)


      return(list(inputs, outputs))

    })



    output$processes_plot <- renderPlotly({

      req(sankey_results())

      validate(need(as.character(input$processes_year) %in% names(sankey_results()[[2]]),
                    'No output data for this year...'))

      processes_sankey(
        sankey_results()[[1]],
        sankey_results()[[2]],
        input$processes_year,
        input$processes_sector_selection,
        input$flow_type
      )
    })


  })
}

## To be copied in the UI
# mod_processes_ui("processes_1", flow_category, sectors)

## To be copied in the server
# mod_processes_server("processes_1",
#                      plot_names,
#                      input_templates,
#                      reactive_deployment_plot_data)
