#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  #--- General setup -----------------------------------------------------------

  options(shiny.maxRequestSize = 1000*1024^2) # Increase max file size to ~1GB

  original_wd <- getwd()

  comit_package_version <- get_comit_version()


  # make temp directory for file saving
  tmpdir <- tempdir()
  setwd(tmpdir)

  #--- set all global reactives ------------------------------------------------

  out_files <- reactiveVal()
  output_workbooks <- reactiveVal()

  input_templates <- reactiveVal()

  plot_names <- reactiveVal()
  plot_data <- reactiveVal()
  this_set_of_runs <- reactiveVal()

  reactive_cost_plot_data <- reactiveVal()
  reactive_emissions_plot_data <- reactiveVal()
  reactive_fuel_plot_data <- reactiveVal()
  reactive_deployment_plot_data <- reactiveVal()

  cf_workbooks <- reactiveVal()
  scenario_workbooks <- reactiveVal()

  emissions_attr <- reactiveVal()
  energy_attr <- reactiveVal()


  my_cols <- reactiveVal()

  files_to_remove <- list()

  # And set common datatable settings
  datatable_options <- list(paging = FALSE,
                            scrollX = TRUE,
                            scrollY = '50vh', #this means the table will be 50% of viewport height
                            server = FALSE,
                            dom = 'Brtip',
                            buttons = list('copy',
                                           list(extend = 'csv',
                                                title = 'Download',
                                                text = 'Download')),
                            search = NULL)


  #--- Run a new Model ---------------------------------------------------------
  mod_model_server('model_1',
                   original_wd,
                   input_templates,
                   out_files,
                   plot_names,
                   this_set_of_runs,
                   plot_data,
                   emissions_attr,
                   energy_attr,
                   files_to_remove,
                   comit_package_version)


  #--- load previous results ---------------------------------------------------
  mod_upload_server("upload_1",
                    plot_names,
                    plot_data,
                    this_set_of_runs,
                    emissions_attr,
                    energy_attr,
                    input_templates)

  #--- Output standard plots and tables ----------------------------------------
  mod_outputs_server(
    "outputs_1",
    plot_names,
    plot_data,
    this_set_of_runs,
    reactive_cost_plot_data,
    reactive_fuel_plot_data,
    reactive_emissions_plot_data,
    reactive_deployment_plot_data,
    my_cols,
    datatable_options
  )

  #--- Attribution -------------------------------------------------------------
  mod_attribution_server("attribution_1",
                         input_templates,
                         plot_names,
                         energy_attr,
                         emissions_attr,
                         datatable_options)

  #--- Processes ---------------------------------------------------------------
  mod_processes_server("processes_1",
                       plot_names,
                       input_templates,
                       reactive_deployment_plot_data)

  #--- Final settings ----------------------------------------------------------

  # revert wd on end
   session$onSessionEnded(function() {
     setwd(original_wd)
   })

   # Perform garbage clearance every minute to free up memory
   observe({
     invalidateLater(60000)
     gc(verbose = FALSE)
   })

}


