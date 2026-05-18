#' model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_model_ui <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),

    absolutePanel(
      id = ns('model_box'),
      tags$div(h1("COMIT", class = 'comit_header', id = ns('main_comit_header'))),

      tags$div("Cost Optimisation Model for Industrial Technologies",
               style = "text-align: center; padding-bottom: 5px; font-size: 16px"),

      tags$div(
        fileInput(
          inputId = ns("file_upload"),
          label = NULL,
          accept = ".xlsx",
          multiple = TRUE,
          placeholder = "Select COMIT input to model",
          width = "100%",
          buttonLabel = 'Upload'
        ),
        style = "margin: auto; width: 90%;"
      ),

      tags$div(textOutput(ns("issues")),
               style = "padding-left: 20%; color: #CCCCCC;"),
      tags$div(textOutput(ns("model_number_text")),
               style = "padding-left: 20%; color: #CCCCCC;"),
      tags$div(textOutput(ns("model_type")),
               style = "padding-left: 20%; color: #CCCCCC;"),
      tags$div(textOutput(ns("progress_text")),
               style = "padding-left: 20%; color: #CCCCCC;"),
      tags$div(textOutput(ns("completion_message")),
               style = "padding-left: 20%;"),

      tags$div(
        progressBar(
          id = ns('progressBar'),
          value = 0,
          total = 100,
          title = '',
          display_pct = TRUE,
          striped = TRUE
        ),
        tags$br(),
        shinyjs::disabled(
          shiny::downloadButton(
            ns('downloadData'),
            'Download Outputs',
            style = paste0(
              ' margin: 0;',
              'position: absolute;',
              'top: 95%;',
              'left: 50%;',
              '-ms-transform: translate(-50%, -50%);',
              'transform: translate(-50%, -50%);',
              'font-size: 10px;'
            )
          )
        ),
        style = 'margin: auto; margin-top: 15px; width: 70%; font-size: 0;'
      ),

      top = '25%',
      height = '50%',
      left = '20%',
      width = '60%'
    ),

    tags$div() # spacer to help position footer
  )
}

#' model Server Functions
#'
#' @noRd
mod_model_server <- function(id,
                             original_wd,
                             input_templates,
                             out_files,
                             plot_names,
                             this_set_of_runs,
                             plot_data,
                             emissions_attr,
                             energy_attr,
                             files_to_remove,
                             comit_package_version) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ---- Outputs defaults ----------------------------------------------------
    output$issues <- renderText("")
    output$model_number_text <- renderText("")
    output$model_type <- renderText("")
    output$progress_text <- renderText("")
    output$completion_message <- renderText("")

    # Keep a simple internal history of files we created, for cleanup.
    files_created <- character()

    # ---- Run a new model -----------------------------------------------------
    observeEvent(
      input$file_upload,
      ignoreInit = TRUE,
      {
        shinyjs::disable("file_upload")
        on.exit(shinyjs::enable("file_upload"), add = TRUE)

        id_workbook <- showNotification(
          'Validating upload...',
          type = 'message',
          duration = NULL,
          closeButton = FALSE
        )

        # remove any previous messages
        app_text_update('model_number_text', '')
        app_text_update('model_type', '')
        app_text_update('progress_text', '')
        app_text_update('issues', '')
        app_text_update('completion_message', '')

        # reset progressbar if already ran a model
        updateProgressBar(id = 'progressBar', value = 0)

        # Ensure not NULL and there's at least 1 file
        req(input$file_upload, nrow(input$file_upload) > 0)

        # --- Validate & parse uploads --------------------------------------------
        n_inputs    <- nrow(input$file_upload)
        file_names  <- names(input_templates())
        all_inputs  <- list()
        these_runs  <- character(0)
        bad_files   <- character(0)
        good_paths <- character(0)
        good_names <- character(0)

        for(nr in seq_len(n_inputs)) {

          this_path <- input$file_upload[nr, "datapath", drop = TRUE]
          this_name_raw <- basename(input$file_upload[nr, "name", drop = TRUE])

          # Basic checks up front
          if (!is.character(this_path)
              || is.na(this_path)
              || !nzchar(this_path)
              || !file.exists(this_path)) {

            bad_files <- c(bad_files, this_name_raw)
            next

          }

          good_paths <- c(good_paths, this_path)
          good_names <- c(good_names, this_name_raw)


          # Parse & tidy
          tryCatch({
            this_input <- read_excel_data_template(this_path) #%>% tidy()

            # Derive run name and ensure uniqueness
            this_name <- get_run_name(this_name_raw)

            if (isTruthy(input_templates())) {
              this_name <- dedupe_names(this_name, existing = file_names)
            }

            file_names <- c(file_names, this_name)
            these_runs <- c(these_runs, this_name)
            all_inputs[[length(all_inputs) + 1]] <- this_input

          }, error = function(err) {
            bad_files <<- c(bad_files, this_name_raw)
            error_notification(err)
          })
        }


        # If nothing valid, tell the user and stop
        if (length(all_inputs) == 0) {
          shinyjs::disable("downloadData")

          issue_message <- paste0(
            "No valid inputs found. The following files could not be processed: \n",
            paste(unique(bad_files), collapse = ", \n")
            )

          app_text_update('issues', issue_message)

          return(invisible(NULL))
        }


        # Register inputs
        names(all_inputs) <- these_runs
        input_templates(c(input_templates(), all_inputs))

        if (length(bad_files)) {
          issues_message <- paste0(
              "Proceeding with ", length(all_inputs), " valid file(s). ",
              "Skipped: \n", paste(unique(bad_files), collapse = ", \n")
            )

          app_text_update('issues', issues_message)

        }

        # Emd validation notification and start comit notification
        removeNotification(id_workbook)

        id_model <- showNotification(
          'Running comit...',
          type = 'message',
          duration = NULL,
          closeButton = FALSE
        )
        on.exit(removeNotification(id_model), add = TRUE)


        # --- Run model(s) -----------------------------------------------------
        show_spinners()

        output_list <- NULL
        tryCatch({
          output_list <- run_model_for_all_inputs(all_inputs,
                                                  good_paths,
                                                  good_names,
                                                  original_wd,
                                                  comit_package_version)
        }, error = function(err) {
          app_text_update('issues',
                          paste("Model run failed:", conditionMessage(err)))
          updateProgressBar(id = 'progressBar', value = 100)
          app_text_update('completion_message',
                          "Upload an ammended input spreadsheet to try again.")
        })


        if (is.null(output_list) || !length(output_list)) {
          return(invisible(NULL))
        }

        # Note: output_list[[1]] = files_to_zip; [[2]] = workbooks; [[3]] = names
        files_to_zip <- output_list[[1]]
        out_files(files_to_zip %||% character(0))

        # Track for cleanup
        files_created <- unique(c(files_created, files_to_zip))


        # --- Trigger download availability ------------------------------------
        if (length(out_files()) > 0) {
          shinyjs::enable("downloadData")
          shinyjs::click("downloadData")
          # Trigger the button programmatically (namespaced)
          #runjs(sprintf("$('#%s')[0].click();", ns('downloadData')))
        } else {
          shinyjs::disable("downloadData")
          app_text_update('issues', "No outputs were produced.")
        }


        # --- Populate reactive structures for plotting/attribution ------------
        # Errors here should never block downloading.
        try({
          output_list[[3]] <- get_run_name(output_list[[3]])

          # Ensure unique plot names
          if (isTruthy(plot_names())) {
            output_list[[3]] <- lapply(output_list[[3]],
                                       dedupe_names,
                                       plot_names()) %>%
              unlist(use.names = FALSE)
          }

          plot_names(c(plot_names(), output_list[[3]]))
          this_set_of_runs(output_list[[3]])

          # Name the returned workbooks and merge into plot_data
          names(output_list[[2]]) <- output_list[[3]]
          plot_data(c(plot_data(), output_list[[2]]))

          # Attribution inputs
          these_emissions <- lapply(output_list[[2]], read_outputs, type = "Emissions")
          these_energy    <- lapply(output_list[[2]], read_outputs, type = "Energy")
          names(these_emissions) <- output_list[[3]]
          names(these_energy)    <- output_list[[3]]

          emissions_attr(c(emissions_attr(), these_emissions))
          energy_attr(   c(energy_attr(),    these_energy))

          # Optional audio cue
          if (requireNamespace("beepr", quietly = TRUE)) {
            suppressWarnings(beepr::beep(10))
          }

          app_text_update('completion_message', "Complete.")
        }, silent = TRUE)
      })



    # ---- Save model outputs --------------------------------------------------
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0(
          "comit_output_",
          format(Sys.time(), "%Y%m%d_%Hh%Mm%Ss"),
          ".zip"
        )
      },
      content = function(fname) {
        files <- out_files()
        if (is.null(files) || length(files) == 0) {
          output$issues <- renderUI("No output data available to download.")
          return(invisible(NULL))
        }

        files <- unique(files[file.exists(files)])
        if (length(files) == 0) {
          output$issues <- renderUI("Output files are missing; nothing to download.")
          return(invisible(NULL))
        }

        zip(zipfile = fname, files = files)
      },
      contentType = "application/zip"
    )


    # ---- Cleanup on session end ------------------------------------------------
    session$onSessionEnded(function() {
      # Remove any files we know we created
      for (temp_file in unique(files_created)) {
        if (is.character(temp_file) && nzchar(temp_file) && file.exists(temp_file)) {
          try(suppressWarnings(file.remove(temp_file)), silent = TRUE)
        }
      }
    })

    # ---- Return reactivity -----------------------------------------------------
    return(list(
      input_templates = input_templates,
      out_files       = out_files,
      plot_names      = plot_names,
      this_set_of_runs = this_set_of_runs,
      plot_data       = plot_data,
      emissions_attr  = emissions_attr,
      energy_attr     = energy_attr
    ))


  })
}

## To be copied in the UI
# mod_model_ui("model_1")

## To be copied in the server
# mod_model_server('model_1',
#                  original_wd,
#                  input_templates,
#                  out_files,
#                  plot_names,
#                  this_set_of_runs,
#                  plot_data,
#                  emissions_attr,
#                  energy_attr,
#                  files_to_remove,
#                  comit_package_version)


# For testing this module ======================================================
#
# Unhash the chunk below to generate an app for just this tab, using the above
# functions.

# model_ui <- bslib::page_fluid(mod_model_ui('model_1'))
#
# model_server <- function(input, output, session) {
#
#   # get all reactives and common objects #------------------------------------
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
#   #---------------------------------------------------------------------------
#   # Run model
#
#   mod_model_server('model_1',
#                    getwd(),
#                    input_templates,
#                    out_files,
#                    plot_names,
#                    this_set_of_runs,
#                    plot_data,
#                    emissions_attr,
#                    energy_attr,
#                    files_to_remove,
#                    comit_package_version)
# }
#
# shinyApp(model_ui, model_server)
