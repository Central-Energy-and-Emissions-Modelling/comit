
# UI ---------------------------------------------------------------------------

#' upload UI Function
#'
#' @description Shiny module UI for uploading model outputs and inputs.
#'
#' @param id Module id.
#'
#' @return A UI tag list for inclusion in a Shiny app.
#' @noRd
mod_upload_ui <- function(id) {

  ns <- NS(id)

  tagList(
    tags$br(),
    tags$div(
      h2("COMIT", class = "comit_second_header", id = ns("second_header"),
         style = "margin-top: 60px;")
    ),
    tags$h4("Completed Runs", style = "text-align: center; font-size: 15px"),
    tags$br(),

    tags$div(tags$b("Saved Outputs (.xlsx)")),
    tags$small("Upload one or more solved COMIT model output workbooks."),
    fileInput(
      inputId = ns("outputs_upload"),
      label = NULL,
      accept = c(".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
      multiple = TRUE,
      buttonLabel = "Upload",
      placeholder = "Solved COMIT model outputs",
      width = "100%"
    ),
    tags$br(),

    tags$div(tags$b("Input Data (.xlsx)")),
    tags$small("Upload COMIT input workbooks (used for assumptions, not modelled)."),
    fileInput(
      inputId = ns("inputs_upload"),
      label = NULL,
      accept = c(".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
      multiple = TRUE,
      buttonLabel = "Upload",
      placeholder = "COMIT input (for assumptions, not modelled)",
      width = "100%"
    )
  )
}


#' upload UI Function for main app version (not development)
#'
#' @description Shiny module UI for uploading model outputs and inputs.
#'
#' @param id Module id.
#'
#' @return A UI tag list for inclusion in a Shiny app.
#' @noRd
mod_upload_ui_lite <- function(id) {

  ns <- NS(id)

  tagList(
    tags$br(),
    tags$div(
      h2("COMIT", class = "comit_second_header", id = ns("second_header"),
         style = "margin-top: 60px;")
    ),
    tags$h4("Completed Runs", style = "text-align: center; font-size: 15px"),
    tags$br(),

    tags$div(tags$b("Saved Outputs (.xlsx)")),
    tags$small("Upload one or more solved COMIT model output workbooks."),
    fileInput(
      inputId = ns("outputs_upload"),
      label = NULL,
      accept = c(".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
      multiple = TRUE,
      buttonLabel = "Upload",
      placeholder = "Solved COMIT model outputs",
      width = "100%"
    ),
    tags$br()
  )
}



#' upload Server Function (matches UI ids: outputs_upload / inputs_upload)
#'
#' @description Ingests uploaded COMIT outputs and inputs, de-duplicates names,
#' reads files with per-file error isolation, and updates app reactives.
#'
#' @param id Module id.
#' @param plot_names ReactiveVal (character vector) of run names.
#' @param plot_data ReactiveVal (named character vector) run name -> workbook path.
#' @param this_set_of_runs ReactiveVal (character vector) of latest added run names.
#' @param emissions_attr ReactiveVal (named list) run name -> emissions data.
#' @param energy_attr ReactiveVal (named list) run name -> energy data.
#' @param input_templates ReactiveVal (named list) template name -> tidied input data.
#'
#' @return A list of references to the supplied reactives (for chaining).
#' @noRd
mod_upload_server <- function(
    id,
    plot_names,
    plot_data,
    this_set_of_runs,
    emissions_attr,
    energy_attr,
    input_templates
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---------------------- Upload modelled OUTPUTS ----------------------------
    observeEvent(input$outputs_upload, {

      shinyjs::disable("outputs_upload")
      on.exit(shinyjs::enable("outputs_upload"), add = TRUE)

      req(!is.null(input$outputs_upload))

      upl <- input$outputs_upload
      req(NROW(upl) > 0)

      # Show waiter over the file input
      waiter_ids <- c(ns("outputs_upload"))
      show_waiters(waiter_ids, html = waiting_screen, color = "#001a2b")
      on.exit(hide_waiters(waiter_ids), add = TRUE)

      #show_spinners()

      # Validate extension (UI accept is only advisory)
      bad <- which(!vapply(upl$name, is_xlsx, logical(1)))
      if (length(bad)) {
        showNotification(sprintf(
          "Only .xlsx files are supported. Offending files: %s",
          paste(upl$name[bad], collapse = ", ")),
          type = 'error'
        )

        return(invisible(NULL))
      }

      base_names <- basename(upl$name)
      raw_names  <- vapply(base_names, get_run_name, character(1))
      existing   <- isolate(null_default(plot_names(), character(0)))
      final_all  <- dedupe_names(raw_names, existing = existing)

      batch <- read_with_progress(upl, base_names, read_outputs_safe, "Reading")
      if (!any(batch$ok)) return(invisible(NULL))

      keep        <- batch$ok
      run_names   <- final_all[keep]
      workbooks   <- batch$paths[keep]
      outs        <- batch$results[keep]
      emissions_l <- lapply(outs, `[[`, "Emissions")
      energy_l    <- lapply(outs, `[[`, "Energy")

      names(workbooks)   <- run_names
      names(emissions_l) <- run_names
      names(energy_l)    <- run_names

      isolate({
        plot_names(c(plot_names(), run_names))
        plot_data(c(plot_data(), workbooks))
        this_set_of_runs(run_names)
        emissions_attr(c(emissions_attr(), emissions_l))
        energy_attr(c(energy_attr(), energy_l))
      })

      shiny::showNotification(
        sprintf("Loaded: %s", paste(run_names, collapse = ", ")),
        type = "message", duration = 5
      )
    }, ignoreInit = TRUE)

    # ---------------------- Upload INPUT workbooks -----------------------------
    observeEvent(input$inputs_upload, {

      shinyjs::disable("inputs_upload")
      on.exit(shinyjs::enable("inputs_upload"), add = TRUE)

      req(!is.null(input$inputs_upload))
      upl <- input$inputs_upload
      req(NROW(upl) > 0)

      waiter_ids <- c(ns("inputs_upload"))
      show_waiters(waiter_ids, html = waiting_screen, color = "#001a2b")
      on.exit(hide_waiters(waiter_ids), add = TRUE)

      bad <- which(!vapply(upl$name, is_xlsx, logical(1)))
      if (length(bad)) {
        showNotification(sprintf(
          "Only .xlsx files are supported. Offending files: %s",
          paste(upl$name[bad], collapse = ", "),
          type = 'error'
        ))

        return(invisible(NULL))
      }

      base_names <- basename(upl$name)
      raw_names  <- vapply(base_names, get_run_name, character(1))
      existing   <- isolate(null_default(names(input_templates()), character(0)))
      final_all  <- dedupe_names(raw_names, existing = existing)

      batch <- read_with_progress(upl, base_names, read_excel_data_template, "Reading")
      if (!any(batch$ok)) return(invisible(NULL))

      keep         <- batch$ok
      template_nm  <- final_all[keep]
      inputs_list  <- batch$results[keep]
      names(inputs_list) <- template_nm

      isolate({
        input_templates(c(input_templates(), inputs_list))
      })

      shiny::showNotification(
        sprintf("Loaded inputs: %s", paste(template_nm, collapse = ", ")),
        type = "message", duration = 5
      )
    }, ignoreInit = TRUE)
  })

  list(
    id = id,
    plot_names = plot_names,
    plot_data = plot_data,
    this_set_of_runs = this_set_of_runs,
    emissions_attr = emissions_attr,
    energy_attr = energy_attr,
    input_templates = input_templates
  )
}


### For dev testing ####--------------------------------------------------------
#
# upload_ui <- bslib::page_fluid(mod_upload_ui('upload_1'))
#
# upload_server <- function(input, output, session) {
#
#     # get all reactives and common objects #------------------------------------
#     comit_package_version <- desc_get_version()
#
#     out_files <- reactiveVal()
#     output_workbooks <- reactiveVal()
#
#     input_templates <- reactiveVal()
#
#     plot_names <- reactiveVal()
#     plot_data <- reactiveVal()
#     this_set_of_runs <- reactiveVal()
#
#     reactive_cost_plot_data <- reactiveVal()
#     reactive_emissions_plot_data <- reactiveVal()
#     reactive_fuel_plot_data <- reactiveVal()
#     reactive_deployment_plot_data <- reactiveVal()
#
#     cf_workbooks <- reactiveVal()
#     scenario_workbooks <- reactiveVal()
#
#     emissions_attr <- reactiveVal()
#     energy_attr <- reactiveVal()
#
#
#     my_cols <- reactiveVal()
#
#     files_to_remove <- list()
#
#     mod_upload_server("upload_1",
#                       plot_names,
#                       plot_data,
#                       this_set_of_runs,
#                       emissions_attr,
#                       energy_attr,
#                       input_templates)
#
# }
#
# shinyApp(upload_ui, upload_server)
