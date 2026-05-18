
#For running the models in server for rshiny app
# original_wd - just used for logging solve times in development

run_model_for_all_inputs <- function(all_inputs,
                                     paths,
                                     names,
                                     original_wd = '',
                                     comit_package_version = NULL) {
  withCallingHandlers({

    # initialize list of paths to return later
    fs <- c()
    all_workbooks <- list()
    output_run_names <- c()

    nr_of_scenarios <- length(all_inputs)

    # now iterate through each input solving each model and writing the outputs
    for (nr in 1:nr_of_scenarios) {

      comit_tic('Full model timing')
      in_app=TRUE


      start_time_raw <- Sys.time()
      start_time <- format(start_time_raw, '%H:%M')

      update_message <- paste0("Running model [", nr, " / ", nr_of_scenarios, '].',
                               ' Start time: ', start_time, '.')

      app_text_update('model_number_text', update_message)



      # read data
      message("Reading input file")
      progress_updater('read_excel_data_template', in_app, all_inputs[[nr]])

      raw_data <- all_inputs[[nr]]


      models_to_run <- c('Scenario', 'Counterfactual')
      models_to_run <- models_to_run[c(raw_data$model_parameters$run_main,
                                       raw_data$model_parameters$run_counterfactual)]

      for(model in models_to_run) {

        comit_tic(msg = paste0('Model type: ', model))
        app_text_update('model_type', paste0('Model type: ', model))

        # get solution and other info
        message("Getting least cost")

        if(model == 'Scenario') {

          solved <- comit_solver(raw_data, in_app = TRUE)

        } else {

          solved <- comit_counterfactual_solver(raw_data)

        }

        gc()

        # See if solver managed to solve
        solved_check(solved)

        tables <- create_tables(solved, raw_data)

        message("Creating output spreadsheet")
        wb <- create_output_xlsx(tables, raw_data, comit_package_version)


        progress_updater('create_output_xlsx', in_app, raw_data)

        # Output total time so it is recorded in log
        run_time <- round(difftime(Sys.time(), start_time_raw, units = 'mins'), 2)
        message(paste0("Total run time: ",
                       run_time, ' minutes'))

        # adding log of all messages to excel spreadsheet
        add_log_to_wb(wb)

        comit_tic('saving files')

        input_file_name <- sub('.xlsx$', '', names[nr])
        input_file_name <- sub('input_', '', input_file_name) # remove word 'input'

        path <- paste0(model, '_', input_file_name, '.xlsx') # match input name

        output_run_names <- c(output_run_names, path) # list of outputs only for selector (no inputs)

        saveWorkbook(wb, file = path, overwrite = TRUE)

        # save input file as well for record keeping
        file.copy(from = paths[nr],
                  to = names[nr])

        fs <- c(fs, path)

        ## save for plotting
        all_workbooks[[get_run_name(path)]] <- wb

        initialise_log() # to clear the warning log

        comit_toc()
        comit_toc()

      }

      fs <- c(fs, basename(names[nr])) # append file path for output and input


      comit_toc()
      progress_updater('saving files', in_app, raw_data)

    }


    #### FOR DEVELOPMENT PURPOSES ####------------------------------------------

    #log_model_timings(raw_data, original_wd) # If model times are to be logged unhash this line!

    #---------------------------------------------------------------------------

    return(list(fs, all_workbooks, output_run_names))

  }, warning = function(w){

    custom_log(paste0('WARNING: ', w$message))

  }, message = function(m) {
    shinyjs::html(id = "progress_text",
                  html = m$message,
                  add = FALSE)

    custom_log(m$message)
  })
}





#' Check if the model found a valid solution
#'
#' @param solved, solved model object as returned by `comit_problem_solver()`.
#'
#' @return NULL, error is thrown if the model found no valid solution.
solved_check <- function(solved) {

  if(solved$solution$solver == 'highs') {

    if(solved$solution$status_message != 'Optimal') {
      stop(
        paste0(
          "ERROR! Solver could not find optimal solution: ",
          solved$solution$status_message
        )
      )
    }

  } else {

    if (solved$solution$status$code != 0) {
      stop(
        paste0(
          "ERROR! Solver could not find optimal solution: ",
          solved$solution$status$msg$message
        )
      )
    }

  }


}



#' Produce the required tables from the solved model
#'
#' @param solved the solved model object, produced from `comit_problem_solver()`.
#' @param data list of input data produced by `read_excel_data_template()`.
#' @param in_app, default = TRUE. Boolean for whether or not the function is
#'  being ran as part of the main app, if it is the progress bar will be updated.
#'
#' @return list of all tables to be written to excel workbook.
#' @export
create_tables <- function(solved, data, in_app = TRUE) {

  message("Creating output tables")

  tables <- create_output_tables(
    solved$solution,
    solved$data,
    solved$decision_variables,
    solved$PV_coefficients,
    "site_ID"
  )

  progress_updater('create_output_tables', in_app, data)

  if (data$model_parameters$output_cluster_level) {

    tables_cluster <- create_output_tables(
      solved$solution,
      solved$data,
      solved$decision_variables,
      solved$PV_coefficients,
      "cluster"
    )

    tables <- c(tables_cluster, tables)

  }

  return(tables)
}




add_log_to_wb <- function(wb) {
  # adding log of all messages to excel spreadsheet
  addWorksheet(wb, 'Log')

  writeData(wb,
            'Log',
            do.call(rbind,
                    lapply(log_env$log_entries,
                           function(x) data.frame(Log = x))),
            startRow = 1,
            startCol = 1)
}

