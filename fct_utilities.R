
.datatable.aware <- TRUE # this is needed in order to use lazy_dt
options(datatable.showProgress = FALSE)


#' Use tic function from tictoc package to record times, but only when tictoc
#' is available.
#'
#' @param msg string describing stage of model.
comit_tic <- function(msg){

  if(system.file(package = 'tictoc') != ''){
    tictoc::tic(msg)
  }

}


#' Use toc function from tictoc package to record times, but only when tictoc
#' is available.
comit_toc <- function(){

  if(system.file(package = 'tictoc') != ''){
    tictoc::toc(log = TRUE, quiet = TRUE)
  }

}


#' Create a txt file of the model parameters and timings
#'
#' Used for recording timings of the model to track changes in speed during
#'  development.
#'
#' @param file_location string for the directory to find the 'timing_logs' folder.
#'  Default is an emtpy string ''. Make sure the filepath ends with '/'.
#' @inheritParams comit_solver
#'
#' @return NULL. Saves out a txt file into timing_logs folder
#' @export
log_model_timings <- function(data, file_location = ''){

  log.txt <- tictoc::tic.log(format = TRUE)

  params <- lapply(colnames(data$model_parameters), function(x) {
    paste0(x, ': ', as.character(data$model_parameters[1, x]))
  }) %>%
    unlist()

  log.txt <- c('PARAMETERS', '========', params, '\n',
               'TIMINGS', '========', log.txt)

  current_time <- format(Sys.time(), '%Y_%m_%d_%HH%MM') # for naming

  # output the log (but only if the folder is available - e.g. for developerss)
  if(file.exists(paste0(file_location, '/timing_logs'))) {

    fileConn <- file(paste0(file_location,
                            '/timing_logs/comit_solver_timings_',
                            current_time, '.txt'))

    writeLines(unlist(log.txt), fileConn)
    close(fileConn)
  }

  tictoc::tic.clearlog()

  return(NULL)
}


#' Update the rshiny progress bar when running the model through the app
#'
#' The timings are pulled from the input spreadsheet and are inferred from
#'  previous runs to give a representative estimate of the progress made after
#'  each stage of the process has been completed.
#'
#' @param finished_stage, the point in the model that has just finished. These
#'  should be one of the values in `data$model_timings$stage`.
#' @param in_app boolean, TRUE if code is being run in the app,
#'  in which case the function is executed, otherwise it is ignored.
#' @inheritParams comit_solver
#'
#' @return NULL. Progress bar is updated to the value associated with the
#'  finished stage in `data$model_timings`
#' @export
progress_updater <- function(finished_stage, in_app, data) {

  if(in_app == TRUE){

    model_timings_path <- system.file('extdata',
                                      'model_timings.xlsx',
                                      package = 'comit')

    model_timings <- read_input_sheet(model_timings_path, 'model_timings')

    progress_value <- model_timings %>%
      filter(stage == finished_stage) %>%
      {if(data$model_parameters$timestep == 1)
        pull(., one_year_times_to_print_after_stage)
        else pull(., five_year_times_to_print_after_stage)
      }

    updateProgressBar(id = 'progressBar', value = progress_value)
  }

  return(NULL)
}



#' Print message in app with calling handler
app_text_update <- function(html_id, this_message) {

  withCallingHandlers({
    message(this_message)
  }, message = function(m) {
    shinyjs::html(id = html_id,
                  html = m$message,
                  add = FALSE)
  })

}


## Get package version for metadata in outputs
get_comit_version <- function(){

  # For in app dev
  comit_package_version <- tryCatch(as.character(desc_get_version()),
                                    error = function(e) NULL)

  # For installed versions
  if (is.null(comit_package_version)) {
    comit_package_version <- tryCatch(
      desc_get_version(file = system.file(package = "comit")) %>% as.character(),
      error = function(e) 'NA')
  }

  return(comit_package_version)
}


