
#===============================================================================

# set up wait screen

waiting_screen <- tagList(
  spin_fading_circles()
)

# comit_waiter <- tagList(
#   waiter::spin_wandering_cubes()
# )

# This chunk allows all warnings and messages to be stored in the output

log_env <- new.env()


initialise_log <- function() {

  log_env$log_entries <- list()
  log_env$log_entries[[1]] <- "This sheet contains all warnings and messages from the model run"
  log_env$log_entries[[2]] <- "" # adding a space before the actual messages

}

initialise_log()


custom_log <- function(message){

  log_env$log_entries <- append(log_env$log_entries, list(message))

}

shinyOptions(shiny.error = custom_log,
             shiny.warn = custom_log,
             shiny.message = custom_log)


# ==============================================================================


#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "comit")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv(
      "R_CONFIG_ACTIVE",
      "default"
    )
  ),
  use_parent = TRUE,
  # Modify this if your config file is somewhere else
  file = app_sys("golem-config.yml")
) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}
