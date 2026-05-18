

#' Generate names of the present value functions to run
#'
#' @return vector of strings for the names of the functions to be run to produce
#'  the decision variable coeffecients in the objective function.
#' @export
get_pv_functions <- function(data) {

  # Raise error if constraints_to_include tab not present - this is always needed
  if(is.null(data$objective_function)){
    stop('objective_function tab not provided')
  }

  pv_functions <- data$objective_function %>%
    filter(include) %>%
    pull(term)


  # Check that all functions stated are in the system
  for(pv_function in pv_functions){
    if(!exists(pv_function)) {
      stop(paste0('PV function "', pv_function,
                  '", does not exist. Check valid objective function term names are provided',
                  ' in the objective_function tab of the input file.'))
    }
  }

  return(pv_functions)
}

