
#' Generate names of the constraint functions to run
#'
#' @return vector of strings for the names of the functions to be run to produce
#'  the matrix.
#' @export
get_constraint_functions <- function(data) {

  if(is.null(data$constraints_to_include)){
    stop('constraints_to_include tab not provided')
  }

  constraint_functions <- data$constraints_to_include %>%
    filter(include) %>%
    pull(constraint)

  # Check that all functions stated are in the system
  for(c_function in constraint_functions){
    if(!exists(c_function)) {
      stop(paste0('Constraint function "', c_function,
                  '", does not exist. Check valid constraint names are provided',
                  ' in the constraints_to_include tab of the input file.'))
    }
  }

  return(constraint_functions)
}



#' Calls a specific constraint functions to generate a constraint list before
#' converting to a matrix
#'
#' Calls the function supplied and gives data and decision_variables as the
#'  arguments. The subsequent list returned from the function is then passed
#'  to get_constraint_matrix to convert into matrix format.
#'
#' @param constraint_name, str name of function to be run, usually passed from
#'  an item of [`get_constraint_functions()`]
#' @param data list of dataframes - raw_data after initial processing from
#'  [`process_sites()`]
#' @param decision_variables dataframe of decision variables as produced by
#'  [`create_decision_variables()`]
#'
#' @return sparseMatrix for the constraint named, see
#'  [`get_constraint_matrix()`] for more details.
#' @export
run_constraint_function <- function(constraint_name,
                                    data,
                                    decision_variables,
                                    in_app) {

  message("Creating ", gsub("_", " ", constraint_name), " constraint\n")

  this_constraint_list <- do.call(get(constraint_name),
                                  args = list(data,
                                              decision_variables))

  constraint_size_check(constraint_name,
                        this_constraint_list,
                        data,
                        decision_variables,
                        issue_limit = 0.05,
                        in_app = in_app)

  this_constraint_matrix <-
    get_constraint_matrix(this_constraint_list,
                          n_decision_variables = nrow(decision_variables))

  return(this_constraint_matrix)

}


#' Convert a set of constraints from a list to a sparse matrix format
#'
#' Reformat a set of constraints from pulling the indices of decision variables,
#'  values of coeffecients, directions and rhs values and converting them into
#'  another list which importantly contains a sparse matrix for the coeffecient
#'  values. This allows for many constraints to be combined later without
#'  maxing out computer memory.
#'
#' @param constraint_group list of lists, for a set of constraints for a given
#'  type of constraints, such as production constraints.
#'  Each sub-list contains four elements:
#'    * column_indices (int) indicating the decision variables effected, varying
#'      in length
#'    * values (num) indicating the coefficients for the variables effected, same length
#'      as column_indices
#'    * direction (chr) indicating the direction of the constraint '==', '<=' or
#'      '>='. Length 1.
#'    * rhs (num) the value of the right hand side of the constraint, length 1.
#'
#' @param n_decision_variables integer, the total number of decision variables
#'  in the model.
#'
#' @return list of 3 for the set of constraints where:
#'  * directions (chr) is a vector of directions for each constraints ('==', '<='
#'    or >='). There is one direction for every row (one per constraint).
#'  * right hand side (num) is a vector of the rhs side values for each constraint.
#'    There is one rhs value for each row.
#'  * constraint_matrix (dgCMatrix) a sparse matrix where values 'i' provides a
#'    reference to a constraint as a row in the matrix, 'p' provides the
#'    pointers for each column (decision variable) which is a smart
#'    way of stating how many non-zero elements there are in each column.
#'    By using i and p in combination, all of the locations of the non-zero
#'    elements in the matrix can be found. 'v' provides the value of the
#'    coefficient itself at a given location.
#'
#' @export
get_constraint_matrix <- function(constraint_group, n_decision_variables) {

  # Account for empty constraint
  if(is.null(constraint_group)){
    return(NULL)
  }

  n_constraints <- length(constraint_group)

  # stop the run and flag any issues if they exist
  constraint_checks(constraint_group)

  # get flat rhs and direction values by transposing the list
  transposed_constraints <- transpose(constraint_group)

  directions <- flatten_chr(transposed_constraints$direction)

  right_hand_side <- flatten_dbl(transposed_constraints$rhs)

  # create sparse matrix of coefficients ---------------------------------------

  n_decisions_per_constraint <- sapply(transposed_constraints$column_indices,
                                       length)

  # get row indices to repeat as many times as there are multiple decision
  # variables in the constraint, as we need to point to each one individually
  # in the sparse matrix
  row_number <- rep(seq_along(1:n_constraints),
                    n_decisions_per_constraint)

  # these are the indices for the decision variables
  column_number <- flatten_int(transposed_constraints$column_indices)

  values <- flatten_dbl(transposed_constraints$values)


  constraint_matrix <- Matrix::sparseMatrix(i = row_number, # constraint index
                                            j = column_number, # decision variable index
                                            x = values, # coefficient
                                            dims = c(max(row_number),
                                                     n_decision_variables))

  return(list(directions = directions,
              right_hand_side = right_hand_side,
              constraint_matrix = constraint_matrix))
}



#' Combine a list of constraint sets into a single large set of constraints
#'
#' Collapse a series of constraint sets from different constraint types (such
#'  production or emissions) into a single list containing all the required
#'  information for each constraint, ready for modelling.
#'
#' @param matrix_constraints a list for each constraint set, which contains a
#'  sub list for direction, rhs and a sparse matrix for coefficients - see
#'  [`get_constraint_matrix()`] for more details.
#'
#' @return a list with 3 elements, representing the entire set of constraints:
#'  * directions, a vector for the direction for each constraint.
#'  * rhs, a vector of the right hand side values for each constraint
#'  * matr, a simple triplet matrix. This is a converted version of all the input
#'   matrices binded together. The simple triplet matrix is slower to work with
#'   but required by the model, so it is converted here at the last stage. 'i'
#'   references a constraint (row), 'j' references a decision variable (column)
#'   and 'v' contains the value for the coefficient.
#' @export
combine_matrix_constraints <- function(matrix_constraints) {

  message('Forming constraints matrix')

  # Check inputs
  stopifnot(purrr::every(matrix_constraints,
                         function(x){length(x) == 3 | is.null(x)}))

  # remove null elements from list and transpose the list, so that we get a list
  # all rhs, all direction and all constraint coefficients separately
  x <- purrr::discard(matrix_constraints, is.null) %>%
    transpose()

  # after transposing, it's easy to get all the directions and rhs
  directions <- flatten_chr(x$directions)
  rhs <- flatten_dbl(x$right_hand_side)

  constraint_coefficients <- do.call(rbind, x$constraint_matrix)

  # to get column indices, repeat each index for as many non-0 elements in the
  # column. See seq_len for info on supplying a vector to the second argument
  # to see how this is working.
  column_indexes <-  rep(
    seq_len(ncol(constraint_coefficients)), # n of decision vars
            diff(constraint_coefficients@p) # n of non-0 decision vars in each row (constraint)
    )

  # now reformat the sparse matrix to a simple triplet
  triple_mat <- slam::simple_triplet_matrix(
    i = constraint_coefficients@i + 1, # because previous sparse matrix has index 0!
    j = column_indexes,
    v = constraint_coefficients@x,
    nrow = constraint_coefficients@Dim[1],
    ncol = constraint_coefficients@Dim[2]
  )

  constraints <- list(directions = directions,
                      rhs = rhs,
                      matr = triple_mat)

  return(constraints)
}




#' Run a series of checks on constraint sets and stop the process if issues are
#' found
#'
#' Check the constraint sets for the expected shape and that there is no NA
#' values in the coefficient matrix.
#'
#' @inheritParams get_constraint_matrix
#'
#' @return NULL, process stopped if checks fail
#' @export
constraint_checks <- function(constraint_group) {

  stopifnot(purrr::every(constraint_group, function(x){length(x) == 4}))
  stopifnot(purrr::every(constraint_group, function(x){length(x$column_indices) == length(x$values)}))

  # check contents for NA or NaN, they will cause errors later
  if(purrr::every(constraint_group, function(x){all(is.finite(x$column_indices))}) == FALSE)
  {
    i <- purrr::detect_index(constraint_group, function(x){all(is.finite(x$column_indices)) == FALSE})
    stop(sprintf("Error: List element %d contains NA or NaN values in the column indices vector", i))
  }


  if(purrr::every(constraint_group, function(x){all(is.finite(x$values))}) == FALSE)
  {
    i <- purrr::detect_index(constraint_group, function(x){all(is.finite(x$values)) == FALSE})
    stop(sprintf("Error: List element %d contains NA or NaN values in the column indices vector", i))
  }

  # Check column indices are only referenced once per constraint list (row)
  if(purrr::some(constraint_group, function(x){
    length(x$column_indices) != length(unique(x$column_indices))})) {
    i <- purrr::detect_index(constraint_group, function(x){
      length(x$column_indices) != length(unique(x$column_indices))})
    stop(sprintf("Error: List element %d contains duplicates in the column indices vector", i))
  }

}




#' Raise message if a constraint includes more than a specified % of the total
#' decision variables for a given period
#'
#' This tests whether more than a specified proportion of decision variables
#'  within a modelled period are used for a single constraint list. A message
#'  is raised if the proportion is exceeded.
#'
#' Exceeding the limit isn't inherently a problem, but large constraints do
#'  impact on solve times, so this function flags when large constraints are
#'  found to ensure that users are aware.
#'
#' @param this_constraint_list, list of constraint data produced by individual
#'  constraint functions. The list is a list of nested lists, where each nested
#'  list is a single constraint containing elements for column_indices, values,
#'  direction and rhs.
#' @param issue_limit, numeric (default = 0.5%). This is the proportion that, if
#'  exceeded, the function will raise a message.
#' @inheritParams run_constraint_function
#'
#' @returns NULL
#' @export
constraint_size_check <- function(constraint_name,
                                  this_constraint_list,
                                  data,
                                  decision_variables,
                                  issue_limit = 0.05,
                                  in_app = FALSE) {

  # Get number of periods in model
  n_periods <- (((data$model_parameters$end_year
                  - data$model_parameters$start_year)
                 / data$model_parameters$timestep)
                + 1)

  # Find the approximate number of decision variables in a single period for
  # the model, since constraints are always set for each period it makes more sense
  # when finding out how dense the constraints are.
  approx_yearly_n_decisions <- nrow(decision_variables)/n_periods

  # When relevant send message to console/app to flag issues
  if(!is.null(this_constraint_list)) {

    constraint_props <- sapply(this_constraint_list, function(x) {
      length(x[['values']]) / approx_yearly_n_decisions
    })

    if(max(constraint_props > issue_limit)) {
      message(constraint_name, ' constrains a large number of decision',
              ' variables at once. This may cause long run times.\n')
      if(!in_app) { # only print when in development. Not useful for users.
        message('Approximate % of decision variables constrained in period: ',
                paste0(round(constraint_props * 100, 2), '%', collapse = ', '),
                '\n')
      }
    }

  }

  return(NULL)
}







# Old versions - keep for testing (dual code) ----------------------------------

# These are adaptations of the previous functions/methods used to create the
# constraints. They are saved here and called only in testing as a way of cross
# checking the newer versions. These methods are slower (hence the updates).

old_list2matrix <- function(constraint) {
  # check input parameters. Constraint may be empty (i.e. null)
  if(is.null(constraint)) {return(NULL)}

  # check contents for NA or NaN, they will cause errors later
  if(purrr::every(constraint, function(x){all(is.finite(x$column_indices))}) == FALSE)
  {
    i <- purrr::detect_index(constraint, function(x){all(is.finite(x$column_indices)) == FALSE})
    stop(sprintf("Error: List element %d contains NA or NaN values in the column indices vector", i))
  }
  if(purrr::every(constraint, function(x){all(is.finite(x$values))}) == FALSE)
  {
    i <- purrr::detect_index(constraint, function(x){all(is.finite(x$values)) == FALSE})
    stop(sprintf("Error: List element %d contains NA or NaN values in the column indices vector", i))
  }

  # getting constraint direction and right hand side is easy with transposition of list
  transposed_constraint <- transpose(constraint)
  directions <- flatten_chr(transposed_constraint$direction)
  right_hand_side = flatten_dbl(transposed_constraint$rhs)

  # create sparse matrix of coefficients from the column indexes and values
  # The row index can be gotten from the length of each column indices element
  n_rows <- sapply(transposed_constraint$column_indices,length)
  row_number <- rep(seq_along(n_rows), n_rows)

  column_number <- flatten_int(transposed_constraint$column_indices)

  values <- flatten_dbl(transposed_constraint$values)

  constraint_matrix <- slam::simple_triplet_matrix(i = row_number,
                                                   j = column_number,
                                                   v = values)

  return(list(directions = directions,
              right_hand_side = right_hand_side,
              constraint_matrix = constraint_matrix))
}



get_old_constraints <- function(data, decision_variables, constraint_functions) {

  #constraint_functions <- get_constraint_functions()

  constraints <-
    lapply(constraint_functions, function(x) {
      print(x)
      old_list2matrix(do.call(
        get(x),
        list(data, decision_variables = decision_variables)
      ))
    })

  # add set of supply chain constraints
  #constraints %<>% append(supply_chain_constraints(data, decision_variables))
  # not looking at supply chain constraints for this example because not backwards
  # compatible

  # remove null elements from list and transpose
  x <- purrr::discard(constraints, is.null) %>%
    transpose()

  directions <- flatten_chr(x$directions)
  rhs <- flatten_dbl(x$right_hand_side)

  # to row bind the constraint matrices, set their number of columns to the number of decision variables in model
  x$constraint_matrix <- lapply(x$constraint_matrix, function(x){x$ncol <- nrow(decision_variables); x})
  constraint_coefficients <- do.call(rbind, x$constraint_matrix)

  constraints <- list(directions = directions,
                      rhs = rhs,
                      matr = constraint_coefficients)

  return(constraints)
}





