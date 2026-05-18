
#-------------------------------------------------------------------------------
# get input data
raw_data <- suppressWarnings(
  tryCatch(expr = {standard_data_read(testing = TRUE)},
           error = function(err) {standard_data_read()})
)

# get progress at different parts of the system

input_data <- comit_preprocess_inputs(raw_data, in_app)

objective_function <- comit_objective_function(input_data, in_app)

decision_variables <- objective_function[[1]]
PV_coefficients <- objective_function[[2]]

constraints <- comit_constraints(input_data, decision_variables, in_app)


#-------------------------------------------------------------------------------

## Some helper functions

#' Function to calculate total number of possible combinations with repetition
#' allowed, to use in testing.
#'
#' @param r int, number to choose
#' @param n int, number of things to choose from
#'
#' @return int, total possible combinations
combos_with_repitition <- function(r, n) {

  combos <- (
    factorial(r + n - 1)
    / (factorial(r) * factorial(n - 1))
  )

  return(combos)
}

#-------------------------------------------------------------------------------

# path to input template test file

input_template_test_file <- '../../data_template_archive/input_template_for_testing.xlsx'


# Find number of periods that are modelled, based on input parameters.
# inclusive of start year so need to add 1 to total steps.
# this should be put into a seperate helper script
number_of_time_steps <- 1 + (
  (raw_data$model_parameters$end_year
   - raw_data$model_parameters$start_year)
  / (raw_data$model_parameters$timestep)
)


# list of regions
regions <- c(
  "North_East",
  "North_West",
  "Yorkshire_Humber",
  "East_Midlands",
  "West_Midlands",
  "East",
  "London",
  "South_East",
  "South_West",
  "Wales",
  "Scotland",
  "Northern_Ireland"
)

# list of sectors
sectors <- unique(raw_data$new_sector_mapping$IPM_sector)

# list of clusters
clusters <- raw_data$cluster_radius$cluster

# list of clusters that are used
used_clusters <- raw_data$Cluster_location %>%
  filter(use_cluster) %>%
  pull(Cluster)



# Create some mock constraints -------------------------------------------------

# create a mock set of constraints like

# A
# 1 1 0 0 0 0 .... (all 0)
# 0 1 1 0 0 0 ....
# 0 0 1 1 0 0 ....
# dir: ==
# rhs: 1

# each constraint is a row
constraint_set_a_1 <- list(
  column_indices = c(1, 2),
  values = c(1, 1),
  direction = '==',
  rhs = 1
)

constraint_set_a_2 <- list(
  column_indices = c(2, 3),
  values = c(1, 1),
  direction = '==',
  rhs = 1
)

constraint_set_a_3 <- list(
  column_indices = c(3, 4),
  values = c(1, 1),
  direction = '==',
  rhs = 1
)

# Final list of constraints set
constraint_set_a <- list(
  constraint_set_a_1,
  constraint_set_a_2,
  constraint_set_a_3
)

# manually create the expectded matrix

matrix_a_start <- matrix(c(1, 1, 0, 0,
                           0, 1, 1, 0,
                           0, 0, 1, 1),
                         nrow = 3,
                         byrow = TRUE)

matrix_a_end <- matrix(0, nrow = 3, ncol = nrow(decision_variables) - 4)

matrix_a <- cbind(matrix_a_start, matrix_a_end)



# B
# 0 2 2 0 0 0 .... (all 0)
# 0 0 2 2 0 0 ....
# 0 0 0 2 2 0 ....
# dir: >=
# rhs: 0

constraint_set_b_1 <- list(
  column_indices = c(2, 3),
  values = c(2, 2),
  direction = '>=',
  rhs = 0
)

constraint_set_b_2 <- list(
  column_indices = c(3, 4),
  values = c(2, 2),
  direction = '>=',
  rhs = 0
)

constraint_set_b_3 <- list(
  column_indices = c(4, 5),
  values = c(2, 2),
  direction = '>=',
  rhs = 0
)

# Final list of constraints set
constraint_set_b <- list(
  constraint_set_b_1,
  constraint_set_b_2,
  constraint_set_b_3
)


# manually create the expectded matrix

matrix_b_start <- matrix(c(0, 2, 2, 0, 0,
                           0, 0, 2, 2, 0,
                           0, 0, 0, 2, 2),
                         nrow = 3,
                         byrow = TRUE)

matrix_b_end <- matrix(0, nrow = 3, ncol = nrow(decision_variables) - 5)

matrix_b <- cbind(matrix_b_start, matrix_b_end)


mock_constraints_coeffecients <- rbind(matrix_a, matrix_b)

mock_constraints <- list(directions = c(rep('==', 3), rep('>=', 3)),
                         rhs = c(rep(1, 3), rep(0, 3)),
                         matr = mock_constraints_coeffecients)


remove(matrix_a_start, matrix_a_end, matrix_b_start, matrix_b_end,
       constraint_set_a_1, constraint_set_a_2, constraint_set_a_3,
       constraint_set_b_1, constraint_set_b_2, constraint_set_b_3,
       mock_constraints_coeffecients)





