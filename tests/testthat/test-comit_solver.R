
# Get solved problem
solved_data <- comit_problem_solver(input_data,
                                    objective_function[[1]],
                                    objective_function[[2]],
                                    constraints)


# comit_preprocess_inputs
test_that("Tables present in inputs still remain, and the expected new ones
          are added",
          {

            expect_equal(
              names(input_data),
              c(
                names(raw_data),
                'NAEI_clean',
                'site_demand',
                'site_H2C02_transport'
              )
            )
          })


# comit_preprocess_inputs
test_that("There are no empty tables after preprocessing the data", {

  expect_true(all(sapply(input_data, function(x) {nrow(x) != 0})))

})


# comit_objective_function
test_that("objective function data contains 2 elements which are non-emtpty
          tables", {

  expect_equal(length(objective_function), 2)

  expect_true(nrow(objective_function[[1]]) != 0)
  expect_true(nrow(objective_function[[2]]) != 0)
})


# comit_objective_function
test_that("decision variables and coeffecients are of the same length and have
          the same variable indices", {

            decision_variables <- objective_function[[1]]
            coefficients <- objective_function[[1]]

            expect_equal(nrow(decision_variables),
                         nrow(coefficients))

            expect_setequal(decision_variables$variable_index,
                            coefficients$variable_index)
})


# comit_constraints
test_that("constraint elements are of the expected type", {

  expect_length(constraints, 3)

  expect_type(constraints[[1]], 'character')
  expect_type(constraints[[2]], 'double')
  expect_s3_class(constraints[[3]], 'simple_triplet_matrix')

})


# comit_constraints
test_that("Only valid values are present", {

  all(!is.na(constraints[[2]]))

  expect_true(all(constraints[[1]] %in% c('==', '<=', '>=')))

  # only have indexes for actual constraints
  all(between(constraints[[3]]$i, 0, constraints[[3]]$nrow))

  # only have indexes for actual decision variables
  all(between(constraints[[3]]$j, 0, nrow(decision_variables)))

  # No NA values for constraint coefs
  all(!is.na(constraints[[3]]$v))

})




#comit_problem_solver
test_that('comit_problem_solver creates a list of the correct length',
          {
            expect_length(solved_data, 4)
          })


#comit_problem_solver
test_that('outputs match their equivalent inputs when they should be unchanged',
          {
            expect_equal(input_data, solved_data[[2]])
            expect_equal(objective_function[[1]], solved_data[[3]])
            expect_equal(objective_function[[2]], solved_data[[4]])

          })




#comit_problem_solver
test_that('a positive solution is found, with the correct number of decision
          variables', {

            solution <- solved_data[1]

            solution$solution$status_message == "Optimal" # solution found successfully
            solution$solution$objective_value > 0 # positive solution found

            # correct number of decision variables
            expect_length(solution$solution$solution,
                          nrow(objective_function[[1]]))

          })




# implement a problem with a know solution to ensure that the correct results generated


#comit_problem_solver
test_that('comit_problem_solver produces the correct solution to a known
          linear programming problem',
          {
            # The problem used is taken from the diet problem on page 71 of
            # 'Introduction to Mathematical Programming Applications & Algorithms'
            # by Wayne L. Winston. This is a trivial minimization problem that has
            # a known solution that can be used to validate our function.

            #### Set up what is needed in the correct format

            constraints_to_include <- data.frame(
              constraint = 'minimum_hydrogen_plant_size',
              include = FALSE
            )

            diet_data <- list(constraints_to_include = constraints_to_include,
                              model_parameters = list(timestep = 1000))
            #note: timestep is arbitrary to allow condition test in comit_highs_solver

            diet_decision_variables <- data.frame(
              variable_index = c(1, 2, 3, 4),
              variable_name = c('n_brownies', 'n_ice_cream', 'n_cola', 'n_cheesecake')
            )

            diet_coeffecients <- data.frame(
              variable_index = c(1, 2, 3, 4),
              coefficient = c(50, 20, 30, 80)
            )

            diet_direction <- c('>=', '>=', '>=', '>=')
            diet_rhs <- c(500, 6, 10, 8)

            diet_constraint_matr <- matrix(c(400, 200, 150, 500,
                                             3, 2, 0, 0,
                                             2, 2, 4, 4,
                                             2, 4, 1, 5),
                                           nrow = 4,
                                           byrow = TRUE) %>%
              slam::as.simple_triplet_matrix()


            diet_constraints <- list(directions = diet_direction,
                                     rhs = diet_rhs,
                                     matr = diet_constraint_matr)

            solved <- comit_problem_solver(diet_data,
                                           diet_decision_variables,
                                           diet_coeffecients,
                                           diet_constraints)

            expect_equal(solved$solution$objective_value, 90)
            expect_equal(solved$solution$solution, c(0, 3, 1, 0))

          })




#comit_problem_solver
test_that('comit_problem_solver produces the correct solution to a known
          mixed-integer linear programming problem',
          {
            # The problem used is taken the steel production problem specified at
            # https://uk.mathworks.com/help/optim/ug/mixed-integer-linear-programming-basics-problem-based.html"
            # This is a trivial minimization problem that has
            # a known solution that can be used to validate our function.

            #### Set up what is needed in the correct format

            # all we actually need from the data at this point is the parameter for H2_size,
            # here set to a value > 0

            constraints_to_include <- data.frame(
              constraint = 'minimum_hydrogen_plant_size',
              include = TRUE
            )

            steel_data <- list(constraints_to_include = constraints_to_include,
                               model_parameters = list(timestep = 1000))
            #note: timestep is arbitrary to allow condition test in comit_highs_solver)

            steel_decision_variables <- data.frame(
              variable_index = c(1:8),
              variable_name = c('ingot_1', 'ingot_2', 'ingot_3', 'ingot_4',
                                'alloy_1', 'alloy_2', 'alloy_3', 'scrap'),
              variable_type = c(rep('b_inary', 4), # needs to start "b_" to be detected
                                rep('continuous', 4))
            )

            steel_coeffecients <- data.frame(
              variable_index = c(1:8),
              coefficient = c(5*350, 3*330, 4*310, 6*280,
                              500, 450, 400, 100)
            )

            steel_direction <- c('==', '==', '==')
            steel_rhs <- c(25, 1.25, 1.25)

            steel_constraint_matr <-
              matrix(c(5, 3, 4, 6, 1, 1, 1, 1,
                       5*0.05, 3*0.04, 4*0.05, 6*0.03, 0.08, 0.07, 0.06, 0.03,
                       5*0.03, 3*0.03, 4*0.04, 6*0.04, 0.06, 0.07, 0.08, 0.09),
                     nrow = 3,
                     byrow = TRUE) %>%
              slam::as.simple_triplet_matrix()


            steel_constraints <- list(directions = steel_direction,
                                     rhs = steel_rhs,
                                     matr = steel_constraint_matr)

            solved <- comit_problem_solver(steel_data,
                                           steel_decision_variables,
                                           steel_coeffecients,
                                           steel_constraints)

            expect_equal(solved$solution$objval, 8495)
            expect_equal(solved$solution$solution,
                         c(1, 1, 0, 1, 7.25, 0, 0.25, 3.5))



            ### now make sure get wrong answer if h2 setting is 0, as will
            # instead solved as an lp.

            steel_data$constraints_to_include$include[
              steel_data$constraints_to_include$constraint == 'minimum_hydrogen_plant_size'
            ] <- FALSE

            solved <- comit_problem_solver(steel_data,
                                           steel_decision_variables,
                                           steel_coeffecients,
                                           steel_constraints)

            expect_true(solved$solution$objective_value != 8495)
            expect_true(any(solved$solution$solution
                            !=  c(1, 1, 0, 1, 7.25, 0, 0.25, 3.5)))

          })




# test changes give expected result
test_that('comit_solver finds the same obj function value, for a constant input',
          {
            # This allows to see that no changes to the solution are introduced
            # when making code changes that should not effect the outcome. When
            # changes that do effect the outcome are deliberately made, this test
            # will fail and the new objective value to be used in tests going
            # forward should be replaced with the current value below.

            expect_equal(round(solved_data$solution$objective_value, 0), 105827)

          })








