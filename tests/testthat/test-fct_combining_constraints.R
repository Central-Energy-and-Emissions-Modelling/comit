

# get_constraint_functions
test_that('get_constraint_functions produces a character list of the correct
          length', {

            constraint_functions <- get_constraint_functions(input_data)

            n_true_constraints <- input_data$constraints_to_include %>%
              filter(include) %>%
              nrow()

            expect_type(constraint_functions, 'character')
            expect_length(constraint_functions, n_true_constraints)

          })


# get_constraint_functions
test_that('get_constraint_functions produces an error if there is no constraint
          tab.', {

          mock_data <- input_data[names(input_data) != "constraints_to_include"]


          expect_error(constraint_functions <- get_constraint_functions(mock_data))

          })


# get_constraint_functions
test_that('get_constraint_functions produces an error if an invalid constraint
          name is set to be included.', {

            mock_data <- input_data

            mock_data$constraints_to_include <- data.frame(
              constraint = c('production',
                             'availability',
                             'made_up_constraint'),
              include = c(TRUE, TRUE, TRUE)
            )

            expect_error(get_constraint_functions(mock_data))
          })




# combine_matrix_constraints()
test_that('new constraint combination method provides an equivalent matrix to
          the previous version', {

            # this is a long test but it makes sense to test everything together
            # to avoid rerunning things

            # pick 3 random constraints to test to save time
            constraint_functions <- get_constraint_functions(input_data) %>%
              sample(., 3)

            # old method of forming constraints
            old_constraints <- get_old_constraints(input_data,
                                                   decision_variables,
                                                   constraint_functions)

            # new method
            constraints <- lapply(constraint_functions,
                                  run_constraint_function,
                                  input_data,
                                  decision_variables)

            new_constraints <- constraints %>%
              combine_matrix_constraints()

            # test the rhs and directions match
            expect_equal(new_constraints$rhs, old_constraints$rhs)
            expect_equal(new_constraints$directions, old_constraints$directions)

            # now test the matrix
            expect_equal(new_constraints$matr$ncol, old_constraints$matr$ncol)
            expect_equal(new_constraints$matr$nrow, old_constraints$matr$nrow)



            # now test some random values - this helps to check its at 0 where
            # it should be
            row_index <- unique(new_constraints$matr$i) # decision variable index
            col_index <- unique(new_constraints$matr$j) # constraint index

            # sample 1000 to test, or all indexes if there isn't 1000
            random_rows <- sample(row_index, min(1000, length(row_index)))
            random_cols <- sample(col_index, min(1000, length(col_index)))

            new_matrix <- matrix(new_constraints$matr[random_rows, random_cols, ],
                                 nrow = length(random_rows))

            old_matrix <- matrix(old_constraints$matr[random_rows, random_cols, ],
                                 nrow = length(random_rows))

            expect_equal(new_matrix, old_matrix)


            # approach 2, to get more numbers to test, make sure we get the points in the
            # sparse matrix where numbers are stored, the above can sometimes do
            # so only by coincidence when there are lots of 0s

            test_sample <- 200

            to_test <- sample(1:length(new_constraints$matr$i), test_sample)

            new_rows <- new_constraints$matr$i[to_test]
            new_cols <- new_constraints$matr$j[to_test]

            point_values_match <- sapply(1:test_sample, function(i) {
              new_constraints$matr[new_rows[i], new_cols[i]]$v ==
                old_constraints$matr[new_rows[i], new_cols[i]]$v
            })

            expect_true(all(point_values_match))

          })





# get_constraint_matrix
test_that('get_constraint_matrix produces the expected ouput from a simple
          known matrix', {

  # run the function on a manually coded set of constraints
  test_output <- get_constraint_matrix(constraint_set_a, nrow(decision_variables))

  test_output_matrix <- matrix(test_output$constraint_matrix,
                               ncol = nrow(decision_variables))

  # note that matrix_a is coded in the setup.R script
  expect_equal(test_output_matrix, matrix_a)
  expect_equal(test_output$directions, c('==', '==', '=='))
  expect_equal(test_output$right_hand_side, c(1, 1, 1))
  })



# then make a second matrix (maybe a third too), combine them and test the same
# things.
# combine_matrix_constraints

test_that('combine_matrix_constraints produces the expected ouput as manually
           combining two simple known matrices', {
             test_constraint_set_a <- get_constraint_matrix(constraint_set_a,
                                                            nrow(decision_variables))
             test_constraint_set_b <- get_constraint_matrix(constraint_set_b,
                                                            nrow(decision_variables))

             test_constraint_list <- list(test_constraint_set_a,
                                          test_constraint_set_b)

             test_combination <- combine_matrix_constraints(test_constraint_list)

             test_matrix <- matrix(test_combination$matr,
                                   ncol = nrow(decision_variables))

             expect_equal(test_matrix, mock_constraints$matr)
             expect_equal(test_combination$directions,
                          mock_constraints$directions)
             expect_equal(test_combination$rhs, mock_constraints$rhs)
           })



## need to test run_constraint_matrix
## need to test constraint_checks
