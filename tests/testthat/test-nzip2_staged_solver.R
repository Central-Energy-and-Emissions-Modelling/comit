
# comit_staged_solver
# Here we make sure that the code that produces the stages for testing is still
# up to date with the original function.
# This is to ensure the code underpinning many of the tests themselves are
# functioning as expected.
test_that("comit_staged_solver is identical to comit solver, to ensure its up
          to date.", {

  # supressing warnings and messages here to keep the testing output clean
  staged_vers <- comit_staged_solver(raw_data) %>%
    suppressMessages() %>%
    suppressWarnings()

  orig <- comit_solver(raw_data) %>%
    suppressMessages() %>%
    suppressWarnings()

  expect_equal(staged_vers$solution$objval, orig$solution$objective_value)
})


## This currently a time consuming test. Could we create a slimmed down rds
# version of the raw_data file with fewer sites and technologies to test this?
