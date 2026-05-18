test_that("dtplyr datatable setting is set to TRUE", {
  expect_true(.datatable.aware)
})


# Test cases
test_that("formulate_availability_constraint returns correct constraints", {

  # Sample data for testing
  availability_constraint_data <- data.table::data.table(
    variable_index.used_capacity = c(1, 2),
    variable_index.available_capacity = c(3, 4),
    total_factor = c(0.5, 0.75)
  )


  # Expected output
  expected_output <- list(
    list(
      column_indices = c(1, 3),
      values = c(1, -0.5),
      direction = "<=",
      rhs = 0
    ),
    list(
      column_indices = c(2, 4),
      values = c(1, -0.75),
      direction = "<=",
      rhs = 0
    )
  )


  result <- formulate_availability_constraint(availability_constraint_data)
  expect_equal(result, expected_output)
})


test_that("formulate_availability_constraint handles empty data", {
  empty_data <- data.table::data.table(
    variable_index.used_capacity = integer(),
    variable_index.available_capacity = integer(),
    total_factor = numeric()
  )
  expect_error(formulate_availability_constraint(empty_data))
})







# Test cases
test_that("availability returns correct constraints", {

  # Sample data for testing
  data <- list(
    Technologies = data.table::data.table(
      code = c("tech1", "tech2"),
      availability_factor = c(0.8, 0.9),
      capacity_to_activity_factor = c(1.2, 1.1)
    )
  )

  decision_variables <- data.table::data.table(
    site_ID = c(1, 1, 1, 1),
    code = c("tech1", "tech2", "tech1", "tech2"),
    year = c(2025, 2025, 2025, 2025),
    variable_type = c("used_capacity", "used_capacity", "available_capacity", "available_capacity"),
    variable_index = c(1, 2, 3, 4)
  )

  # Expected output
  expected_output <- list(
    list(
      column_indices = c(1, 3),
      values = c(1, -0.96),
      direction = "<=",
      rhs = 0
    ),
    list(
      column_indices = c(2, 4),
      values = c(1, -0.99),
      direction = "<=",
      rhs = 0
    )
  )

  result <- availability(data, decision_variables)
  expect_equal(result, expected_output)
})


test_that("availability handles empty data", {
  empty_data <- list(Technologies = data.frame())
  empty_decision_variables <-  data.frame()
  expect_error(result <- availability(empty_data, empty_decision_variables))
})

