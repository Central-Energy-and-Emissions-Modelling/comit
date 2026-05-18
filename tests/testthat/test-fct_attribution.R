# Sample data for testing
mock_attrib_data <- list(
  resource_efficiency = data.frame(
    commodity = c("A", "A", "B", "B", "C", "C", "D", "D", "E", "E"),
    r_efficiency = c(0.8, 0.7, 0.6, 0.5, 0.9, 0.85, 0.7, 0.65, 0.5, 0.45),
    year = c(2030, 2035, 2030, 2035, 2030, 2035, 2030, 2035, 2030, 2035),
    stringsAsFactors = FALSE
  ),
  energy_efficiency = data.frame(
    commodity = c("A", "A", "B", "B", "C", "C", "D", "D", "E", "E"),
    efficiency = c(0.7, 0.6, 0.5, 0.4, 0.8, 0.75, 0.6, 0.55, 0.4, 0.35),
    year = c(2030, 2035, 2030, 2035, 2030, 2035, 2030, 2035, 2030, 2035),
    stringsAsFactors = FALSE
  ),
  Technologies = data.frame(
    output_commodity = c("A", "B", "C", "D", "E", "A"),
    sector = c("X", "X", "Y", "Y", "Z", "X"),
    stringsAsFactors = FALSE
  )
)

cf_energy_aggregated <- data.frame(
  year = c(2030, 2030, 2030, 2035, 2035, 2035, 2030, 2035),
  primary_output = c("A", "B", "C", "A", "B", "C", "D", "D"),
  fuel_category = c("coal", "oil", "gas", "coal", "oil", "gas", "coal", "oil"),
  total_energy = c(100, 200, 300, 150, 250, 350, 400, 450),
  total_emissions = c(10, 20, 30, 15, 25, 17.5, 40, 90),
  stringsAsFactors = FALSE
)


sector_outputs <- get_sector_outputs(mock_attrib_data)


# Test for get_sector_commodity_lookup
test_that("get_sector_commodity_lookup returns correct lookup table", {
  result <- get_sector_commodity_lookup(mock_attrib_data)
  expected <- data.frame(
    output_commodity = c("A", "B", "C", "D", "E"),
    sector = c("X", "X", "Y", "Y", "Z"),
    stringsAsFactors = FALSE
  )
  expect_equal(result, expected)
})


# Test for sort_efficiencies with resource efficiency
test_that("sort_efficiencies returns correct resource efficiency data", {
  result <- sort_efficiencies(mock_attrib_data, "resource")
  expected <- data.frame(
    sector = c("X", "X", "Y", "Y", "Z", "Z"),
    year = c(2030, 2035, 2030, 2035, 2030, 2035),
    efficiency = c(0.7, 0.6, 0.8, 0.75, 0.5, 0.45),
    stringsAsFactors = FALSE
  )
  expect_equal(result$sector, expected$sector)
  expect_equal(result$year, expected$year)
  expect_equal(result$efficiency, expected$efficiency)
})


# Test for sort_efficiencies with energy efficiency
test_that("sort_efficiencies returns correct energy efficiency data", {
  result <- sort_efficiencies(mock_attrib_data, "energy")
  expected <- data.frame(
    sector = c("X", "X", "Y", "Y", "Z", "Z"),
    year = c(2030, 2035, 2030, 2035, 2030, 2035),
    efficiency = c(0.6, 0.5, 0.7, 0.65, 0.4, 0.35),
    stringsAsFactors = FALSE
  )
  expect_equal(result$sector, expected$sector)
  expect_equal(result$year, expected$year)
  expect_equal(result$efficiency, expected$efficiency)
})


# Test for invalid efficiency type
test_that("sort_efficiencies throws error for invalid efficiency type", {
  expect_error(sort_efficiencies(mock_attrib_data, "invalid"),
               "Invalid efficiency type. Choose 'resource' or 'energy'.")
})





# Test for attr_aggregations_by_type without exclusion
test_that("attr_aggregations_by_type returns correct data without exclusion", {

  result <- attr_aggregations_by_type(cf_energy_aggregated, sector_outputs) %>%
    arrange(year, primary_output)

  expected <- data.frame(
    year = c(2030, 2030, 2030, 2030, 2035, 2035, 2035, 2035),
    primary_output = c("A", "B", "C", "D", "A", "B", "C", "D"),
    total_energy = c(100, 200, 300, 400, 150, 250, 350, 450),
    total_emissions = c(10, 20, 30, 40, 15, 25, 17.5, 90),
    sector = c("X", "X", "Y", "Y", "X", "X", "Y", "Y"),
    emissions_intensity = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.05, 0.2),
    stringsAsFactors = FALSE
  ) %>%
    arrange(year, primary_output) %>%
    tibble()

  expect_equal(result, expected)
})


# Test for attr_aggregations_by_type with exclusion
test_that("attr_aggregations_by_type returns correct data with exclusion", {

  result <- attr_aggregations_by_type(cf_energy_aggregated,
                                      sector_outputs,
                                      exclusion_type = c("coal")) %>%
    arrange(year, primary_output)

  expected <- data.frame(
    year = c(2030, 2030, 2035, 2035, 2035),
    primary_output = c("B", "C", "D", "B", "C"),
    total_energy = c(200, 300, 450, 250, 350),
    total_emissions = c(20, 30, 90, 25, 17.5),
    sector = c("X", "Y", "Y", "X", "Y"),
    emissions_intensity = c(0.1, 0.1, 0.2, 0.1, 0.05),
    stringsAsFactors = FALSE
  ) %>%
    arrange(year, primary_output) %>%
    tibble()

  expect_equal(result, expected)
})

# Test for attr_aggregations_by_type with multiple exclusions
test_that("attr_aggregations_by_type returns correct data with multiple exclusions", {
  result <- attr_aggregations_by_type(cf_energy_aggregated,
                                      sector_outputs,
                                      exclusion_type = c("coal", "oil")) %>%
    arrange(year, primary_output)

  expected <- data.frame(
    year = c(2030, 2035),
    primary_output = c("C", "C"),
    total_energy = c(300, 350),
    total_emissions = c(30, 17.5),
    sector = c("Y", "Y"),
    emissions_intensity = c(0.1, 0.05),
    stringsAsFactors = FALSE
  ) %>%
    arrange(year, primary_output) %>%
    tibble()

  expect_equal(result, expected)
})









