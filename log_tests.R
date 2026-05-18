# This script runs all tests and creates a table of the results for record
# keeping.

if(sys.nframe() == 0) {

  # For log
  # con <- file('test.log')
  # sink(con)
  #
  # devtools::test()
  #
  # sink()

  # For table version
  my_tests <- devtools::test(reporter = testthat::ListReporter)

  my_tests_tidy <- my_tests %>%
    as_tibble() %>%
    select(file, test, n_tests = nb, failed, warning, passed, time_taken = real)

  write.csv(my_tests_tidy, 'test_results.csv')

}



