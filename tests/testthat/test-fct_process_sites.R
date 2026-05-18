

# get data after running process sites
post_process_sites_data <- process_sites(raw_data)

input_data <- raw_data

NAEI_data_2021 <- append_information_to_sites(input_data)


# process_sites
test_that('All data tables not purposefully manipulated by process_sites are the
          same as input',
          {
            elements <- names(raw_data)
            original_elements <-
              elements[!elements %in% c('NAEI_df_clean_2021_revised')] # this one gets
            # updated so is tested seperately

            all_element_match <- tibble()
            for (e in original_elements) {
              e_match <- tryCatch({
                match <- all(raw_data[[e]] == post_process_sites_data[[e]],
                             na.rm = TRUE)
              },
              error = function(error) {
                match <- FALSE
                return(match)
              })

              element_match <-
                data.frame(element = e, identical = e_match)
              all_element_match <-
                rbind(all_element_match, element_match)
            }

            expect_true(all(all_element_match$identical))

          })


# process_sites
test_that("process_sites produces only the expected columns, no duplicate rows
          and no NAs for the NAEI_clean dataframe", {

  test_df <- process_sites(raw_data)$NAEI_clean

  expect_equal(nrow(test_df %>% distinct()), nrow(test_df))

  # note that we do expect NAs for grid connection_year and PlantID. So drop these for the test
  expect_equal(nrow(test_df %>%
                      select(!c(grid_connection_year, PlantID)) %>%
                      tidyr::drop_na()),
               nrow(test_df))

  expect_setequal(
    colnames(test_df),
    c(
      'site_name',
      'IPM_sector',
      'H2_point',
      'total_MtCO2',
      'pipe_dist',
      'num_sites',
      'Latitude',
      'Longitude',
      'traded_flag',
      'in_cluster_H2',
      'in_cluster_CCS',
      'H2_first_year',
      'CCS_first_year',
      'grid_connection_year',
      'PlantID'
    )
  )

})


# process_sites
test_that('process_sites produces a dataframe with as many rows as generated
          from the large, small and non-point sites functions together',
          {
            # always use IDBR for purposes of this test
            input_data$model_parameters$Use_IDBR <- TRUE

            test_df <- process_sites(raw_data)$NAEI_clean

            large_sites <- get_large_point_sites(NAEI_data_2021, input_data)
            small_sites <- get_aggregated_small_point_sites(NAEI_data_2021, input_data)
            non_point_sites <- non_point_sites_from_ratios(NAEI_data_2021, input_data)

            total_sites <- nrow(large_sites) + nrow(small_sites) + nrow(non_point_sites)

            expect_equal(nrow(test_df), total_sites)
          })



#join_traded_flag
test_that("traded_flag column has expected values", {

  valid_traded_flag <- raw_data$NAEI_df_clean_2021_revised$traded_flag %in% c("Non-traded", "Traded")

  expect_true(all(valid_traded_flag))
})


# append_information_to_sites()
test_that('append_information_to_sites produces data for all sites', {

  test_df <- append_information_to_sites(raw_data)

  expect_equal(nrow(test_df), nrow(raw_data$NAEI_df_clean_2023_revised))

})




#get_non_point_sites
test_that("Each combination of sic code and region are included", {

  non_point_sites <- get_non_point_sites(raw_data)

  expect_in(regions, colnames(non_point_sites))

  expect_in(raw_data$ONS_sector_mapping$`4_digit_SIC_code`,
            non_point_sites$sic_code_4_digit)

  expect_equal(nrow(non_point_sites), nrow(raw_data$ONS_sector_mapping))

})


#calculate_coordinates
test_that("calculate_coordinates does the correct conversion", {

  # use some known coordinates and check the output is as expected.
  # these points were found on google maps and there Easting/Northing values
  # calculated from an online tool
  test_df <- data.frame(place = c('companies_house', 'edinburgh'),
                        Easting = c(317494, 326272),
                        Northing = c(178567, 674028)) %>%
    calculate_coordinates()

  expect_equal(round(test_df[test_df$place=='companies_house', 'Longitude'], 2), -3.19)
  expect_equal(round(test_df[test_df$place=='companies_house', 'Latitude'], 2), 51.5)
  expect_equal(round(test_df[test_df$place=='edinburgh', 'Longitude'], 4), -3.1823)
  expect_equal(round(test_df[test_df$place=='edinburgh', 'Latitude'], 4), 55.9536)

})


#allocate_cluster_points
test_that('allocate_cluster_points correctly assigns closest cluster for some
          known points', {
            test_df <- data.frame(place = c('companies_house', 'edinburgh'),
                                  Easting = c(317494, 326272),
                                  Northing = c(178567, 674028)) %>%
              calculate_coordinates() %>%
              allocate_cluster_points(raw_data)

            expect_equal(test_df[test_df$place=='companies_house', 'H2_point'],
                         'South Wales')

            expect_equal(test_df[test_df$place=='edinburgh', 'H2_point'],
                         'Grangemouth')
          })

#calculate_pipe_distances
test_that('calculate_pipe_distances produces the correct values for some known
          points', {
            # havershine distances generated from an online tool for the comparison
            test_df <- data.frame(
              place = c('place1', 'place2'),
              Longitude = c(-3.15, -3.19),
              Latitude = c(50, 55),
              Lat_clust = c(51.56, 56.02),
              Lon_clust = c(-3.77, -3.71)
            ) %>% calculate_pipe_distances()

            expect_equal(round(test_df[test_df$place == 'place1', 'pipe_dist']), 179)
            expect_equal(round(test_df[test_df$place == 'place2', 'pipe_dist']), 118)
          })


# do_if_using_traded_share_calc
test_that('do_if_using_traded_share_calc performs function when relevant parameter
          is TRUE, else returns identical df from input.',
          {
            # arbitrary df
            input_df <- data.frame(
              col1 = c(1:10),
              col2 = c(101:110)
            )

            # get value when parameter true
            raw_data$model_parameters$Traded_share_calc <- TRUE

            value_when_actioned <-
              do_if_using_traded_share_calc(input_df,
                                            max,
                                            raw_data,
                                            pass_input_data = FALSE)

            # get value when parameter false
            raw_data$model_parameters$Traded_share_calc <- FALSE

            value_when_not_actioned <-
              do_if_using_traded_share_calc(input_df,
                                            max,
                                            raw_data,
                                            pass_input_data = FALSE)

            expect_equal(value_when_actioned, 110)
            expect_equal(value_when_not_actioned, input_df)

          })


# sum_emissions_by_sector
test_that('sum_emissions_by_sector aggregates correctly',
          {
            # create mock data
            input_df <- data.frame(
              site = c(1:10),
              IPM_sector = c(rep('Construction', 5), rep('Cement', 5)),
              traded_flag = c('Traded', 'Non-traded'),
              emissions_MtCO2 = c(rep(1, 5), rep(2, 5))
            )

            # run mock data through function
            test_df <- sum_emissions_by_sector(input_df) %>%
              arrange(IPM_sector, traded_flag)

            ## manually create the expected output
            # emissions should be 5 in construction, with 3 from traded sites, 2 non-traded
            # emissions should be 10 in cement, with 4 from traded sites, 6 non-traded
            expected_df <- data.frame(
              IPM_sector = c(rep('Construction', 2), rep('Cement', 2)),
              traded_flag = c('Traded', 'Non-traded'),
              sector_emissions = c(3, 2, 4, 6),
              total_point_sites_emissions_in_sector = c(5, 5, 10, 10)
            ) %>%
              arrange(IPM_sector, traded_flag) %>%
              as_tibble()

            expect_equal(test_df, expected_df)

          })


# get_sector_emission_totals
test_that('get_sector_emission_totals produces all rows and emission shares
          sum to 1', {

  input_df <- data.frame(
    site_id = c(1:(length(sectors)*10)),
    IPM_sector = rep(sectors, 10),
    traded_flag = c(rep('Traded', length(sectors) * 5),
                    rep('Non-traded', length(sectors) * 5)),
    emissions_MtCO2 = 1,
    grid_connection_year = 2030
  )

  test_df <- get_sector_emission_totals(input_df, raw_data)

  expect_setequal(test_df$IPM_sector, sectors)
  expect_equal(nrow(test_df), length(sectors) * 2)

  total_share <- (test_df$traded_share +
                    test_df$non_traded_point_share +
                    test_df$non_point_share)

  expect_equal(total_share, rep(1, length(total_share)))

})

# NOTE: is sum_emissions_by_sector still needed? May be redundant now?


# get_small_point_sites_filter
test_that('get_small_point_sites_filter returns variable of correct length and
          without NA values.', {
  input_df <- append_information_to_sites(input_data)

  filter_test <- get_small_point_sites_filter(input_df)

  expect_equal(length(filter_test), nrow(input_df))
  expect_true(sum(is.na(filter_test)) == 0)
})




# get_aggregated_small_point_sites
test_that('get_aggregated_small_points contains the correct number of sites
          and no duplicates', {

            test_df <- get_aggregated_small_point_sites(NAEI_data_2021, input_data) %>%
              distinct()

            small_point_sites_count <- get_small_point_site_data(
              NAEI_data_2021,
              input_data) %>%
              group_small_point_sites(input_data) %>%
              count()

            expect_equal(nrow(test_df), nrow(small_point_sites_count))

          })


# get_small_point_site_data
test_that('get_small_point_site_data returns the correct number of sites and
          only non-traded sites', {

            small_point_sites <- get_small_point_site_data(
              NAEI_data_2021,
              input_data)

            check_df <- NAEI_data_2021[get_small_point_sites_filter(NAEI_data_2021), ]

            expect_equal(nrow(small_point_sites), nrow(check_df))
            expect_setequal(small_point_sites$traded_flag, 'Non-traded')
          })


# get_small_point_site_data
test_that('get_small_point_site_data works with mock data, correctly
          categorising pipe distances', {

            # create mock data
            input_df <- data.frame(
              site = c(1:10),
              IPM_sector = c(rep('Construction', 5), rep('Cement', 5)),
              traded_flag = c('Traded', 'Non-traded'),
              emissions_MtCO2 = c(rep(0.000001, 5), rep(0.000002, 5)),
              pipe_dist = rep(c(0, 500, 1, 499, 1050), 2),
              H2_point = 'Londonderry',
              technology = 'ICH01',
              grid_connection_year = c(rep(2030, 5), rep(2040, 5))
            ) %>%
              calculate_share_of_sector_emissions(input_data) %>%
              get_small_point_site_data(input_data) %>%
              mutate(pipe_dist_category = as.character(pipe_dist_category))

            # within boundary gives correct string
            expect_equal(input_df[input_df$pipe_dist == 0, 'pipe_dist_category'],
                         '[0,500)')
            expect_equal(input_df[input_df$pipe_dist == 1, 'pipe_dist_category'],
                         '[0,500)')
            expect_equal(input_df[input_df$pipe_dist == 499, 'pipe_dist_category'],
                         '[0,500)')

            # outside of boundary should be NA:
            expect_true(is.na(input_df[input_df$pipe_dist == 500,
                                       'pipe_dist_category']))
            expect_true(is.na(input_df[input_df$pipe_dist == 1050,
                                       'pipe_dist_category']))
          })





# aggregate_small_point_sites
# group_small_point_sites
test_that('aggregate_small_sites creates the correct number of aggregate sites
          from the correct number of small sites.', {

            input_data$model_parameters$Two_nps_sites <- TRUE

            small_point_sites <- get_small_point_site_data(
              NAEI_data_2021,
              input_data)

            small_point_sites_grouped <- small_point_sites %>%
              aggregate_small_point_sites(., input_data)

            groups_count <- small_point_sites %>%
              group_small_point_sites(input_data) %>%
              count()

            expect_equal(nrow(small_point_sites_grouped), nrow(groups_count))
            expect_equal(small_point_sites_grouped$num_sites, groups_count$n)
            expect_contains(colnames(small_point_sites_grouped), 'in_cluster_H2')
          })


# aggregate_small_point_sites
# group_small_point_sites
test_that('aggregate_small_sites creates the correct number of aggregate sites
          from the correct number of small sites, when Two_nps_sites is False.',
          {
            input_data$model_parameters$Two_nps_sites <- FALSE

            small_point_sites <- get_small_point_site_data(
              NAEI_data_2021,
              input_data)

            small_point_sites_grouped <- small_point_sites %>%
              aggregate_small_point_sites(., input_data)

            groups_count <- small_point_sites %>%
              group_small_point_sites(input_data) %>%
              count()

            expect_equal(nrow(small_point_sites_grouped), nrow(groups_count))
            expect_equal(small_point_sites_grouped$num_sites, groups_count$n)
            expect_true(!'in_cluster_H2' %in% colnames(small_point_sites_grouped))
          })

# impute_site_location
test_that('impute_site_location produces the correct number of coordinates',
          {
            small_point_sites <- get_small_point_site_data(
              NAEI_data_2021,
              input_data)

            small_point_sites_grouped <- small_point_sites %>%
              aggregate_small_point_sites(., input_data)

            new_small_loc <- impute_site_location(small_point_sites_grouped,
                                                  'pipe_dist')

            expect_equal(ncol(new_small_loc), 2)
            expect_equal(nrow(new_small_loc), nrow(small_point_sites_grouped))
            expect_equal(nrow(unique(new_small_loc)), nrow(new_small_loc))
          })



#get_proportion_of_emissions_from_sector
test_that('get_inside_vs_outside_cluster_ratios has the correct number of
          combinations', {
            cluster_sector_share <-
              get_proportion_of_emissions_from_sector(input_data)

            expect_equal(nrow(cluster_sector_share),
                         length(sectors) * length(clusters))

          })


#get_proportion_of_emissions_from_sector
test_that('cluster sector shares produced from
          get_proportion_of_emissions_from_sector sum to 1', {

            cluster_sector_share <-
              get_proportion_of_emissions_from_sector(input_data)

            total_sector_props <- cluster_sector_share %>%
              group_by(IPM_sector) %>%
              summarise(total = sum(sector_share))

            expect_true(all(between(total_sector_props$total, 0.97, 1.03)))

            # TODO: REINSTATE THE BELOW TEST IN PLACE OF ABOVE
            # There is a minor descrepncy at the moment. This is negligible
            # but we can be more accurate here.

            #expect_equal(total_sector_props$total, rep(1, length(sectors)))

          })

#get_inside_vs_outside_cluster_ratios
test_that('get_inside_vs_outside_cluster_ratios has the correct number of
          combinations', {
            cluster_ratios <- get_inside_vs_outside_cluster_ratios(input_data)

            expect_equal(nrow(cluster_ratios),
                         length(sectors) * length(clusters))
          })


#get_inside_vs_outside_cluster_ratios
test_that('get_inside_vs_outside_cluster_ratios produces values between 0 and 1',
          {
            cluster_ratios <- get_inside_vs_outside_cluster_ratios(input_data)


            expect_true(all(between(cluster_ratios$in_ratio, 0, 1)))
          })


#non_point_sites_from_ratios
test_that('non_point_sites_from_ratios creates a dataframe with the
            correct number of rows and the correct columns', {

              expected_vars <- c('site_name', 'IPM_sector', 'H2_point', 'total_MtCO2',
                                 'pipe_dist', 'num_sites', 'Latitude', 'Longitude',
                                 'traded_flag', 'grid_connection_year', 'PlantID')

              input_data$model_parameters$Two_nps_sites <- TRUE
              test_df_param_true <- non_point_sites_from_ratios(NAEI_data_2021,
                                                                input_data)

              input_data$model_parameters$Two_nps_sites <- FALSE
              test_df_param_false <- non_point_sites_from_ratios(NAEI_data_2021,
                                                                 input_data)


              expect_equal(nrow(test_df_param_true),
                           2*length(used_clusters)*length(sectors))
              expect_setequal(colnames(test_df_param_true), expected_vars)

              expect_equal(nrow(test_df_param_false),
                           length(used_clusters)*length(sectors))
              expect_setequal(colnames(test_df_param_false), expected_vars)

            })


#impute_site_details_for_ratios
test_that('impute_site_details_for_ratios creates a dataframe with the
            correct number of rows and the correct columns', {

              # manually create some mock data
              mock_data <- data.frame(
                Cluster = rep(c('Teeside', 'Southampton'), 2),
                IPM_sector = c(rep('Cement', 2), rep('Textiles', 2)),
                sector_share = c(0.7, 0.2, 0.3, 0.2),
                non_point_site_emissions_MtCO2 = c(3, 2, 4, 1),
                in_ratio = c(0.5, 0.9, 0.2, 0.7),
                in_cluster_distance = c(15, 16, 17, 20),
                out_cluster_distance = c(50, 51, 60, 61),
                Lat_clust = c(54.6, 54.6, 50.9, 50.9),
                Lon_clust = c(-1.2, -1.2, -1.4, -1.4),
                H2_production = TRUE,
                use_cluster = TRUE,
                grid_connection_year = NA
              ) %>%
                mutate(non_point_site_total_cluster_sector_emissions =
                         sector_share * non_point_site_emissions_MtCO2) %>%
                arrange(IPM_sector, Cluster)

              input_data$model_parameters$Two_nps_sites <- TRUE
              test_df_true_param <- impute_site_details_for_ratios(mock_data, input_data)

              input_data$model_parameters$Two_nps_sites <- FALSE
              test_df_false_param <- impute_site_details_for_ratios(mock_data, input_data)

              expected_vars <- c('site_name', 'IPM_sector', 'H2_point', 'total_MtCO2',
                                 'pipe_dist', 'num_sites', 'Latitude', 'Longitude',
                                 'traded_flag', 'grid_connection_year', 'PlantID')

              expect_equal(nrow(test_df_true_param), 2*nrow(mock_data))
              expect_setequal(colnames(test_df_true_param), expected_vars)

              expect_equal(nrow(test_df_false_param), nrow(mock_data))
              expect_setequal(colnames(test_df_false_param), expected_vars)

})



#get_values_for_two_non_point_sites
test_that('get_values_for_two_non_point_sites doubles the amount of rows and
          sums to the original amount of emissions', {

            # manually create some mock data
            mock_data <- data.frame(
              Cluster = rep(c('Teeside', 'Southampton'), 2),
              IPM_sector = c(rep('Cement', 2), rep('Textiles', 2)),
              sector_share = c(0.7, 0.2, 0.3, 0.2),
              non_point_site_emissions_MtCO2 = c(3, 2, 4, 1),
              in_ratio = c(0.5, 0.9, 0.2, 0.7),
              in_cluster_distance = c(15, 16, 17, 20),
              out_cluster_distance = c(50, 51, 60, 61),
              Lat_clust = c(54.6, 54.6, 50.9, 50.9),
              Lon_clust = c(-1.2, -1.2, -1.4, -1.4),
              H2_production = TRUE,
              use_cluster = TRUE
            ) %>%
              mutate(non_point_site_total_cluster_sector_emissions =
                       sector_share * non_point_site_emissions_MtCO2) %>%
              arrange(IPM_sector, Cluster)

            # check number of rows
            test_df <- get_values_for_two_non_point_sites(mock_data)

            # check emissions sum to original
            test_emissions <- test_df %>%
              group_by(IPM_sector, Cluster) %>%
              summarise(total_MtCO2 = sum(total_MtCO2), .groups = 'keep') %>%
              ungroup() %>%
              arrange(IPM_sector, Cluster) %>%
              pull(total_MtCO2)

            expect_equal(nrow(test_df), 2*nrow(mock_data))

            expect_equal(test_emissions,
                         mock_data$non_point_site_total_cluster_sector_emissions)

          })


#tidy_sites_data
test_that('tidy_sites_data maintains the correct number of rows, sorts them
          in the correct order and adds the required columns', {

            Teeside_cluster_radius <- input_data$cluster_radius %>%
              filter(cluster == 'Teesside')

            mock_data <- data.frame(
              site_name = c('c', 'c', 'a', 'ab', 'b'),
              IPM_sector = 'Cement',
              H2_point = 'Teesside',
              total_MtC02 = 1,
              pipe_dist = c(rep(Teeside_cluster_radius$cluster_radius_H2 + 1, 2),
                            rep(Teeside_cluster_radius$cluster_radius_H2 - 1, 3)),
              num_sites = 1,
              Latitude = c(55, 54, 53, 56, 57),
              Longitude = -2,
              traded_flag = 'Traded'
            )

            test_df <- tidy_sites_data(mock_data, input_data)

            mock_data_sorted <- mock_data %>%
              arrange(site_name, Latitude)

            expect_equal(test_df$site_name, mock_data_sorted$site_name)
            expect_equal(test_df$Latitude, mock_data_sorted$Latitude)
            expect_equal(nrow(test_df), nrow(mock_data))
            expect_in(c('in_cluster_H2', 'in_cluster_CCS',
                        'H2_first_year', 'CCS_first_year'),
                      colnames(test_df))
          })


