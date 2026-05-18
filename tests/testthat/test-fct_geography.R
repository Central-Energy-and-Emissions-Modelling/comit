
#hav.dist
test_that("hav.dist calculates correct distances", {

  # Test case 1: Distance between the same point should be 0
  expect_equal(hav.dist(0, 0, 0, 0), 0)

  # Test case 2: Distance between Lands End and John O'Groats
  expect_equal(round(hav.dist(-5.71481, 50.06648, -3.07009, 58.64410)), 969)

  # Test case 3: Distance between Old Trafford and Ethiad Stadium
  expect_equal(round(hav.dist(-2.29134, 53.463316, -2.19999, 53.48329), 1), 6.4)

  # Test case 4: Distance between Cardiff Castle and Castell Coch
  expect_equal(round(hav.dist(-3.18099, 51.48234, -3.25487, 51.53596), 1), 7.9)

  # expected answers taken from calculator at
  # www.vcalc.com/wiki/vcalc/havershine-distance
})



#assign_site_region
test_that('assign_site_region assigns the correct region values to some artificial
          sites', {

            # test on some fake sites
            sites <- data.frame(
              site_ID = c(1, 2, 3, 4),
              site_name = c('Landsend', 'Old Trafford', 'Cardiff Castle', 'Tokyo'),
              Latitude = c(50.07, 53.46, 51.48, 35.68),
              Longitude = c(-5.71, -2.29, -3.18, 139.77)
            )

            sites %<>% assign_site_region()

            expect_equal(sites$region, c('South West (England)',
                                         'North West (England)',
                                         'Wales',
                                         NA_character_))
          })


#region_lookup
test_that('region_lookup can be used to correctly classify the clusters into
          the standard gor regions', {

            cluster_locations <- raw_data$Cluster_location %>%
              left_join(region_lookup(., link_to_cluster = TRUE),
                        by = 'Cluster')

            manual_cluster_classifications <- c(
              'North East (England)',
              'Yorkshire and The Humber',
              'Yorkshire and The Humber',
              'South East (England)',
              'Wales',
              'North West (England)',
              'Scotland',
              'Scotland',
              'East Midlands (England)',
              'South East (England)',
              'Northern Ireland'
            )

            expect_equal(cluster_locations$region, manual_cluster_classifications)
          })

