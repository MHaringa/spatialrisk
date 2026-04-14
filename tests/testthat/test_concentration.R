library(spatialrisk)
context("concentration")

test_that("value is equal to concentration for small radius", {
  conc <- radius_sum(Groningen[1, ], Groningen, value = "amount",
                     radius = 0.00001, display_progress = FALSE)
  expect_equal(conc$amount, conc$radius_sum)
})


test_that("sum of value column obtained from points_in_circle fn is equal to
          concentration", {

            # Check first observation
            uit1 <- radius_sum(Groningen[1, ],
                               Groningen,
                               value = "amount",
                               radius = 200,
                               display_progress = FALSE)
            sum1 <- points_within_radius(Groningen,
                                         lon_center = uit1$lon,
                                         lat_center = uit1$lat,
                                         radius = 200)$amount
            expect_equal(uit1$radius_sum, sum(sum1))

            # Check second observation
            uit2 <- radius_sum(Groningen[2, ],
                               Groningen,
                               value = "amount",
                               radius = 200,
                               display_progress = FALSE)
            sum2 <- points_within_radius(Groningen,
                                         lon_center = uit2$lon,
                                         lat_center = uit2$lat,
                                         radius = 200)$amount
            expect_equal(uit2$radius_sum, sum(sum2))

            # Check last observation
            uit3 <- radius_sum(Groningen[nrow(Groningen), ],
                               Groningen,
                               value = "amount",
                               radius = 200,
                               display_progress = FALSE)
            sum3 <- points_within_radius(Groningen,
                                         lon_center = uit3$lon,
                                         lat_center = uit3$lat,
                                         radius = 200)$amount
            expect_equal(uit3$radius_sum, sum(sum3))
          })

test_that("error should be returned for nonpositive radius", {
  expect_error(radius_sum(Groningen[2, ],
                          Groningen,
                          value = "amount",
                          radius = -1,
                          display_progress = FALSE))
})


test_that("lon and lat are same for input and output", {
  indf1 <- Groningen[1, ]
  outdf1 <- radius_sum(Groningen[1:3, ],
                       Groningen,
                       value = "amount",
                       display_progress = FALSE)[1, ]
  expect_equal(indf1$lon, outdf1$lon)
  expect_equal(indf1$lat, outdf1$lat)
})
