library(spatialrisk)
context("concentration")

test_that("value is equal to concentration for small radius", {
  conc <- concentration(Groningen[1, ], Groningen, value = amount,
                        radius = 0.00001, display_progress = FALSE)
  expect_equal(conc$amount, conc$concentration)
})

test_that("sum of value column obtained from points_in_circle fn is equal to
          concentration", {

            # Check first observation
            uit1 <- concentration(Groningen[1, ], Groningen, value = amount,
                                  radius = 200,
                                  display_progress = FALSE)
            sum1 <- points_in_circle(Groningen, lon_center = uit1$lon,
                                     lat_center = uit1$lat, radius = 200)$amount
            expect_equal(uit1$concentration, sum(sum1))

            # Check second observation
            uit2 <- concentration(Groningen[2, ], Groningen, value = amount,
                                  radius = 200,
                                  display_progress = FALSE)
            sum2 <- points_in_circle(Groningen, lon_center = uit2$lon,
                                     lat_center = uit2$lat, radius = 200)$amount
            expect_equal(uit2$concentration, sum(sum2))

            # Check last observation
            uit3 <- concentration(Groningen[nrow(Groningen), ], Groningen,
                                  value = amount,
                                  radius = 200, display_progress = FALSE)
            sum3 <- points_in_circle(Groningen, lon_center = uit3$lon,
                                     lat_center = uit3$lat, radius = 200)$amount
            expect_equal(uit3$concentration, sum(sum3))
          })

test_that("error should be returned for nonpositive radius", {
  expect_error(concentration(Groningen[2, ], Groningen, value = amount,
                             radius = -1, display_progress = FALSE))
})


test_that("lon and lat are same for input and output", {
  indf1 <- Groningen[1, ]
  outdf1 <- concentration(Groningen[1:3, ], Groningen, value = amount,
                          display_progress = FALSE)[1, ]
  expect_equal(indf1$lon, outdf1$lon)
  expect_equal(indf1$lat, outdf1$lat)
})
