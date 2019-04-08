library(spatialrisk)
context("Points_in_circle")

test_that("distance in output from points in circle is equal to distance from haversine", {

  # Check first observation
  uit <- points_in_circle(Groningen, 6.52, 53.24)
  expect_equal(uit[1,]$distance_m, haversine(53.24, 6.52, uit[1,]$lat, uit[1,]$lon))

  # Check second observation
  expect_equal(uit[2,]$distance_m, haversine(53.24, 6.52, uit[2,]$lat, uit[2,]$lon))
})

test_that("lon and lat are same for input and output", {

  # Check first observation
  indf1 <- Groningen[1,]
  outdf1 <- points_in_circle(Groningen, indf1$lon, indf1$lat)[1,]
  expect_equal(indf1$lon, outdf1$lon)
  expect_equal(indf1$lat, outdf1$lat)

})


