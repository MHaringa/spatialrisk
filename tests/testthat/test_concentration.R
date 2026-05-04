library(spatialrisk)
context("concentration")

test_that("value is equal to concentration for small radius", {
  conc <- radius_sum(Groningen[1, ], Groningen, value = "amount",
                     radius = 0.00001, progress = FALSE)
  expect_equal(conc$amount, conc$radius_sum)
})


test_that("sum of value column obtained from points_within_radius fn is equal to
          concentration", {

            # Check first observation
            uit1 <- radius_sum(Groningen[1, ],
                               Groningen,
                               value = "amount",
                               radius = 200,
                               progress = FALSE)
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
                               progress = FALSE)
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
                               progress = FALSE)
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
                          progress = FALSE),
               "finite positive")
})


test_that("lon and lat are same for input and output", {
  indf1 <- Groningen[1, ]
  outdf1 <- radius_sum(Groningen[1:3, ],
                       Groningen,
                       value = "amount",
                       progress = FALSE)[1, ]
  expect_equal(indf1$lon, outdf1$lon)
  expect_equal(indf1$lat, outdf1$lat)
})

test_that("custom result column can be used", {
  out <- radius_sum(Groningen[1:3, ],
                    Groningen,
                    value = "amount",
                    radius = 200,
                    progress = FALSE,
                    result_col = "amount_within_200m")

  expect_true("amount_within_200m" %in% names(out))
  expect_false("radius_sum" %in% names(out))
})

test_that("new radius_sum API accepts named targets and reference arguments", {
  targets <- data.frame(x = Groningen$lon[1:3],
                        y = Groningen$lat[1:3])
  reference <- data.frame(lon_ref = Groningen$lon,
                          lat_ref = Groningen$lat,
                          amount = Groningen$amount)

  out <- radius_sum(targets = targets,
                    reference = reference,
                    value = "amount",
                    lon_targets = "x",
                    lat_targets = "y",
                    lon_reference = "lon_ref",
                    lat_reference = "lat_ref",
                    radius = 200,
                    progress = FALSE)

  expect_true("radius_sum" %in% names(out))
  expect_equal(nrow(out), nrow(targets))
})

test_that("deprecated concentration API still accepts sub and full", {
  out <- expect_warning(
    concentration(sub = Groningen[1:3, ],
                  full = Groningen,
                  value = amount,
                  radius = 200,
                  display_progress = FALSE),
    "deprecated"
  )

  expect_true("radius_sum" %in% names(out))
  expect_equal(nrow(out), 3)
})

test_that("existing result column is not overwritten", {
  targets <- Groningen[1:3, ]
  targets$radius_sum <- 1

  expect_error(
    radius_sum(targets,
               reference = Groningen,
               value = "amount",
               progress = FALSE),
    "already contains"
  )
})

test_that("invalid inputs fail clearly", {
  expect_error(
    radius_sum(Groningen[1, ], Groningen, value = "amount", radius = Inf,
               progress = FALSE),
    "finite positive"
  )

  expect_error(
    radius_sum(Groningen[1, ], Groningen, value = "amount",
               progress = NA),
    "TRUE or FALSE"
  )

  expect_error(
    radius_sum(Groningen[1, ], Groningen, value = "missing",
               progress = FALSE),
    "required columns"
  )
})

test_that("missing reference coordinates and values are ignored", {
  full <- Groningen[1:3, ]
  full$amount[1] <- NA_real_
  full$lon[2] <- NA_real_
  full$lat[3] <- NA_real_

  out <- radius_sum(Groningen[1, ],
                    full,
                    value = "amount",
                    radius = 200,
                    progress = FALSE)

  expect_equal(out$radius_sum, 0)
})
