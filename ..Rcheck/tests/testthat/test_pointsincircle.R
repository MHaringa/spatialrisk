library(spatialrisk)
context("points_in_circle")

test_that("distance in output from points in circle is equal to distance from
          haversine", {

            # Check first observation
            uit <- points_within_radius(Groningen, 6.52, 53.24)
            expect_equal(uit[1, ]$distance_m, haversine(53.24, 6.52,
                                                        uit[1, ]$lat,
                                                        uit[1, ]$lon))

            # Check second observation
            expect_equal(uit[2, ]$distance_m, haversine(53.24, 6.52,
                                                        uit[2, ]$lat,
                                                        uit[2, ]$lon))
          })

test_that("lon and lat are same for input and output", {

  # Check first observation
  indf1 <- Groningen[1, ]
  outdf1 <- points_within_radius(Groningen, indf1$lon, indf1$lat)[1, ]
  expect_equal(indf1$lon, outdf1$lon)
  expect_equal(indf1$lat, outdf1$lat)

})

test_that("multiple centers preserve existing id column", {
  df <- Groningen[1:5, ]
  df$id <- paste0("policy-", seq_len(nrow(df)))

  out <- points_within_radius(
    df,
    lon_center = df$lon[1:2],
    lat_center = df$lat[1:2],
    radius = 200
  )

  expect_true("center_index" %in% names(out))
  expect_true("distance_m" %in% names(out))
  expect_true(all(out$id %in% df$id))
  expect_false(is.numeric(out$id))
})

test_that("missing center returns empty result", {
  out <- points_within_radius(Groningen, lon_center = NA_real_,
                              lat_center = 53.24)
  expect_equal(nrow(out), 0)
  expect_true("distance_m" %in% names(out))
})

test_that("invalid inputs fail clearly", {
  expect_error(
    points_within_radius(Groningen, 6.52, 53.24, radius = Inf),
    "finite positive"
  )
  expect_error(
    points_within_radius(Groningen, c(6.52, 6.53), 53.24),
    "same length"
  )
  expect_error(
    points_within_radius(Groningen, Inf, 53.24),
    "infinite"
  )
})
