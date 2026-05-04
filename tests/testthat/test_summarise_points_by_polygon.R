library(spatialrisk)
context("summarise_points_by_polygon")

make_polygons <- function() {
  geometry <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)))),
    crs = 4326
  )
  sf::st_sf(area = c("a", "b"), geometry = geometry)
}

make_points <- function() {
  data.frame(
    lon = c(0.25, 0.75, 1.25, 3),
    lat = c(0.25, 0.75, 0.25, 3),
    amount = c(10, 20, 30, 40)
  )
}

test_that("summarise_points_by_polygon sums values by polygon", {
  out <- expect_message(
    summarise_points_by_polygon(make_polygons(), make_points(), "amount"),
    "outside"
  )

  expect_s3_class(out, "sf")
  expect_true("amount_sum" %in% names(out))
  expect_equal(out$amount_sum, c(30, 30))
})

test_that("summarise_points_by_polygon names output from value and function", {
  out <- summarise_points_by_polygon(make_polygons(), make_points(), "amount",
                                     fun = mean, outside = "ignore")

  expect_true("amount_mean" %in% names(out))
  expect_equal(out$amount_mean, c(15, 30))
})

test_that("summarise_points_by_polygon accepts custom coordinates and output", {
  points <- data.frame(x = c(0.25, 1.25),
                       y = c(0.25, 0.25),
                       insured = c(5, 7))

  out <- summarise_points_by_polygon(make_polygons(), points, "insured",
                                     fun = sum, lon = "x", lat = "y",
                                     output_col = "total_insured")

  expect_true("total_insured" %in% names(out))
  expect_equal(out$total_insured, c(5, 7))
})

test_that("summarise_points_by_polygon validates inputs", {
  polygons <- make_polygons()
  points <- make_points()

  expect_error(summarise_points_by_polygon(data.frame(id = 1), points,
                                           "amount"),
               "sf object")
  expect_error(summarise_points_by_polygon(polygons, points, "missing"),
               "must contain")
  expect_error(summarise_points_by_polygon(polygons,
                                           transform(points, lon = as.character(lon)),
                                           "amount"),
               "numeric")
  expect_error(summarise_points_by_polygon(polygons, points, "amount",
                                           output_col = "area"),
               "already contains")
})

test_that("points_to_polygon remains a deprecated wrapper", {
  out <- expect_warning(
    points_to_polygon(make_polygons(), make_points(),
                      sum(amount, na.rm = TRUE)),
    "deprecated"
  )

  expect_s3_class(out, "sf")
  expect_true("output" %in% names(out))
  expect_equal(out$output, c(30, 30))
})
