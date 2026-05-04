library(spatialrisk)

continuous_metric_toy_data <- function() {
  metric <- data.frame(
    x = c(4300000, 4300050, 4300000, 4300200, 4301000, 4301200),
    y = c(3200000, 3200000, 3200075, 3200000, 3201000, 3201200),
    amount = c(10, 20, 30, 40, 1, 1)
  )
  convert_crs_df(metric, crs_from = 3035, crs_to = 4326,
                 lon_from = "x", lat_from = "y",
                 lon_to = "lon", lat_to = "lat")
}

test_that("default concentration_hotspot uses pair refinement", {
  x <- Groningen[1:120, c("lon", "lat", "amount")]

  out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    cell_size = 100,
    progress = FALSE
  )

  expect_s3_class(out, "hotspot")
  expect_equal(attr(out, "method"), "continuous")
  expect_equal(attr(out, "refinement_methods"), "pair_intersections")
  expect_equal(names(out), c("hotspots", "contributing_points"))
})

test_that("continuous output structure matches grid method", {
  x <- Groningen[1:120, c("lon", "lat", "amount")]

  grid_out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    cell_size = 100,
    grid_precision = 5,
    method = "grid",
    progress = FALSE
  )
  pair_out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    cell_size = 100,
    progress = FALSE
  )

  expect_s3_class(pair_out, "hotspot")
  expect_equal(names(pair_out), names(grid_out))
  expect_equal(names(pair_out$hotspots), c("id", "lon", "lat", "amount_sum"))
  expect_equal(names(pair_out$contributing_points),
               c("id", "data_row", "lon", "lat", "amount", "distance_m",
                 "amount_sum"))
})

test_that("continuous agrees with full pair intersections on a local toy case", {
  toy <- continuous_metric_toy_data()

  continuous <- concentration_hotspot(
    toy,
    value = "amount",
    radius = 110,
    cell_size = 100,
    progress = FALSE
  )
  full_pair <- spatialrisk:::concentration_hotspot_pair_intersections(
    toy,
    value = "amount",
    radius = 110,
    progress = FALSE
  )

  expect_equal(continuous$hotspots$amount_sum,
               full_pair$hotspots$amount_sum)
  expect_equal(sort(continuous$contributing_points$data_row),
               sort(full_pair$contributing_points$data_row))
})

test_that("explicit grid method remains available", {
  x <- Groningen[1:120, c("lon", "lat", "amount")]

  grid_out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    cell_size = 100,
    grid_precision = 5,
    method = "grid",
    progress = FALSE
  )
  pair_out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    cell_size = 100,
    progress = FALSE
  )

  expect_equal(attr(grid_out, "method"), "grid")
  expect_equal(attr(pair_out, "method"), "continuous")
})

test_that("continuous top_n removes selected points", {
  x <- Groningen[1:200, c("lon", "lat", "amount")]

  out <- concentration_hotspot(
    x,
    value = "amount",
    top_n = 2,
    radius = 200,
    cell_size = 100,
    progress = FALSE
  )

  expect_equal(nrow(out$hotspots), 2)
  first_row <- out$contributing_points$data_row[out$contributing_points$id == 1]
  second_row <- out$contributing_points$data_row[out$contributing_points$id == 2]
  expect_length(intersect(first_row, second_row), 0)
})

test_that("continuous falls back to grid refinement above point limit", {
  x <- Groningen[1:200, c("lon", "lat", "amount")]

  out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    cell_size = 100,
    grid_precision = 5,
    max_refinement_points = 1,
    progress = FALSE
  )

  expect_equal(attr(out, "method"), "continuous")
  expect_equal(attr(out, "refinement_methods"), "grid")
})

test_that("continuous helper is not exported", {
  expect_false("concentration_hotspot_continuous" %in%
                 getNamespaceExports("spatialrisk"))
})

test_that("continuous progress reports search steps", {
  x <- Groningen[1:50, c("lon", "lat", "amount")]

  msg <- capture.output(concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    cell_size = 100,
    progress = TRUE
  ))

  expect_true(any(grepl("terra focal screening", msg)))
  expect_true(any(grepl("local refinement subset", msg)))
  expect_true(any(grepl("selected concentration", msg)))
})
