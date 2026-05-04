library(spatialrisk)

test_that("concentration_hotspot returns descriptive output components", {
  x <- Groningen[1:200, c("lon", "lat", "amount")]

  out <- concentration_hotspot(x, value = "amount", radius = 200,
                               cell_size = 100, grid_precision = 5,
                               progress = FALSE)

  expect_s3_class(out, "hotspot")
  expect_equal(attr(out, "method"), "continuous")
  expect_equal(names(out), c("hotspots", "contributing_points"))
  expect_equal(names(out$hotspots), c("id", "lon", "lat", "amount_sum"))
  expect_equal(names(out$contributing_points),
               c("id", "data_row", "lon", "lat", "amount", "distance_m",
                 "amount_sum"))
  expect_false("concentration" %in% names(out$hotspots))
  expect_false("conc" %in% names(out$contributing_points))
  expect_false("ix" %in% names(out$contributing_points))
})

test_that("concentration_hotspot accepts named data argument", {
  x <- Groningen[1:50, c("lon", "lat", "amount")]

  out <- concentration_hotspot(data = x, value = "amount", radius = 200,
                               cell_size = 100, grid_precision = 5,
                               progress = FALSE)

  expect_s3_class(out, "hotspot")
  expect_equal(names(out), c("hotspots", "contributing_points"))
})

test_that("progress is validated", {
  x <- Groningen[1:50, c("lon", "lat", "amount")]

  expect_error(
    concentration_hotspot(x, value = "amount", radius = 200,
                          cell_size = 100, progress = NA),
    "`progress` must be"
  )
})

test_that("concentration_hotspot requires string value", {
  x <- Groningen[1:50, c("lon", "lat", "amount")]

  expect_error(
    concentration_hotspot(x, value = amount, radius = 200,
                          cell_size = 100, progress = FALSE),
    "`value` must"
  )
})

test_that("concentration_hotspot validates coordinate and value columns", {
  x <- Groningen[1:50, c("lon", "lat", "amount")]

  x_missing_lon <- x
  x_missing_lon$lon[1] <- NA_real_
  expect_error(
    concentration_hotspot(x_missing_lon, value = "amount", radius = 200,
                          cell_size = 100, grid_precision = 5,
                          progress = FALSE),
    "must not contain missing"
  )

  x_missing_value <- x
  x_missing_value$amount[1] <- NA_real_
  expect_error(
    concentration_hotspot(x_missing_value, value = "amount", radius = 200,
                          cell_size = 100, grid_precision = 5,
                          progress = FALSE),
    "`value` column"
  )

  x_bad_type <- x
  x_bad_type$amount <- as.character(x_bad_type$amount)
  expect_error(
    concentration_hotspot(x_bad_type, value = "amount", radius = 200,
                          cell_size = 100, grid_precision = 5,
                          progress = FALSE),
    "must be numeric"
  )

  x_conflict <- x
  x_conflict$amount_sum <- 1
  expect_error(
    concentration_hotspot(x_conflict, value = "amount", radius = 200,
                          cell_size = 100, grid_precision = 5,
                          progress = FALSE),
    "reserved output column 'amount_sum'"
  )
})

test_that("concentration_hotspot accepts custom coordinate columns", {
  x <- data.frame(x = Groningen$lon[1:100],
                  y = Groningen$lat[1:100],
                  insured = Groningen$amount[1:100])

  out <- concentration_hotspot(x, value = "insured", radius = 200,
                               cell_size = 100, grid_precision = 5,
                               lon = "x", lat = "y", progress = FALSE)

  expect_s3_class(out, "hotspot")
  expect_equal(attr(out, "method"), "continuous")
  expect_equal(names(out$hotspots), c("id", "x", "y", "insured_sum"))
  expect_equal(names(out$contributing_points),
               c("id", "data_row", "x", "y", "insured", "distance_m",
                 "insured_sum"))
})

test_that("concentration_hotspot validates search precision", {
  x <- Groningen[1:50, c("lon", "lat", "amount")]

  expect_error(
    concentration_hotspot(x, value = "amount", radius = 200,
                          cell_size = 100, grid_precision = 200,
                          progress = FALSE),
    "`grid_precision` > `cell_size`"
  )
})

test_that("concentration_hotspot requires a metric projected CRS", {
  x <- Groningen[1:50, c("lon", "lat", "amount")]

  expect_error(
    concentration_hotspot(x, value = "amount", radius = 200,
                          cell_size = 100, grid_precision = 5,
                          crs_metric = 4326, progress = FALSE),
    "meter units"
  )
})

test_that("plot.hotspot validates plotting arguments", {
  x <- Groningen[1:50, c("lon", "lat", "amount")]
  out <- concentration_hotspot(x, value = "amount", radius = 200,
                               cell_size = 100, grid_precision = 5,
                               progress = FALSE)

  expect_error(
    plot(out, max.rad = NA_real_),
    "`max.rad` must be"
  )
})

test_that("max_cover_circle is not exported", {
  expect_false("max_cover_circle" %in% getNamespaceExports("spatialrisk"))
})
