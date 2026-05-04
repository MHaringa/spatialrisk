library(spatialrisk)

test_that("concentration_hotspot returns descriptive output components", {
  x <- Groningen[1:200, c("lon", "lat", "amount")]

  out <- concentration_hotspot(x, value = "amount", radius = 200,
                               cell_size = 100, grid_precision = 5,
                               progress = FALSE)

  expect_s3_class(out, "hotspot")
  expect_equal(attr(out, "method"), "terra_pair_refine")
  expect_true(all(c("hotspots", "contributing_points", "conc_df", "pts_df") %in%
                    names(out)))
  expect_equal(out$hotspots, out$conc_df)
  expect_equal(out$contributing_points, out$pts_df)
  expect_true(all(c("lon", "lat", "concentration", "id") %in%
                    names(out$hotspots)))
  expect_true(all(c("lon", "lat", "amount", "id", "conc") %in%
                    names(out$contributing_points)))
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
})

test_that("concentration_hotspot accepts custom coordinate columns", {
  x <- data.frame(x = Groningen$lon[1:100],
                  y = Groningen$lat[1:100],
                  insured = Groningen$amount[1:100])

  out <- concentration_hotspot(x, value = "insured", radius = 200,
                               cell_size = 100, grid_precision = 5,
                               lon = "x", lat = "y", progress = FALSE)

  expect_s3_class(out, "hotspot")
  expect_equal(attr(out, "method"), "terra_pair_refine")
  expect_true(all(c("x", "y", "concentration", "id") %in%
                    names(out$hotspots)))
  expect_true(all(c("x", "y", "insured", "id", "conc") %in%
                    names(out$contributing_points)))
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
