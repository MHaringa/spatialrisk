library(spatialrisk)

metric_toy_data <- function() {
  metric <- data.frame(
    x = c(4300000, 4300050, 4300000, 4300200),
    y = c(3200000, 3200000, 3200075, 3200000),
    amount = c(10, 20, 30, 40)
  )
  convert_crs_df(metric, crs_from = 3035, crs_to = 4326,
                 lon_from = "x", lat_from = "y",
                 lon_to = "lon", lat_to = "lat")
}

test_that("observed method returns a hotspot object", {
  x <- Groningen[1:100, c("lon", "lat", "amount")]

  out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    progress = FALSE,
    method = "observed"
  )

  expect_s3_class(out, "hotspot")
  expect_equal(names(out), c("hotspots", "contributing_points"))
})

test_that("observed hotspot output is compatible with concentration_hotspot", {
  x <- Groningen[1:100, c("lon", "lat", "amount")]

  out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    progress = FALSE,
    method = "observed"
  )

  expect_equal(names(out$hotspots), c("id", "lon", "lat", "amount_sum"))
  expect_equal(names(out$contributing_points),
               c("id", "data_row", "lon", "lat", "amount", "distance_m",
                 "amount_sum"))
  expect_null(attr(out, "rasterized"))
  expect_null(attr(out, "focal"))
  expect_equal(attr(out, "threshold"), NA_real_)
})

test_that("top_n = 1 works on Groningen", {
  x <- Groningen[1:100, c("lon", "lat", "amount")]

  out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    progress = FALSE,
    method = "observed"
  )

  expect_equal(nrow(out$hotspots), 1)
  expect_equal(unique(out$contributing_points$id), 1)
  expect_equal(out$hotspots$amount_sum,
               sum(out$contributing_points$amount))
})

test_that("top_n = 2 removes first hotspot points before second search", {
  x <- Groningen[1:200, c("lon", "lat", "amount")]

  out <- concentration_hotspot(
    x,
    value = "amount",
    top_n = 2,
    radius = 200,
    progress = FALSE,
    method = "observed"
  )

  expect_equal(nrow(out$hotspots), 2)
  first_row <- out$contributing_points$data_row[out$contributing_points$id == 1]
  second_row <- out$contributing_points$data_row[out$contributing_points$id == 2]
  expect_length(intersect(first_row, second_row), 0)
})

test_that("observed observed-points search works", {
  x <- Groningen[1:100, c("lon", "lat", "amount")]

  out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    progress = FALSE,
    method = "observed"
  )

  expect_gt(out$hotspots$amount_sum, 0)
})

test_that("observed-points strategy works on a deterministic toy dataset", {
  toy <- metric_toy_data()

  out <- concentration_hotspot(
    toy,
    value = "amount",
    radius = 110,
    progress = FALSE,
    method = "observed"
  )

  expect_equal(out$hotspots$amount_sum, 60)
  expect_equal(sort(out$contributing_points$data_row), 1:3)
})

test_that("indexed_points_in_radius_cpp matches points_within_radius", {
  toy <- metric_toy_data()
  metric <- convert_crs_df(toy, crs_from = 4326, crs_to = 3035,
                           lon_from = "lon", lat_from = "lat",
                           lon_to = "x", lat_to = "y")
  metric$ix <- seq_len(nrow(metric))

  center <- metric[1, ]
  observed <- indexed_points_in_radius_cpp(
    x_center = center$x,
    y_center = center$y,
    x_ref = metric$x,
    y_ref = metric$y,
    value_ref = metric$amount,
    ix_ref = metric$ix,
    radius = 110,
    cell_width = 110
  )

  toy$id <- seq_len(nrow(toy))
  haversine <- points_within_radius(toy, lon_center = toy$lon[1],
                                    lat_center = toy$lat[1],
                                    radius = 110)

  expect_equal(sort(observed$ix), sort(haversine$id))
})

test_that("observed and grid hotspot concentrations are comparable", {
  x <- Groningen[1:100, c("lon", "lat", "amount")]

  grid_out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    cell_size = 100,
    grid_precision = 5,
    method = "grid",
    progress = FALSE
  )
  observed_out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    progress = FALSE,
    method = "observed"
  )

  expect_lte(observed_out$hotspots$amount_sum,
             grid_out$hotspots$amount_sum * 1.05)
  expect_gt(observed_out$hotspots$amount_sum, 0)
})

test_that("observed hotspot validates missing and invalid inputs", {
  x <- Groningen[1:20, c("lon", "lat", "amount")]

  expect_error(
    concentration_hotspot(x, value = "missing",
                          progress = FALSE,
                          method = "observed"),
    "doesn't exist"
  )

  x_bad_lon <- x
  x_bad_lon$lon[1] <- NA_real_
  expect_error(
    concentration_hotspot(x_bad_lon, value = "amount",
                          progress = FALSE,
                          method = "observed"),
    "must not contain missing"
  )

  x_bad_value <- x
  x_bad_value$amount[1] <- NA_real_
  expect_error(
    concentration_hotspot(x_bad_value, value = "amount",
                          progress = FALSE,
                          method = "observed"),
    "must not contain missing"
  )

  expect_error(
    concentration_hotspot(x, value = "amount", radius = -1,
                          progress = FALSE,
                          method = "observed"),
    "positive"
  )
})

test_that("observed hotspot concentration plot can be created", {
  testthat::skip_if_not_installed("mapview")

  x <- Groningen[1:100, c("lon", "lat", "amount")]
  out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    progress = FALSE,
    method = "observed"
  )

  expect_s4_class(plot(out, type = "concentration"), "mapview")
})

test_that("observed hotspot gives clear errors for terra diagnostic plots", {
  x <- Groningen[1:100, c("lon", "lat", "amount")]
  out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    progress = FALSE,
    method = "observed"
  )

  expect_error(
    plot(out, type = "focal"),
    "only available for terra-based"
  )
})

test_that("observed helpers are not exported as public API", {
  exports <- getNamespaceExports("spatialrisk")

  expect_false("concentration_hotspot_observed" %in% exports)
  expect_false("max_cover_circle_observed" %in% exports)
})
