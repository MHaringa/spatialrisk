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

test_that("indexed method returns a hotspot object", {
  x <- Groningen[1:100, c("lon", "lat", "amount")]

  out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    progress = FALSE,
    method = "indexed"
  )

  expect_s3_class(out, "hotspot")
  expect_true(all(c("conc_df", "pts_df", "hotspots",
                    "contributing_points") %in% names(out)))
  expect_equal(out$conc_df, out$hotspots)
  expect_equal(out$pts_df, out$contributing_points)
})

test_that("indexed hotspot output is compatible with concentration_hotspot", {
  x <- Groningen[1:100, c("lon", "lat", "amount")]

  out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    progress = FALSE,
    method = "indexed"
  )

  expect_true(all(c("lon", "lat", "concentration", "id") %in%
                    names(out$conc_df)))
  expect_true(all(c("lon", "lat", "amount", "ix", "id", "conc",
                    "distance_m") %in% names(out$pts_df)))
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
    method = "indexed"
  )

  expect_equal(nrow(out$conc_df), 1)
  expect_equal(unique(out$pts_df$id), 1)
  expect_equal(out$conc_df$concentration, sum(out$pts_df$amount))
})

test_that("top_n = 2 removes first hotspot points before second search", {
  x <- Groningen[1:200, c("lon", "lat", "amount")]

  out <- concentration_hotspot(
    x,
    value = "amount",
    top_n = 2,
    radius = 200,
    progress = FALSE,
    method = "indexed"
  )

  expect_equal(nrow(out$conc_df), 2)
  first_ix <- out$pts_df$ix[out$pts_df$id == 1]
  second_ix <- out$pts_df$ix[out$pts_df$id == 2]
  expect_length(intersect(first_ix, second_ix), 0)
})

test_that("indexed observed-points search works", {
  x <- Groningen[1:100, c("lon", "lat", "amount")]

  out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    progress = FALSE,
    method = "indexed"
  )

  expect_gt(out$conc_df$concentration, 0)
})

test_that("observed-points strategy works on a deterministic toy dataset", {
  toy <- metric_toy_data()

  out <- concentration_hotspot(
    toy,
    value = "amount",
    radius = 110,
    progress = FALSE,
    method = "indexed"
  )

  expect_equal(out$conc_df$concentration, 60)
  expect_equal(sort(out$pts_df$ix), 1:3)
})

test_that("indexed_points_in_radius_cpp matches points_within_radius", {
  toy <- metric_toy_data()
  metric <- convert_crs_df(toy, crs_from = 4326, crs_to = 3035,
                           lon_from = "lon", lat_from = "lat",
                           lon_to = "x", lat_to = "y")
  metric$ix <- seq_len(nrow(metric))

  center <- metric[1, ]
  indexed <- indexed_points_in_radius_cpp(
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

  expect_equal(sort(indexed$ix), sort(haversine$id))
})

test_that("indexed and terra hotspot concentrations are comparable", {
  x <- Groningen[1:100, c("lon", "lat", "amount")]

  terra_out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    cell_size = 100,
    grid_precision = 5,
    method = "terra",
    progress = FALSE
  )
  indexed_out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    progress = FALSE,
    method = "indexed"
  )

  expect_lte(indexed_out$conc_df$concentration,
             terra_out$hotspots$concentration * 1.05)
  expect_gt(indexed_out$conc_df$concentration, 0)
})

test_that("indexed hotspot validates missing and invalid inputs", {
  x <- Groningen[1:20, c("lon", "lat", "amount")]

  expect_error(
    concentration_hotspot(x, value = "missing",
                          progress = FALSE,
                          method = "indexed"),
    "doesn't exist"
  )

  x_bad_lon <- x
  x_bad_lon$lon[1] <- NA_real_
  expect_error(
    concentration_hotspot(x_bad_lon, value = "amount",
                          progress = FALSE,
                          method = "indexed"),
    "must not contain missing"
  )

  x_bad_value <- x
  x_bad_value$amount[1] <- NA_real_
  expect_error(
    concentration_hotspot(x_bad_value, value = "amount",
                          progress = FALSE,
                          method = "indexed"),
    "must not contain missing"
  )

  expect_error(
    concentration_hotspot(x, value = "amount", radius = -1,
                          progress = FALSE,
                          method = "indexed"),
    "positive"
  )
})

test_that("indexed hotspot concentration plot can be created", {
  testthat::skip_if_not_installed("mapview")

  x <- Groningen[1:100, c("lon", "lat", "amount")]
  out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    progress = FALSE,
    method = "indexed"
  )

  expect_s4_class(plot(out, type = "concentration"), "mapview")
})

test_that("indexed hotspot gives clear errors for terra diagnostic plots", {
  x <- Groningen[1:100, c("lon", "lat", "amount")]
  out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    progress = FALSE,
    method = "indexed"
  )

  expect_error(
    plot(out, type = "focal"),
    "only available for terra-based"
  )
})

test_that("indexed helpers are not exported as public API", {
  exports <- getNamespaceExports("spatialrisk")

  expect_false("concentration_hotspot_indexed" %in% exports)
  expect_false("max_cover_circle_indexed" %in% exports)
})
