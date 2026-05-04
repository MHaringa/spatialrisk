library(spatialrisk)

pair_refine_metric_toy_data <- function() {
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
    refinement_buffer = 2000,
    progress = FALSE
  )

  expect_s3_class(out, "hotspot")
  expect_equal(attr(out, "method"), "terra_pair_refine")
  expect_equal(attr(out, "refinement_methods"), "pair_intersections")
  expect_true(all(c("conc_df", "pts_df", "hotspots",
                    "contributing_points") %in% names(out)))
})

test_that("pair_refine output structure matches concentration_hotspot", {
  x <- Groningen[1:120, c("lon", "lat", "amount")]

  terra_out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    cell_size = 100,
    grid_precision = 5,
    progress = FALSE
  )
  pair_out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    cell_size = 100,
    refinement_buffer = 2000,
    progress = FALSE
  )

  expect_s3_class(pair_out, "hotspot")
  expect_equal(names(pair_out), names(terra_out))
  expect_true(all(c("lon", "lat", "concentration", "id") %in%
                    names(pair_out$conc_df)))
  expect_true(all(c("lon", "lat", "amount", "ix", "id", "conc") %in%
                    names(pair_out$pts_df)))
})

test_that("pair_refine equals full pair intersections when local area contains all points", {
  toy <- pair_refine_metric_toy_data()

  pair_refine <- concentration_hotspot(
    toy,
    value = "amount",
    radius = 110,
    cell_size = 100,
    refinement_buffer = 5000,
    progress = FALSE
  )
  full_pair <- spatialrisk:::concentration_hotspot_pair_intersections(
    toy,
    value = "amount",
    radius = 110,
    progress = FALSE
  )

  expect_equal(pair_refine$conc_df$concentration,
               full_pair$conc_df$concentration)
  expect_equal(sort(pair_refine$pts_df$ix), sort(full_pair$pts_df$ix))
})

test_that("explicit terra method remains available", {
  x <- Groningen[1:120, c("lon", "lat", "amount")]

  terra_out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    cell_size = 100,
    grid_precision = 5,
    method = "terra",
    progress = FALSE
  )
  pair_out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    cell_size = 100,
    refinement_buffer = 2000,
    progress = FALSE
  )

  expect_null(attr(terra_out, "method"))
  expect_equal(attr(pair_out, "method"), "terra_pair_refine")
})

test_that("pair_refine top_n removes selected points", {
  x <- Groningen[1:200, c("lon", "lat", "amount")]

  out <- concentration_hotspot(
    x,
    value = "amount",
    top_n = 2,
    radius = 200,
    cell_size = 100,
    refinement_buffer = 2000,
    progress = FALSE
  )

  expect_equal(nrow(out$conc_df), 2)
  first_ix <- out$pts_df$ix[out$pts_df$id == 1]
  second_ix <- out$pts_df$ix[out$pts_df$id == 2]
  expect_length(intersect(first_ix, second_ix), 0)
})

test_that("pair_refine falls back to grid refinement above point limit", {
  x <- Groningen[1:200, c("lon", "lat", "amount")]

  out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    cell_size = 100,
    grid_precision = 5,
    refinement_buffer = 2000,
    max_refinement_points = 1,
    progress = FALSE
  )

  expect_equal(attr(out, "method"), "terra_pair_refine")
  expect_equal(attr(out, "refinement_methods"), "grid")
})

test_that("pair_refine helper is not exported", {
  expect_false("concentration_hotspot_pair_refine" %in%
                 getNamespaceExports("spatialrisk"))
})

test_that("pair_refine progress reports search steps", {
  x <- Groningen[1:50, c("lon", "lat", "amount")]

  msg <- capture.output(concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    cell_size = 100,
    refinement_buffer = 2000,
    progress = TRUE
  ))

  expect_true(any(grepl("terra focal screening", msg)))
  expect_true(any(grepl("local refinement subset", msg)))
  expect_true(any(grepl("selected concentration", msg)))
})
