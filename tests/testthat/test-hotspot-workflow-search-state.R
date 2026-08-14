library(spatialrisk)

metric_workflow_data <- function(x, y, amount) {
  metric <- data.frame(
    x = 4300000 + x,
    y = 3200000 + y,
    amount = amount
  )
  convert_crs_df(
    metric,
    crs_from = 3035,
    crs_to = 4326,
    lon_from = "x",
    lat_from = "y",
    lon_to = "lon",
    lat_to = "lat"
  )
}

test_that("prepared and selected states use different candidate searches", {
  data <- Groningen[1:80, c("lon", "lat", "amount")]
  prepared <- prepare_spatialrisk(data, value = "amount", radius = 200,
                                  cell_size = 100)

  full <- optimize_hotspot(prepared, progress = FALSE)
  selected <- select_candidates(prepared, progress = FALSE)
  screened <- optimize_hotspot(selected, progress = FALSE)

  expect_equal(attr(full, "candidate_search"), "full")
  expect_equal(attr(full, "refinement_methods"),
               "pair_intersections_full")
  expect_equal(attr(screened, "candidate_search"), "screened")
})

test_that("prepared raster includes points on portfolio extent boundaries", {
  data <- metric_workflow_data(
    x = c(0, 10, 20),
    y = c(0, 10, 20),
    amount = c(1, 2, 3)
  )
  prepared <- prepare_spatialrisk(data, value = "amount", radius = 10,
                                  cell_size = 1)

  expect_false(anyNA(prepared$state$point_cells$cell))
  expect_equal(
    sum(terra::values(prepared$state$rasterized), na.rm = TRUE),
    sum(data$amount)
  )
})

test_that("selected candidate subset does not restrict candidate scoring", {
  data <- metric_workflow_data(
    x = c(10, 10, 29),
    y = c(-1, 1, 0),
    amount = c(1, 1, 100)
  )
  selected <- prepare_spatialrisk(data, value = "amount", radius = 10,
                                  cell_size = 1)
  selected$params$method <- "continuous"
  selected$params$max_refinement_points <- 1000
  # This hand-built search state deliberately uses an explicit threshold,
  # which disables the automatic focal-cell pruning rule.
  selected$params$threshold <- 0
  selected$candidates <- list(
    approximate_center = data.frame(x = 4300000, y = 3200000),
    threshold = 0,
    cells = data.frame(
      cell = 1L,
      x = 4300000,
      y = 3200000,
      focal_value = 0
    ),
    local_data_rows = c(1L, 2L),
    local_points = 2L,
    refinement = "pair_intersections"
  )

  out <- optimize_hotspot(selected, progress = FALSE)

  expect_equal(out$hotspots$amount_sum, 102)
  expect_equal(sort(out$contributing_points$data_row), 1:3)
  expect_equal(attr(out, "candidate_search"), "screened")
})

test_that("full geometric search retains an observed-point optimum", {
  data <- metric_workflow_data(
    x = c(0, 50, 100),
    y = c(0, 0, 0),
    amount = c(10, 1, 1)
  )
  prepared <- prepare_spatialrisk(data, value = "amount", radius = 10,
                                  cell_size = 5)

  out <- optimize_hotspot(prepared, progress = FALSE)
  centre <- convert_crs_df(
    out$hotspots,
    crs_from = 4326,
    crs_to = 3035,
    lon_from = "lon",
    lat_from = "lat",
    lon_to = "x",
    lat_to = "y"
  )

  expect_equal(out$hotspots$amount_sum, 10)
  expect_equal(centre$x, 4300000, tolerance = 1e-5)
  expect_equal(centre$y, 3200000, tolerance = 1e-5)
})

test_that("full continuous search can improve on observed centres", {
  data <- metric_workflow_data(c(0, 15), c(0, 0), c(1, 1))
  prepared <- prepare_spatialrisk(data, value = "amount", radius = 10,
                                  cell_size = 5)

  continuous <- optimize_hotspot(prepared, progress = FALSE)
  observed_state <- select_candidates(prepared, method = "observed",
                                      progress = FALSE)
  observed <- optimize_hotspot(observed_state, progress = FALSE)

  centre <- convert_crs_df(
    continuous$hotspots,
    crs_from = 4326,
    crs_to = 3035,
    lon_from = "lon",
    lat_from = "lat",
    lon_to = "x",
    lat_to = "y"
  )
  observed_xy <- convert_crs_df(
    data,
    crs_from = 4326,
    crs_to = 3035,
    lon_from = "lon",
    lat_from = "lat",
    lon_to = "x",
    lat_to = "y"
  )
  distances <- sqrt(
    (observed_xy$x - centre$x)^2 + (observed_xy$y - centre$y)^2
  )

  expect_equal(continuous$hotspots$amount_sum, 2)
  expect_equal(observed$hotspots$amount_sum, 1)
  expect_equal(distances, c(10, 10), tolerance = 1e-3)
  expect_false(any(distances < 1e-5))
})

test_that("full geometric search never uses the grid fallback", {
  data <- metric_workflow_data(c(0, 15), c(0, 0), c(1, 1))
  prepared <- prepare_spatialrisk(data, value = "amount", radius = 10,
                                  cell_size = 5)
  prepared$params$max_refinement_points <- 1

  out <- expect_warning(
    optimize_hotspot(prepared, progress = FALSE),
    "does not trigger grid fallback"
  )

  expect_equal(attr(out, "refinement_methods"),
               "pair_intersections_full")
})

test_that("greedy removal works for full and screened search states", {
  data <- metric_workflow_data(
    x = c(0, 15, 100, 115),
    y = c(0, 0, 0, 0),
    amount = c(5, 5, 3, 3)
  )
  prepared <- prepare_spatialrisk(data, value = "amount", radius = 10,
                                  cell_size = 5)
  full <- optimize_hotspot(prepared, n_hotspots = 2, progress = FALSE)
  first <- full$contributing_points$data_row[
    full$contributing_points$id == 1
  ]
  second <- full$contributing_points$data_row[
    full$contributing_points$id == 2
  ]
  expect_length(intersect(first, second), 0)
  expect_equal(full$hotspots$amount_sum, c(10, 6))

  screened_data <- Groningen[1:200, c("lon", "lat", "amount")]
  screened_state <- prepare_spatialrisk(
    screened_data, value = "amount", radius = 200, cell_size = 100
  )
  screened_state <- select_candidates(screened_state, progress = FALSE)
  screened <- optimize_hotspot(
    screened_state, n_hotspots = 2, progress = FALSE
  )
  first <- screened$contributing_points$data_row[
    screened$contributing_points$id == 1
  ]
  second <- screened$contributing_points$data_row[
    screened$contributing_points$id == 2
  ]
  expect_length(intersect(first, second), 0)
})

test_that("full geometric search requires non-negative weights", {
  data <- metric_workflow_data(c(0, 15), c(0, 0), c(1, -1))
  prepared <- prepare_spatialrisk(data, value = "amount", radius = 10,
                                  cell_size = 5)

  expect_error(
    optimize_hotspot(prepared, progress = FALSE),
    "non-negative"
  )
})
