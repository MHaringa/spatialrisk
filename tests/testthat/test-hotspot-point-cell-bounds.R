point_cell_test_bounds <- function(raster, centres, points, radius) {
  cells <- terra::cellFromXY(raster, as.matrix(centres))
  xy <- terra::xyFromCell(raster, cells)
  spatialrisk:::cell_point_upper_bounds_cpp(
    as.integer(cells), xy[, 1], xy[, 2],
    as.integer(terra::cellFromXY(raster, as.matrix(points[, c("x", "y")]))),
    points$x, points$y, points$amount, radius,
    terra::res(raster)[1], terra::res(raster)[2],
    terra::nrow(raster), terra::ncol(raster)
  )
}

test_that("point bounds preserve boundary tolerance without counting whole cells", {
  raster <- terra::rast(xmin = 0, xmax = 600, ymin = 0, ymax = 400,
                        resolution = 100, crs = "EPSG:3035")
  points <- data.frame(x = c(300, 350, 300, 0), y = c(50, 50, 50, 350),
                       amount = c(7, 10000, 3, 0))
  bound <- point_cell_test_bounds(raster, cbind(50, 50), points, 200)
  expect_equal(bound, 10, tolerance = 1e-8)
  # The scoring tolerance admits this point just beyond the ideal disk boundary.
  scored <- spatialrisk:::indexed_concentration_best_cpp(
    100 - 1e-8, 50, points$x, points$y, points$amount, 1:4, 200, 200
  )
  expect_equal(scored$concentration, 10)
  expect_gte(bound, scored$concentration)
  # A lower bound tied with the true objective must retain this cell.
  expect_true(bound >= 10)
  expect_error(point_cell_test_bounds(
    raster, cbind(50, 50), transform(points, amount = -amount), 200
  ), "non-negative")
})

test_that("point bounds cover square and rectangular cells at projected coordinates", {
  set.seed(9354)
  for (resolution in list(c(100, 100), c(150, 80))) {
    raster <- terra::rast(xmin = 4300000, xmax = 4301200,
                          ymin = 3200000, ymax = 3201200,
                          resolution = resolution, crs = "EPSG:3035")
    points <- data.frame(x = runif(80, 4300010, 4301190),
                         y = runif(80, 3200010, 3201190),
                         amount = sample(0:100, 80, replace = TRUE))
    cells <- c(1, 20, 35, terra::ncell(raster))
    centres <- terra::xyFromCell(raster, cells)
    bounds <- point_cell_test_bounds(raster, centres, points, 200)
    offsets <- expand.grid(x = c(-0.5, 0, 0.5), y = c(-0.5, 0, 0.5))
    for (i in seq_along(cells)) {
      scored <- spatialrisk:::indexed_concentration_best_cpp(
        centres[i, 1] + offsets$x * terra::res(raster)[1],
        centres[i, 2] + offsets$y * terra::res(raster)[2],
        points$x, points$y, points$amount, seq_len(nrow(points)), 200, 200
      )
      expect_gte(bounds[i], scored$concentration)
    }
  }
})

test_that("tightened screening matches full geometry on deterministic portfolios", {
  for (seed in 1:12) {
    set.seed(seed)
    metric <- data.frame(x = 4300000 + runif(40, 0, 1000),
                         y = 3200000 + runif(40, 0, 1000),
                         amount = sample(1:100, 40, replace = TRUE))
    data <- convert_crs_df(metric, 3035, 4326, "x", "y", "lon", "lat")
    model <- prepare_spatialrisk(data, "amount", radius = 200, cell_size = 100)
    selected <- select_candidates(model, progress = FALSE)
    full <- optimize_hotspot(model, progress = FALSE)
    screened <- optimize_hotspot(selected, progress = FALSE)
    expect_true(all(selected$candidates$cells$point_upper_bound >=
                      selected$candidates$threshold))
    expect_equal(screened$hotspots$amount_sum, full$hotspots$amount_sum)
    expect_equal(sum(screened$contributing_points$amount), full$hotspots$amount_sum)
    expect_lte(selected$candidates$threshold, full$hotspots$amount_sum)
  }
})

test_that("greedy bounds exclude removed records from the stored cell mapping", {
  metric <- data.frame(x = 4300000 + c(0, 70, 160, 1500, 1580, 1680),
                       y = 3200000 + c(0, 20, 0, 0, 20, 0),
                       amount = c(20, 40, 60, 2, 4, 6))
  data <- convert_crs_df(metric, 3035, 4326, "x", "y", "lon", "lat")
  model <- prepare_spatialrisk(data, "amount", radius = 100, cell_size = 50)
  full <- optimize_hotspot(model, n_hotspots = 2, progress = FALSE)
  selected <- select_candidates(model, progress = FALSE)
  screened <- optimize_hotspot(selected, n_hotspots = 2, progress = FALSE)
  expect_equal(screened$hotspots$amount_sum, c(120, 12))
  expect_equal(screened$hotspots$amount_sum, full$hotspots$amount_sum)
  expect_false(anyDuplicated(screened$contributing_points$data_row) > 0)
  active <- model$metric[4:6, ]
  tightened <- spatialrisk:::tighten_hotspot_candidate_cells(
    spatialrisk:::cells_above_threshold_with_values(model$state$focal, 0),
    model$state, active, "amount", 100, 0
  )
  expect_true(all(tightened$cells$point_upper_bound <= sum(active$amount) + 1e-6))
})

test_that("Groningen screening is selective without changing its objective", {
  model <- prepare_spatialrisk(Groningen[, c("lon", "lat", "amount")], "amount")
  selected <- select_candidates(model, progress = FALSE)
  expect_lt(nrow(selected$candidates$cells), 10)
  expect_gte(selected$candidates$threshold, 64172)
  hotspot <- optimize_hotspot(selected, progress = FALSE)
  expect_equal(hotspot$hotspots$amount_sum, 64438)
  expect_equal(nrow(hotspot$contributing_points), 208)
  custom <- select_candidates(model, threshold = 1000, progress = FALSE)
  expect_false("point_upper_bound" %in% names(custom$candidates$cells))
})
