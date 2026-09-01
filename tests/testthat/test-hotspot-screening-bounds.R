screening_test_portfolio <- function() {
  metric <- data.frame(
    x = c(4299500, 4300500, 4299955, 4300105, 4300030),
    y = c(3199500, 3200500, 3199956.6987, 3199956.6987, 3200086.6025),
    amount = c(0, 0, 10, 20, 30)
  )
  data <- convert_crs_df(metric, 3035, 4326, "x", "y", "lon", "lat")
  data$ix <- seq_len(nrow(data))
  data
}

screening_test_params <- function(radius = 100, cell_size = 100) {
  list(
    value = "amount",
    radius = radius,
    cell_size = cell_size,
    crs_metric = 3035,
    lon = "lon",
    lat = "lat"
  )
}

exact_metric_sum <- function(x, y, metric, radius) {
  inside <- (metric$x - x)^2 + (metric$y - y)^2 <= radius^2 + 1e-7
  sum(metric$amount[inside])
}

test_that("moving window covers radius plus the full raster-cell diagonal", {
  raster <- terra::rast(
    terra::ext(0, 850, 0, 800), resolution = 100, crs = "EPSG:3035"
  )
  resolution <- terra::res(raster)
  bound <- 200 + sqrt(sum(resolution^2))
  window <- spatialrisk:::mw_create(raster, radius = 200)

  expect_equal(ncol(window), 1 + 2 * ceiling(bound / resolution[1]))
  expect_equal(nrow(window), 1 + 2 * ceiling(bound / resolution[2]))
})

test_that("moving-window mask uses Euclidean cell-centre distance", {
  raster <- terra::rast(
    terra::ext(0, 900, 0, 800), resolution = c(150, 100), crs = "EPSG:3035"
  )
  radius <- 200
  resolution <- terra::res(raster)
  bound <- radius + sqrt(sum(resolution^2))
  window <- spatialrisk:::mw_create(raster, radius)
  row_offset <- seq_len(nrow(window)) - ceiling(nrow(window) / 2)
  col_offset <- seq_len(ncol(window)) - ceiling(ncol(window) / 2)
  expected <- outer(
    row_offset * resolution[2], col_offset * resolution[1],
    function(dy, dx) dx^2 + dy^2 <= bound^2
  )

  expect_equal(!is.na(window), expected)
})

test_that("expanded focal values upper-bound centres throughout each cell", {
  data <- screening_test_portfolio()
  params <- screening_test_params(radius = 100, cell_size = 100)
  state <- spatialrisk:::initialise_terra_hotspot_state(
    data, "amount", params$radius, params$cell_size, "lon", "lat", 3035
  )
  metric <- convert_crs_df(data, 4326, 3035, "lon", "lat", "x", "y")
  focal_values <- terra::values(state$focal, mat = FALSE)
  finite_cells <- which(is.finite(focal_values))
  cells <- finite_cells[seq_len(min(8L, length(finite_cells)))]
  resolution <- terra::res(state$focal)
  offsets <- rbind(
    c(0, 0),
    c(0.499, 0), c(-0.499, 0),
    c(0, 0.499), c(0, -0.499),
    c(0.499, 0.499), c(-0.499, 0.499),
    c(0.499, -0.499), c(-0.499, -0.499)
  )

  for (cell in cells) {
    centre <- terra::xyFromCell(state$focal, cell)
    for (k in seq_len(nrow(offsets))) {
      x <- centre[1] + offsets[k, 1] * resolution[1]
      y <- centre[2] + offsets[k, 2] * resolution[2]
      expect_lte(
        exact_metric_sum(x, y, metric, params$radius),
        focal_values[cell] + 1e-8
      )
    }
  }
})

test_that("automatic threshold is a feasible metric radius sum", {
  data <- screening_test_portfolio()
  params <- screening_test_params(radius = 100, cell_size = 100)
  state <- spatialrisk:::initialise_terra_hotspot_state(
    data, "amount", params$radius, params$cell_size, "lon", "lat", 3035
  )
  metric <- convert_crs_df(data, 4326, 3035, "lon", "lat", "x", "y")
  lower_bound <- spatialrisk:::metric_hotspot_lower_bound(
    spatialrisk:::top_n_focals(state$focal, 5), data, params
  )
  full <- spatialrisk:::concentration_hotspot_pair_intersections(
    data, "amount", radius = params$radius, progress = FALSE
  )

  expect_equal(
    lower_bound$concentration,
    exact_metric_sum(lower_bound$x, lower_bound$y, metric, params$radius)
  )
  expect_lte(lower_bound$concentration, full$hotspots$amount_sum)
})

test_that("safe screening retains a pair-intersection optimum", {
  data <- screening_test_portfolio()
  params <- screening_test_params(radius = 100, cell_size = 100)
  state <- spatialrisk:::initialise_terra_hotspot_state(
    data, "amount", params$radius, params$cell_size, "lon", "lat", 3035
  )
  full <- spatialrisk:::concentration_hotspot_pair_intersections(
    data, "amount", radius = params$radius, progress = FALSE
  )
  centre <- convert_crs_df(
    full$hotspots, 4326, 3035, "lon", "lat", "x", "y"
  )
  optimum_cell <- terra::cellFromXY(
    state$raster, cbind(centre$x, centre$y)
  )
  threshold <- spatialrisk:::estimate_hotspot_candidate_threshold(
    state$focal, data, params
  )
  selected <- spatialrisk:::cells_above_threshold_with_values(
    state$focal, threshold
  )
  screened <- concentration_hotspot(
    data,
    value = "amount",
    radius = params$radius,
    cell_size = params$cell_size,
    progress = FALSE
  )

  # The known optimum is pair-derived and includes three positive-weight points
  # assigned to adjacent raster cells.
  expect_gt(length(unique(state$point_cells$cell[data$amount > 0])), 1)
  expect_true(optimum_cell %in% selected$cell)
  expect_equal(screened$hotspots$amount_sum, full$hotspots$amount_sum)
  expect_equal(screened$hotspots$amount_sum, 60)
})

test_that("raster extent retains pair centres outside the point bounding box", {
  a <- 142
  metric <- data.frame(
    x = c(0, a, 1000, 0, 1000),
    y = c(0, a, 0, 1000, 1000),
    amount = c(1, 1, 0, 0, 0)
  )
  data <- convert_crs_df(metric, 3035, 4326, "x", "y", "lon", "lat")
  model <- prepare_spatialrisk(
    data, value = "amount", radius = 200, cell_size = 100
  )
  full <- optimize_hotspot(model, progress = FALSE)
  screened <- model |>
    select_candidates(progress = FALSE) |>
    optimize_hotspot(progress = FALSE)
  centre <- convert_crs_df(
    full$hotspots, 4326, 3035, "lon", "lat", "x", "y"
  )
  extent <- terra::ext(model$state$raster)

  # Both exact pair centres lie just beyond one side of the old half-cell
  # margin. The radius-expanded raster must still contain the selected centre.
  expect_true(centre$x < -50 || centre$y < -50)
  expect_gte(centre$x, extent$xmin)
  expect_gte(centre$y, extent$ymin)
  expect_equal(full$hotspots$amount_sum, 2)
  expect_equal(screened$hotspots$amount_sum, 2)
})

test_that("screened search matches full geometry on small random portfolios", {
  for (seed in 1:8) {
    set.seed(8000 + seed)
    metric <- data.frame(
      x = 4300000 + runif(24, -450, 450),
      y = 3200000 + runif(24, -450, 450),
      amount = sample(0:50, 24, replace = TRUE)
    )
    data <- convert_crs_df(metric, 3035, 4326, "x", "y", "lon", "lat")
    model <- prepare_spatialrisk(
      data, value = "amount", radius = 125, cell_size = 75
    )
    full <- optimize_hotspot(model, progress = FALSE)
    screened <- model |>
      select_candidates(progress = FALSE) |>
      optimize_hotspot(progress = FALSE)

    expect_equal(screened$hotspots$amount_sum, full$hotspots$amount_sum)
    expect_equal(attr(screened, "refinement_methods"), "pair_intersections")
  }
})

test_that("a user threshold disables the safe centre-filter guarantee", {
  old <- options(spatialrisk.profile = TRUE)
  on.exit(options(old), add = TRUE)
  data <- screening_test_portfolio()
  model <- prepare_spatialrisk(
    data, value = "amount", radius = 100, cell_size = 100
  ) |>
    select_candidates(threshold = 0, progress = FALSE)
  out <- optimize_hotspot(model, progress = FALSE)

  expect_false(attr(out, "profile")[[1]]$centre_filter_applied)
})

streaming_reference_result <- function(x, y, value, radius) {
  n <- length(x)
  spatialrisk:::pair_intersection_best_groups_cpp(
    list(seq_len(n)), x, y, value, seq_len(n),
    radius = radius, cell_width = radius,
    selected_cell_ids = integer(), raster_geometry = rep(0, 8),
    filter_centres = FALSE, global_only = TRUE
  )
}

exhaustive_reference_result <- function(x, y, value, radius) {
  spatialrisk:::pair_intersection_best_cpp(
    x, y, x, y, value, radius = radius, cell_width = radius
  )
}

test_that("streaming geometry covers numerical one-disk edge cases", {
  cases <- list(
    one_point = list(x = 0, y = 0, value = 7, radius = 10, optimum = 7),
    below_2r = list(
      x = c(0, 19.999), y = c(0, 0), value = c(2, 5),
      radius = 10, optimum = 7
    ),
    tangent = list(
      x = c(0, 20), y = c(0, 0), value = c(2, 5),
      radius = 10, optimum = 7
    ),
    above_2r = list(
      x = c(0, 20.001), y = c(0, 0), value = c(2, 5),
      radius = 10, optimum = 5
    ),
    coincident_zero = list(
      x = c(0, 0, 30), y = c(0, 0, 0), value = c(0, 9, 2),
      radius = 10, optimum = 9
    ),
    exact_disk_boundary = list(
      x = c(0, 10, -10), y = c(0, 0, 0), value = c(1, 100, 3),
      radius = 10, optimum = 104
    ),
    unequal_weights = list(
      x = c(0, 15, 40), y = c(0, 0, 0), value = c(1e-3, 1e12, 1),
      radius = 10, optimum = 1e12 + 1e-3
    ),
    large_coordinates = list(
      x = 1e9 + c(0, 20), y = -1e9 + c(0, 0), value = c(4, 6),
      radius = 10, optimum = 10
    ),
    equal_maxima = list(
      x = c(0, 100), y = c(0, 0), value = c(11, 11),
      radius = 10, optimum = 11
    )
  )

  for (case in cases) {
    exhaustive <- exhaustive_reference_result(
      case$x, case$y, case$value, case$radius
    )
    streaming <- streaming_reference_result(
      case$x, case$y, case$value, case$radius
    )
    expect_equal(streaming$concentration, exhaustive$concentration)
    expect_equal(streaming$concentration, case$optimum)
  }
})

test_that("threshold equality is retained and strict pruning is respected", {
  raster <- terra::rast(
    terra::ext(0, 300, 0, 100), resolution = 100, crs = "EPSG:3035"
  )
  terra::values(raster) <- c(10, 11, 30)

  equality <- spatialrisk:::cells_above_threshold_with_values(raster, 10)
  nearly_all <- spatialrisk:::cells_above_threshold_with_values(raster, 9)
  nearly_none <- spatialrisk:::cells_above_threshold_with_values(raster, 30)

  expect_true(1L %in% equality$cell)
  expect_equal(sort(equality$focal_value), c(10, 11, 30))
  expect_equal(nrow(nearly_all), 3)
  expect_equal(nearly_none$focal_value, 30)
})

test_that("screening is stable at raster boundaries across cell sizes", {
  cases <- list(boundary = data.frame(
    x = 4300000 + c(0, 100, 200, 300, -500, 800, -500, 800),
    y = 3200000 + c(0, 0, 0, 0, -500, -500, 800, 800),
    amount = c(1, 10, 20, 1, 0, 0, 0, 0)
  ))

  for (metric in cases) {
    data <- convert_crs_df(metric, 3035, 4326, "x", "y", "lon", "lat")
    for (cell_size in c(25, 50, 100, 150, 200)) {
      model <- prepare_spatialrisk(
        data, value = "amount", radius = 200, cell_size = cell_size
      )
      full <- optimize_hotspot(model, progress = FALSE)
      screened <- model |>
        select_candidates(progress = FALSE) |>
        optimize_hotspot(progress = FALSE)
      expect_equal(screened$hotspots$amount_sum, full$hotspots$amount_sum)
    }
  }
})
