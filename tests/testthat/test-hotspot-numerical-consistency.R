test_that("sweep preselection includes tolerance-expanded boundary records", {
  for (radius in c(1, 10, 200)) {
    for (angle in c(0, 0.3, 1.7, pi, 5.8)) {
      for (offset in c(0, 4300000)) {
        delta <- 4.5e-10 * radius
        xy <- rbind(c(radius, 0), c(0, radius),
                    rep(-(radius + delta) / sqrt(2), 2))
        rotation <- matrix(c(cos(angle), -sin(angle),
                             sin(angle), cos(angle)), 2)
        xy <- xy %*% rotation + offset
        full <- spatialrisk:::pair_intersection_best_cpp(
          xy[, 1], xy[, 2], xy[, 1], xy[, 2], c(3, 3, 7), radius, radius
        )
        sweep <- spatialrisk:::pair_intersection_best_groups_cpp(
          list(1:3), xy[, 1], xy[, 2], c(3, 3, 7), 1:3, radius, radius,
          integer(), rep(0, 8), FALSE, global_only = TRUE
        )
        expect_equal(sweep$concentration, full$concentration,
                     info = paste(radius, angle, offset))
        expect_equal(sum(sweep$selected[[1]]$value), sweep$concentration)
        if (radius == 10 && angle == 0 && offset == 0) {
          expect_equal(sweep$concentration, 13)
        }
      }
    }
  }
})

test_that("nearby angular events retain their own geometric candidate centres", {
  for (seed in 1:12) {
    set.seed(seed)
    angles <- c(0, pi / 2, pi + runif(6, -1e-12, 1e-12))
    x <- 100 * cos(angles)
    y <- 100 * sin(angles)
    weights <- sample(c(0, 1, 5, 100), length(x), replace = TRUE)
    full <- spatialrisk:::pair_intersection_best_cpp(x, y, x, y, weights, 100, 100)
    sweep <- spatialrisk:::pair_intersection_best_groups_cpp(
      list(seq_along(x)), x, y, weights, seq_along(x), 100, 100,
      integer(), rep(0, 8), FALSE, global_only = TRUE
    )
    expect_equal(sweep$concentration, full$concentration)
  }
})

test_that("sweep includes scoring neighbours just beyond the generating pair radius", {
  x <- c(0, 20, 20 + 4.5e-9)
  sweep <- spatialrisk:::pair_intersection_best_groups_cpp(
    list(1:2), x, c(0, 0, 0), c(2, 3, 7), 1:3, 10, 10,
    integer(), rep(0, 8), FALSE, global_only = TRUE
  )
  expect_equal(sweep$concentration, 12)
  expect_setequal(sweep$selected[[1]]$ix, 1:3)
})

test_that("sweep confirmation handles signed and unequal weight scales", {
  x <- c(0, 10, 20, 25, 30)
  y <- c(0, 8, 0, -5, 0)
  for (weights in list(c(3, -20, 8, 1, 6), c(1e18, 1, 10, 1e17, 0))) {
    full <- spatialrisk:::pair_intersection_best_cpp(x, y, x, y, weights, 10, 10)
    sweep <- spatialrisk:::pair_intersection_best_groups_cpp(
      list(1:5), x, y, weights, 1:5, 10, 10,
      integer(), rep(0, 8), FALSE, global_only = TRUE
    )
    expect_equal(sweep$concentration, full$concentration)
  }
})

test_that("projected grid scoring includes closed boundaries and all active records", {
  x <- c(-10, 0, 10, 0, 0, 10 + 4.5e-9, 10 + 1e-4)
  y <- c(0, -10, 0, 10, 0, 0, 0)
  value <- c(1, 2, 3, 4, 20, 7, 1)
  grid <- spatialrisk:::indexed_grid_best_cpp(1L, 5, 5, x, y, value, 10, 3L, 10)
  expect_equal(grid$x, 0)
  expect_equal(grid$y, 0)
  expect_equal(grid$concentration, 37)
  selected <- spatialrisk:::indexed_points_in_radius_cpp(
    grid$x, grid$y, x, y, value, seq_along(x), 10, 10
  )
  expect_setequal(selected$ix, 1:6)
  expect_equal(sum(selected$value), grid$concentration)
})

test_that("radius tolerance also reaches across an index-cell boundary", {
  x <- c(-10 + 1e-9, 10 + 4.5e-9)
  selected <- spatialrisk:::indexed_points_in_radius_cpp(
    0, 0, x, c(0, 0), c(2, 7), 1:2, 10, 10
  )
  score <- spatialrisk:::indexed_concentration_best_cpp(
    0, 0, x, c(0, 0), c(2, 7), 1:2, 10, 10
  )
  expect_setequal(selected$ix, 1:2)
  expect_equal(score$concentration, 9)
})

test_that("grid and forced fallback reconcile in the requested projected CRS", {
  for (crs in c(3035, 3857)) {
    xy <- data.frame(x = c(0, 100, 240, 390, 1000, 1100, 1240) + 4300000,
                     y = rep(3200000, 7), amount = c(2, 3, 7, 11, 5, 13, 17))
    portfolio <- convert_crs_df(xy, crs, 4326, "x", "y", "longitude", "latitude")
    metric <- convert_crs_df(portfolio, 4326, crs, "longitude", "latitude", "x", "y")
    results <- lapply(c("grid", "continuous"), function(method) {
      concentration_hotspot(
        portfolio, value = "amount", radius = 200, n_hotspots = 2,
        method = method, max_refinement_points = 1, grid_spacing = 10,
        crs_metric = crs, lon = "longitude", lat = "latitude", progress = FALSE
      )
    })
    expect_equal(results[[1]]$hotspots, results[[2]]$hotspots)
    expect_equal(attr(results[[2]], "refinement_methods"), rep("grid", 2))
    for (result in results) {
      centres <- convert_crs_df(result$hotspots, 4326, crs,
                               "longitude", "latitude", "x", "y")
      active <- seq_len(nrow(portfolio))
      for (i in 1:2) {
        distance <- sqrt((metric$x - centres$x[i])^2 +
                         (metric$y - centres$y[i])^2)
        covered <- active[distance[active] <= 200 + 1e-5]
        rows <- result$contributing_points[result$contributing_points$id == i, ]
        expect_setequal(rows$data_row, covered)
        expect_equal(sum(rows$amount), result$hotspots$amount_sum[i])
        expect_equal(rows$distance_m, distance[rows$data_row], tolerance = 1e-5)
        active <- setdiff(active, covered)
      }
    }
  }
})
