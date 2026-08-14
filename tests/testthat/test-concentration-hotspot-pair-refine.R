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
    grid_spacing = 5,
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
    grid_spacing = 5,
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

test_that("continuous n_hotspots removes selected points", {
  x <- Groningen[1:200, c("lon", "lat", "amount")]

  out <- concentration_hotspot(
    x,
    value = "amount",
    n_hotspots = 2,
    radius = 200,
    cell_size = 100,
    progress = FALSE
  )

  expect_equal(nrow(out$hotspots), 2)
  first_row <- out$contributing_points$data_row[out$contributing_points$id == 1]
  second_row <- out$contributing_points$data_row[out$contributing_points$id == 2]
  expect_length(intersect(first_row, second_row), 0)
})

test_that("continuous n_hotspots concentrations are non-increasing", {
  x <- Groningen[1:300, c("lon", "lat", "amount")]

  out <- concentration_hotspot(
    x,
    value = "amount",
    n_hotspots = 4,
    radius = 200,
    cell_size = 100,
    progress = FALSE
  )

  expect_true(all(diff(out$hotspots$amount_sum) <= 0))
})

test_that("continuous hotspot result is stable across cell sizes", {
  x <- Groningen[1:300, c("lon", "lat", "amount")]
  cell_sizes <- c(25, 50, 100, 150, 200)

  out <- lapply(cell_sizes, function(cell_size) {
    concentration_hotspot(
      x,
      value = "amount",
      radius = 200,
      cell_size = cell_size,
      progress = FALSE
    )
  })

  amount_sum <- vapply(out, function(result) result$hotspots$amount_sum,
                       numeric(1))
  contributing_rows <- lapply(out, function(result) {
    sort(result$contributing_points$data_row)
  })

  expect_equal(amount_sum, rep(amount_sum[[1]], length(amount_sum)))
  expect_equal(contributing_rows, rep(contributing_rows[1], length(cell_sizes)))
})

test_that("continuous pair refinement cache reuses unaffected cells", {
  x <- Groningen[1:120, c("lon", "lat", "amount")]
  x$ix <- seq_len(nrow(x))
  state <- spatialrisk:::initialise_terra_hotspot_state(
    x,
    value = "amount",
    radius = 200,
    cell_size = 100,
    lon = "lon",
    lat = "lat",
    crs_metric = 3035
  )
  metric <- convert_crs_df(x, 4326, 3035, "lon", "lat", "x", "y")
  threshold <- spatialrisk:::estimate_hotspot_candidate_threshold(
    state$focal,
    x,
    list(value = "amount", cell_size = 100, radius = 200,
         crs_metric = 3035, lon = "lon", lat = "lat")
  )
  candidate_cells <- spatialrisk:::cells_above_threshold_with_values(
    state$focal,
    threshold
  )

  first <- spatialrisk:::pair_refine_candidate_cells(
    candidate_cells = candidate_cells,
    metric = metric,
    state = state,
    value = "amount",
    radius = 200,
    cell_size = 100,
    max_refinement_points = 1000,
    cache = list()
  )
  second <- spatialrisk:::pair_refine_candidate_cells(
    candidate_cells = candidate_cells,
    metric = metric,
    state = state,
    value = "amount",
    radius = 200,
    cell_size = 100,
    max_refinement_points = 1000,
    cache = first$cache
  )

  expect_gt(first$cache_misses, 0)
  expect_equal(second$cache_hits, nrow(candidate_cells))
  expect_equal(second$cache_misses, 0)
  expect_equal(second$concentration, first$concentration)

  invalidated <- spatialrisk:::invalidate_pair_refine_cache(
    first$cache,
    removed_ix = first$selected$ix[1],
    affected_cells = integer()
  )
  expect_lt(length(invalidated), length(first$cache))
})

test_that("prepared raster cells recover the same local points as a full scan", {
  x <- Groningen[1:200, c("lon", "lat", "amount")]
  x$ix <- seq_len(nrow(x))
  state <- spatialrisk:::initialise_terra_hotspot_state(
    x, "amount", radius = 200, cell_size = 100,
    lon = "lon", lat = "lat", crs_metric = 3035
  )
  metric <- convert_crs_df(x, 4326, 3035, "lon", "lat", "x", "y")
  candidate <- spatialrisk:::top_n_focals(state$focal, n = 1)
  cell <- terra::cellFromXY(
    state$raster,
    matrix(c(candidate$x[1], candidate$y[1]), ncol = 2)
  )

  indexed <- spatialrisk:::local_pair_refine_subset(
    metric, candidate$x[1], candidate$y[1], radius = 200, cell_size = 100,
    state = state, cell = cell
  )
  scanned <- spatialrisk:::local_pair_refine_subset(
    metric, candidate$x[1], candidate$y[1], radius = 200, cell_size = 100
  )
  expect_equal(sort(metric$ix[indexed]), sort(metric$ix[scanned]))

  active <- metric[-indexed[1], , drop = FALSE]
  indexed_active <- spatialrisk:::local_pair_refine_subset(
    active, candidate$x[1], candidate$y[1], radius = 200, cell_size = 100,
    state = state, cell = cell
  )
  expect_false(metric$ix[indexed[1]] %in% active$ix[indexed_active])
})

test_that("batched pair refinement matches separate candidate-cell calls", {
  x <- Groningen[1:160, c("lon", "lat", "amount")]
  x$ix <- seq_len(nrow(x))
  state <- spatialrisk:::initialise_terra_hotspot_state(
    x, "amount", radius = 200, cell_size = 100,
    lon = "lon", lat = "lat", crs_metric = 3035
  )
  metric <- convert_crs_df(x, 4326, 3035, "lon", "lat", "x", "y")
  candidate_cells <- spatialrisk:::cells_above_threshold_with_values(
    state$focal,
    max(terra::values(state$focal), na.rm = TRUE) * 0.9
  )
  groups <- lapply(seq_len(nrow(candidate_cells)), function(i) {
    spatialrisk:::local_pair_refine_subset(
      metric, candidate_cells$x[i], candidate_cells$y[i],
      radius = 200, cell_size = 100, state = state,
      cell = candidate_cells$cell[i]
    )
  })
  groups <- groups[lengths(groups) > 0]

  batched <- spatialrisk:::pair_intersection_best_groups_cpp(
    lapply(groups, as.integer), metric$x, metric$y, metric$amount, metric$ix,
    radius = 200, cell_width = 200,
    selected_cell_ids = integer(), raster_geometry = rep(0, 8),
    filter_centres = FALSE
  )
  separate <- lapply(groups, function(rows) {
    spatialrisk:::pair_intersection_best_cpp(
      metric$x[rows], metric$y[rows], metric$x, metric$y, metric$amount,
      radius = 200, cell_width = 200
    )
  })

  expect_equal(batched$x, vapply(separate, `[[`, numeric(1), "x"))
  expect_equal(batched$y, vapply(separate, `[[`, numeric(1), "y"))
  expect_equal(
    batched$concentration,
    vapply(separate, `[[`, numeric(1), "concentration")
  )
  expect_gt(
    batched$diagnostics$raw_point_pairs,
    batched$diagnostics$unique_point_pairs
  )
  expect_gt(
    batched$diagnostics$raw_observed_centres,
    batched$diagnostics$unique_observed_centres
  )
  expect_equal(
    batched$diagnostics$evaluated_centres,
    batched$diagnostics$unique_observed_centres +
      2 * batched$diagnostics$unique_point_pairs
  )

  filtered <- spatialrisk:::pair_intersection_best_groups_cpp(
    lapply(groups, as.integer), metric$x, metric$y, metric$amount, metric$ix,
    radius = 200, cell_width = 200,
    selected_cell_ids = as.integer(candidate_cells$cell),
    raster_geometry = spatialrisk:::hotspot_raster_geometry_vector(state),
    filter_centres = TRUE
  )
  expect_true(filtered$diagnostics$centre_filter_applied)
  expect_lt(filtered$diagnostics$evaluated_centres,
            batched$diagnostics$evaluated_centres)
  expect_equal(max(filtered$concentration), max(batched$concentration))
})

test_that("continuous falls back to grid refinement above point limit", {
  x <- Groningen[1:200, c("lon", "lat", "amount")]

  out <- concentration_hotspot(
    x,
    value = "amount",
    radius = 200,
    cell_size = 100,
    grid_spacing = 5,
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
