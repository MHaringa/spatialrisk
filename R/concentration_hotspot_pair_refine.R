#' @noRd
concentration_hotspot_pair_refine <- function(
    data,
    value,
    top_n = 1,
    radius = 200,
    cell_size = 100,
    grid_precision = 1,
    max_refinement_points = 1000,
    threshold = NULL,
    lon = "lon",
    lat = "lat",
    crs_metric = 3035,
    progress = TRUE
) {
  value <- validate_hotspot_value(value)
  validate_pair_refine_input(data, value, top_n, radius, cell_size,
                             grid_precision, max_refinement_points, lon, lat,
                             crs_metric, progress)
  check_hotspot_threshold(threshold)

  data$ix <- seq_len(nrow(data))
  original <- data
  state <- initialise_terra_hotspot_state(data, value, radius, cell_size,
                                          lon, lat, crs_metric)
  metric <- convert_crs_df(data, 4326, crs_metric, lon, lat, "x", "y")

  pts_lst <- vector("list", top_n)
  conc_lst <- vector("list", top_n)
  output_col <- hotspot_sum_column(value)
  input_threshold <- threshold
  threshold_out <- NA_real_
  refinement_methods <- character(top_n)

  hotspot_progress(progress, "Using continuous hotspot search.")

  for (i in seq_len(top_n)) {
    hotspot_progress(progress, "Hotspot ", i, " of ", top_n,
                     ": terra focal screening.")
    threshold_i <- if (is.null(input_threshold)) {
      estimate_hotspot_candidate_threshold(
        state$focal,
        data,
        list(value = value, cell_size = cell_size, radius = radius,
             crs_metric = crs_metric, lon = lon, lat = lat)
      )
    } else {
      input_threshold
    }
    threshold_out <- threshold_i
    candidate_cells <- cells_above_threshold_with_values(state$focal,
                                                         threshold_i)
    if (nrow(candidate_cells) == 0L) {
      rlang::abort("No candidate cells found above the hotspot threshold.",
                   call = NULL)
    }

    hotspot_progress(progress, "Hotspot ", i, " of ", top_n,
                     ": ", nrow(candidate_cells),
                     " focal candidate cells above lower bound.")

    pair_candidate <- pair_refine_candidate_cells(
      candidate_cells = candidate_cells,
      metric = metric,
      value = value,
      radius = radius,
      cell_size = cell_size,
      max_refinement_points = max_refinement_points
    )
    hotspot_progress(progress, "Hotspot ", i, " of ", top_n,
                     ": largest local refinement subset has ",
                     pair_candidate$local_points, " points.")

    if (!isTRUE(pair_candidate$use_grid)) {
      hotspot_progress(progress, "Hotspot ", i, " of ", top_n,
                       ": pair-intersection refinement over candidate cells.")
      selected <- pair_candidate$selected
      center_ll <- convert_crs_df(data.frame(x = pair_candidate$x,
                                             y = pair_candidate$y),
                                  crs_from = crs_metric, crs_to = 4326,
                                  lon_from = "x", lat_from = "y",
                                  lon_to = lon, lat_to = lat)
      refinement_methods[[i]] <- "pair_intersections"
    } else {
      hotspot_progress(progress, "Hotspot ", i, " of ", top_n,
                       ": a candidate area exceeds max_refinement_points = ",
                       max_refinement_points, "; using grid refinement.")
      # Very dense local subsets can make the pair construction too expensive;
      # the grid fallback preserves a bounded runtime for large portfolios.
      candidate <- refine_terra_hotspot_candidate(
        focal = state$focal,
        data = data,
        value = value,
        cell_size = cell_size,
        grid_precision = grid_precision,
        threshold_cache = empty_hotspot_cache(),
        concentration_cache = empty_hotspot_cache(),
        radius = radius,
        crs_metric = crs_metric,
        lon = lon,
        lat = lat
      )
      center_ll <- candidate$hotspot
      selected <- points_within_radius(data,
                                       lon_center = center_ll[[lon]][1],
                                       lat_center = center_ll[[lat]][1],
                                       lon = lon,
                                       lat = lat,
                                       radius = radius)
      if (!"ix" %in% names(selected)) {
        rlang::abort("Selected points do not contain point indices.",
                     call = NULL)
      }
      refinement_methods[[i]] <- "grid"
    }

    selected_rows <- original[match(selected$ix, original$ix), , drop = FALSE]
    if ("distance_m" %in% names(selected)) {
      selected_rows$distance_m <- selected$distance_m
    }
    selected_rows$id <- i
    selected_rows[[output_col]] <- sum(selected_rows[[value]])

    hotspot <- data.frame(
      lon = center_ll[[lon]][1],
      lat = center_ll[[lat]][1],
      value_sum = sum(selected_rows[[value]]),
      id = i
    )
    names(hotspot)[1:2] <- c(lon, lat)
    names(hotspot)[names(hotspot) == "value_sum"] <- output_col

    pts_lst[[i]] <- selected_rows
    conc_lst[[i]] <- hotspot

    hotspot_progress(progress, "Hotspot ", i, " of ", top_n,
                     ": selected concentration ", hotspot[[output_col]][1],
                     ".")

    if (top_n > 1 && i < top_n) {
      # Removing selected contributors before the next iteration gives
      # non-overlapping hotspot assignments, matching the reporting use case.
      data <- data[!data$ix %in% selected$ix, , drop = FALSE]
      metric <- metric[!metric$ix %in% selected$ix, , drop = FALSE]

      if (nrow(data) == 0L) {
        rlang::abort("Need more rows", call = NULL)
      }

      state$spatvctr <- state$spatvctr[
        !state$spatvctr$ix %in% selected$ix,
      ]
      cells <- map_points_to_cells(selected_rows, state$focal, lon, lat, 4326,
                                   crs_metric)
      extent <- terra::ext(state$raster, cells)
      state$rasterized <- update_rasterize(state$rasterized, extent,
                                           state$spatvctr, value)
      state$focal <- update_focal(state$focal, state$rasterized, extent,
                                  state$moving_window)
    }
  }

  out <- new_hotspot_object(
    hotspots = do.call(rbind, c(conc_lst, make.row.names = FALSE)),
    contributing_points = do.call(rbind, pts_lst),
    radius = radius,
    rasterized = state$rasterized,
    focal = state$focal,
    threshold = threshold_out,
    value = value,
    lon = lon,
    lat = lat,
    crs_metric = crs_metric
  )
  attr(out, "method") <- "continuous"
  attr(out, "refinement_methods") <- refinement_methods
  out
}

pair_refine_candidate_cells <- function(candidate_cells, metric, value, radius,
                                        cell_size, max_refinement_points) {
  best_concentration <- -Inf
  best_x <- NA_real_
  best_y <- NA_real_
  best_selected <- NULL
  max_local_points <- 0L

  for (j in seq_len(nrow(candidate_cells))) {
    local_ix <- local_pair_refine_subset(metric, candidate_cells$x[j],
                                         candidate_cells$y[j], radius,
                                         cell_size)
    max_local_points <- max(max_local_points, length(local_ix))
    if (length(local_ix) == 0L) {
      next
    }

    if (length(local_ix) > max_refinement_points) {
      return(list(use_grid = TRUE, local_points = length(local_ix)))
    }

    local_metric <- metric[local_ix, , drop = FALSE]
    # In the continuous fixed-radius problem, an optimum can occur at a point
    # location or at one of the two circle centres induced by a pair of points.
    best_local <- pair_intersection_best_cpp(
      x_ref = local_metric$x,
      y_ref = local_metric$y,
      value_ref = local_metric[[value]],
      ix_ref = local_metric$ix,
      radius = radius,
      cell_width = radius
    )

    selected <- indexed_points_in_radius_cpp(
      x_center = best_local$x[1],
      y_center = best_local$y[1],
      x_ref = metric$x,
      y_ref = metric$y,
      value_ref = metric[[value]],
      ix_ref = metric$ix,
      radius = radius,
      cell_width = radius
    )

    full_concentration <- sum(selected$value)
    if (full_concentration > best_concentration) {
      best_concentration <- full_concentration
      best_x <- best_local$x[1]
      best_y <- best_local$y[1]
      best_selected <- selected
    }
  }

  if (is.null(best_selected) || nrow(best_selected) == 0L) {
    rlang::abort("No points found inside selected hotspot radius.",
                 call = NULL)
  }

  list(
    use_grid = FALSE,
    x = best_x,
    y = best_y,
    concentration = best_concentration,
    local_points = max_local_points,
    selected = best_selected
  )
}

#' @noRd
concentration_hotspot_pair_intersections <- function(
    data,
    value,
    top_n = 1,
    radius = 200,
    lon = "lon",
    lat = "lat",
    crs_metric = 3035,
    progress = TRUE
) {
  value <- validate_hotspot_value(value)
  validate_indexed_hotspot_input(data, value, top_n, radius, lon, lat,
                                 crs_metric, progress, radius)

  original <- data
  original$ix <- seq_len(nrow(original))
  metric <- convert_crs_df(original, 4326, crs_metric, lon, lat, "x", "y")

  pts_lst <- vector("list", top_n)
  conc_lst <- vector("list", top_n)
  output_col <- hotspot_sum_column(value)

  for (i in seq_len(top_n)) {
    best <- pair_intersection_best_cpp(
      x_ref = metric$x,
      y_ref = metric$y,
      value_ref = metric[[value]],
      ix_ref = metric$ix,
      radius = radius,
      cell_width = radius
    )
    selected <- indexed_points_in_radius_cpp(best$x[1], best$y[1], metric$x,
                                             metric$y, metric[[value]],
                                             metric$ix, radius, radius)
    selected_rows <- original[match(selected$ix, original$ix), , drop = FALSE]
    selected_rows$distance_m <- selected$distance_m
    selected_rows$id <- i
    selected_rows[[output_col]] <- sum(selected_rows[[value]])

    center_ll <- convert_crs_df(data.frame(x = best$x[1], y = best$y[1]),
                                crs_from = crs_metric, crs_to = 4326,
                                lon_from = "x", lat_from = "y",
                                lon_to = lon, lat_to = lat)
    hotspot <- data.frame(
      lon = center_ll[[lon]][1],
      lat = center_ll[[lat]][1],
      value_sum = sum(selected_rows[[value]]),
      id = i
    )
    names(hotspot)[1:2] <- c(lon, lat)
    names(hotspot)[names(hotspot) == "value_sum"] <- output_col
    pts_lst[[i]] <- selected_rows
    conc_lst[[i]] <- hotspot

    if (top_n > 1 && progress) {
      cat("\rFinished", i, "of", top_n)
    }
    if (top_n > 1 && i < top_n) {
      metric <- metric[!metric$ix %in% selected$ix, , drop = FALSE]
      if (nrow(metric) == 0L) {
        rlang::abort("Need more rows", call = NULL)
      }
    }
  }

  out <- new_hotspot_object(
    hotspots = do.call(rbind, c(conc_lst, make.row.names = FALSE)),
    contributing_points = do.call(rbind, pts_lst),
    radius = radius,
    rasterized = NULL,
    focal = NULL,
    threshold = NA_real_,
    value = value,
    lon = lon,
    lat = lat,
    crs_metric = crs_metric
  )
  attr(out, "method") <- "continuous_exact"
  out
}

validate_pair_refine_input <- function(data, value, top_n, radius, cell_size,
                                       grid_precision, max_refinement_points,
                                       lon, lat, crs_metric, progress) {
  check_input(data, value, top_n, radius, cell_size, grid_precision)
  check_hotspot_columns(data, value, lon, lat)
  check_hotspot_crs_metric(crs_metric)
  check_hotspot_progress(progress)
  if (!is.numeric(max_refinement_points) ||
      length(max_refinement_points) != 1L ||
      is.na(max_refinement_points) || !is.finite(max_refinement_points) ||
      round(max_refinement_points) != max_refinement_points ||
      max_refinement_points <= 0) {
    stop("`max_refinement_points` must be a single positive integer.",
         call. = FALSE)
  }
  invisible(NULL)
}

terra_screening_center <- function(focal) {
  top <- top_n_focals(focal, n = 1)
  data.frame(x = top$x[1], y = top$y[1], concentration = top$val[1])
}

local_pair_refine_subset <- function(metric, x_center, y_center, radius,
                                     cell_size) {
  search_radius <- radius + max(radius, 2 * sqrt(2) * cell_size)
  dx <- metric$x - x_center
  dy <- metric$y - y_center
  which(dx * dx + dy * dy <= search_radius * search_radius)
}
