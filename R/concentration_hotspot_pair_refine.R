#' @noRd
concentration_hotspot_pair_refine <- function(
    data,
    value,
    top_n = 1,
    radius = 200,
    cell_size = 100,
    grid_precision = 1,
    max_refinement_points = 1000,
    lon = "lon",
    lat = "lat",
    crs_metric = 3035,
    progress = TRUE
) {
  value <- validate_hotspot_value(value)
  validate_pair_refine_input(data, value, top_n, radius, cell_size,
                             grid_precision, max_refinement_points, lon, lat,
                             crs_metric, progress)

  data$ix <- seq_len(nrow(data))
  original <- data
  state <- initialise_terra_hotspot_state(data, value, radius, cell_size,
                                          lon, lat, crs_metric)
  metric <- convert_crs_df(data, 4326, crs_metric, lon, lat, "x", "y")

  pts_lst <- vector("list", top_n)
  conc_lst <- vector("list", top_n)
  output_col <- hotspot_sum_column(value)
  threshold <- NA_real_
  refinement_methods <- character(top_n)

  hotspot_progress(progress, "Using continuous hotspot search.")

  for (i in seq_len(top_n)) {
    hotspot_progress(progress, "Hotspot ", i, " of ", top_n,
                     ": terra focal screening.")
    approximate <- terra_screening_center(state$focal)
    threshold <- approximate$concentration[1]

    local_ix <- local_pair_refine_subset(metric, approximate$x[1],
                                         approximate$y[1], radius,
                                         cell_size)
    if (length(local_ix) == 0L) {
      rlang::abort("No points found in local pair-refinement search area.",
                   call = NULL)
    }

    local_metric <- metric[local_ix, , drop = FALSE]
    hotspot_progress(progress, "Hotspot ", i, " of ", top_n,
                     ": local refinement subset has ", nrow(local_metric),
                     " points.")

    if (nrow(local_metric) <= max_refinement_points) {
      hotspot_progress(progress, "Hotspot ", i, " of ", top_n,
                       ": pair-intersection refinement.")
      best <- pair_intersection_best_cpp(
        x_ref = local_metric$x,
        y_ref = local_metric$y,
        value_ref = local_metric[[value]],
        ix_ref = local_metric$ix,
        radius = radius,
        cell_width = radius
      )

      selected <- indexed_points_in_radius_cpp(
        x_center = best$x[1],
        y_center = best$y[1],
        x_ref = metric$x,
        y_ref = metric$y,
        value_ref = metric[[value]],
        ix_ref = metric$ix,
        radius = radius,
        cell_width = radius
      )

      if (nrow(selected) == 0L) {
        rlang::abort("No points found inside selected hotspot radius.",
                     call = NULL)
      }

      center_ll <- convert_crs_df(data.frame(x = best$x[1], y = best$y[1]),
                                  crs_from = crs_metric, crs_to = 4326,
                                  lon_from = "x", lat_from = "y",
                                  lon_to = lon, lat_to = lat)
      refinement_methods[[i]] <- "pair_intersections"
    } else {
      hotspot_progress(progress, "Hotspot ", i, " of ", top_n,
                       ": local subset exceeds max_refinement_points = ",
                       max_refinement_points, "; using grid refinement.")
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
    threshold = threshold,
    value = value,
    lon = lon,
    lat = lat,
    crs_metric = crs_metric
  )
  attr(out, "method") <- "continuous"
  attr(out, "refinement_methods") <- refinement_methods
  out
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
