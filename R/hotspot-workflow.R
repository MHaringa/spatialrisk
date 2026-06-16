#' Prepare fixed-radius concentration hotspot analysis
#'
#' @description
#' `prepare_spatialrisk()`, `select_candidates()`, and `optimize_hotspot()`
#' expose the main steps used by \code{\link{concentration_hotspot}}. They are useful
#' when the intermediate search state needs to be inspected or when the same
#' prepared portfolio is used in more than one hotspot search strategy.
#'
#' @param data A data.frame containing point-level exposures. Must include
#'   longitude, latitude, and the value of interest.
#' @param value A string giving the numeric column in `data` to aggregate
#'   within each radius.
#' @param radius Numeric. Radius of the circle in meters.
#' @param cell_size Numeric. Size of the raster cells used for the initial
#'   screening raster.
#' @param lon A string giving the longitude column in `data`.
#' @param lat A string giving the latitude column in `data`.
#' @param crs_metric Numeric. EPSG code for a projected CRS with meter units.
#'   The default `3035` is ETRS89 / LAEA Europe.
#' @param x A prepared spatial-risk workflow object returned by
#'   `prepare_spatialrisk()` or `select_candidates()`.
#' @param grid_precision Numeric. Approximate spacing in meters used for
#'   grid-based refinement.
#' @param max_refinement_points Positive integer. Maximum number of local points
#'   used for pair-intersection refinement before falling back to grid
#'   refinement.
#' @param method Hotspot search strategy. `"continuous"` is the default and
#'   searches for centres that may lie between observed points. `"observed"`
#'   searches only observed point locations. `"grid"` uses the grid-refinement
#'   workflow.
#' @param threshold Optional numeric lower bound for candidate focal cells.
#'   If `NULL`, the lower bound is estimated using the same preliminary
#'   refinement step as `concentration_hotspot()`.
#' @param top_n Positive integer. Number of non-overlapping hotspots to return.
#' @param progress Logical. Whether to print progress messages.
#' @param type Plot type. `"auto"` shows the prepared raster before candidate
#'   selection and selected focal candidate cells afterwards.
#' @param ... Additional arguments passed to `mapview::mapview()`.
#'
#' @return
#' `prepare_spatialrisk()` and `select_candidates()` return an object of class
#' `spatialrisk_hotspot_workflow`. `optimize_hotspot()` returns the same
#' `hotspot` object structure as \code{\link{concentration_hotspot}}.
#'
#' @details
#' The three-step interface decomposes the hotspot workflow without replacing
#' `concentration_hotspot()`. The wrapper remains the simplest public function
#' for normal use, while the decomposed functions make the intermediate
#' candidate selection visible.
#'
#' In `select_candidates()`, `threshold = NULL` estimates a lower bound by
#' taking the highest focal raster cells, refining those cells on a small local
#' grid, and using the best refined value as the candidate-cell threshold. The
#' selected candidates are focal cells whose moving-window sum is at least this
#' lower bound. These candidates describe the current search state. When
#' `optimize_hotspot(top_n > 1)` or `concentration_hotspot(top_n > 1)` is used,
#' the points in the selected hotspot are removed and the candidate-selection
#' logic is run again for the next hotspot. Therefore the number of candidate
#' cells shown by `select_candidates()` for the first iteration does not limit
#' the number of hotspots returned by `top_n`.
#'
#' @examples
#' portfolio <- Groningen[1:200, c("lon", "lat", "amount")]
#'
#' model <- prepare_spatialrisk(portfolio, value = "amount", radius = 200,
#'                              cell_size = 100)
#' model <- select_candidates(model, progress = FALSE)
#' hotspot <- optimize_hotspot(model, top_n = 1, progress = FALSE)
#'
#' hotspot$hotspots
#'
#' @author Martin Haringa
#'
#' @export
prepare_spatialrisk <- function(data, value, radius = 200, cell_size = 100,
                                lon = "lon", lat = "lat",
                                crs_metric = 3035) {
  value <- validate_hotspot_value(value)
  data <- as.data.frame(data)
  check_hotspot_columns(data, value, lon, lat)
  check_hotspot_crs_metric(crs_metric)
  check_hotspot_radius(radius)
  check_hotspot_cell_size(cell_size)

  original <- data
  original$ix <- seq_len(nrow(original))
  # Metric coordinates are stored once so all later distance calculations use
  # Euclidean geometry in the chosen projected CRS.
  metric <- convert_crs_df(original, 4326, crs_metric, lon, lat, "x", "y")
  # The prepared object keeps both the rasterised portfolio and the metric
  # points, making the paper workflow reproducible step by step.
  state <- initialise_prepare_hotspot_state(original, value, radius, cell_size,
                                            lon, lat, crs_metric)

  out <- list(
    data = original,
    metric = metric,
    params = list(
      value = value,
      radius = radius,
      lon = lon,
      lat = lat,
      crs_metric = crs_metric,
      cell_size = cell_size
    ),
    candidates = NULL,
    state = state
  )
  class(out) <- c("spatialrisk_hotspot_workflow", class(out))
  out
}

#' @rdname prepare_spatialrisk
#' @export
select_candidates <- function(x, grid_precision = 1,
                              max_refinement_points = 1000,
                              method = c("continuous", "grid", "observed"),
                              threshold = NULL, progress = TRUE) {
  UseMethod("select_candidates")
}

#' @export
select_candidates.spatialrisk_hotspot_workflow <- function(
    x,
    grid_precision = 1,
    max_refinement_points = 1000,
    method = c("continuous", "grid", "observed"),
    threshold = NULL,
    progress = TRUE
) {
  method <- match.arg(method)
  p <- complete_hotspot_workflow_params(x$params)
  check_hotspot_threshold(threshold)
  validate_hotspot_search_settings(
    data = x$data,
    value = p$value,
    top_n = 1,
    radius = p$radius,
    cell_size = p$cell_size,
    grid_precision = grid_precision,
    max_refinement_points = max_refinement_points,
    lon = p$lon,
    lat = p$lat,
    crs_metric = p$crs_metric,
    progress = progress
  )

  x$params$grid_precision <- grid_precision
  x$params$max_refinement_points <- max_refinement_points
  x$params$method <- method
  x$params$threshold <- threshold

  if (method == "observed") {
    # Observed-point candidates form a fast deterministic baseline; they do not
    # guarantee the continuous optimum when the best centre lies between points.
    hotspot_progress(progress, "Selecting observed point candidates.")
    x$candidates <- data.frame(
      data_row = x$data$ix,
      x = x$metric$x,
      y = x$metric$y
    )
    x$state <- NULL
    return(x)
  }

  hotspot_progress(progress, "Selecting terra focal candidate area.")
  state <- x$state
  if (is.null(state)) {
    state <- initialise_prepare_hotspot_state(
      x$data, p$value, p$radius, p$cell_size, p$lon, p$lat, p$crs_metric
    )
  }
  if (is.null(state$focal)) {
    stop("Candidate selection requires a focal raster. Try a smaller ",
         "`cell_size`, a larger dataset extent, or `method = \"observed\"`.",
         call. = FALSE)
  }
  if (is.null(threshold)) {
    threshold <- estimate_hotspot_candidate_threshold(state$focal, x$data, p)
  }
  # Candidate cells are an interpretable inspection layer: their focal value is
  # high enough to warrant local refinement in the next optimisation step.
  candidate_cells <- cells_above_threshold_with_values(state$focal, threshold)
  approximate <- terra_screening_center(state$focal)
  local_ix <- local_pair_refine_subset(
    x$metric, approximate$x[1], approximate$y[1], p$radius, p$cell_size
  )

  x$state <- state
  x$candidates <- list(
    approximate_center = approximate,
    threshold = threshold,
    cells = candidate_cells,
    local_data_rows = x$metric$ix[local_ix],
    local_points = length(local_ix),
    refinement = if (method == "continuous" &&
                     length(local_ix) <= max_refinement_points) {
      "pair_intersections"
    } else if (method == "continuous") {
      "grid"
    } else {
      "grid"
    }
  )
  x
}

#' @rdname prepare_spatialrisk
#' @export
optimize_hotspot <- function(x, top_n = 1, progress = TRUE) {
  UseMethod("optimize_hotspot")
}

#' @export
optimize_hotspot.spatialrisk_hotspot_workflow <- function(x, top_n = 1,
                                                          progress = TRUE) {
  p <- complete_hotspot_workflow_params(x$params)
  validate_hotspot_search_settings(
    data = x$data,
    value = p$value,
    top_n = top_n,
    radius = p$radius,
    cell_size = p$cell_size,
    grid_precision = p$grid_precision,
    max_refinement_points = p$max_refinement_points,
    lon = p$lon,
    lat = p$lat,
    crs_metric = p$crs_metric,
    progress = progress
  )

  if (is.null(x$candidates)) {
    # If optimisation is called directly after preparation, select candidates
    # with the same defaults as the wrapper before running the final search.
    x <- select_candidates(
      x,
      grid_precision = p$grid_precision,
      max_refinement_points = p$max_refinement_points,
      method = p$method,
      threshold = p$threshold,
      progress = progress
    )
  }

  data <- x$data
  data$ix <- NULL

  if (p$method == "observed") {
    hotspot_progress(progress, "Using observed-points hotspot search.")
    return(concentration_hotspot_indexed(
      data = data,
      value = p$value,
      top_n = top_n,
      radius = p$radius,
      lon = p$lon,
      lat = p$lat,
      crs_metric = p$crs_metric,
      print_progress = progress,
      cell_size = p$radius
    ))
  }

  if (p$method == "grid") {
    hotspot_progress(progress, "Using grid-refinement hotspot search.")
    return(concentration_hotspot_terra(
      data = data,
      value = p$value,
      top_n = top_n,
      radius = p$radius,
      cell_size = p$cell_size,
      grid_precision = p$grid_precision,
      lon = p$lon,
      lat = p$lat,
      crs_metric = p$crs_metric,
      progress = progress
    ))
  }

  concentration_hotspot_pair_refine(
    data = data,
    value = p$value,
    top_n = top_n,
    radius = p$radius,
    cell_size = p$cell_size,
    grid_precision = p$grid_precision,
    max_refinement_points = p$max_refinement_points,
    threshold = p$threshold,
    lon = p$lon,
    lat = p$lat,
    crs_metric = p$crs_metric,
    progress = progress
  )
}

#' @export
print.spatialrisk_hotspot_workflow <- function(x, ...) {
  p <- complete_hotspot_workflow_params(x$params)
  cat("<spatialrisk_hotspot_workflow>\n")
  cat("Rows:", nrow(x$data), "\n")
  cat("Value:", p$value, "\n")
  cat("Radius:", p$radius, "meters\n")
  cat("Method:", p$method, "\n")
  if (!is.null(x$candidates)) {
    if (is.list(x$candidates)) {
      cat("Candidate cells:", nrow(x$candidates$cells), "\n")
      cat("Candidate threshold:", x$candidates$threshold, "\n")
      cat("Candidate local points:", x$candidates$local_points, "\n")
      cat("Refinement:", x$candidates$refinement, "\n")
    } else {
      cat("Candidate centres:", nrow(x$candidates), "\n")
    }
  }
  invisible(x)
}

#' @rdname prepare_spatialrisk
#' @method plot spatialrisk_hotspot_workflow
#' @export
plot.spatialrisk_hotspot_workflow <- function(x, type = c("auto", "raster",
                                                          "candidates"),
                                              ...) {
  type <- match.arg(type)
  if (type == "auto") {
    type <- if (is.null(x$candidates)) "raster" else "candidates"
  }

  if (type == "candidates" && is.null(x$candidates)) {
    stop("Run `select_candidates()` before plotting candidate cells.",
         call. = FALSE)
  }

  if (!requireNamespace("mapview", quietly = TRUE)) {
    stop("mapview is needed for this function to work. Install it via ",
         "install.packages(\"mapview\")", call. = FALSE)
  }
  p <- complete_hotspot_workflow_params(x$params)

  if (type == "raster") {
    if (is.null(x$state) || is.null(x$state$rasterized)) {
      stop("`x` does not contain a prepared raster.", call. = FALSE)
    }
    rasterized <- x$state$rasterized
    names(rasterized) <- paste0(p$value, "_sum")
    return(mapview::mapview(
      rasterized,
      layer.name = paste0(p$value, " sum per raster cell"),
      ...
    ))
  }

  if (is.data.frame(x$candidates)) {
    candidates <- convert_crs_df(x$candidates, p$crs_metric, 4326,
                                 lon_from = "x", lat_from = "y",
                                 lon_to = p$lon, lat_to = p$lat)
    candidates <- convert_df_to_sf(candidates, p$lon, p$lat, 4326,
                                   p$crs_metric)
    return(mapview::mapview(candidates, layer.name = "Observed candidates",
                            ...))
  }

  candidate_polygons <- candidate_cells_polygons(x$state$focal,
                                                 x$candidates$cells)
  mapview::mapview(candidate_polygons,
                   zcol = "focal_value",
                   layer.name = "Candidate focal cells",
                   ...)
}

check_hotspot_radius <- function(radius) {
  if (!is.numeric(radius) || length(radius) != 1L ||
      is.na(radius) || !is.finite(radius) || radius <= 0) {
    stop("`radius` must be a single finite positive number.", call. = FALSE)
  }
  invisible(NULL)
}

check_hotspot_cell_size <- function(cell_size) {
  if (!is.numeric(cell_size) || length(cell_size) != 1L ||
      is.na(cell_size) || !is.finite(cell_size) || cell_size <= 0) {
    stop("`cell_size` must be a single finite positive number.",
         call. = FALSE)
  }
  invisible(NULL)
}

check_hotspot_threshold <- function(threshold) {
  if (!is.null(threshold) &&
      (!is.numeric(threshold) || length(threshold) != 1L ||
       is.na(threshold) || !is.finite(threshold))) {
    stop("`threshold` must be NULL or a single finite numeric value.",
         call. = FALSE)
  }
  invisible(NULL)
}

validate_hotspot_search_settings <- function(data, value, top_n, radius,
                                             cell_size, grid_precision,
                                             max_refinement_points, lon, lat,
                                             crs_metric, progress) {
  validate_pair_refine_input(data, value, top_n, radius, cell_size,
                             grid_precision, max_refinement_points, lon, lat,
                             crs_metric, progress)
}

complete_hotspot_workflow_params <- function(params) {
  defaults <- list(
    cell_size = 100,
    grid_precision = 1,
    max_refinement_points = 1000,
    method = "continuous",
    threshold = NULL
  )
  utils::modifyList(defaults, params)
}

estimate_hotspot_candidate_threshold <- function(focal, data, params) {
  top_focals <- top_n_focals(focal, n = 5)
  threshold_candidates <- concentration_per_candidate_cell(
    top_focals,
    data,
    params$value,
    params$cell_size,
    points = 10,
    cache = empty_hotspot_cache(),
    radius = params$radius,
    crs_metric = params$crs_metric,
    lon = params$lon,
    lat = params$lat
  )
  lower_bound <- highest_concentration_candidate(
    threshold_candidates,
    top_focals,
    empty_hotspot_cache()
  )
  lower_bound$concentration[1]
}

initialise_prepare_hotspot_state <- function(data, value, radius, cell_size,
                                             lon, lat, crs_metric) {
  tryCatch(
    initialise_terra_hotspot_state(data, value, radius, cell_size,
                                   lon, lat, crs_metric),
    error = function(e) {
      metric_sf <- convert_crs_df(data, 4326, crs_metric, lon, lat, "x", "y")
      terra_crs <- paste0("EPSG:", crs_metric)
      spatvctr <- terra::vect(metric_sf, geom = c("x", "y"), crs = terra_crs)
      raster <- terra::rast(spatvctr, res = cell_size)
      rasterized <- terra::rasterize(spatvctr, raster, field = value, fun = sum)
      list(
        spatvctr = spatvctr,
        raster = raster,
        rasterized = rasterized,
        moving_window = NULL,
        focal = NULL
      )
    }
  )
}

cells_above_threshold_with_values <- function(focal, threshold) {
  cells <- cells_above_threshold(focal, threshold)
  if (is.null(cells) || nrow(cells) == 0L) {
    return(data.frame(cell = integer(), x = numeric(), y = numeric(),
                      focal_value = numeric()))
  }
  values <- terra::values(focal, mat = FALSE)
  cells$focal_value <- values[cells$cell]
  cells
}

candidate_cells_polygons <- function(focal, candidate_cells) {
  out <- focal
  vals <- rep(NA_real_, length(terra::values(focal, mat = FALSE)))
  vals[candidate_cells$cell] <- candidate_cells$focal_value
  names(out) <- "focal_value"
  terra::values(out) <- vals
  terra::as.polygons(out, na.rm = TRUE)
}
