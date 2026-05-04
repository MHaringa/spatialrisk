#' Identify fixed-radius concentration hotspots
#'
#' @description Identifies centre coordinates of fixed-radius circles with high
#' local concentration. In insurance applications this can be used to find
#' locations where the total insured value within a prescribed radius is largest.
#'
#' @param data A data.frame containing point-level exposures. Must include columns
#'   for longitude, latitude, and the value of interest.
#' @param value A string giving the name of the numeric column in \code{data} to
#'   aggregate within each radius.
#' @param top_n Positive integer greater or equal to 1. Specifies how many
#'   non-overlapping hotspots are returned. Default is \code{1}.
#' @param radius Numeric. Radius of the circle in meters. This is typically the
#'   regulatory or scenario radius. Default is \code{200}.
#' @param cell_size Numeric. Size of the initial screening cells in meters.
#'   This is used by \code{method = "continuous"} and \code{method = "grid"}.
#'   Smaller values give a finer initial search but increase computation time.
#'   \code{method = "observed"} searches observed point locations and does not
#'   use this value as a search-grid resolution. Default is \code{100}.
#' @param grid_precision Numeric. Approximate spacing in meters used for
#'   grid-based refinement. This is used by \code{method = "grid"} and by
#'   \code{method = "continuous"} only when the local subset is larger than
#'   \code{max_refinement_points} and the method falls back to grid refinement.
#'   It is not used by \code{method = "observed"}. Smaller values evaluate more
#'   candidate centres and increase search precision. Default is \code{1}.
#' @param max_refinement_points Positive integer. Maximum number of local points
#'   used for pair-intersection refinement. If the local subset contains more
#'   points, \code{method = "continuous"} automatically falls back to the grid
#'   refinement used by \code{method = "grid"}. Default is \code{1000}.
#' @param lon A string giving the longitude column in \code{data}. Default is
#'   \code{"lon"}.
#' @param lat A string giving the latitude column in \code{data}. Default is
#'   \code{"lat"}.
#' @param crs_metric Numeric. EPSG code for a projected CRS with meter units,
#'   used for distances, buffers, raster cells, and pair-intersection
#'   calculations. The default \code{3035} is ETRS89 / LAEA Europe and is a
#'   suitable default for Europe-wide applications. For other regions, choose a
#'   metric CRS appropriate to the study area, for example a local UTM zone,
#'   \code{5070} for the conterminous United States, or \code{3577} for
#'   Australia. For Asian portfolios there is no single universal choice; use a
#'   national projected CRS or the relevant UTM zone. Default is \code{3035}.
#' @param progress Logical. Whether to print progress messages for the main
#'   hotspot search steps. This is useful for larger portfolios and for
#'   \code{top_n > 1}. Default is \code{TRUE}.
#' @param method Hotspot search strategy. \code{"continuous"} is the default
#'   and searches for a centre that may lie between observed points.
#'   \code{"observed"} searches only observed point locations as candidate
#'   centres. \code{"grid"} uses the original grid-refinement workflow.
#'
#' @return An object of class \code{hotspot}. The main components are
#'   \code{hotspots}, containing the selected centre coordinates and summed
#'   values, and \code{contributing_points}, containing the points inside the
#'   selected hotspot radii. The summed value column is named from \code{value};
#'   for example, \code{value = "amount"} creates an \code{amount_sum} column.
#'   In \code{contributing_points}, \code{data_row} gives the row number of the
#'   contributing point in the original input data.
#'
#' @details The default \code{method = "continuous"} first uses terra
#'   rasterisation and focal sums to locate an approximate hotspot area. It then
#'   refines the local result by evaluating observed local points and the circle
#'   centres implied by local point pairs. The local refinement subset is chosen
#'   automatically around the terra-selected approximate centre, using a
#'   conservative margin based on \code{radius} and \code{cell_size}. If more
#'   than \code{max_refinement_points} local points are involved, it falls back
#'   to the grid refinement used by \code{method = "grid"}. In that fallback
#'   case, \code{grid_precision} controls the local refinement grid; otherwise
#'   the pair-intersection step does not use \code{grid_precision}. The
#'   pair-refined result is exact only within the terra-selected local search
#'   area. The \code{"observed"} method is fast and deterministic, but can miss
#'   a larger hotspot when the optimal centre lies between observed points. The
#'   \code{"grid"} method uses a grid-based search with local refinement;
#'   smaller \code{grid_precision} values generally increase precision and
#'   computation time.
#'
#' @examples
#' portfolio <- Groningen[1:200, c("lon", "lat", "amount")]
#'
#' hotspot <- concentration_hotspot(
#'   portfolio,
#'   value = "amount",
#'   radius = 200,
#'   cell_size = 100,
#'   progress = FALSE,
#'   top_n = 2
#' )
#'
#' hotspot$hotspots
#' head(hotspot$contributing_points)
#'
#' observed_hotspot <- concentration_hotspot(
#'   portfolio,
#'   value = "amount",
#'   radius = 200,
#'   method = "observed",
#'   progress = FALSE
#' )
#'
#' rbind(
#'   continuous = hotspot$hotspots[1, ],
#'   observed = observed_hotspot$hotspots
#' )
#'
#' @author Martin Haringa
#'
#' @export
concentration_hotspot <- function(data, value, top_n = 1, radius = 200,
                                  cell_size = 100, grid_precision = 1,
                                  max_refinement_points = 1000,
                                  lon = "lon", lat = "lat",
                                  crs_metric = 3035,
                                  progress = TRUE,
                                  method = c("continuous", "grid",
                                             "observed")) {

  method <- match.arg(method)
  value <- validate_hotspot_value(value)

  if (method == "observed") {
    hotspot_progress(progress, "Using observed-points hotspot search.")
    return(concentration_hotspot_indexed(
      data = data,
      value = value,
      top_n = top_n,
      radius = radius,
      lon = lon,
      lat = lat,
      crs_metric = crs_metric,
      print_progress = progress,
      cell_size = radius
    ))
  }

  if (method == "continuous") {
    return(concentration_hotspot_pair_refine(
      data = data,
      value = value,
      top_n = top_n,
      radius = radius,
      cell_size = cell_size,
      grid_precision = grid_precision,
      max_refinement_points = max_refinement_points,
      lon = lon,
      lat = lat,
      crs_metric = crs_metric,
      progress = progress
    ))
  }

  validate_terra_hotspot_input(data, value, top_n, radius, cell_size,
                               grid_precision, lon, lat, crs_metric,
                               progress)
  hotspot_progress(progress, "Using grid-refinement hotspot search.")
  concentration_hotspot_terra(data, value, top_n, radius, cell_size,
                              grid_precision, lon, lat, crs_metric,
                              progress)
}

concentration_hotspot_terra <- function(data, value, top_n, radius, cell_size,
                                        grid_precision, lon, lat, crs_metric,
                                        progress) {

  pts_lst <- vector("list", top_n)
  conc_lst <- vector("list", top_n)

  threshold_db <- conc_db <- empty_hotspot_cache()
  output_col <- hotspot_sum_column(value)

  data$ix <- seq_len(nrow(data))
  state <- initialise_terra_hotspot_state(data, value, radius, cell_size,
                                          lon, lat, crs_metric)

  for (i in seq_len(top_n)){
    hotspot_progress(progress, "Hotspot ", i, " of ", top_n,
                     ": terra focal screening.")

    candidate <- refine_terra_hotspot_candidate(
      focal = state$focal,
      data = data,
      value = value,
      cell_size = cell_size,
      grid_precision = grid_precision,
      threshold_cache = threshold_db,
      concentration_cache = conc_db,
      radius = radius,
      crs_metric = crs_metric,
      lon = lon,
      lat = lat
    )

    hc <- candidate$hotspot
    hc$id <- i
    pic <- hotspot_contributing_points(data, hc, radius, lon, lat)
    pic$id <- i
    pic[[output_col]] <- hc$concentration[1]

    hotspot_progress(progress, "Hotspot ", i, " of ", top_n,
                     ": selected concentration ", hc$concentration[1], ".")

    if (top_n > 1 && i < top_n) {
      data <- data[!data$ix %in% pic$ix, ]

      if (nrow(data) == 0) {
        rlang::abort("Need more rows", call = NULL)
      }

      update <- update_terra_hotspot_state(
        state = state,
        selected_points = pic,
        value = value,
        lon = lon,
        lat = lat,
        crs_metric = crs_metric,
        threshold_candidates = candidate$threshold_candidates,
        refined_candidates = candidate$refined_candidates,
        threshold_cache = threshold_db,
        concentration_cache = conc_db,
        radius = radius
      )
      state <- update$state
      threshold_db <- update$threshold_cache
      conc_db <- update$concentration_cache
    }

    pts_lst[[i]] <- pic
    conc_lst[[i]] <- hc
  }

  out <- new_hotspot_object(
    hotspots = do.call(rbind, c(conc_lst, make.row.names = FALSE)),
    contributing_points = do.call(rbind, pts_lst),
    radius = radius,
    rasterized = state$rasterized,
    focal = state$focal,
    threshold = candidate$threshold,
    value = value,
    lon = lon,
    lat = lat,
    crs_metric = crs_metric
  )
  attr(out, "method") <- "grid"
  out
}

validate_terra_hotspot_input <- function(data, value, top_n, radius, cell_size,
                                         grid_precision, lon, lat, crs_metric,
                                         progress) {
  check_input(data, value, top_n, radius, cell_size, grid_precision)
  check_hotspot_columns(data, value, lon, lat)
  check_hotspot_crs_metric(crs_metric)
  check_hotspot_progress(progress)
  invisible(NULL)
}

initialise_terra_hotspot_state <- function(data, value, radius, cell_size,
                                           lon, lat, crs_metric) {
  metric_sf <- convert_crs_df(data, 4326, crs_metric, lon, lat, "x", "y")
  terra_crs <- paste0("EPSG:", crs_metric)
  spatvctr <- terra::vect(metric_sf, geom = c("x", "y"), crs = terra_crs)
  raster <- terra::rast(spatvctr, res = cell_size)
  rasterized <- terra::rasterize(spatvctr, raster, field = value, fun = sum)
  mw <- mw_create(raster, radius)
  focal <- terra::focal(rasterized, w = mw, fun = "sum", na.rm = TRUE)

  list(
    spatvctr = spatvctr,
    raster = raster,
    rasterized = rasterized,
    moving_window = mw,
    focal = focal
  )
}

refine_terra_hotspot_candidate <- function(focal, data, value, cell_size,
                                           grid_precision, threshold_cache,
                                           concentration_cache, radius,
                                           crs_metric, lon, lat) {
  top_focals <- top_n_focals(focal, n = 5)
  threshold_candidates <- concentration_per_candidate_cell(
    top_focals,
    data,
    value,
    cell_size,
    points = 10,
    cache = threshold_cache,
    radius = radius,
    crs_metric = crs_metric,
    lon = lon,
    lat = lat
  )
  lower_bound <- highest_concentration_candidate(
    threshold_candidates,
    top_focals,
    threshold_cache
  )
  threshold <- lower_bound$concentration[1]
  candidate_cells <- cells_above_threshold(focal, threshold)
  refinement_points <- max(1L, floor(cell_size / grid_precision))
  refined_candidates <- concentration_per_candidate_cell(
    candidate_cells,
    data,
    value,
    cell_size,
    points = refinement_points,
    cache = concentration_cache,
    radius = radius,
    crs_metric = crs_metric,
    lon = lon,
    lat = lat
  )
  hotspot <- highest_concentration_candidate(
    refined_candidates,
    candidate_cells,
    concentration_cache
  )

  list(
    hotspot = hotspot,
    threshold = threshold,
    threshold_candidates = threshold_candidates,
    refined_candidates = refined_candidates
  )
}

hotspot_contributing_points <- function(data, hotspot, radius, lon, lat) {
  points_within_radius(data,
                       lon_center = hotspot[[lon]][1],
                       lat_center = hotspot[[lat]][1],
                       lon = lon,
                       lat = lat,
                       radius = radius)
}

update_terra_hotspot_state <- function(state, selected_points, value, lon, lat,
                                       crs_metric, threshold_candidates,
                                       refined_candidates, threshold_cache,
                                       concentration_cache, radius) {
  state$spatvctr <- state$spatvctr[
    !state$spatvctr$ix %in% selected_points$ix,
  ]
  affected_cells <- map_points_to_cells(selected_points, state$focal, lon, lat,
                                        4326, crs_metric, r = radius)
  threshold_cache <- update_hotspot_cache(threshold_candidates,
                                          threshold_cache,
                                          affected_cells)
  concentration_cache <- update_hotspot_cache(refined_candidates,
                                              concentration_cache,
                                              affected_cells)

  cells <- map_points_to_cells(selected_points, state$focal, lon, lat, 4326,
                               crs_metric)
  extent <- terra::ext(state$raster, cells)
  state$rasterized <- update_rasterize(state$rasterized, extent,
                                       state$spatvctr, value)
  state$focal <- update_focal(state$focal, state$rasterized, extent,
                              state$moving_window)

  list(
    state = state,
    threshold_cache = threshold_cache,
    concentration_cache = concentration_cache
  )
}

empty_hotspot_cache <- function() {
  data.frame(lon = vector("numeric"),
             lat = vector("numeric"),
             concentration = vector("numeric"),
             cell = vector("integer"))
}

new_hotspot_object <- function(hotspots, contributing_points, radius,
                               rasterized, focal, threshold, value, lon, lat,
                               crs_metric) {
  output <- format_hotspot_output_columns(hotspots, contributing_points, value,
                                          lon, lat)
  hotspots <- output$hotspots
  contributing_points <- output$contributing_points
  rownames(hotspots) <- NULL
  rownames(contributing_points) <- NULL
  out <- list(
    hotspots = hotspots,
    contributing_points = contributing_points
  )
  attr(out, "radius") <- radius
  attr(out, "rasterized") <- rasterized
  attr(out, "focal") <- focal
  attr(out, "threshold") <- threshold
  attr(out, "value") <- value
  attr(out, "lon") <- lon
  attr(out, "lat") <- lat
  attr(out, "crs_metric") <- crs_metric
  class(out) <- append("hotspot", class(out))
  out
}

#' Plot concentration hotspot results
#'
#' @description Visualise objects returned by \code{concentration_hotspot()}.
#' The default plot shows hotspot
#' centres, fixed-radius buffers, and the contributing points. For terra-based
#' results, diagnostic raster layers can also be plotted.
#'
#' @param x An object of class \code{hotspot}.
#' @param type Plot type. \code{"concentration"} shows hotspot buffers and
#'   contributing points and works for all hotspot search methods.
#'   \code{"focal"}, \code{"rasterized"}, and \code{"updated_focal"} are
#'   diagnostic terra layers and are only available for terra-based results.
#' @param color1 Optional colour or colours for hotspot buffers and points. If
#'   \code{NULL}, colours are chosen with \code{grDevices::hcl.colors()}.
#' @param max.rad Maximum point radius passed to \code{mapview::mapview()}.
#'   Default is \code{20}.
#' @param ... Additional arguments passed to \code{mapview::mapview()} for the
#'   contributing point layer when \code{type = "concentration"}, or to the
#'   raster mapview call for diagnostic raster layers.
#'
#' @return A \code{mapview} object.
#'
#' @details The observed-points hotspot method does not create terra raster or
#' focal objects. For observed-points results, use
#' \code{type = "concentration"}.
#'
#' @method plot hotspot
#' @export
plot.hotspot <- function(x, type = c("concentration", "focal",
                                     "rasterized", "updated_focal"),
                         color1 = NULL, max.rad = 20, ...) {
  type <- match.arg(type)
  rasterized <- attr(x, "rasterized")
  focal <- attr(x, "focal")
  threshold <- attr(x, "threshold")
  radius <- attr(x, "radius")
  value <- attr(x, "value")
  lon <- attr(x, "lon")
  lat <- attr(x, "lat")
  crs_metric <- attr(x, "crs_metric")
  hotspots <- hotspot_centres(x)
  contributing_points <- hotspot_points(x)

  if (type == "focal") {
    check_hotspot_raster_layer(focal, "focal")
  }
  if (type == "rasterized") {
    check_hotspot_raster_layer(rasterized, "rasterized")
  }
  if (type == "updated_focal") {
    check_hotspot_raster_layer(focal, "updated_focal")
  }
  check_hotspot_plot_radius(max.rad, "`max.rad`")

  if (!requireNamespace("mapview", quietly = TRUE)) {
    stop("mapview is needed for this function to work. Install it via ",
         "install.packages(\"mapview\")", call. = FALSE)
  }

  if (type == "concentration") {
    check_hotspot_plot_data(hotspots, contributing_points, value, lon, lat,
                            radius, crs_metric)

    cw <- convert_df_to_sf(hotspots, lon, lat, 4326, crs_metric)
    mpv <- convert_df_to_sf(contributing_points, lon, lat, 4326, crs_metric)
    mpv$.plot_value <- hotspot_plot_size(contributing_points[[value]])
    cw_buffer <- sf::st_buffer(cw, dist = radius, nQuadSegs = 50)
    cw_buffer$id <- as.factor(cw_buffer$id)
    mpv$id <- as.factor(mpv$id)
    legend <- nrow(cw_buffer) > 1

    if (is.null(color1)) {
      color1 <- grDevices::hcl.colors(max(1L, nrow(cw_buffer)),
                                      palette = "viridis")
    }

    point_args <- c(
      list(x = mpv,
           cex = ".plot_value",
           min.rad = 5,
           max.rad = max.rad,
           zcol = "id",
           layer.name = "Contributing points",
           legend = FALSE),
      list(...)
    )
    if (!legend) {
      point_args$col.region <- color1[1]
    }

    buffer_args <- list(x = cw_buffer,
                        zcol = "id",
                        alpha.regions = .1,
                        layer.name = "Hotspot radius",
                        legend = legend)
    if (!legend) {
      buffer_args$col.region <- color1[1]
    }

    ut <- do.call(mapview::mapview, point_args) +
      do.call(mapview::mapview, buffer_args)
  }

  if (type == "focal") {
    ut <- mapview::mapview(focal, ...)
  }

  if (type == "rasterized") {
    ut <- mapview::mapview(rasterized, ...)
  }

  if (type == "updated_focal") {
    if (!is.numeric(threshold) || length(threshold) != 1L ||
        is.na(threshold)) {
      stop("`type = \"updated_focal\"` requires a finite hotspot threshold.",
           call. = FALSE)
    }
    foc_upd <- terra::classify(focal, cbind(-Inf, threshold, NA), right = FALSE)
    foc_trim <- terra::trim(foc_upd)
    ut <- mapview::mapview(foc_trim, ...)
  }
  ut
}

#' @export
print.hotspot <- function(x, ...) {
  hotspots <- hotspot_centres(x)
  cat("<hotspot>\n")
  cat("Number of hotspots:", nrow(hotspots), "\n")
  cat("Radius:", attr(x, "radius"), "meters\n")
  cat("Value:", attr(x, "value"), "\n\n")
  print(hotspots, ...)
  invisible(x)
}

check_hotspot_progress <- function(progress) {
  if (!is.logical(progress) || length(progress) != 1L || is.na(progress)) {
    stop("`progress` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(NULL)
}

hotspot_progress <- function(progress, ...) {
  if (isTRUE(progress)) {
    cat(paste0(...), "\n")
  }
  invisible(NULL)
}

check_hotspot_columns <- function(data, value, lon, lat) {
  column_args <- list(value = value, lon = lon, lat = lat)
  if (!all(vapply(column_args, is.character, logical(1))) ||
      !all(vapply(column_args, length, integer(1)) == 1) ||
      any(is.na(unlist(column_args, use.names = FALSE)))) {
    stop("Column arguments must be single non-missing strings.", call. = FALSE)
  }

  required <- c(lon, lat, value)
  if (!all(required %in% names(data))) {
    stop("`data` must contain columns '", lon, "', '", lat, "', and '",
         value, "'.", call. = FALSE)
  }

  numeric_cols <- required[
    !vapply(data[required], is.numeric, logical(1))
  ]
  if (length(numeric_cols) > 0L) {
    stop("The following columns must be numeric: ",
         paste(numeric_cols, collapse = ", "), call. = FALSE)
  }

  if (anyNA(data[[lon]]) || anyNA(data[[lat]])) {
    stop("Longitude and latitude columns must not contain missing values.",
         call. = FALSE)
  }

  if (anyNA(data[[value]])) {
    stop("`value` column must not contain missing values.", call. = FALSE)
  }

  output_col <- hotspot_sum_column(value)
  check_hotspot_output_name_conflicts(data, output_col)

  invisible(NULL)
}

check_hotspot_crs_metric <- function(crs_metric) {
  if (!is.numeric(crs_metric) || length(crs_metric) != 1L ||
      is.na(crs_metric) || !is.finite(crs_metric)) {
    stop("`crs_metric` must be a single finite EPSG code.", call. = FALSE)
  }

  crs <- tryCatch(sf::st_crs(crs_metric), error = function(e) NA)
  if (is.na(crs)) {
    stop("`crs_metric` must be a valid EPSG code.", call. = FALSE)
  }

  units_gdal <- crs$units_gdal
  if (is.null(units_gdal) || is.na(units_gdal) ||
      !tolower(units_gdal) %in% c("metre", "meter", "metres", "meters")) {
    stop("`crs_metric` must use meter units. Choose a projected metric CRS, ",
         "not longitude/latitude coordinates.", call. = FALSE)
  }

  invisible(NULL)
}

hotspot_centres <- function(x) {
  x$hotspots
}

hotspot_points <- function(x) {
  x$contributing_points
}

hotspot_sum_column <- function(value) {
  paste0(value, "_sum")
}

format_hotspot_output_columns <- function(hotspots, contributing_points,
                                          value, lon, lat) {
  output_col <- hotspot_sum_column(value)

  if ("concentration" %in% names(hotspots)) {
    names(hotspots)[names(hotspots) == "concentration"] <- output_col
  }
  hotspots$cell <- NULL
  if ("ix" %in% names(contributing_points)) {
    names(contributing_points)[names(contributing_points) == "ix"] <-
      "data_row"
  }

  hotspots <- hotspot_reorder_columns(hotspots, c("id", lon, lat,
                                                  output_col))
  contributing_points <- hotspot_reorder_columns(
    contributing_points,
    c("id", "data_row", lon, lat)
  )

  list(hotspots = hotspots, contributing_points = contributing_points)
}

hotspot_reorder_columns <- function(data, first) {
  first <- first[first %in% names(data)]
  data[c(first, setdiff(names(data), first))]
}

check_hotspot_output_name_conflicts <- function(data, output_col) {
  reserved <- c(output_col, "data_row")
  conflicts <- intersect(reserved, names(data))
  if (length(conflicts) > 0L) {
    stop("`data` already contains reserved output column",
         if (length(conflicts) > 1L) "s " else " ",
         paste0("'", conflicts, "'", collapse = ", "),
         ". Rename before calling `concentration_hotspot()`.",
         call. = FALSE)
  }
  invisible(NULL)
}

check_hotspot_plot_data <- function(hotspots, contributing_points, value, lon,
                                    lat, radius, crs_metric) {
  if (!is.data.frame(hotspots) || nrow(hotspots) == 0L) {
    stop("`x` does not contain hotspot centres to plot.", call. = FALSE)
  }

  if (!is.data.frame(contributing_points) || nrow(contributing_points) == 0L) {
    stop("`x` does not contain contributing points to plot.", call. = FALSE)
  }

  output_col <- hotspot_sum_column(value)
  required_hotspots <- c(lon, lat, output_col, "id")
  required_points <- c(lon, lat, value, output_col, "id")
  if (!all(required_hotspots %in% names(hotspots))) {
    stop("`x` has incomplete hotspot centre data.", call. = FALSE)
  }
  if (!all(required_points %in% names(contributing_points))) {
    stop("`x` has incomplete contributing point data.", call. = FALSE)
  }
  if (!is.numeric(radius) || length(radius) != 1L || is.na(radius) ||
      radius <= 0) {
    stop("`x` has an invalid hotspot radius.", call. = FALSE)
  }
  check_hotspot_crs_metric(crs_metric)
  invisible(NULL)
}

check_hotspot_raster_layer <- function(layer, type) {
  if (is.null(layer)) {
    stop("`type = \"", type, "\"` is only available for terra-based ",
         "hotspot results. Use `type = \"concentration\"` for methods ",
         "without terra diagnostic layers.", call. = FALSE)
  }
  invisible(NULL)
}

check_hotspot_plot_radius <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x <= 0) {
    stop(name, " must be a single positive numeric value.", call. = FALSE)
  }
  invisible(NULL)
}

hotspot_plot_size <- function(value) {
  if (!is.numeric(value)) {
    stop("The hotspot value column must be numeric to scale point sizes.",
         call. = FALSE)
  }
  if (anyNA(value)) {
    stop("The hotspot value column must not contain missing values.",
         call. = FALSE)
  }

  value <- pmax(value, 0)
  if (all(value == 0)) {
    return(rep(1, length(value)))
  }
  sqrt(value)
}

validate_hotspot_value <- function(value) {
  value <- tryCatch(
    value,
    error = function(e) {
      stop("`value` must be a single non-missing string.", call. = FALSE)
    }
  )

  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop("`value` must be a single non-missing string.", call. = FALSE)
  }

  value
}
