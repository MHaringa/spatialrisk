# Deprecated compatibility wrappers.
# Keep these small and remove after lifecycle escalation in a later release.

#' Deprecated aliases
#'
#' @description
#' These functions are deprecated compatibility wrappers. Use the replacement
#' functions shown in the warning messages.
#'
#' @name spatialrisk-deprecated
NULL

#' @rdname radius_sum
#' @export
#' @param sub Deprecated. Use \code{targets} instead.
#' @param full Deprecated. Use \code{reference} instead.
#' @param lon_sub Deprecated. Use \code{lon_targets} instead.
#' @param lat_sub Deprecated. Use \code{lat_targets} instead.
#' @param lon_full Deprecated. Use \code{lon_reference} instead.
#' @param lat_full Deprecated. Use \code{lat_reference} instead.
#' @param display_progress Deprecated. Use \code{progress} in
#'   \code{radius_sum()} instead.
concentration <- function(sub, full, value,
                          lon_sub = lon, lat_sub = lat,
                          lon_full = lon, lat_full = lat,
                          radius = 200, display_progress = TRUE,
                          result_col = "radius_sum") {
  lifecycle::deprecate_warn("0.8.0", "concentration()", "radius_sum()")

  lon_sub <- deparse(substitute(lon_sub))
  lat_sub <- deparse(substitute(lat_sub))
  lon_full <- deparse(substitute(lon_full))
  lat_full <- deparse(substitute(lat_full))
  value <- deparse(substitute(value))

  radius_sum(targets = sub, reference = full, value = value,
             lon_targets = lon_sub, lat_targets = lat_sub,
             lon_reference = lon_full, lat_reference = lat_full,
             radius = radius, progress = display_progress,
             result_col = result_col)
}

#' @rdname points_within_radius
#' @export
#' @description Deprecated.
#'
#' `points_in_circle()` was renamed to [points_within_radius()].
points_in_circle <- function(data, lon_center, lat_center, lon = lon, lat = lat,
                             radius = 200, sort = TRUE) {
  lifecycle::deprecate_warn("0.8.0", "points_in_circle()",
                            "points_within_radius()")

  lon <- deparse(substitute(lon))
  lat <- deparse(substitute(lat))

  points_within_radius(data = data,
                       lon_center = lon_center,
                       lat_center = lat_center,
                       lon = lon,
                       lat = lat,
                       radius = radius,
                       sort = sort)
}

#' @rdname map_points
#' @export
#' @description Deprecated.
#' @param df Deprecated. Use \code{data} instead.
#'
#' `plot_points()` was renamed to [map_points()].
plot_points <- function(df, value = NULL, lon = "lon", lat = "lat",
                        crs = 4326, at = NULL, layer_name = NULL, ...) {
  lifecycle::deprecate_warn("0.8.0", "plot_points()", "map_points()")
  map_points(data = df, value = value, lon = lon, lat = lat, crs = crs,
             at = at, layer_name = layer_name, ...)
}

#' @rdname summarise_points_by_polygon
#' @export
#' @description Deprecated.
#' @param sf_map Deprecated. Use \code{polygons} instead.
#' @param df Deprecated. Use \code{points} instead.
#' @param oper Deprecated expression used to aggregate values.
#' @param outside_print Deprecated. Use \code{outside} instead.
#'
#' `points_to_polygon()` was renamed to [summarise_points_by_polygon()].
points_to_polygon <- function(sf_map, df, oper, crs = 4326,
                              outside_print = FALSE) {
  lifecycle::deprecate_warn("0.8.0", "points_to_polygon()",
                            "summarise_points_by_polygon()")

  polygons <- transform_polygons(sf_map, crs, repair_geometry = TRUE)
  polygon_id <- ".spatialrisk_polygon_id"
  while (polygon_id %in% names(polygons)) {
    polygon_id <- paste0(".", polygon_id)
  }
  polygons[[polygon_id]] <- seq_len(nrow(polygons))

  if (!all(c("lon", "lat") %in% names(df))) {
    stop("`df` must contain columns 'lon' and 'lat'.", call. = FALSE)
  }

  df_sf <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = crs)
  joined <- suppressMessages(sf::st_join(polygons, df_sf))

  outside <- suppressMessages(
    sf::st_join(df_sf, polygons[, polygon_id, drop = FALSE], left = TRUE)
  )
  outside_count <- sum(is.na(sf::st_drop_geometry(outside)[[polygon_id]]))
  if (outside_count > 0) {
    msg <- paste(outside_count, "points are outside any polygon.")
    if (isTRUE(outside_print)) {
      warning(msg, call. = FALSE)
    } else {
      message(msg)
    }
  }

  joined_df <- joined
  sf::st_geometry(joined_df) <- NULL
  oper <- substitute(oper)
  summary_df <- eval.parent(substitute(
    data.table::data.table(joined_df)[, .(output = oper), by = polygon_id]
  ))

  out <- merge(polygons, summary_df, by = polygon_id, all.x = TRUE,
               sort = FALSE)
  out <- out[order(out[[polygon_id]]), , drop = FALSE]
  out[[polygon_id]] <- NULL
  rownames(out) <- NULL
  out
}

#' @rdname spatialrisk-deprecated
#' @export
#' @description Deprecated.
#' @param df Deprecated wrapper argument.
#' @param value Deprecated wrapper argument.
#' @param top_n Deprecated wrapper argument.
#' @param radius Deprecated wrapper argument.
#' @param cell_size Deprecated wrapper argument.
#' @param grid_precision Deprecated wrapper argument.
#' @param lon Deprecated wrapper argument.
#' @param lat Deprecated wrapper argument.
#' @param crs_metric Deprecated wrapper argument.
#' @param print_progress Deprecated wrapper argument.
#'
#' `find_highest_concentration()` was renamed to [concentration_hotspot()].
find_highest_concentration <- function(df, value, top_n = 1, radius = 200,
                                       cell_size = 100, grid_precision = 1,
                                       lon = "lon", lat = "lat",
                                       crs_metric = 3035,
                                       print_progress = TRUE) {
  lifecycle::deprecate_warn("0.8.0", "find_highest_concentration()",
                            "concentration_hotspot()")

  value_expr <- substitute(value)
  value <- if (is.character(value_expr)) value_expr else deparse(value_expr)

  concentration_hotspot(data = df, value = value, top_n = top_n,
                        radius = radius, cell_size = cell_size,
                        grid_precision = grid_precision,
                        lon = lon, lat = lat,
                        crs_metric = crs_metric,
                        progress = print_progress)
}

#' Choropleth map of an sf object with ggplot2
#'
#' @description Deprecated. Use [choropleth()] instead.
#'
#' @param sf_object An object of class \code{sf} containing polygon geometries.
#' @param value Column in \code{sf_object} used to shade the polygons.
#' @param n Integer. Number of clusters to use in Fisher classification.
#' @param dig.lab Integer. Number of digits to display in legend labels.
#' @param legend_title Character. Title for the legend.
#' @param option Character string indicating the colormap option passed to
#'   \code{ggplot2}.
#' @param direction Numeric. Order of colors in the scale.
#'
#' @return A \code{ggplot} object containing the choropleth map.
#'
#' @details
#' `choropleth_ggplot2()` is deprecated. Use [choropleth()] instead.
#'
#' @export
choropleth_ggplot2 <- function(sf_object, value = output, n = 7, dig.lab = 2,
                               legend_title = "Class", option = "D",
                               direction = 1) {
  lifecycle::deprecate_warn("0.8.0", "choropleth_ggplot2()", "choropleth()")

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via ",
         "install.packages(\"ggplot2\")", call. = FALSE)
  }

  value <- deparse(substitute(value))
  vector_value <- sf_object[[value]]

  result <- tryCatch(
    {
      suppressWarnings({
        cluster <- classInt::classIntervals(vector_value, n = n,
                                            style = "fisher",
                                            intervalClosure = "right")[[2]]
        sf_object$clustering <- cut(vector_value, breaks = cluster,
                                    include.lowest = TRUE, dig.lab = dig.lab)
      })

      ggplot2::ggplot(sf_object) +
        ggplot2::geom_sf(ggplot2::aes(fill = clustering), size = .1,
                         color = "grey85")  +
        ggplot2::coord_sf(datum = NA) +
        ggplot2::scale_fill_viridis_d(direction = direction, option = option) +
        ggplot2::theme_void() +
        ggplot2::labs(fill = legend_title)
    },
    error = function(e) {
      ggplot2::ggplot(sf_object) +
        ggplot2::geom_sf(ggplot2::aes(fill = vector_value), size = .1,
                         color = "grey85")  +
        ggplot2::coord_sf(datum = NA) +
        ggplot2::scale_fill_viridis_c(direction = direction, option = option) +
        ggplot2::theme_void() +
        ggplot2::labs(fill = legend_title)
    }
  )

  result
}

#' Interpolate values using spherical splines
#'
#' @description Deprecated. Spline interpolation and smoothing on the sphere.
#' This function is outside the main scope of \pkg{spatialrisk} and will be
#' removed in a future release.
#'
#' @param observations data.frame of observations.
#' @param targets data.frame of locations to calculate the interpolated and
#' smoothed values for.
#' @param value Column with values in \code{observations}.
#' @param lon_obs Column in \code{observations} with longitude.
#' @param lat_obs Column in \code{observations} with latitude.
#' @param lon_targets Column in \code{targets} with longitude.
#' @param lat_targets Column in \code{targets} with latitude.
#' @param k Basis dimension. For small data sets reduce \code{k} manually.
#'
#' @return Object equal to \code{targets} with an extra prediction column.
#'
#' @importFrom stats as.formula
#'
#' @references \code{\link[mgcv:smooth.construct.sos.smooth.spec]{Splines on
#' the sphere}}
#'
#' @export
interpolate_spline <- function(observations, targets, value, lon_obs = lon,
                               lat_obs = lat, lon_targets = lon,
                               lat_targets = lat, k = 50) {
  lifecycle::deprecate_warn(
    "0.8.0",
    "interpolate_spline()",
    details = paste(
      "Spatial interpolation is outside the core scope of spatialrisk.",
      "Use a dedicated spatial modelling workflow instead."
    )
  )

  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("mgcv is needed for this function to work. Install it via ",
         "install.packages(\"mgcv\")", call. = FALSE)
  }

  lon_targets <- deparse(substitute(lon_targets))
  lat_targets <- deparse(substitute(lat_targets))
  lon_obs <- deparse(substitute(lon_obs))
  lat_obs <- deparse(substitute(lat_obs))
  value <- deparse(substitute(value))

  names(observations)[names(observations) == lon_obs] <- "lon"
  names(observations)[names(observations) == lat_obs] <- "lat"
  names(targets)[names(targets) == lon_targets] <- "lon"
  names(targets)[names(targets) == lat_targets] <- "lat"

  f <- stats::as.formula(
    paste0(value, " ~ s(lat, lon, bs = \"sos\", m = -1, k = ", k, ")")
  )

  pred_gam <- tryCatch(
    { mgcv::gam(f, data = observations) },
    error = function(e) {
      stop("Model fitting failed. A common cause is that `k` is too large for ",
           "the amount of data.", call. = FALSE)
    }
  )

  response <- as.numeric(mgcv::predict.gam(pred_gam, targets,
                                           type = "response"))
  targets[, paste0(value, "_pred")] <- response
  targets
}
