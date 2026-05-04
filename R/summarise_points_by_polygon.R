#' Summarise point values by polygon
#'
#' @description Spatially joins point data to polygon geometries and summarises
#' a numeric point attribute for each polygon.
#'
#' @param polygons An object of class \code{sf} containing polygon geometries.
#' @param points A data.frame containing point coordinates and the value to
#'   summarise.
#' @param value A string giving the name of the numeric column in \code{points}
#'   to summarise.
#' @param fun A summary function, such as \code{sum}, \code{mean}, or
#'   \code{length}. Default is \code{sum}.
#' @param lon A string with the name of the longitude column in \code{points}.
#'   Default is \code{"lon"}.
#' @param lat A string with the name of the latitude column in \code{points}.
#'   Default is \code{"lat"}.
#' @param crs Coordinate reference system of the point coordinates. Default is
#'   \code{4326}.
#' @param output_col Optional string giving the name of the output column. If
#'   \code{NULL}, the name is created from \code{value} and \code{fun}, for
#'   example \code{"amount_sum"} or \code{"amount_mean"}.
#' @param na.rm Logical. Whether to remove missing values when \code{fun}
#'   supports an \code{na.rm} argument. Default is \code{TRUE}.
#' @param outside What to do when points fall outside all polygons:
#'   \code{"message"} (default), \code{"warning"}, or \code{"ignore"}.
#' @param repair_geometry Logical. Whether to try \code{sf::st_buffer(x, 0)}
#'   if transforming polygon geometries fails. Default is \code{TRUE}.
#'
#' @return An \code{sf} object equal to \code{polygons} with an additional
#'   summary column.
#'
#' @importFrom sf st_as_sf
#' @importFrom sf st_buffer
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_join
#' @importFrom sf st_transform
#'
#' @examples
#' summarise_points_by_polygon(
#'   polygons = nl_postcode2,
#'   points = insurance,
#'   value = "amount",
#'   fun = sum
#' )
#'
#' @export
summarise_points_by_polygon <- function(polygons, points, value, fun = sum,
                                        lon = "lon", lat = "lat",
                                        crs = 4326, output_col = NULL,
                                        na.rm = TRUE,
                                        outside = c("message", "warning",
                                                    "ignore"),
                                        repair_geometry = TRUE) {
  outside <- match.arg(outside)

  if (!inherits(polygons, "sf")) {
    stop("`polygons` must be an sf object.", call. = FALSE)
  }

  column_args <- list(value, lon, lat)
  if (!is.null(output_col)) {
    column_args <- c(column_args, list(output_col))
  }

  if (!all(vapply(column_args, is.character, logical(1))) ||
      !all(vapply(column_args, length, integer(1)) == 1) ||
      any(is.na(unlist(column_args, use.names = FALSE)))) {
    stop("Column arguments must be single non-missing strings.", call. = FALSE)
  }

  if (!all(c(lon, lat, value) %in% names(points))) {
    stop("`points` must contain columns '", lon, "', '", lat, "', and '",
         value, "'.", call. = FALSE)
  }

  if (!is.numeric(points[[lon]]) || !is.numeric(points[[lat]])) {
    stop(lon, " and ", lat, " must be numeric.", call. = FALSE)
  }

  if (!is.numeric(points[[value]])) {
    stop("Column '", value, "' must be numeric.", call. = FALSE)
  }

  if (!is.numeric(crs) || length(crs) != 1 || is.na(crs)) {
    stop("`crs` must be a single numeric EPSG code.", call. = FALSE)
  }

  if (!is.logical(na.rm) || length(na.rm) != 1 || is.na(na.rm)) {
    stop("`na.rm` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(repair_geometry) || length(repair_geometry) != 1 ||
      is.na(repair_geometry)) {
    stop("`repair_geometry` must be TRUE or FALSE.", call. = FALSE)
  }

  fun_expr <- substitute(fun)
  fun_name <- summarise_fun_name(fun_expr, fun)
  fun <- match.fun(fun)

  if (is.null(output_col)) {
    output_col <- paste(value, fun_name, sep = "_")
  }

  if (output_col %in% names(polygons)) {
    stop("`polygons` already contains column '", output_col, "'. Choose a ",
         "different `output_col` to avoid overwriting data.", call. = FALSE)
  }

  polygon_id <- ".spatialrisk_polygon_id"
  while (polygon_id %in% names(polygons)) {
    polygon_id <- paste0(".", polygon_id)
  }

  polygons_work <- polygons
  polygons_work[[polygon_id]] <- seq_len(nrow(polygons_work))
  polygons_work <- transform_polygons(polygons_work, crs, repair_geometry)

  points_sf <- sf::st_as_sf(points, coords = c(lon, lat), crs = crs)
  joined <- suppressMessages(
    sf::st_join(points_sf[, value, drop = FALSE],
                polygons_work[, polygon_id, drop = FALSE],
                left = TRUE)
  )
  joined_df <- sf::st_drop_geometry(joined)

  outside_count <- sum(is.na(joined_df[[polygon_id]]))
  if (outside_count > 0 && outside != "ignore") {
    msg <- paste(outside_count, "points are outside any polygon.")
    if (outside == "warning") {
      warning(msg, call. = FALSE)
    } else {
      message(msg)
    }
  }

  joined_df <- joined_df[!is.na(joined_df[[polygon_id]]), , drop = FALSE]

  if (nrow(joined_df) == 0) {
    summary_df <- data.frame(
      .spatialrisk_polygon_id = integer(),
      .spatialrisk_summary = numeric()
    )
    names(summary_df) <- c(polygon_id, output_col)
  } else {
    summary_values <- stats::aggregate(
      joined_df[[value]],
      by = list(joined_df[[polygon_id]]),
      FUN = function(x) apply_summary_fun(fun, x, na.rm)
    )
    names(summary_values) <- c(polygon_id, output_col)
    summary_df <- summary_values
  }

  out <- merge(polygons_work, summary_df, by = polygon_id, all.x = TRUE,
               sort = FALSE)
  out <- out[order(out[[polygon_id]]), , drop = FALSE]
  out[[polygon_id]] <- NULL
  rownames(out) <- NULL
  out
}

summarise_fun_name <- function(fun_expr, fun) {
  if (is.character(fun) && length(fun) == 1 && !is.na(fun)) {
    return(make.names(fun))
  }

  expr_name <- deparse(fun_expr, width.cutoff = 500)
  if (length(expr_name) == 1 && grepl("^[[:alnum:]_.]+$", expr_name)) {
    return(make.names(expr_name))
  }

  "summary"
}

apply_summary_fun <- function(fun, x, na.rm) {
  tryCatch(
    fun(x, na.rm = na.rm),
    error = function(e) fun(x)
  )
}

transform_polygons <- function(polygons, crs, repair_geometry) {
  tryCatch(
    sf::st_transform(polygons, crs = crs),
    error = function(e) {
      if (!isTRUE(repair_geometry)) {
        stop(e)
      }
      sf::st_transform(sf::st_buffer(polygons, 0), crs = crs)
    }
  )
}
