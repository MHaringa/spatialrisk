#' Find points within radius around one or more center coordinates
#'
#' @description This function selects rows from a data frame whose
#' longitude/latitude coordinates fall within a given radius (in meters)
#' from one or more specified center points. It also calculates the distance
#' of each point to the center.
#'
#' @param data A data frame containing at least longitude and latitude columns.
#' @param lon_center Numeric scalar or vector, longitude(s) of the circle center(s).
#' @param lat_center Numeric scalar or vector, latitude(s) of the circle center(s).
#' @param lon A string with the name of the longitude column in `data`.
#' @param lat A string with the name of the latitude column in `data`.
#' @param radius Numeric, circle radius in meters. Default is 200.
#' @param sort Logical, if `TRUE` results are sorted by distance within each center.
#'
#' @return A data frame subset of `data` with an extra column `distance_m` and
#'   if multiple centers are provided, also a column `center_index`.
#'
#' @export
points_within_radius <- function(data, lon_center, lat_center,
                                 lon = "lon", lat = "lat",
                                 radius = 200, sort = TRUE) {

  if (!all(c(lon, lat) %in% names(data))) {
    stop("`data` does not contain columns '", lon, "' and '", lat, "'.",
         call. = FALSE)
  }

  if (!is.numeric(data[[lon]]) || !is.numeric(data[[lat]])) {
    stop(lon, " and ", lat, " must be numeric.", call. = FALSE)
  }

  if (!is.numeric(lon_center) || !is.numeric(lat_center)) {
    stop("`lon_center` and `lat_center` must be numeric.", call. = FALSE)
  }

  if (any(is.infinite(c(lon_center, lat_center)))) {
    stop("`lon_center` and `lat_center` must not contain infinite values.",
         call. = FALSE)
  }

  if (length(lon_center) != length(lat_center)) {
    stop("`lon_center` and `lat_center` must be the same length.",
         call. = FALSE)
  }

  if (length(lon_center) < 1) {
    stop("`lon_center` and `lat_center` must not be empty.", call. = FALSE)
  }

  if (!is.numeric(radius) || length(radius) != 1 || !is.finite(radius) ||
      radius <= 0) {
    stop("`radius` must be a single finite positive numeric value.",
         call. = FALSE)
  }

  if (!is.logical(sort) || length(sort) != 1 || is.na(sort)) {
    stop("`sort` must be TRUE or FALSE.", call. = FALSE)
  }

  df <- data.frame("lon" = data[[lon]], "lat" = data[[lat]])

  if (length(lon_center) > 1) {
    incircle <- points_in_radius_multi_cpp(df, lat_center, lon_center, radius)
    incircle_df <- data[incircle$id, , drop = FALSE]
    incircle_df$distance_m <- incircle$distance_m
    incircle_df$center_index <- incircle$center_index
    if (sort) {
      incircle_df <- incircle_df[order(incircle_df$center_index,
                                       incircle_df$distance_m), ]
    }
    rownames(incircle_df) <- NULL
    incircle_df
  } else {
    incircle <- points_in_radius_cpp(df, lat_center, lon_center, radius)
    incircle_df <- data[incircle$id, , drop = FALSE]
    incircle_df$distance_m <- incircle$distance_m
    if (sort) {
      incircle_df <- incircle_df[order(incircle_df$distance_m), ]
    }
    rownames(incircle_df) <- NULL
    incircle_df
  }
}
