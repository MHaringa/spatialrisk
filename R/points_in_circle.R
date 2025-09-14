#' Find points within a circle around a center coordinate
#'
#' @description This function selects rows from a data frame whose
#' longitude/latitude coordinates fall within a given radius (in meters) from a
#' specified center point. It also calculates the distance of each point to the
#' center.
#'
#' @param data A data frame containing at least longitude and latitude columns.
#' @param lon_center Numeric scalar, longitude of the circle center.
#' @param lat_center Numeric scalar, latitude of the circle center.
#' @param lon Name of the longitude column in \code{data}.
#' @param lat Name of the latitude column in \code{data}.
#' @param radius Numeric, circle radius in meters. Default is 200.
#' @param sort Logical, if \code{TRUE} (default) results are sorted by distance
#'   from the center (closest first). If \code{FALSE}, the order of \code{data}
#'   is preserved.
#'
#' @author Martin Haringa
#'
#' @importFrom Rcpp sourceCpp
#' @import RcppProgress
#'
#' @examples
#' points_in_circle(Groningen, lon_center = 6.571561, lat_center = 53.21326,
#' radius = 60)
#'
#' @return A data frame subset of \code{data} with an extra column
#'   \code{distance_m} giving the distance to the center point.
#'
#' @export
points_in_circle <- function(data, lon_center, lat_center, lon = lon, lat = lat,
                             radius = 200, sort = TRUE) {

  lon <- deparse(substitute(lon))
  lat <- deparse(substitute(lat))
  data_name <- deparse(substitute(data))

  # Column checks
  if (!all(c(lon, lat) %in% names(data))) {
    stop(paste0(data_name, " does not contain columns ", lon, " and ", lat),
         call. = FALSE)
  }

  # Numeric checks
  if (!is.numeric(data[[lon]]) || !is.numeric(data[[lat]])) {
    stop(paste0(lon, " and ", lat, " must be numeric."), call. = FALSE)
  }

  # Center checks
  if (length(lon_center) != 1 || length(lat_center) != 1) {
    stop("lon_center and lat_center must be single numeric values.", call. = FALSE)
  }

  # Distance calculation
  df <- data.frame("lon" = data[[lon]], "lat" = data[[lat]])
  incircle <- haversine_loop_cpp(df, lat_center, lon_center, radius)

  # Join back
  incircle_df <- data[incircle$id, ]
  incircle_df$distance_m <- incircle$distance_m

  if (sort) {
    incircle_df <- incircle_df[order(incircle_df$distance_m), ]
  }

  incircle_df
}


#' @keywords internal
points_in_circle_ <- function(data, lon_center, lat_center, lon = lon,
                              lat = lat, radius = 200) {

  # Turn into character vector
  data_name <- deparse(substitute(data))

  if (!all(c(lon, lat) %in% names(data))) {
    stop(paste0(data_name, " does not contain columns ", lon, " and ", lat),
         call. = FALSE)
  }

  if (!all(is.numeric(c(data[[lon]], data[[lat]])))) {
    stop(paste0(lon, ", ", lat, " should be numeric"), call. = FALSE)
  }

  df <- data.frame("lon" = data[[lon]], "lat" = data[[lat]])

  incircle <- haversine_loop_cpp(df, lat_center, lon_center, radius)

  incircle_df <- data[incircle$id, ]
  incircle_df$distance_m <- incircle$distance_m
  incircle_df[order(incircle_df$distance_m), ]
}



#' Filter observations within circle (vectorized)
#'
#' @description Filter all observations in a data.frame that fall within a
#' circle of a specified radius drawn around a given latitude and longitude
#' point.
#'
#' @param data data.frame with at least columns for longitude and latitude.
#' @param lon_center numeric. Representing the longitude of the circle's center.
#' @param lat_center numeric. Representing the latitude of the circle's center.
#' @param lon column name in \code{data} containing longitudes
#'     (default is \code{lon}).
#' @param lat column name in \code{data} containing latitudes
#'     (default is \code{lat}).
#' @param radius radius of the circle in meters (default is 200m).
#'
#' @author Martin Haringa
#'
#' @importFrom Rcpp sourceCpp
#' @import RcppProgress
#'
#' @examples
#' points_in_circle_vec(Groningen, lon_center = c(6.571561, 6.56561),
#' lat_center = c(53.21326, 53.20326), radius = 60)
#'
#' @return A subset of the input data.frame containing only the observations
#' that fall within the specified circle.
#'
#' @export
points_in_circle_vec <- function(data, lon_center, lat_center,
                                 lon = lon, lat = lat, radius = 200) {

  # Turn into character vector
  lon <- deparse(substitute(lon))
  lat <- deparse(substitute(lat))
  data_name <- deparse(substitute(data))

  if (!all(c(lon, lat) %in% names(data))) {
    stop(paste0(data_name, " does not contain columns ", lon, " and ", lat),
         call. = FALSE)
  }

  if (!all(is.numeric(c(data[[lon]], data[[lat]])))) {
    stop(paste0(lon, ", ", lat, " should be numeric"), call. = FALSE)
  }

  df <- data.frame("lon" = data[[lon]], "lat" = data[[lat]])
  incircle <- haversine_loop_cpp0(df, lat_center, lon_center, radius)
  data$id <- seq.int(nrow(data))
  df_join <- merge(x = incircle, y = data, by = "id", all.x = TRUE)
  x <- df_join[order(df_join$center_index, df_join$distance_m), ]
  rownames(x) <- NULL
  x
}
