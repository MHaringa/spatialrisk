#' Points in circle
#'
#' @description All observations within circle of a certain radius.
#'
#' @param data data.frame with at least columns for longitude and latitude.
#' @param lon_center numeric value referencing to the longitude of the center of the circle
#' @param lat_center numeric value referencing to the latitude of the center of the circle
#' @param lon column name in \code{data} with longitudes (\code{lon} is default).
#' @param lat column name in \code{data} with latitudes (\code{lat} is default).
#' @param radius radius (in meters) (defaults to 200m).
#'
#' @author Martin Haringa
#'
#' @importFrom Rcpp sourceCpp
#' @import RcppProgress
#'
#' @examples
#' points_in_circle(Groningen, lon_center = 6.571561, lat_center = 53.21326, radius = 50)
#'
#' @return data.frame. Column \code{distance_m} gives the distance to the center of the circle (in meters).
#'
#' @export points_in_circle
points_in_circle <- function(data, lon_center, lat_center, lon = lon, lat = lat, radius = 200){

  # Turn into character vector
  lon <- deparse(substitute(lon))
  lat <- deparse(substitute(lat))
  data_name <- deparse(substitute(data))

  if ( !all(c(lon, lat) %in% names(data))) {
    stop(paste0(data_name, " does not contain columns ", lon, " and ", lat))
  }

  if ( !all(is.numeric(c(data[[lon]], data[[lat]]))) ){
    stop(paste0(lon, ", ", lat, " should be numeric"))
  }

  df <- data.frame("lon" = data[[lon]], "lat" = data[[lat]])

  incircle <- haversine_loop_cpp(df, lat_center, lon_center, radius)

  incircle_df <- data[incircle$id,]
  incircle_df$distance_m <- incircle$distance_m
  incircle_df <- incircle_df[order(incircle_df$distance_m),]

  return(incircle_df)
}


















