#' Points in circle
#'
#' @param data Data.
#' @param lon_center Longitude of center point.
#' @param lat_center Latitude of center point.
#' @param lon Name of column in data with longitudes (lon is default).
#' @param lat Name of column in data with latitudes (lat is default).
#' @param radius Major radius (in meters) of the circle.
#'
#' @description The points within radius of the center point (i.e. longitude, latitude).
#'
#' @author Martin Haringa
#'
#' @example
#' points_in_circle(Groningen, lon_center = 6.571561, lat_center = 53.21326, radius = 50)
#'
#' @return A data.frame of coordinates within \code{radius} around \code{(lon_center, lat_center)}. The column \code{distance_m} gives the distance from the center point (in meters).
#'
#' @export points_in_circle
points_in_circle <- function(data, lon_center, lat_center, lon = lon, lat = lat, radius = 200){

  # Turn into character vector
  lon <- deparse(substitute(lon))
  lat <- deparse(substitute(lat))

  df <- data.frame("lon" = data[[lon]], "lat" = data[[lat]])

  incircle <- haversine_loop_cpp(df, lat_center, lon_center, radius)

  incircle_df <- data[incircle$id,]
  incircle_df$distance_m <- incircle$distance_m
  incircle_df <- incircle_df[order(incircle_df$distance_m),]

  return(incircle_df)
}


















