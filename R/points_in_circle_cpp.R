#' Coordinates in circle
#'
#' @param data Data
#' @param lon_center Longitude of center point, in degrees.
#' @param lat_center Latitude of center point, in degrees.
#' @param lon Name of column with longitudes, in degrees.
#' @param lat Name of column with latitudes, in degrees.
#' @param radius Major (equatorial) radius (default is meters) of the ellipsoid. The default value is for WGS84.
#'
#' @return A data.frame of coordinates within \code{radius} around \code{(lon, lat)}.
#' @import data.table
#'
#' @export points_in_circle
points_in_circle <- function(data, lon_center, lat_center, lon = lon, lat = lat, radius = 200){

  df <- data[, c("lon","lat")]

  incircle <- haversine_loop_cpp(df, lat_center, lon_center, radius)

  df1 <- data
  df1$id <- 1:nrow(data)
  df2 <- merge(incircle, df1, by = "id", all.x = TRUE)
  df2$id <- NULL

  return(df2)
}


















