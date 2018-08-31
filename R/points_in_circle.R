#' Points in circle
#'
#' @param data Data
#' @param lon_center Longitude of center point, in degrees.
#' @param lat_center Latitude of center point, in degrees.
#' @param lon Name of column in data with longitudes. The default name is lon.
#' @param lat Name of column in data with latitudes. The default name is lat.
#' @param radius Major (equatorial) radius (default is meters) of the ellipsoid. The default value is for WGS84.
#'
#' @description The points (e.g. addresses) within radius of the center point (i.e. longitude, latitude).
#'
#' @author Martin Haringa
#'
#' @return A data.frame of coordinates within \code{radius} around \code{(lon_center, lat_center)}.
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


















