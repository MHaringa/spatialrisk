#' Coordinates of the four cardinal directions from center
#'
#' @param lon_center Longitude of center point, in degrees.
#' @param lat_center Latitude of center point, in degrees.
#' @param radius Major (equatorial) radius (default is meters) of the ellipsoid. The default value is for WGS84.
#'
#' @return A data.frame consisting of the four coordinates (long,lat) obtained by walking the radius to the four cardinal directions.
#'
#' @examples
#' block_around_point(lon_center = 6.520386, lat_center = 53.24007, radius = 200)
#' @export block_around_point
block_around_point <- function(lon_center, lat_center, radius = 200){
  circumference_earth_in_meters <- 40075000
  toRad <- pi / 180
  buffer <- radius + 1
  one_lat_in_meters <- circumference_earth_in_meters / 360
  one_lon_in_meters <- circumference_earth_in_meters * cos(lat_center * toRad) / 360
  south_lat <- lat_center - buffer / one_lat_in_meters
  north_lat <- lat_center + buffer / one_lat_in_meters
  west_lon <- lon_center - buffer / one_lon_in_meters
  east_lon <- lon_center + buffer / one_lon_in_meters
  return(c(south_lat, north_lat, west_lon, east_lon))
}





