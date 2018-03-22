#' Coordinates of the four cardinal directions from centre
#'
#' @param lon_center Longitude of center point, in degrees.
#' @param lat_center Latitude of center point, in degrees.
#' @param radius Major (equatorial) radius (default is meters) of the ellipsoid. The default value is for WGS84.
#'
#' @return A data.frame consisting of the four coordinates (long,lat) obtained by walking the radius to the four cardinal directions.
#' @importFrom geosphere "destPoint"
#'
#' @examples
#' block_around_point(lon_center = 6.520386, lat_center = 53.24007, radius = 200)
#' @export block_around_point
block_around_point <- function(lon_center, lat_center, radius = 200){

  # Absolute bearing (i.e. angle away (clockwise) from North)
  north <- 0
  east <- 90
  south <- 180
  west <- 270

  return(geosphere::destPoint(c(lon_center, lat_center), c(north, east, south, west), radius))
}



