#' Coordinates of the four cardinal directions from centre
#'
#' @param lon Longitude of point(s), in degrees.
#' @param lat Latitude of point(s), in degrees.
#' @param radius Major (equatorial) radius (default is meters) of the ellipsoid. The default value is for WGS84.
#'
#' @return A data.frame consisting of the four coordinates (long,lat) obtained by walking the radius to the four cardinal directions.
#' @importFrom geosphere "destPoint"
#'
#' @examples
#' block_around_coord(lon = 6.520386, lat = 53.24007, radius = 200)
#' @export block_around_point
block_around_point <- function(lon_center, lat_center, radius = 200){
  north <- 0
  east <- 90
  south <- 180
  west <- 270
  return(geosphere::destPoint(c(lon_center, lat_center), c(north, east, south, west), radius))
}



