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
#' @export block_around_coord
block_around_coord <- function(lon = lon, lat = lat, radius = 200){

  centre <- data.frame(lon = lon, lat = lat)

  centre$west <- geosphere::destPoint(c(lon, lat), 270, radius)[[1]]
  centre$east <- geosphere::destPoint(c(lon, lat), 90, radius)[[1]]
  centre$south <- geosphere::destPoint(c(lon, lat), 180, radius)[[2]]
  centre$north <- geosphere::destPoint(c(lon, lat), 360, radius)[[2]]
  return(centre)
}







