#' Coordinates of the four cardinal directions from centre
#'
#' @param lon Longitude of point(s), in degrees.
#' @param lat Latitude of point(s), in degrees.
#' @param radius Major (equatorial) radius (default is meters) of the ellipsoid. The default value is for WGS84.
#'
#' @return A data.frame consisting of the four coordinates (long,lat) obtained by walking the radius to the four cardinal directions.
#' @export
#'
#' @examples
#' block_around_coord(lon = 6.520386, lat = 53.24007, radius = 200)
block_around_coord <- function(lon = lon, lat = lat, radius = 200){

  tibble(lon = lon, lat = lat) %>%
    mutate(west = geosphere::destPoint(c(lon, lat), 270, radius)[[1]]) %>%
    mutate(east = geosphere::destPoint(c(lon, lat), 90, radius)[[1]]) %>%
    mutate(south = geosphere::destPoint(c(lon, lat), 180, radius)[[2]]) %>%
    mutate(north = geosphere::destPoint(c(lon, lat), 360, radius)[[2]])
}






