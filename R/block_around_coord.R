
#' Title
#'
#' @param lon
#' @param lat
#' @param radius
#'
#' @return
#' @export
#'
#' @examples
block_around_coord <- function(lon = lon, lat = lat, radius = 200){
  tibble(lon = lon, lat = lat) %>%
    mutate(west = geosphere::destPoint(c(lon, lat), 270, radius)[[1]]) %>%
    mutate(east = geosphere::destPoint(c(lon, lat), 90, radius)[[1]]) %>%
    mutate(south = geosphere::destPoint(c(lon, lat), 180, radius)[[2]]) %>%
    mutate(north = geosphere::destPoint(c(lon, lat), 360, radius)[[2]])
}




