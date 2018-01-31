#' Coordinates in circle
#'
#' @param data Data
#' @param lon Longitude of centre point, in degrees.
#' @param lat Latitude of centre point, in degrees.
#' @param radius Major (equatorial) radius (default is meters) of the ellipsoid. The default value is for WGS84.
#'
#' @return A data.frame of coordinates within \code{radius} around \code{(lon, lat)}.
#' @importFrom dplyr "filter"
#' @importFrom dplyr "arrange"
#' @importFrom purrr "pmap_dbl"
#' @import geosphere
#'
#' @examples
#' coord_in_circle(Groningen, lon = 6.5203, lat = 53.24007, radius = 200)
#' @export
coord_in_circle <- function(data, lon = lon, lat = lat, radius = 200){

  if (sum(names(data) %in% c("lon", "lat")) < 2) stop("Data does not have latitude and longitude information included.")

  centre <- data.frame(lon = lon, lat = lat)

  block <- block_around_coord(lon, lat, radius)

  data %>%
    dplyr::filter(lon > block$west & lon < block$east) %>%
    dplyr::filter(lat > block$south & lat < block$north) %>%
    mutate(distanceFromCentre = purrr::pmap_dbl(., function(lon, lat, ...)
      geosphere::distHaversine(c(lon, lat), centre))) %>%
    dplyr::filter(.$distanceFromCentre < radius) %>%
    arrange(distanceFromCentre)
}













