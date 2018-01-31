#' Coordinates in circle
#'
#' @param lon
#' @param lat
#' @param radius
#'
#' @return
#' @export
#'
#' @examples
coord_in_circle <- function(data, lon = lon, lat = lat, radius = 200){

  if (all(names(data) %in% c("lon", "lat")) == FALSE) stop("Data does not have latitude and longitude information included.")

  centre <- data.frame(lon = lon, lat = lat)

  block <- block_around_coord(lon, lat, radius)

  data %>%
    filter(., lon > block$west & lon < block$east) %>%
    filter(., lat > block$south & lat < block$north) %>%
    mutate(distanceFromCentre = purrr::pmap_dbl(., function(lon, lat)
      geosphere::distHaversine(c(lon, lat), centre))) %>%
    filter(., distanceFromCentre < radius)
}



