#' Coordinates in circle
#'
#' @param data Data
#' @param lon Longitude of centre point, in degrees.
#' @param lat Latitude of centre point, in degrees.
#' @param radius Major (equatorial) radius (default is meters) of the ellipsoid. The default value is for WGS84.
#'
#' @return A data.frame of coordinates within \code{radius} around \code{(lon, lat)}.
#' @import data.table
#'
#' @export points_in_circle
points_in_circle <- function(data, lon_center, lat_center, lon = lon, lat = lat, radius = 200){
  lon <- deparse(substitute(lon))
  lat <- deparse(substitute(lat))
  data <- data.table(data)
  block <- block_around_point(lon_center, lat_center, radius)
  tryCatch({
    data_in_block <- data[lon > block[4,1] & lon < block[2,1] & lat > block[3,2] & lat < block[1,2]]
    data_in_circle <- data_in_block[, distance := haversine(lat_center, lon_center, lat, lon),
                                    by = 1:nrow(data_in_block)][distance < radius][order(distance)]
    return(data_in_circle)},
    error = function(e) NA
  )
}



















