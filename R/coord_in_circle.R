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
#' @export
coord_in_circle <- function(data, lon = lon, lat = lat, radius = 200){
  centre <- data.table(lon = lon, lat = lat)
  block <- block_around_coord(lon, lat, radius)
  data_table <- data.table(data)
  df <- data_table[lon > block$west & lon < block$east & lat > block$south & lat < block$north]
  df[, distance_from_centre := dt.haversine(centre$lat, centre$lon, lat, lon), by = 1:nrow(df)]
  df[distance_from_centre < 200]
  data.frame(df[order(distance_from_centre)])
}



















