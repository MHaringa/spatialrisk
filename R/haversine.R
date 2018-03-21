#' Haversine
#'
#' @param lat_from Origin latitude
#' @param lon_from Origin longitude
#' @param lat_to Destination latitude
#' @param lon_to Destination longitude
#'
#' @return A data frame with new location
#' @export haversine
#'
#' @examples
haversine <- function(lat_from, lon_from, lat_to, lon_to){
  toRad <- pi / 180
  lat_to <- lat_to * toRad
  lat_from <- lat_from * toRad
  lon_to <- lon_to * toRad
  lon_from <- lon_from * toRad
  dLat <- (lat_to - lat_from)
  dLon <- (lon_to - lon_from)
  a <- (sin(dLat / 2) ^ 2) + (cos(lat_from) * cos(lat_to)) * (sin(dLon / 2) ^ 2)
  dist <- 2 * atan2(sqrt(a), sqrt(1 - a)) * 6378137
  return(dist)
}
