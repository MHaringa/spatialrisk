#' Haversine great circle distance
#'
#' @param lat_from Origin latitude
#' @param lon_from Origin longitude
#' @param lat_to Destination latitude
#' @param lon_to Destination longitude
#'
#' @return Distance in meters
#' @description The shortest distance between two points (i.e., the 'great-circle-distance' or 'as the crow flies'),
#' according to the 'haversine method'. This method assumes a spherical earth, ignoring ellipsoidal effects. Note
#' that this version is implemented in C++. A quick benchmark to the version of geosphere showed it to be a non-insignificant speed enhancement.
#'
#'
#' @details The Haversine ('half-versed-sine') formula was published by R.W. Sinnott in 1984, although it has been known for much longer.
#'
#' @examples
#' haversine(53.24007, 6.520386, 53.24054, 6.520386)
#'
#' @export
haversine <- function(lat_from, lon_from, lat_to, lon_to){

  if( lat_from > 90 | lat_to > 90) warning('latitude > 90')
  if( lon_from > 360 | lon_to > 360) warning('longitude > 360')

  dist <- haversine_cpp(lat_from, lon_from, lat_to, lon_to)
  return(dist)
}


