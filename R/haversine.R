#' Haversine great-circle distance
#'
#' @description Calculates the shortest distance between two points on the
#' Earth's surface using the Haversine formula, also known as the great-circle
#' distance or "as the crow flies".
#'
#' @param lat_from Numeric. Latitude(s) of the starting point(s) in decimal
#'   degrees (EPSG:4326).
#' @param lon_from Numeric. Longitude(s) of the starting point(s) in decimal
#'   degrees (EPSG:4326).
#' @param lat_to Numeric. Latitude(s) of the destination point(s) in decimal
#'   degrees (EPSG:4326).
#' @param lon_to Numeric. Longitude(s) of the destination point(s) in decimal
#'   degrees (EPSG:4326).
#' @param r Numeric. Radius of the Earth in meters (default = 6378137).
#'
#' @references Sinnott, R.W, 1984. Virtues of the Haversine. Sky and Telescope
#'   68(2): 159.
#'
#' @return A numeric vector with distances in the same unit as `r`
#'   (default: meters).
#'
#' @details This function is vectorized: if multiple coordinates are supplied,
#' it returns one distance for each corresponding pair of points.
#'
#' @examples
#' haversine(53.24007, 6.520386, 53.24054, 6.520386)
#'
#' lat_from <- c(53.24, 52.37)
#' lon_from <- c(6.52, 4.90)
#' lat_to   <- c(48.85, 51.92)
#' lon_to   <- c(2.35, 4.48)
#' haversine(lat_from, lon_from, lat_to, lon_to)
#'
#' @export
haversine <- function(lat_from, lon_from, lat_to, lon_to, r = 6378137) {

  check_coordinate_vectors(
    lat_from = lat_from,
    lon_from = lon_from,
    lat_to = lat_to,
    lon_to = lon_to
  )

  check_earth_radius(r)

  haversine_cpp_vec(
    lat_from = lat_from,
    lon_from = lon_from,
    lat_to = lat_to,
    lon_to = lon_to,
    r = r
  )
}
