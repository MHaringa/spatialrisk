#' Haversine great circle distance
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
#' 68(2): 159.
#'
#' @return A numeric vector with distances in the same unit as \code{r}
#'   (default in meters).
#'
#' @author Martin Haringa
#'
#' @details The Haversine ('half-versed-sine') formula was published by R.W.
#' Sinnott in 1984, although it has been known for much longer.
#'
#' This function is fully vectorized: if multiple coordinates are supplied,
#' it returns a distance for each pair of points.
#'
#' @examples
#' # Single pair
#' haversine(53.24007, 6.520386, 53.24054, 6.520386)
#'
#' # Vectorized usage
#' lat_from <- c(53.24, 52.37)
#' lon_from <- c(6.52, 4.90)
#' lat_to   <- c(48.85, 51.92)
#' lon_to   <- c(2.35, 4.48)
#' haversine(lat_from, lon_from, lat_to, lon_to)
#'
#' @export
haversine <- function(lat_from, lon_from, lat_to, lon_to, r = 6378137) {

  # Type check
  if (!all(unlist(lapply(list(lat_from, lon_from, lat_to, lon_to),
                         is.numeric)))) {
    stop("lat_from, lon_from, lat_to, lon_to must be numeric.", call. = FALSE)
  }

  # Length check
  if (!all(lengths(list(lat_from, lon_from, lat_to,
                        lon_to)) == length(lat_from))) {
    stop("All coordinate vectors must have the same length.", call. = FALSE)
  }

  # Compute distance
  dist <- haversine_cpp_vec(lat_from, lon_from, lat_to, lon_to, r)

  # Handle NA's
  na_output <- sum(is.na(dist))
  if (na_output > 0) {
    warning(na_output,
            " coordinate pairs are missing (NA). Results may be incomplete.")
  }

  dist
}
