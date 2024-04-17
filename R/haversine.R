#' Haversine great circle distance
#'
#' @description Calculates the shortest distance between two points on the
#' Earth's surface using the Haversine formula, also known as the great-circle
#' distance or "as the crow flies".
#'
#' @param lat_from Latitude of the starting point.
#' @param lon_from Longitude of the starting point.
#' @param lat_to Latitude of the destination point.
#' @param lon_to Longitude of the destination point.
#' @param r Radius of the Earth in meters (default = 6378137).
#'
#' @references Sinnott, R.W, 1984. Virtues of the Haversine. Sky and Telescope
#' 68(2): 159.
#'
#' @return Vector of distances in the same unit as \code{r} (default in meters).
#'
#' @author Martin Haringa
#'
#' @details The Haversine ('half-versed-sine') formula was published by R.W.
#' Sinnott in 1984, although it has been known for much longer.
#'
#' @examples
#' haversine(53.24007, 6.520386, 53.24054, 6.520386)
#'
#' @export
haversine <- function(lat_from, lon_from, lat_to, lon_to, r = 6378137) {

  if (!all(unlist(lapply(list(lat_from, lon_from, lat_to, lon_to),
                         is.numeric)))) {
    stop("lat_from, lon_from, lat_to, lon_to should be numeric")
  }

  dist <- haversine_cpp_vec(lat_from, lon_from, lat_to, lon_to, r)

  na_output <- sum(is.na(dist))

  if (na_output > 0) {
    message("Ignoring missing coordinates: ", sum(is.na(dist)),
            " NA's returned")
  }

  dist
}
