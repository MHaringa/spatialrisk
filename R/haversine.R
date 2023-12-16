#' Haversine great circle distance
#'
#' @description The shortest distance between two points (i.e., the
#' 'great-circle-distance' or 'as the crow flies'), according to the 'haversine
#' method'. This method assumes a spherical earth, ignoring ellipsoidal effects.
#' Note that this version is implemented in C++. A quick benchmark to the
#' version of geosphere showed it to be a non-insignificant speed enhancement.
#' The algorithm converges in one-twentieth of the original time.
#'
#' @param lat_from Latitude of point.
#' @param lon_from Longitude of point.
#' @param lat_to Latitude of point.
#' @param lon_to Longitude of point.
#' @param r Radius of the earth; default = 6378137m
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
haversine <- function(lat_from, lon_from, lat_to, lon_to, r = 6378137){

  if (!all(unlist(lapply(list(lat_from, lon_from, lat_to, lon_to),
                         is.numeric)))) {
    stop("lat_from, lon_from, lat_to, lon_to should be numeric")
  }

  dist <- haversine_cpp_vec(lat_from, lon_from, lat_to, lon_to, r)

  na_output <- sum(is.na(dist))

  if ( na_output > 0 ){
    message("Ignoring missing coordinates: ", sum(is.na(dist)),
            " NA's returned")
  }

  return(dist)
}


