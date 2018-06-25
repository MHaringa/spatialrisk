#' Coordinates in circle
#'
#' @param data Data
#' @param lon_center Longitude of center point, in degrees.
#' @param lat_center Latitude of center point, in degrees.
#' @param lon Name of column with longitudes, in degrees.
#' @param lat Name of column with latitudes, in degrees.
#' @param radius Major (equatorial) radius (default is meters) of the ellipsoid. The default value is for WGS84.
#'
#' @return A data.frame of coordinates within \code{radius} around \code{(lon, lat)}.
#' @import data.table
#'
#' @export points_in_circle
points_in_circle <- function(data, lon_center, lat_center, lon = lon, lat = lat, radius = 200){

  # Turn into character vector
  lon <- deparse(substitute(lon))
  lat <- deparse(substitute(lat))

  # Create data.table
  data <- data.table(data)

  # Calculate coordinates of the four cardinal directions
  block <- block_around_point(lon_center, lat_center, radius)

  # Error handling: return NA in case no points available within radius from center
  tryCatch({

    # A simplified "pre-subsetting" before applying the Haversine formula
    data_in_block <- data[lon > block[[3]] & lon < block[[4]] & lat > block[[1]] & lat < block[[2]]]

    # Apply Haversine formula to points in square around center
    data_in_circle <- data_in_block[, distance := haversine(lat_center, lon_center, lat, lon),
                                    by = 1:nrow(data_in_block)][distance < radius][order(distance)]
    return(data_in_circle)},
    error = function(e) NA
  )
}


















