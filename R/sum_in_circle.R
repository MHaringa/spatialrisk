#' Sum in circle
#'
#' @param data Data frame
#' @param value waarde
#' @param lon_center Longitude of center point, in degrees.
#' @param lat_center Latitude of center point, in degrees.
#' @param lon Name of column with longitudes, in degrees.
#' @param lat Name of column with latitudes, in degrees.
#' @param radius Radius in meters
#'
#' @return waarde
#' @import data.table
#'
#'
#' @examples sum_in_circle(Groningen, value, 6.520386, 53.24007)
#' @export sum_in_circle
sum_in_circle <- function(data, value, lon_center, lat_center, lon = lon, lat = lat, radius = 200){

  # Turn into character vector
  value <- deparse(substitute(value))
  lon <- deparse(substitute(lon))
  lat <- deparse(substitute(lat))

  # Create data.table
  data <- data.table(data)

  # Determine points within radius from center
  summation <- points_in_circle(data, lon_center, lat_center, lon, lat, radius)

  # Error handling: return NA in case no points available within radius from center
  tryCatch(
    sum(summation[, value, with = FALSE][[1]], na.rm = TRUE),
    error = function(e) NA
  )
}



