#' Title
#'
#' @param data Data frame
#' @param value waarde
#' @param lon longitude
#' @param lat latitude
#' @param radius radius in meters
#'
#' @return waarde
#'
#'
#' @examples sum_in_circle(Groningen, value, 6.520386, 53.24007)
#' @export sum_in_circle
sum_in_circle <- function(data, value, lon_center, lat_center, lon = lon, lat = lat, radius = 200){
  value <- deparse(substitute(value))
  lon <- deparse(substitute(lon))
  lat <- deparse(substitute(lat))
  data <- data.table(data)
  summation <- points_in_circle(data, lon_center, lat_center, lon, lat, radius)
  tryCatch(
    sum(summation[, value, with = FALSE][[1]], na.rm = TRUE),
    error = function(e) NA
  )
}



