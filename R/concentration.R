#' Concentration
#'
#' @param data Data frame
#' @param value Column name with value
#' @param radius Radius (in meters) (default is 200m)
#' @param lon Column name with longitude (lon is default)
#' @param lat Column name with latitude (lat is default)
#'
#' @return value
#' @import data.table
#'
#' @examples
#' concentration(Groningen[1:10, ], amount, radius = 1000, lon = lon, lat = lat)
#'
#' @export
concentration <- function(data, value, radius = 200, lon = lon, lat = lat){
  dt <- data.table(data)

  value <- deparse(substitute(value))
  lon <- deparse(substitute(lon))
  lat <- deparse(substitute(lat))

  setnames(dt, c(value, lon, lat), c("value", "lon", "lat"))

  concentration <- dt[, concentration := sum_in_circle(dt, value = value, lon_center = lon, lat_center = lat, radius = radius),
                      by = 1:nrow(dt)][order(-concentration)]

  setnames(concentration, c("value", "lon", "lat"), c(value, lon, lat))
  return(concentration)
}










