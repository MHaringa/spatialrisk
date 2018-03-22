#' Concentration
#'
#' @param data Data frame
#' @param value Column name
#' @param radius Radius (in meters)
#'
#' @return value
#' @importFrom data.table
#'
#' @examples
#' concentration(Groningen[1:10, ], amount, radius = 1000)
#'
#' @export
concentration <- function(data, value, radius = 200){
  dt <- data.table(data)
  value <- deparse(substitute(value))
  setnames(dt, value, "value")
  concentration <- dt[, concentration := sum_in_circle(dt, value = value, lon_center = lon, lat_center = lat, radius = radius),
                      by = 1:nrow(dt)][order(-concentration)]
  return(concentration)
}









