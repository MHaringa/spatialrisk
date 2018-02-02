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
#' @importFrom dplyr "select"
#'
#' @examples sum_in_circle(Groningen, value, 6.520386, 53.24007)
#' @export sum_in_circle
sum_in_circle <- function(data, value, lon, lat, radius = 200){

  value <- deparse(substitute(value))

  # if(!value %in% names(data)) stop("Data does not have this column included.")

  df <- coord_in_circle(data, lon = lon, lat = lat, radius)

  tryCatch(
    sum( df[ , value ], na.rm = TRUE ),
    error = function(e) NA
  )
}

sum_in_circle(Groningen, amount, 6.520386, 53.2400, 100)


