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

  value <- enquo(value)
  tryCatch(
    coord_in_circle(data, lon = lon, lat = lat, radius)  %>%
      select(!!value) %>%
      sum(., na.rm = TRUE),
    error = function(e) NA
  )
}
