#' Title
#'
#' @param data
#' @param value
#' @param lon
#' @param lat
#' @param radius
#'
#' @return
#' @export
#'
#' @examples
sum_in_circle <- function(data, value, lon = lon, lat = lat, radius = 200){

  value <- enquo(value)
  tryCatch(
    coord_in_circle(data, lon = lon, lat = lat, radius)  %>%
      select(!!value) %>%
      sum(., na.rm = TRUE),
    error = function(e) NA
  )
}
