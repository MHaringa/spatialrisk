#' Concentration risk
#'
#' @param data Data frame
#' @param value Column
#' @param radius Radius (in meters)
#'
#' @return value
#' @importFrom dplyr "select"
#' @importFrom purrr "pmap_dbl"
#' @importFrom rlang "enquo"
#'
#' @examples
#' concentration_risk(Groningen[1:10, ], amount, radius = 10)
#'
#' @export
concentration_risk <- function(data, value, radius = 200)  {

  data_sub <- select(data, value = !!enquo(value))

  data_sub$lon <- data$lon
  data_sub$lat <- data$lat

  data$concentration <- purrr::pmap_dbl(data_sub, function(value, lon, lat, ...) sum_in_circle(data_sub, value, lon_center = lon, lat_center = lat, radius = radius))

  data$concentration <- ifelse(is.na(data$concentration), value, data$concentration)

  data[ order(data$concentration, decreasing = TRUE), ]
}









