#' Concentration risk
#'
#' @param data Data frame
#' @param value Column
#' @param radius Radius (in meters)
#'
#' @return value
#' @importFrom dplyr "select"
#' @importFrom purrr "pmap_dbl"
#' @importFrom magrittr "%>%"
#'
#' @examples
#' Groningen %>%
#'   filter(amount > 1000) %>%
#'   concentration_risk(., amount)
#'
#' @export
concentration_risk <- function(data, value, radius = 200)  {

  data_sub <- select(data, value = !!enquo(value), lon, lat)

  data$concentration <- purrr::pmap_dbl(data_sub, function(value, lon, lat, ...) sum_in_circle(data_sub, value, lon = lon, lat = lat, radius))

  data$concentration <- ifelse(is.na(data$concentration), value, data$concentration)

  data[ order(data$concentration, decreasing = TRUE), ]
}









