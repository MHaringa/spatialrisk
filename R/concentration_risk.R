#' Concentration risk
#'
#' @param data Data frame
#' @param value Column
#' @param radius Radius (in meters)
#'
#' @return value
#' @importFrom dplyr "filter"
#' @importFrom dplyr "mutate"
#' @importFrom dplyr "arrange"
#' @importFrom purrr "pmap_dbl"
#' @importFrom magrittr "%>%"
#'
#' @examples
#' Groningen %>%
#'   filter(value > 1000) %>%
#'   concentration_risk(., value)
#'
#' @export
concentration_risk <- function(data, value, radius = 200)  {
  value <- enquo(value)
  data %>%
    mutate(concentration = purrr::pmap_dbl(., function(value, lon, lat, ...)
      sum_in_circle(!!value, lon = lon, lat = lat, radius))) %>%
    mutate(concentration = ifelse(is.na(.$concentration), UQ(value),
                                  .$concentration)) %>%
    arrange(desc(.$concentration))
}










