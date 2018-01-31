#' Title
#'
#' @param data
#' @param value
#' @param radius
#'
#' @return
#' @export
#'
#' @examples
#' Groningen %>%
#'   filter(value > 1000) %>%
#'   concentration_risk(., value)
#'
concentration_risk <- function(data, value, radius = 200)  {
  value <- enquo(value)
  data %>%
    mutate(concentration = purrr::pmap_dbl(., function(value, lon, lat, ...)
      sum_in_circle(., !!value, lon = lon, lat = lat, radius))) %>%
    mutate(concentration = ifelse(is.na(concentration), UQ(value),
                                  concentration)) %>%
    arrange(desc(concentration))
}











