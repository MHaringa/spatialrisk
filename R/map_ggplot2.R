#' Map object of class sf using ggplot2
#'
#' @description Takes an object produced by \code{choropleth_sf()}, and creates the correspoding choropleth map.
#'
#' @param sf_object object of class sf
#' @param value column to shade the polygons
#' @param n number of clusters (default is 7)
#' @param dig.lab number of digits in legend (default is 2)
#' @param legend_title title of legend
#'
#' @return ggplot map
#' @export choropleth_ggplot2
#'
#' @import sf
#' @import ggplot2
#' @import viridis
#' @import classInt
#'
#' @author Martin Haringa
#'
#' @examples
#' test <- choropleth_sf(nl_postcode2, insurance, sum(amount, na.rm = TRUE))
#' choropleth_ggplot2(test)
choropleth_ggplot2 <- function(sf_object, value = output, n = 7, dig.lab = 2, legend_title = "Class"){

  value <- deparse(substitute(value))
  vector_value <- sf_object[[value]]

  result <- tryCatch(
    {
      suppressWarnings({
        cluster <- classIntervals(vector_value, n = n, style = 'fisher', intervalClosure = 'right')[[2]]
        sf_object$clustering <- cut(vector_value, breaks = cluster, include.lowest = TRUE, dig.lab = dig.lab)
      })

      ggplot(sf_object) +
        geom_sf(aes(fill = clustering), size = .1, color = "grey85")  +
        coord_sf(datum = NA) +
        scale_fill_viridis_d() +
        theme_void() +
        labs(fill = legend_title)
    },
    error = function(e) {

      ggplot(sf_object) +
        geom_sf(aes(fill = vector_value), size = .1, color = "grey85")  +
        coord_sf(datum = NA) +
        scale_fill_viridis_c() +
        theme_void() +
        labs(fill = legend_title)
    })

  return(result)
}

