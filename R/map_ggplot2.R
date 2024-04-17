#' Map object of class sf using ggplot2
#'
#' @description Takes an object produced by \code{choropleth_sf()}, and creates
#' the correspoding choropleth map.
#'
#' @param sf_object object of class sf
#' @param value column to shade the polygons
#' @param n number of clusters (default is 7)
#' @param dig.lab number of digits in legend (default is 2)
#' @param legend_title title of legend
#' @param option a character string indicating the colormap option to use. Four
#' options are available: "magma" (or "A"), "inferno" (or "B"), "plasma"
#' (or "C"), "viridis" (or "D", the default option) and "cividis" (or "E").
#' @param direction Sets the order of colors in the scale. If \code{1},
#' the default, colors are ordered from darkest to lightest. If \code{-1},
#' the order of colors is reversed.
#'
#' @return ggplot map
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 coord_sf
#' @importFrom ggplot2 scale_fill_viridis_d
#' @importFrom ggplot2 theme_void
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_viridis_c
#' @importFrom ggplot2 aes
#' @importFrom classInt classIntervals
#'
#' @author Martin Haringa
#'
#' @examples
#' test <- points_to_polygon(nl_postcode2, insurance, sum(amount, na.rm = TRUE))
#' choropleth_ggplot2(test)
#'
#' @export
choropleth_ggplot2 <- function(sf_object, value = output, n = 7, dig.lab = 2,
                               legend_title = "Class", option = "D",
                               direction = 1) {

  value <- deparse(substitute(value))
  vector_value <- sf_object[[value]]

  result <- tryCatch(
    {
      suppressWarnings(
        {
          cluster <- classInt::classIntervals(vector_value, n = n,
                                              style = "fisher",
                                              intervalClosure = "right")[[2]]
          sf_object$clustering <- cut(vector_value, breaks = cluster,
                                      include.lowest = TRUE, dig.lab = dig.lab)
        }
      )

      ggplot2::ggplot(sf_object) +
        ggplot2::geom_sf(ggplot2::aes(fill = clustering), size = .1,
                         color = "grey85")  +
        ggplot2::coord_sf(datum = NA) +
        ggplot2::scale_fill_viridis_d(direction = direction, option = option) +
        ggplot2::theme_void() +
        ggplot2::labs(fill = legend_title)
    },
    error = function(e) {
      ggplot2::ggplot(sf_object) +
        ggplot2::geom_sf(ggplot2::aes(fill = vector_value), size = .1,
                         color = "grey85")  +
        ggplot2::coord_sf(datum = NA) +
        ggplot2::scale_fill_viridis_c(direction = direction, option = option) +
        ggplot2::theme_void() +
        ggplot2::labs(fill = legend_title)
    }
  )

  result
}
