#' Choropleth map of an sf object with ggplot2
#'
#' @description Creates a choropleth map from an object of class \code{sf}.
#' If the chosen variable can be classified into discrete intervals using
#' Fisher's natural breaks, the polygons are shaded by cluster. Otherwise,
#' the variable is visualized on a continuous scale.
#'
#' @param sf_object An object of class \code{sf} containing polygon geometries.
#' @param value Column in \code{sf_object} used to shade the polygons
#'   (default = \code{output}).
#' @param n Integer. Number of clusters to use in Fisher classification
#'   (default = 7).
#' @param dig.lab Integer. Number of digits to display in legend labels
#'   (default = 2).
#' @param legend_title Character. Title for the legend (default = "Class").
#' @param option Character string indicating the colormap option passed to
#'   \code{viridis}. Options are:
#'   \itemize{
#'     \item \code{"magma"} (or \code{"A"})
#'     \item \code{"inferno"} (or \code{"B"})
#'     \item \code{"plasma"} (or \code{"C"})
#'     \item \code{"viridis"} (or \code{"D"}, default)
#'     \item \code{"cividis"} (or \code{"E"})
#'   }
#' @param direction Numeric. Order of colors in the scale.
#'   If \code{1} (default), colors go from darkest to lightest.
#'   If \code{-1}, the order is reversed.
#'
#' @return A \code{ggplot} object containing the choropleth map.
#'
#' @details
#' The function first attempts to classify the chosen variable into
#' \code{n} clusters using Fisher's natural breaks
#' (\code{classInt::classIntervals()}). If this fails (e.g. due to insufficient
#' unique values), the function falls back to a continuous scale.
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
#' test <- points_to_polygon(
#'   nl_postcode2,
#'   insurance,
#'   sum(amount, na.rm = TRUE)
#' )
#' choropleth_ggplot2(test, value = output)
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
