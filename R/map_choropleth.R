#' Create choropleth map
#'
#' @description Creates a choropleth map from an `sf` object, typically produced
#' by \code{points_to_polygon()}. Polygons are shaded according to values in a
#' specified column, with clustering based on the Fisher–Jenks algorithm. This
#' method minimizes within-class variance and maximizes between-class variance,
#' making it a common choice for choropleth maps.
#'
#' @param sf_object An object of class \code{sf}.
#' @param value A string giving the name of the column used to shade the
#'   polygons.
#' @param id_name A string giving the name of the column containing polygon IDs
#'   (used for tooltips in interactive mode).
#' @param mode A string indicating whether to create a static map
#'   (\code{"plot"}, default) or an interactive map (\code{"view"}).
#' @param n Integer; number of clusters. Default is \code{7}.
#' @param legend_title A string giving the legend title.
#' @param palette A palette name or vector of colors. See
#'   \code{tmaptools::palette_explorer()} for available palettes.
#'   Prefix the name with \code{"-"} to reverse the order. Default is
#'   \code{"viridis"}.
#'
#' @details The function uses the Fisher–Jenks algorithm
#' (\code{style = "fisher"}) to classify values into \code{n} groups.
#'
#' @return A \code{tmap} object (static or interactive, depending on
#' \code{mode}).
#'
#' @importFrom tmap tmap_mode
#' @importFrom tmap tm_basemap
#' @importFrom tmap tm_compass
#' @importFrom tmap tm_layout
#' @importFrom tmap tm_polygons
#' @importFrom tmap tm_scale_bar
#' @importFrom tmap tm_shape
#' @import viridis
#'
#' @author Martin Haringa
#'
#' @examples
#' test <- points_to_polygon(nl_provincie, insurance, sum(amount, na.rm = TRUE))
#' choropleth(test)
#' choropleth(test, id_name = "areaname", mode = "view")
#'
#' @export
choropleth <- function(sf_object, value = "output", id_name = "areaname",
                       mode = "plot", n = 7, legend_title = "Clustering",
                       palette = "viridis") {

  suppressMessages(tmap::tmap_mode(mode))

  if (mode == "view") {
    output <- tmap::tm_shape(sf_object) +
      tmap::tm_polygons(value,
                        id = id_name,
                        fill.scale = tmap::tm_scale_intervals(
                          style = "fisher",
                          values = palette,
                          n = n
                        ),
                        fill.legend = tmap::tm_legend(title = legend_title),
                        fill_alpha = .5) +
      tmap::tm_basemap(c("OpenStreetMap", "Esri.WorldGrayCanvas",
                         "Esri.WorldTopoMap"))
  } else {
    output <- tmap::tm_shape(sf_object) +
      tmap::tm_polygons(value,
                        id = id_name,
                        fill.scale = tmap::tm_scale_intervals(
                          style = "fisher",
                          values = palette,
                          n = n
                        ),
                        fill.legend = tmap::tm_legend(title = legend_title),
                        lwd = .1) +
      tmap::tm_compass(position = c("right", "bottom")) +
      tmap::tm_scalebar(position = c("left", "bottom")) +
      tmap::tm_layout(frame = FALSE)
  }

  output
}
