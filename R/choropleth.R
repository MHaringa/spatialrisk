#' Create choropleth map
#'
#' @description Takes an object produced by \code{points_to_polygon()}, and creates the corresponding choropleth map.
#' The given clustering is according to the Fisher-Jenks algorithm. This commonly used method for choropleths seeks to reduce
#' the variance within classes and maximize the variance between classes.
#'
#' @param sf_object object of class sf
#' @param value column name to shade the polygons
#' @param id_name column name of ids to plot
#' @param mode choose between static ('plot' is default) and interactive map ('view')
#' @param n number of clusters (default is 7)
#' @param legend_title title of legend
#' @param palette palette name or a vector of colors. See tmaptools::palette_explorer() for the named palettes. Use a "-" as prefix to reverse the palette. The default palette is "viridis".
#'
#' @return tmap
#' @export choropleth
#'
#' @importFrom tmap tmap_mode
#' @importFrom tmap tm_shape
#' @importFrom tmap tm_polygons
#' @importFrom tmap tm_basemap
#' @importFrom tmap tm_compass
#' @importFrom tmap tm_scale_bar
#' @importFrom tmap tm_layout
#' @import viridis
#'
#' @author Martin Haringa
#'
#' @examples
#' test <- points_to_polygon(nl_provincie, insurance, sum(amount, na.rm = TRUE))
#' choropleth(test)
#' choropleth(test, id_name = "areaname", mode = "view")
choropleth <- function(sf_object, value = "output", id_name = "areaname",
                            mode = "plot", n = 7, legend_title = "Clustering",
                            palette = "viridis"){

  if (!requireNamespace("tmap", quietly = TRUE)) {
    stop("tmap is needed for this function to work. Install it via install.packages(\"tmap\")", call. = FALSE)
  }

  if (mode == "view"){
    suppressMessages({
      tmap::tmap_mode("view")
    })

    output <- tmap::tm_shape(sf_object) +
      tmap::tm_polygons(value,
                        id = id_name,
                        palette = palette,
                        style = "fisher",
                        n = n,
                        title = legend_title,
                        alpha = .5) +
      tmap::tm_basemap(c("OpenStreetMap", "Esri.WorldGrayCanvas", "Esri.WorldTopoMap"))
  }

  else{
    suppressMessages({
      tmap::tmap_mode("plot")
    })
    output <- tmap::tm_shape(sf_object) +
      tmap::tm_polygons(value,
                        id = id_name,
                        palette = palette,
                        style = "fisher",
                        title = legend_title,
                        n = n,
                        lwd = .1) +
      tmap::tm_compass(position = c("right", "bottom")) +
      tmap::tm_scale_bar(position = c("left", "bottom")) +
      tmap::tm_layout(frame = FALSE)
  }

  return(output)
}
