#' Map object of class sf using tmap (deprecated function; use 'choropleth' instead)
#'
#' @description Takes an object produced by \code{choropleth_sf()}, and creates the correspoding choropleth map.
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
#' @export choropleth_tmap
#'
#'
#' @author Martin Haringa
choropleth_tmap <- function(sf_object, value = "output", id_name = "areaname",
                            mode = "plot", n = 7, legend_title = "Clustering",
                            palette = "viridis"){

  .Deprecated("choropleth")
  choropleth(sf_object = sf_object, value = value, id_name = id_name,
             mode = mode, n = n, legend_title = legend_title,
             palette = palette)
}
