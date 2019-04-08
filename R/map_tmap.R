#' Map object of class sf using tmap
#'
#' @description Takes an object produced by \code{choropleth_sf()}, and creates the correspoding choropleth map.
#'
#' @param sf_object object of class sf
#' @param value column name to shade the polygons
#' @param id_name column name of ids to plot
#' @param mode choose between static ('plot' is default) and interactive map ('view')
#' @param legend_title title of legend
#' @param bgcolor background color (default is "gray")
#'
#' @return ggplot map
#' @export choropleth_tmap
#'
#' @import sf
#' @import tmap
#' @import viridis
#'
#' @author Martin Haringa
#'
#' @examples
#' test <- choropleth_sf(nl_provincie, insurance, sum(amount, na.rm = TRUE))
#' choropleth_tmap(test)
#' choropleth_tmap(test, id_name = "province_name", mode = "view")
choropleth_tmap <- function(sf_object, value = "output", id_name = "id", mode = "plot", legend_title = "Clustering", bgcolor = "gray"){

  if (mode == "view"){
    suppressMessages({
      tmap_mode("view")
    })

    output <- tm_shape(sf_object) +
      tm_polygons(value,
                  id = id_name,
                  palette = "viridis",
                  style = "fisher",
                  title = legend_title,
                  alpha = .5)
  }

  else{
    suppressMessages({
      tmap_mode("plot")
    })
    output <- tm_shape(sf_object) +
      tm_polygons(value,
                  id = id_name,
                  palette = "viridis",
                  style = "fisher",
                  title = legend_title,
                  border.col = "white",
                  lwd = .1) +
      tm_compass(position = c("right", "bottom")) +
      tm_scale_bar(position = c("left", "bottom")) +
      tm_style(bgcolor)
  }

  return(output)
}
