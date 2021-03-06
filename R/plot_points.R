#' Create map with points
#'
#' @description Create map for data.frame with points.
#'
#' @param df data.framw with column for lon and lat
#' @param value column in df
#' @param lon column with lon
#' @param lat column with lat
#' @param palette color palette
#' @param legend_position position for legend (default is "bottomleft")
#' @param crs crs (default is 4326)
#'
#' @return leaflet map
#'
#' @importFrom sf st_as_sf
#' @importFrom colourvalues colour_values
#' @importFrom leaflet colorNumeric
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addProviderTiles
#' @importFrom leafgl addGlPoints
#' @importFrom leaflet addLegend
#' @importFrom leaflet addLayersControl
#' @importFrom leafem addMouseCoordinates
#'
#' @examples \dontrun{
#' plot_points(Groningen, value = amount)
#' }
#'
#' @export
plot_points <- function(df, value, lon = lon, lat = lat, palette = "viridis",
                        legend_position = "bottomleft", crs = 4326){

  value_nm <- deparse(substitute(value))
  lon_nm <- deparse(substitute(lon))
  lat_nm <- deparse(substitute(lat))
  df_nm <- deparse(substitute(df))

  if ( value_nm == ""){
    stop(df_nm, " does not contain column specified in `value`. Specify with argument `value`.",
         call. = FALSE)
  }

  obj_sf <- sf::st_as_sf(df, coords = c(lon_nm, lat_nm), crs = crs)

  cols <- colourvalues::colour_values(obj_sf[[value_nm]], palette = palette)
  qpal <- leaflet::colorNumeric(palette, obj_sf[[value_nm]])

  suppressMessages({
    leaflet::leaflet() %>%

      # Base groups
      leaflet::addTiles(group = "OSM") %>%
      leaflet::addProviderTiles(providers$CartoDB.Positron, group = "Positron (default)") %>%
      leaflet::addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%

      # Overlay groups
      leafgl::addGlPoints(data = obj_sf,
                          fillColor = cols,
                          popup = TRUE,
                          group = "Points") %>%
      leaflet::addLegend(data = obj_sf,
                         pal = qpal,
                         title = value_nm,
                         values = obj_sf[[value_nm]],
                         position = legend_position,
                         group = "Points") %>%
      leafem::addMouseCoordinates() %>%

      # Layers control
      leaflet::addLayersControl(
        baseGroups = c("Positron (default)", "OSM", "Toner Lite"),
        overlayGroups = c("Points"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

}



