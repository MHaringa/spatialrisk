#' Aggregate attributes of coordinates to area level
#'
#' @description A data.frame containing coordinates (in terms of longitude and latitude) is joined to the polygon level.
#'    Then arithmetic operations on the attributes of the coordinates are applied to obtain aggregated values for each polygon.
#'
#' @param sf_map object of class sf
#' @param df data.frame containing coordinates (column names should be 'lon' and 'lat')
#' @param oper an arithmetic operation on the polygon level
#' @param crs coordinate reference system: integer with the EPSG code, or character with proj4string
#' @param outside_print print points that are not within a polygon (default is FALSE).
#'
#' @export choropleth_sf
#'
#' @import sf
#' @import dplyr
#' @importFrom utils capture.output
#'
#' @return an object of class sf
#'
#' @author Martin Haringa
#'
#' @examples
#' choropleth_sf(nl_postcode2, insurance, sum(amount, na.rm = TRUE))
#' \dontrun{
#' shp_read <- sf::st_read(~/path/to/file.shp)
#' choropleth_sf(shp_read, insurance, sum(amount, na.rm = TRUE))
#'}
choropleth_sf <- function(sf_map, df, oper, crs = 4326, outside_print = FALSE){

  oper <- enquo(oper)

  shp_wgs84 <- tryCatch(
    {
      sf_map %>%
        st_transform(crs = crs) %>% # Convert coordinates to WGS84
        dplyr::mutate(id = 1:nrow(sf_map))
    },
    error = function(e) {
      sf_map %>%
        sf::st_buffer(0) %>% # Make invalid geometries valid
        st_transform(crs = crs) %>% # Convert coordinates to WGS84
        dplyr::mutate(id = 1:nrow(sf_map))
    })

  if( !all(c("lon", "lat") %in% names(df)) ) {stop("Data.frame should contain column names 'lon' and 'lat'.")}

  df_sf <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = crs)

  suppressMessages({
    df_map_sf <- sf::st_join(shp_wgs84, df_sf)
    outside <- df_sf[!lengths(sf::st_intersects(df_sf, shp_wgs84)), ]
  })

  if( nrow(outside) > 0 ){
    if( isTRUE(outside_print)){
      message("Points that are not within a polygon:\n", paste0(capture.output(data.frame(outside)), collapse = "\n"))
    }
    else {
      message(nrow(outside), " points fall not within a polygon.")
    }
  }

  # Change sf to data.frame
  df_map <- df_map_sf
  sf::st_geometry(df_map) <- NULL

  # Aggregate
  df_map_sf2 <- df_map %>%
    dplyr::group_by(id) %>%
    dplyr::summarize(output = !! oper) %>%
    dplyr::ungroup()

  out <- dplyr::left_join(shp_wgs84, df_map_sf2, by = "id")
  return(out)
}










