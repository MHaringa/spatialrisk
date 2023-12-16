#' Aggregate attributes of coordinates to area level
#'
#' @description A data.frame containing coordinates (in terms of longitude and
#' latitude) is joined to the polygon level. Then arithmetic operations on the
#' attributes of the coordinates are applied to obtain aggregated values for
#' each polygon.
#'
#' @param sf_map object of class sf
#' @param df data.frame containing coordinates (column names should be 'lon'
#' and 'lat')
#' @param oper an arithmetic operation on the polygon level
#' @param crs coordinate reference system: integer with the EPSG code, or
#' character with proj4string
#' @param outside_print print points that are not within a polygon (default is
#' FALSE).
#'
#' @export points_to_polygon
#'
#' @importFrom sf st_transform
#' @importFrom sf st_buffer
#' @importFrom sf st_join
#' @importFrom sf st_intersects
#' @importFrom sf st_geometry
#' @importFrom data.table data.table
#' @importFrom utils capture.output
#'
#' @return an object of class \code{sf}
#'
#' @author Martin Haringa
#'
#' @examples
#' points_to_polygon(nl_postcode2, insurance, sum(amount, na.rm = TRUE))
#' \dontrun{
#' shp_read <- sf::st_read("~/path/to/file.shp")
#' points_to_polygon(shp_read, insurance, sum(amount, na.rm = TRUE))
#' }
points_to_polygon <- function(sf_map, df, oper, crs = 4326,
                              outside_print = FALSE){

  shp_wgs84 <- tryCatch({
    shp0 <- sf::st_transform(sf_map, crs = crs) # Convert coordinates to WGS84
    shp0$id <- seq.int(nrow(shp0))
    shp0 },
    error = function(e) {
      shp0 <- sf::st_buffer(sf_map, 0) # Make invalid geometries valid
      shp0 <- sf::st_transform(shp0, crs = crs)
      shp0$id <- seq.int(nrow(shp0))
      shp0
    })

  if( !all(c("lon", "lat") %in% names(df)) ) {
    stop("Data.frame should contain column names 'lon' and 'lat'.",
         call. = FALSE)
  }

  df_sf <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = crs)

  suppressMessages({
    df_map_sf <- sf::st_join(shp_wgs84, df_sf)
    outside <- df_sf[!lengths(sf::st_intersects(df_sf, shp_wgs84)), ]
  })

  if( nrow(outside) > 0 ){
    if( isTRUE(outside_print)){
      message("Points that are not within a polygon:\n",
              paste0(capture.output(data.frame(outside)), collapse = "\n"))
    }
    else {
      message(nrow(outside), " points fall not within a polygon.")
    }
  }

  # Change sf to data.frame
  df_map <- df_map_sf
  sf::st_geometry(df_map) <- NULL

  # Aggregate
  oper <- substitute(oper)
  df_map_sf2 <- eval.parent(substitute(
    data.table::data.table(df_map)[, .(output = oper), by = "id"]
    ))

  out <- merge(shp_wgs84, df_map_sf2, by = "id", all.x = TRUE)
  return(out)
}










