#' Aggregate attributes of coordinates to area level (deprecated function; use 'points_to_polygon' instead)
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
choropleth_sf <- function(sf_map, df, oper, crs = 4326, outside_print = FALSE){

  .Defunct("points_to_polygon")
  points_to_polygon(sf_map = sf_map, df = df, oper = oper, crs = crs, outside_print = outside_print)
}










