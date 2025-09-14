#' Create interactive point map
#'
#' @description Creates an interactive map for a data.frame containing
#' point coordinates, colored by a selected variable.
#'
#' @param df A data.frame containing columns for longitude and latitude.
#' @param value A string giving the name of the column in \code{df} to be
#'   visualized.
#' @param lon A string with the name of the column containing longitude
#'   values. Default is \code{"lon"}.
#' @param lat A string with the name of the column containing latitude
#'   values. Default is \code{"lat"}.
#' @param crs Integer; EPSG code for the coordinate reference system.
#'   Default is \code{4326}.
#' @param at Optional numeric vector; breakpoints used for visualization.
#'
#' @return An interactive \code{mapview} object.
#'
#' @importFrom mapview mapview
#' @importFrom sf st_as_sf
#'
#' @examples \dontrun{
#' plot_points(Groningen, value = "amount")
#' }
#'
#' @export
plot_points <- function(df, value, lon = "lon", lat = "lat",
                        crs = 4326, at = NULL) {

  if (!value %in% names(df)) {
    stop("Column '", value,
         "' not found in `df`. Please specify an existing column with `value`.",
         call. = FALSE)
  }

  obj_sf <- sf::st_as_sf(df, coords = c(lon, lat), crs = crs)
  mapview::mapview(obj_sf, zcol = value, layer.name = value, at = at)
}
