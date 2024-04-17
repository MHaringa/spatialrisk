#' Create map with points
#'
#' @description Create map for a data.frame containing points.
#'
#' @param df data.frame containing columns for longitude and latitude.
#' @param value column in \code{df} to be visualized.
#' @param lon column in \code{df} containing longitude values.
#' @param lat column in \code{df} containing latitude values.
#' @param crs crs code for the coordinate reference system (default is 4326).
#' @param at the breakpoints used for visualisation.
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

  if (value == "") {
    stop(df,
         " does not contain column specified in `value`. Specify with `value`.",
         call. = FALSE)
  }

  obj_sf <- sf::st_as_sf(df, coords = c(lon, lat), crs = crs)
  mapview::mapview(obj_sf, zcol = value, layer.name = value, at = at)
}
