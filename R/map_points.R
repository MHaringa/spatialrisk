#' Create interactive point map
#'
#' @description Creates an interactive map for a data.frame containing
#' point coordinates, optionally colored by a selected variable.
#'
#' @param data A data.frame containing columns for longitude and latitude.
#' @param value A string giving the name of the column in \code{data} to be
#'   visualized. If \code{NULL}, points are mapped without a color variable.
#' @param lon A string with the name of the column containing longitude
#'   values. Default is \code{"lon"}.
#' @param lat A string with the name of the column containing latitude
#'   values. Default is \code{"lat"}.
#' @param crs Integer; EPSG code for the coordinate reference system.
#'   Default is \code{4326}.
#' @param at Optional numeric vector; breakpoints used for visualization.
#' @param layer_name Optional layer name passed to \code{mapview}.
#' @param ... Additional arguments passed to \code{mapview::mapview()}.
#'
#' @return An interactive \code{mapview} object.
#'
#' @importFrom sf st_as_sf
#'
#' @examples
#' \dontrun{
#' map_points(Groningen, value = "amount")
#' }
#'
#' @export
map_points <- function(data, value = NULL, lon = "lon", lat = "lat",
                       crs = 4326, at = NULL, layer_name = NULL, ...) {

  if (!requireNamespace("mapview", quietly = TRUE)) {
    stop("mapview is needed for this function to work. Install it via ",
         "install.packages(\"mapview\")", call. = FALSE)
  }

  column_args <- list(lon, lat)
  if (!is.null(value)) {
    column_args <- c(column_args, list(value))
  }

  if (!all(vapply(column_args, is.character, logical(1))) ||
      !all(vapply(column_args, length, integer(1)) == 1) ||
      any(is.na(unlist(column_args, use.names = FALSE)))) {
    stop("Column arguments must be single non-missing strings.", call. = FALSE)
  }

  if (!is.null(value) && !value %in% names(data)) {
    stop("Column '", value,
         "' not found in `data`. Please specify an existing column with `value`.",
         call. = FALSE)
  }

  if (!all(c(lon, lat) %in% names(data))) {
    stop("Columns '", lon, "' and/or '", lat, "' not found in `data`.",
         call. = FALSE)
  }

  if (!is.numeric(data[[lon]]) || !is.numeric(data[[lat]])) {
    stop(lon, " and ", lat, " must be numeric.", call. = FALSE)
  }

  if (!is.numeric(crs) || length(crs) != 1 || is.na(crs)) {
    stop("`crs` must be a single numeric EPSG code.", call. = FALSE)
  }

  if (!is.null(at) && !is.numeric(at)) {
    stop("`at` must be numeric when supplied.", call. = FALSE)
  }

  if (!is.null(layer_name) &&
      (!is.character(layer_name) || length(layer_name) != 1 ||
       is.na(layer_name))) {
    stop("`layer_name` must be a single non-missing string.", call. = FALSE)
  }

  obj_sf <- sf::st_as_sf(data, coords = c(lon, lat), crs = crs)

  if (is.null(layer_name)) {
    layer_name <- if (is.null(value)) "points" else value
  }

  mapview::mapview(obj_sf, zcol = value, layer.name = layer_name, at = at, ...)
}
