#' Create choropleth map
#'
#' @description Creates a choropleth map from an `sf` object, for example one
#' produced by \code{summarise_points_by_polygon()}. Polygons are shaded
#' according to values in a specified column, with clustering based on the
#' Fisher–Jenks algorithm.
#'
#' @param data An object of class \code{sf}.
#' @param value A string giving the name of the column used to shade the
#'   polygons.
#' @param id Optional string giving the name of the column containing polygon IDs
#'   used for tooltips.
#' @param mode A string indicating whether to create a static map
#'   (\code{"plot"}, default) or an interactive map (\code{"view"}).
#' @param n Integer; number of clusters. Default is \code{7}.
#' @param legend_title A string giving the legend title.
#' @param palette A palette name or vector of colors. See
#'   \code{tmaptools::palette_explorer()} for available palettes.
#'   Prefix the name with \code{"-"} to reverse the order. Default is
#'   \code{"viridis"}.
#' @param id_name Deprecated. Use \code{id} instead.
#' @param ... Additional arguments passed to \code{tmap::tm_polygons()}.
#'
#' @details The function uses the Fisher–Jenks algorithm
#' (\code{style = "fisher"}) to classify values into \code{n} groups.
#'
#' @return A \code{tmap} object (static or interactive, depending on
#' \code{mode}).
#'
#' @author Martin Haringa
#'
#' @examples
#' test <- summarise_points_by_polygon(nl_provincie, insurance, "amount")
#' choropleth(test, value = "amount_sum")
#' choropleth(test, value = "amount_sum", id = "areaname", mode = "view")
#'
#' @export
choropleth <- function(data, value = "output", id = NULL,
                       mode = c("plot", "view"), n = 7,
                       legend_title = "Value", palette = "viridis",
                       id_name = NULL, ...) {

  if (!requireNamespace("tmap", quietly = TRUE)) {
    stop("tmap is needed for this function to work. Install it via ",
         "install.packages(\"tmap\")", call. = FALSE)
  }

  mode <- match.arg(mode)

  if (!inherits(data, "sf")) {
    stop("`data` must be an sf object.", call. = FALSE)
  }

  if (!is.null(id_name)) {
    lifecycle::deprecate_warn("0.8.0", "choropleth(id_name)",
                              "choropleth(id)")
    if (is.null(id)) {
      id <- id_name
    }
  }

  column_args <- list(value)
  if (!is.null(id)) {
    column_args <- c(column_args, list(id))
  }

  if (!all(vapply(column_args, is.character, logical(1))) ||
      !all(vapply(column_args, length, integer(1)) == 1) ||
      any(is.na(unlist(column_args, use.names = FALSE)))) {
    stop("Column arguments must be single non-missing strings.", call. = FALSE)
  }

  if (!value %in% names(data)) {
    stop("Column '", value, "' not found in `data`.", call. = FALSE)
  }

  if (!is.null(id) && !id %in% names(data)) {
    stop("Column '", id, "' not found in `data`.", call. = FALSE)
  }

  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1 || n != round(n)) {
    stop("`n` must be a single positive integer.", call. = FALSE)
  }

  if (!is.character(legend_title) || length(legend_title) != 1 ||
      is.na(legend_title)) {
    stop("`legend_title` must be a single non-missing string.", call. = FALSE)
  }

  suppressMessages(tmap::tmap_mode(mode))

  polygon_args <- c(
    list(
      value,
      fill.scale = tmap::tm_scale_intervals(
        style = "fisher",
        values = palette,
        n = n
      ),
      fill.legend = tmap::tm_legend(title = legend_title)
    ),
    list(...)
  )

  if (!is.null(id)) {
    polygon_args$id <- id
  }

  if (mode == "view") {
    polygon_args$fill_alpha <- .5

    tmap::tm_shape(data) +
      do.call(tmap::tm_polygons, polygon_args) +
      tmap::tm_basemap(c("OpenStreetMap", "Esri.WorldGrayCanvas",
                         "Esri.WorldTopoMap"))
  } else {
    polygon_args$lwd <- if (is.null(polygon_args$lwd)) .1 else polygon_args$lwd

    tmap::tm_shape(data) +
      do.call(tmap::tm_polygons, polygon_args) +
      tmap::tm_compass(position = c("right", "bottom")) +
      tmap::tm_scalebar(position = c("left", "bottom")) +
      tmap::tm_layout(frame = FALSE)
  }
}
