#' Find highest concentration
#'
#' @description Identifies the central coordinates of a circle with a fixed
#' radius that maximizes the coverage of demand points.
#'
#' @param df A data.frame containing demand points. Must include at least
#'     columns for longitude, latitude, and the value of interest.
#' @param value Column name in \code{df} with the value of interest to
#'     summarize.
#' @param top_n Positive integer greater or equal to 1 (default is 1).
#'     Specifies how many highest concentration circles are returned.
#'     If \code{top_n > 1}, then after each iteration the points belonging to
#'     the highest concentration are removed from \code{df}. This prevents the
#'     subsequent concentrations from being located in the same area, which
#'     would otherwise repeatedly select overlapping points with the largest
#'     values.
#' @param radius Numeric. Radius of the circle in meters (default = 200).
#' @param cell_size Numeric. Size of the grid cell in meters (default is 100).
#'     Defines the resolution of the initial raster grid. The choice of cell
#'     size depends on the size of the study area. For example, for a country
#'     the size of the Netherlands, cells of 100 × 100 meters are typically
#'     sufficient. For larger areas such as Germany, a cell size of 200 × 200
#'     meters may be more appropriate. The choice of \code{cell_size} does not
#'     affect the final result, only the computational speed.
#' @param grid_precision Numeric. Precision of the search grid in meters
#'     (default is 1). Determines the spacing of sub-points within each raster
#'     cell. For example, with \code{cell_size = 100} and
#'     \code{grid_precision = 1}, 10,000 sub-points (100 × 100) are evaluated
#'     per cell. Larger values reduce the number of sub-points (and runtime),
#'     but also reduce spatial accuracy.
#' @param lon Column name in \code{df} for longitude (default = \code{"lon"}).
#'     Must be in EPSG:4326.
#' @param lat Column name in \code{df} for latitude (default = \code{"lat"}).
#'     Must be in EPSG:4326.
#' @param crs_metric Numeric. Metric Coordinate Reference System (CRS) used in
#'     background calculations. For Europe use
#'     \href{https://epsg.io/3035}{EPSG:3035} (default).
#'     For the United States use \href{https://epsg.io/6317}{EPSG:6317}.
#'     For Asia-Pacific use \href{https://epsg.io/8859}{EPSG:8859}.
#' @param print_progress Logical. Whether to print progress messages
#'     (\code{TRUE}/\code{FALSE}).
#'
#' @importFrom terra ext
#' @importFrom terra focal
#' @importFrom terra rast
#' @importFrom terra rasterize
#' @importFrom terra vect
#'
#' @author Martin Haringa
#'
#' @details A recent regulation by the European Commission mandates insurers to
#' report the maximum insured value of fire risk policies for all buildings
#' partly or fully within a circle of radius 200 meters (see Article 132 - fire
#' risk sub-module - of the Delegated Regulation). This captures the risk of
#' catastrophic fire or explosion, including terrorist attacks.
#'
#' The problem resembles a Maximal Covering Location Problem (MCLP) with a
#' fixed radius, a classic facility location problem. The goal is to select the
#' best locations to maximize coverage of demand points, ensuring each demand
#' point lies within the radius of at least one selected facility.
#'
#' @references Commission Delegated Regulation (EU) (2015). Solvency II
#' Delegated Act 2015/35. Official Journal of the European Union, 58:124.
#'
#' @return A list with two elements:
#' \enumerate{
#' \item A data.frame with the \code{top_n} highest concentrations.
#' \item A data.frame with the subset of \code{df} corresponding to those
#' concentrations.
#' }
#'
#' @examples
#' # Find single highest concentration
#' x <- find_highest_concentration(Groningen, value = "amount")
#' plot(x)
#'
#' # Find top 2 concentrations with smaller grid cells
#' y <- find_highest_concentration(Groningen, "amount",
#'                                 top_n = 2, cell_size = 50)
#' plot(y)
#'
#' @export
find_highest_concentration <- function(df, value, top_n = 1, radius = 200,
                                       cell_size = 100, grid_precision = 1,
                                       lon = "lon", lat = "lat",
                                       crs_metric = 3035,
                                       print_progress = TRUE) {

  check_input(df, value, top_n, radius, cell_size, grid_precision)

  pts_lst <- vector("list", top_n)
  conc_lst <- vector("list", top_n)
  cell_ids <- vector("integer")

  threshold_db <- conc_db <- data.frame(lon = vector("numeric"),
                                        lat = vector("numeric"),
                                        concentration = vector("numeric"),
                                        cell = vector("integer"))

  df$ix <- seq.int(nrow(df))
  metric_sf <- convert_crs_df(df, 4326, crs_metric, lon, lat, "x", "y")
  terra_crs <- paste0("EPSG:", crs_metric)
  spatvctr <- terra::vect(metric_sf, geom = c("x", "y"), crs = terra_crs)
  raster <- terra::rast(spatvctr, res = cell_size)
  rasterized <- terra::rasterize(spatvctr, raster, field = value, fun = sum)
  mw <- mw_create(raster, radius)
  foc <- terra::focal(rasterized, w = mw, fun = "sum", na.rm = TRUE)

  for (i in seq_len(top_n)){

    top_focals <- top_n_focals(foc, n = 5)
    cpc <- conc_per_cell_new(top_focals, df, value, cell_size, 10,
                             threshold_db, radius, crs_metric, 4326, lon, lat)
    hc_lb <- highest_conc(cpc, top_focals, threshold_db)
    threshold <- hc_lb$concentration[1]
    foc_lb <- cells_above_threshold(foc, threshold)
    points <- cell_size / grid_precision
    cpc2 <- conc_per_cell_new(foc_lb, df, value, cell_size, points,
                              conc_db, radius, crs_metric, 4326, lon, lat)
    hc <- highest_conc(cpc2, foc_lb, conc_db)
    hc$id <- i
    pic <- points_in_circle_(df,
                             lon_center = hc[[lon]][1],
                             lat_center = hc[[lat]][1],
                             lon = lon,
                             lat = lat,
                             radius = radius)
    pic$id <- i
    pic$conc <- hc$concentration[1]

    if (top_n > 1 && print_progress) {
      cat("\rFinished", i, "of", top_n)
    }

    if (top_n > 1 && i < top_n) {

      df <- df[!df$ix %in% pic$ix, ]

      if (nrow(df) == 0) {
        rlang::abort("Need more rows", call = NULL)
      }

      spatvctr <- spatvctr[!spatvctr$ix %in% pic$ix, ]
      cell_ids <- map_points_to_cells(pic, foc, lon, lat, 4326, crs_metric,
                                      r = radius)

      threshold_db <- update_db(cpc, threshold_db, cell_ids)
      conc_db <- update_db(cpc2, conc_db, cell_ids)

      cells <- map_points_to_cells(pic, foc, lon, lat, 4326, crs_metric)
      extent <- terra::ext(raster, cells)
      rasterized <- update_rasterize(rasterized, extent, spatvctr, value)
      foc <- update_focal(foc, rasterized, extent, mw)
    }

    pts_lst[[i]] <- pic
    conc_lst[[i]] <- hc
  }

  pts_df <- do.call(rbind, pts_lst)
  conc_df <- do.call(rbind, c(conc_lst, make.row.names = FALSE))

  lst <- list(conc_df = conc_df, pts_df = pts_df)
  attr(lst, "radius") <- radius
  attr(lst, "rasterized") <- rasterized
  attr(lst, "focal") <- foc
  attr(lst, "threshold") <- threshold
  attr(lst, "value") <- value
  attr(lst, "lon") <- lon
  attr(lst, "lat") <- lat
  attr(lst, "crs_metric") <- crs_metric
  class(lst) <- append("concentration", class(lst))
  lst
}


#' Automatically create a plot for objects obtained from
#' \code{find_highest_concentration()}
#'
#' @description Automatically create a plot for objects obtained from
#' \code{find_highest_concentration()}.
#'
#' @param x x object of class \code{concentration} obtained from
#' \code{highest_concentration()}
#' @param type is one of "concentration" (default), "rasterized", "focal",
#' "updated_focal". See details for more information.
#' @param color1 color when one concentration is plotted (default is "#4B0055").
#' @param max.rad maximal radius for size of circles in plot (default is 20).
#' @param ... additional arguments.
#'
#' @importFrom grDevices hcl.colors
#' @importFrom mapview mapview
#' @importFrom sf st_buffer
#' @importFrom terra classify
#' @importFrom terra trim
#'
#' @details More info for type:
#' \enumerate{
#'  \item "concentration": this is..
#'  \item "focal": this is..
#'  \item "rasterized": this is..
#'  \item "updated_focal": this is..
#' }
#'
#' @author Martin Haringa
#'
#' @examples
#' x <- find_highest_concentration(Groningen, "amount")
#' plot(x, "concentration")
#' plot(x, "rasterized")
#' plot(x, "focal")
#' plot(x, "updated_focal")
#'
#' @export
plot.concentration <- function(x, type = c("concentration", "focal",
                                           "rasterized", "updated_focal"),
                               color1 = NULL, max.rad = 20, ...) {

  type <- match.arg(type)
  rasterized <- attr(x, "rasterized")
  focal <- attr(x, "focal")
  threshold <- attr(x, "threshold")
  radius <- attr(x, "radius")
  value <- attr(x, "value")
  lon <- attr(x, "lon")
  lat <- attr(x, "lat")
  crs_metric <- attr(x, "crs_metric")

  if (type == "concentration") {
    cw <- convert_df_to_sf(x[[1]], lon, lat, 4326, crs_metric)
    mpv <- convert_df_to_sf(x[[2]], lon, lat, 4326, crs_metric)
    mpv$logvalue <- log(mpv[[value]])

    cw_buffer <- sf::st_buffer(cw, dist = radius, nQuadSegs = 50)
    cw_buffer$id <- as.factor(cw_buffer$id)
    leg <- nrow(cw_buffer) > 1
    if (is.null(color1)) {
      color1 <- grDevices::hcl.colors(1, palette = "viridis")
    }
    mv_pts <- do.call(mapview::mapview, c(list(mpv,
                                               cex = "logvalue",
                                               min.rad = 5,
                                               max.rad = max.rad,
                                               zcol = "id",
                                               layer.name = "Points",
                                               legend = FALSE),
                                          list(col.region = color1)[!leg]))
    mv_buffer <- do.call(mapview::mapview, c(list(x = cw_buffer,
                                                  zcol = "id",
                                                  alpha.regions = .1,
                                                  layer.name = "Highest",
                                                  legend = leg),
                                             list(col.region = color1)[!leg]))
    ut <- mv_pts + mv_buffer
  }

  if (type == "focal") {
    ut <- mapview::mapview(focal)
  }

  if (type == "rasterized") {
    ut <- mapview::mapview(rasterized)
  }

  if (type == "updated_focal") {
    foc_upd <- terra::classify(focal, cbind(-Inf, threshold, NA), right = FALSE)
    foc_trim <- terra::trim(foc_upd)
    ut <- mapview::mapview(foc_trim)
  }
  ut
}


#' @export
print.concentration <- function(x, ...) {
  attr(x, "class") <- NULL
  attr(x, "radius") <- NULL
  attr(x, "rasterized") <- NULL
  attr(x, "focal") <- NULL
  attr(x, "threshold") <- NULL
  attr(x, "value") <- NULL
  attr(x, "lon") <- NULL
  attr(x, "lat") <- NULL
  attr(x, "crs_metric") <- NULL
  print.default(x, ...)
}


