
# r2 <- project(raster1, "EPSG:3035")
# crs(r2, describe = TRUE)
# area 1 Europe - European Union (EU) countries and candidates. Europe - onshore
# and offshore: Albania; Andorra; Austria; Belgium; Bosnia and Herzegovina;
# Bulgaria; Croatia; Cyprus; Czechia; Denmark; Estonia; Faroe Islands; Finland;
# France; Germany; Gibraltar; Greece; Hungary; Iceland; Ireland; Italy; Kosovo;
# Latvia; Liechtenstein; Lithuania; Luxembourg; Malta; Monaco; Montenegro;
# Netherlands; North Macedonia; Norway including Svalbard and Jan Mayen; Poland;
# Portugal including Madeira and Azores; Romania; San Marino; Serbia; Slovakia;
# Slovenia; Spain including Canary Islands; Sweden; Switzerland; Turkey; United
# Kingdom (UK) including Channel Islands and Isle of Man; Vatican City State




#' Find highest concentration
#'
#' @description Determines the central coordinates of a circle with a constant
#' radius that maximizes the coverage of insured fire risk, where the fire risk
#' is defined as assuming 100 percent damage on the total sum of the capital
#' insured for each building located partly or fully within this radius.
#'
#' A recent regulation by the European Commission mandates insurance companies
#' to report the maximum value of insured fire risk policies for all buildings
#' partially or fully situated within a circle with a radius of 200 meters.
#'
#' @param df data.frame. Should include at least columns for longitude,
#'     latitude, and the value of interest to summarize.
#' @param value column name with value of interest to summarize in \code{df}.
#' @param top_n positive integer value greater or equal to 1 (default is 1).
#' @param radius numeric. Radius of the circle in meters (default is 200).
#' @param cell_size numeric. Size of cell in meters (default is 100).
#' @param grid_precision numeric. Precision of grid in meters (default is 1).
#'
#' @importFrom terra ext
#' @importFrom terra focal
#' @importFrom terra rast
#' @importFrom terra rasterize
#' @importFrom terra vect
#'
#' @author Martin Haringa
#'
#' @details This problem resembles a Maximal Covering Location Problem (MCLP)
#' with a fixed radius, belonging to the category of facility location problems.
#' The main aim is to select the best locations for a predetermined number of
#' facilities to achieve maximum coverage of demand points within a specified
#' radius of each facility. In essence, the objective is to identify optimal
#' facility locations to cover as many demand points as feasible, while ensuring
#' that each demand point falls within the designated distance (radius) of at
#' least one facility.
#'
#' @return list
#'
#' @examples
#' x <- find_highest_concentration(Groningen, "amount")
#' plot(x)
#'
#'
#' @export
find_highest_concentration <- function(df, value, top_n = 1, radius = 200,
                                       cell_size = 100, grid_precision = 1) {

  check_input(df, value, top_n, radius, cell_size, grid_precision)

  pts_lst <- vector("list", top_n)
  conc_lst <- vector("list", top_n)
  cell_ids <- vector("integer")

  threshold_db <- conc_db <- data.frame(lon = vector("numeric"),
                                        lat = vector("numeric"),
                                        concentration = vector("numeric"),
                                        cell = vector("integer"))

  df$ix <- seq.int(nrow(df))
  metric_sf <- convert_crs_df(df, 4326, 3035, "lon", "lat", "x", "y")
  spatvctr <- terra::vect(metric_sf, geom = c("x", "y"), crs = "EPSG:3035")
  raster <- terra::rast(spatvctr, res = cell_size)
  rasterized <- terra::rasterize(spatvctr, raster, field = value, fun = sum)
  mw <- mw_create(raster, radius)
  foc <- terra::focal(rasterized, w = mw, fun = "sum", na.rm = TRUE)

  for (i in seq_len(top_n)){

    top_focals <- top_n_focals(foc, n = 5)
    cpc <- conc_per_cell_new(top_focals, df, value, cell_size, 10,
                             threshold_db, radius)
    hc_lb <- highest_conc(cpc, top_focals, threshold_db)
    threshold <- hc_lb$concentration[1]
    foc_lb <- cells_above_threshold(foc, threshold)
    points <- cell_size / grid_precision
    cpc2 <- conc_per_cell_new(foc_lb, df, value, cell_size, points,
                              conc_db, radius)
    hc <- highest_conc(cpc2, foc_lb, conc_db)
    hc$id <- i
    pic <- points_in_circle(df,
                            lon_center = hc$lon[1],
                            lat_center = hc$lat[1],
                            radius = radius)
    pic$id <- i
    pic$conc <- hc$concentration[1]

    if (top_n > 1) {
      cat("\rFinished", i, "of", top_n)
    }

    if (top_n > 1 && i < top_n) {
      # update inputs for next iteration
      df <- df[!df$ix %in% pic$ix, ]
      if (nrow(df) == 0) {
        rlang::abort("Need more rows", call = NULL)
      }
      spatvctr <- spatvctr[!spatvctr$ix %in% pic$ix, ]
      cell_ids <- map_points_to_cells(pic, foc, radius)
      threshold_db <- update_db(cpc, threshold_db, cell_ids)
      conc_db <- update_db(cpc2, conc_db, cell_ids)

      cells <- map_points_to_cells(pic, foc)
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

  if (type == "concentration") {
    cw <- convert_df_to_sf(x[[1]])
    mpv <- convert_df_to_sf(x[[2]])
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


#' Print output
#'
#' @param x input from function
#' @param ... additional arguments
#'
#' @export
print.concentration <- function(x, ...) {
  attr(x, "class") <- NULL
  attr(x, "radius") <- NULL
  attr(x, "rasterized") <- NULL
  attr(x, "focal") <- NULL
  attr(x, "threshold") <- NULL
  attr(x, "value") <- NULL
  print.default(x, ...)
}
