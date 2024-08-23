#' Convert data.frame to simple features (sf) object
#'
#' This function converts a data.frame to a simple features (sf) object.
#'
#' @param df data.frame containing longitude and latitude columns
#' @param lon column name for longitude values (default: "lon").
#' @param lat column name for latitude values (default: "lat").
#' @param crs_from crs of the original coordinate system (default: 4326).
#' @param crs_to crs of the target coordinate system (default: 3035).
#'
#' @return Returns an sf object with the specified coordinate reference system.
#'
#' @importFrom sf st_as_sf
#' @importFrom sf st_transform
#'
#' @author Martin Haringa
#'
#' @keywords internal
convert_df_to_sf <- function(df, lon = "lon", lat = "lat",
                             crs_from = 4326, crs_to = 3035) {
  df_sf <- sf::st_as_sf(df, coords = c(lon, lat), crs = crs_from)
  sf::st_transform(df_sf, crs_to) # A metric CRS
}



#' Convert Coordinate Reference System (CRS)
#'
#' @description Convert Coordinate Reference System (CRS) of a data.frame
#' from one CRS to another.
#'
#' @param df data.frame to be converted.
#' @param crs_from CRS code of the original coordinate system (default: 3035).
#' @param crs_to CRS code of the target coordinate system (default: 4326).
#' @param lon_from column name of longitude values in \code{df} (default: "x").
#' @param lat_from column name of latitude values in \code{df} (default: "y").
#' @param lon_to column name for longitude values in the converted data frame
#'     (default: "lon").
#' @param lat_to column name for latitude values in the converted data frame
#'     (default: "lat").
#'
#' @return data.frame with converted coordinates
#'
#' @importFrom sf st_coordinates
#' @importFrom sf st_drop_geometry
#'
#' @author Martin Haringa
#'
#' @export
convert_crs_df <- function(df, crs_from = 3035, crs_to = 4326,
                           lon_from = "x", lat_from = "y",
                           lon_to = "lon", lat_to = "lat") {
  if (!is.null(df) && nrow(df) > 0) {
    df_sf <- convert_df_to_sf(df, lon_from, lat_from, crs_from, crs_to)
    coord <- as.data.frame(sf::st_coordinates(df_sf))
    names(coord) <- c(lon_to, lat_to)
    cbind(sf::st_drop_geometry(df_sf), coord)
  } else {
    NULL
  }
}



#' Create focal ("moving window") weight matrix
#'
#' @description Create a focal ("moving window") weight matrix for use in
#' \code{terra::focal()}.
#'
#' @param r SpatRaster.
#' @param radius radius of the circle (in units of the crs).
#'
#' @importFrom terra distance
#' @importFrom terra ext
#' @importFrom terra rast
#' @importFrom terra res
#'
#' @details \code{mw_create()} is a modified version of
#' \code{terra::focalMat()}. While \code{terra::focalMat()} creates a matrix
#' where the border is the distance from the center of the focal cell,
#' \code{mw_create()} creates a matrix where the border of the moving window
#' is the distance from the edge of the focal cell.
#'
#' @author Martin Haringa
#'
#' @keywords internal
mw_create <- function(r, radius) {
  d <- radius
  rs <- terra::res(r)
  nx <- 1 + 2 * ceiling(d / rs[1])
  ny <- 1 + 2 * ceiling(d / rs[2])
  m <- matrix(ncol = nx, nrow = ny)
  m[ceiling(ny / 2), ceiling(nx / 2)] <- 1
  x <- terra::rast(m, crs = "+proj=utm +zone=1 +datum=WGS84")
  terra::ext(x) <- c(xmin = 0, xmax = nx * rs[1], ymin = 0, ymax = ny * rs[2])
  dist_diag_cell <- sqrt(2 * (rs[1] ^ 2))
  d <- as.matrix(terra::distance(x), wide = TRUE) <= d + dist_diag_cell
  d / d
}

#' Identify the focal indices with the highest values
#'
#' @description Generate a data.frame containing the cell indices of the focal
#' cells with the \code{n} highest values. Additionally, include columns for
#' the coordinates \code{(xy)} corresponding to the center of each cell.
#'
#' @param focal focal as output from \code{terra::focal()}.
#' @param n positive integer value greater or equal to 1.
#'
#' @importFrom terra values
#' @importFrom terra xyFromCell
#'
#' @author Martin Haringa
#'
#' @keywords internal
top_n_focals <- function(focal, n) {
  val <- terra::values(focal, mat = FALSE)
  ix <- highest_indices_cpp(val, n) # Faster than base::order()
  xyfromcell <- terra::xyFromCell(focal, ix)
  cbind(data.frame(cell = ix, val = val[ix]), xyfromcell)
}


#' Identify the focal cells exceeding the threshold
#'
#' @description Generate a data.frame containing the cell indices of the focal
#' cells surpassing the specified threshold. Additionally, include columns for
#' the coordinates (xy) corresponding to the center of each cell.
#'
#' @param focal focal as output from \code{terra::focal()}.
#' @param threshold lower (numeric) threshold boundary.
#'
#' @importFrom terra classify
#' @importFrom terra xyFromCell
#'
#' @author Martin Haringa
#'
#' @keywords internal
cells_above_threshold <- function(focal, threshold) {
  x <- terra::classify(focal, cbind(-Inf, threshold, NA), right = FALSE)
  vecp <- as.vector(terra::values(x))
  cellix <- which(!is.na(vecp))
  xyfromcell <- terra::xyFromCell(focal, cellix)
  cbind(data.frame(cell = cellix), xyfromcell)
}


#' Map point coordinates to cell indices
#'
#' @description Map point coordinates to cell indices.
#'
#' @param pts data.frame with \code{lon} and \code{lat} columns in CRS 4326.
#' @param focal focal (SpatRaster).
#' @param lon character.
#' @param lat character.
#' @param crs_from crs from
#' @param crs_to crs to
#' @param r buffer around extent (in units of the crs).
#'
#' @importFrom sf st_coordinates
#' @importFrom terra cellFromXY
#' @importFrom terra cells
#' @importFrom terra ext
#'
#' @author Martin Haringa
#'
#' @keywords internal
map_points_to_cells <- function(pts, focal, lon, lat, crs_from, crs_to,
                                r = NULL) {
  pts_sf <- convert_df_to_sf(pts, lon, lat, crs_from, crs_to)
  xy <- sf::st_coordinates(pts_sf)
  gcp <- terra::cellFromXY(focal, xy)
  if (!is.null(r)) {
    ext_box <- terra::ext(focal, gcp)
    gcp <- terra::cells(focal, ext_box + r) # box plus radius
  }
  gcp
}


#' Update current rasterize for the next iteration
#'
#' @description Update current rasterize for the next iteration.
#'
#' @param old_rasterized SpatRaster used in the current iteration.
#' @param extent Extent of the cells corresponding to the coordinates with the
#'     highest concentration for the current iteration.Extent is output from
#'     terra::ext().
#' @param new_spatvector Updated SpatVector for next iteration.
#' @param col Character. Variable name in \code{new_spatvector}.
#'
#' @details Spatial extent refers to the geographic area covered by a spatial
#' dataset. It defines the boundaries in terms of its geographic coordinates
#' (north, east, south, west).
#'
#' @importFrom terra crop
#' @importFrom terra merge
#' @importFrom terra rasterize
#'
#' @author Martin Haringa
#'
#' @keywords internal
update_rasterize <- function(old_rasterized, extent, new_spatvector, col) {
  crop_rasterized <- terra::crop(old_rasterized, extent)
  rasterized_box <- terra::rasterize(new_spatvector, crop_rasterized,
                                     field = col, fun = sum, background = 0)
  terra::merge(old_rasterized, rasterized_box, first = FALSE)
}


#' Update current focal for the next iteration
#'
#' @description Update current focal for the next iteration.
#'
#' @param old_focal Focal obtained from \code{terra::focal()} for the current
#'     iteration.
#' @param new_rasterized Output obtained from \code{update_rasterize()}
#'     for the next iteration.
#' @param extent Extent of the cells corresponding to the coordinates with the
#'     highest concentration for the current iteration. Extent is output from
#'     \code{terra::ext()}.
#' @param mw Moving window obtained from \code{create_mw()}.
#'
#' @details Spatial extent refers to the geographic area covered by a spatial
#' dataset. It defines the boundaries in terms of its geographic coordinates
#' (north, east, south, west).
#'
#' @details An focal is updated by the following steps:
#' \enumerate{
#'  \item The extent of the cells corresponding to
#'  the coordinates with the highest concentration (xmin, xmax, ymin, ymax)
#'  is determined.
#'  \item A buffer of size two times the radius plus the
#'  cell size around this extent. All cells within this new extent will impact
#'  the focal values.
#'  \item Subset (crop) the rasterized data to include only the cells
#'  within this new extent. Perform focal calculations only for these new
#'  cells.
#'  \item As the focal values of the cells near the borders may be
#'  inflated, crop the result again to include only the cells within a radius
#'  of the original extent.
#'  \item Merge this updated focal with the previous focal object.
#' }
#'
#' @importFrom terra crop
#' @importFrom terra focal
#' @importFrom terra merge
#' @importFrom terra xres
#'
#' @author Martin Haringa
#'
#' @keywords internal
update_focal <- function(old_focal, new_rasterized, extent, mw) {
  cell_size <- terra::xres(new_rasterized)
  mw_cells <- (nrow(mw) - 1) / 2
  mw_dist <- mw_cells * cell_size
  extent_focal <- extent + (2 * mw_dist) # Make ext bigger focal
  crop_rast <- terra::crop(new_rasterized, extent_focal)
  crop_focal <- terra::focal(crop_rast, w = mw, fun = "sum", na.rm = TRUE)
  extent_update <- extent + mw_dist # Update only cells within radius
  focal_update <- terra::crop(crop_focal, extent_update) # Make bbox smaller
  terra::merge(old_focal, focal_update, first = FALSE)
}


#' Determine the concentrations within the highest focal cells for the current
#' iteration
#'
#' @description Determine the concentrations within the highest focal cells for
#' the current iteration.
#'
#' @param high_foc data.frame containing cell ids with the top n focal values
#'     from the current iteration.
#' @param dff data.frame with all observations
#' @param value column name in `dff` to find concentrations for.
#' @param size size of cell in meters.
#' @param points number of points per `size`.
#' @param db data.frame containing previously saved highest concentrations.
#' @param radius radius of circle in meters.
#' @param crs_from crs from
#' @param crs_to crs to
#'
#' @author Martin Haringa
#'
#' @keywords internal
conc_per_cell_new <- function(high_foc, dff, value, size, points, db, radius,
                              crs_from, crs_to, lon, lat) {
  ix_new <- setdiff(high_foc$cell, db$cell)
  hf_new <- high_foc[high_foc$cell %in% ix_new, ]
  hf_new <- convert_crs_df(hf_new, crs_from, crs_to)
  colnames(dff)[colnames(dff) == value] <- "value"
  colnames(dff)[colnames(dff) == lon] <- "lon"
  colnames(dff)[colnames(dff) == lat] <- "lat"
  if (!is.null(hf_new)) {
    mc <- max_conc_per_cell_cpp(hf_new, dff, points, size, radius)
    colnames(mc)[colnames(mc) == "lon"] <- lon
    colnames(mc)[colnames(mc) == "lat"] <- lat
    mc
  } else {
    NULL
  }
}


#' Find the highest concentration for the current iteration
#'
#' @description Find the highest concentration for the current iteration.
#'
#' @param hf_conc_new highest concentrations from the current iteration,
#'     retrieved from \code{conc_per_cell_new()}.
#' @param high_foc data.frame containing cell ids with the top n focal values
#'     from the current iteration.
#' @param db data.frame containing previously saved highest concentrations.
#'
#' @author Martin Haringa
#'
#' @keywords internal
highest_conc <- function(hf_conc_new, high_foc, db) {
  ix_old <- setdiff(high_foc$cell, hf_conc_new$cell)
  hf_conc_old <- db[db$cell %in% ix_old, ]
  hf_conc <- rbind(hf_conc_old, hf_conc_new)
  hf_conc[which.max(hf_conc$concentration), ]
}


#' Save highest concentrations per cell for subsequent iterations
#'
#' @description Save highest concentrations per cell for subsequent iterations.
#'
#' @param hf_conc_new highest concentrations from the current iteration,
#'     obtained from \code{conc_per_cell_new()}.
#' @param db data.frame containing previously saved highest concentrations.
#' @param cells cells containing points associated with the current highest
#'     concentration to be removed from \code{db}.
#'
#' @author Martin Haringa
#'
#' @keywords internal
update_db <- function(hf_conc_new, db, cells) {
  rb <- rbind(db, hf_conc_new)
  rb[!rb$cell %in% cells, ]
}




#' @importFrom rlang abort
#'
#' @noRd
check_input <- function(df, value, top_n, radius, cell_size, grid_precision) {

  if (!value %in% colnames(df)) {
    msg <- "Can't find concentrations for columns that don't exist."
    error_msg <- paste0("Column `", value, "` doesn't exist.")
    rlang::abort(c(msg, "x" = error_msg), call = NULL)
  }

  if (!is.numeric(top_n) || round(top_n) != top_n || top_n <= 0) {
    msg <- paste0("Can't find the `top_n = ", top_n, "` highest concentrations")
    error_msg <- paste0("`top_n = ", top_n, "` is not a positive integer.")
    rlang::abort(c(msg, "x" = error_msg), call = NULL)
  }

  if (!is.numeric(radius) || radius <= 0) {
    msg <- paste0("Can't find concentrations with `radius = ", radius, "`.")
    error_msg <- paste0("`radius` is not a positive number.")
    rlang::abort(c(msg, "x" = error_msg), call = NULL)
  }

  if (!is.numeric(cell_size) || cell_size <= 0) {
    msg <- paste0("Can't find concentrations with `cell_size = ",
                  cell_size, "`.")
    error_msg <- paste0("`cell_size` is not a positive number.")
    rlang::abort(c(msg, "x" = error_msg), call = NULL)
  }

  if (!is.numeric(grid_precision) || grid_precision <= 0) {
    msg <- paste0("Can't find concentrations with `grid_precision = ",
                  grid_precision, "`.")
    error_msg <- paste0("`grid_precision` is not a positive number.")
    rlang::abort(c(msg, "x" = error_msg), call = NULL)
  }

  if (cell_size > radius) {
    msg <- paste0("Can't find concentrations with `cell_size` > `radius`.")
    error_msg <- paste0("`cell_size` = ", cell_size,
                        " > `radius` = ", radius, ".")
    rlang::abort(c(msg, "x" = error_msg), call = NULL)
  }
}



