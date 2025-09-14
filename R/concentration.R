#' Concentration calculation
#'
#' @description Calculates the concentration, which is the sum of all
#' observations within a circle of a certain radius.
#'
#' @param sub A data.frame of target points for which concentration risk
#'     is calculated. Must include at least columns for longitude and latitude.
#' @param full A data.frame containing reference points. Must include at least
#'     columns for longitude, latitude, and the value of interest to summarize.
#' @param value Column name in \code{full} containing the values to be summed.
#' @param lon_sub Column name in \code{sub} for longitude (default: \code{lon}).
#' @param lat_sub Column name in \code{sub} for latitude (default: \code{lat}).
#' @param lon_full Column name in \code{full} for longitude (default: \code{lon}).
#' @param lat_full Column name in \code{full} for latitude (default: \code{lat}).
#' @param radius Numeric. Radius of the circle in meters. Must be positive
#'     (default: 200).
#' @param display_progress Logical. Whether to display a progress bar
#'     (\code{TRUE}/\code{FALSE}). Default is \code{TRUE}.
#'
#' @return A data.frame equal to \code{sub} with an additional numeric column
#'     \code{concentration} containing the summed values from \code{full}.
#'
#' @details
#' This function uses a C++ backend for efficient distance calculations
#' (Haversine formula). For each point in \code{sub}, it finds all points in
#' \code{full} within the specified radius and sums their \code{value}.
#'
#' @useDynLib spatialrisk
#' @importFrom Rcpp sourceCpp
#' @importFrom Rcpp evalCpp
#' @import RcppProgress
#' @importFrom rlang abort
#'
#' @author Martin Haringa
#'
#' @examples
#' # Target points
#' sub <- data.frame(location = c("p1", "p2"),
#'                   lon = c(6.561561, 6.561398),
#'                   lat = c(53.21369, 53.21326))
#'
#' # Reference points with values
#' full <- data.frame(lon = c(6.5614, 6.5620, 6.5630),
#'                    lat = c(53.2132, 53.2140, 53.2150),
#'                    amount = c(10, 20, 15))
#'
#' # Calculate concentration within 100 meters
#' concentration(sub, full, value = amount, radius = 100)
#'
#' @export
concentration <- function(sub, full, value,
                          lon_sub = lon, lat_sub = lat,
                          lon_full = lon, lat_full = lat,
                          radius = 200, display_progress = TRUE) {

  if (!is.numeric(radius) || radius <= 0) {
    msg <- paste0("Can't find concentrations with `radius = ", radius, "`.")
    error_msg <- paste0("`radius` is not a positive number.")
    rlang::abort(c(msg, "x" = error_msg), call = NULL)
  }

  # Turn into character vector
  sub_name <- deparse(substitute(sub))
  full_name <- deparse(substitute(full))
  lon_sub <- deparse(substitute(lon_sub))
  lat_sub <- deparse(substitute(lat_sub))
  lon_full <- deparse(substitute(lon_full))
  lat_full <- deparse(substitute(lat_full))
  value <- deparse(substitute(value))

  if (!all(c(lon_sub, lat_sub) %in% names(sub))) {
    stop(paste0(sub_name, " does not contain columns ", lon_sub, " and ",
                lat_sub), call. = FALSE)
  }

  if (!all(c(lon_full, lat_full) %in% names(full))) {
    stop(paste0(full_name, " does not contain columns ", lon_full, " and ",
                lat_full), call. = FALSE)
  }

  if (!all(is.numeric(c(sub[[lon_sub]], sub[[lat_sub]], full[[lon_full]],
                        full[[lat_full]], full[[value]])))) {
    stop(paste0("the following variables should be numeric: ", lon_sub, ", ",
                lat_sub, ", ", lon_full, ", ", lat_full, ", ", value),
         call. = FALSE)
  }

  sub_df <- data.frame("lon" = sub[[lon_sub]], "lat" = sub[[lat_sub]])
  full_df <- data.frame("lon" = full[[lon_full]], "lat" = full[[lat_full]],
                        "value" = full[[value]])

  concentration_df <- concentration_loop_cpp(sub_df, full_df, radius,
                                             display_progress)

  sub$concentration <- concentration_df$cumulation
  sub
}

#' @keywords internal
concentration_ <- function(sub, full, value,
                           lon_sub = lon, lat_sub = lat,
                           lon_full = lon, lat_full = lat,
                           radius = 200, display_progress = TRUE) {

  if (!is.numeric(radius) || radius <= 0) {
    msg <- paste0("Can't find concentrations with `radius = ", radius, "`.")
    error_msg <- paste0("`radius` is not a positive number.")
    rlang::abort(c(msg, "x" = error_msg), call = NULL)
  }

  # Turn into character vector
  sub_name <- deparse(substitute(sub))
  full_name <- deparse(substitute(full))

  if (!all(c(lon_sub, lat_sub) %in% names(sub))) {
    stop(paste0(sub_name, " does not contain columns ", lon_sub, " and ",
                lat_sub), call. = FALSE)
  }

  if (!all(c(lon_full, lat_full) %in% names(full))) {
    stop(paste0(full_name, " does not contain columns ", lon_full, " and ",
                lat_full), call. = FALSE)
  }

  if (!all(is.numeric(c(sub[[lon_sub]], sub[[lat_sub]], full[[lon_full]],
                        full[[lat_full]], full[[value]])))) {
    stop(paste0("the following variables should be numeric: ", lon_sub, ", ",
                lat_sub, ", ", lon_full, ", ", lat_full, ", ", value),
         call. = FALSE)
  }

  sub_df <- data.frame("lon" = sub[[lon_sub]], "lat" = sub[[lat_sub]])
  full_df <- data.frame("lon" = full[[lon_full]], "lat" = full[[lat_full]],
                        "value" = full[[value]])

  concentration_df <- concentration_loop_cpp(sub_df, full_df, radius,
                                             display_progress)

  sub$concentration <- concentration_df$cumulation
  sub
}
