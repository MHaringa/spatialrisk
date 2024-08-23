#' Concentration calculation
#'
#' @description Calculates the concentration, which is the sum of all
#' observations within a circle of a certain radius.
#'
#' @param sub data.frame of target points to calculate concentration risk for,
#'     including at least columns for longitude and latitude.
#' @param full data.frame containing reference points, where the function finds
#'     locations within a radius from the target points. Should include at least
#'     columns for longitude, latitude, and the value of interest to summarize.
#' @param value column name with value of interest to summarize in \code{full}.
#' @param lon_sub column name in \code{sub} for longitude
#'     (default is \code{lon}).
#' @param lat_sub column name in \code{sub} for latitude
#'     (default is \code{lat}).
#' @param lon_full column name in \code{full} for longitude in \code{full}
#'     (default is \code{lon}).
#' @param lat_full column name in \code{full} for latitude in \code{full}
#'     (default is \code{lat}).
#' @param radius numeric. Radius of the circle in meters (default is 200).
#' @param display_progress boolean indicating whether to show progress bar
#' (TRUE/FALSE). Defaults to TRUE.
#'
#' @return A data.frame equal to \code{sub} including an additional
#' column \code{concentration}.
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
#' df <- data.frame(location = c("p1", "p2"), lon = c(6.561561, 6.561398),
#'  lat = c(53.21369, 53.21326))
#' concentration(df, Groningen, value = amount, radius = 100)
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
