#' Sum values within a radius around target coordinates
#'
#' @description Calculates the sum of all observations from a reference data set
#' that fall within a given radius (in meters) of each target point.
#'
#' @param sub A data.frame of target points for which sums are calculated.
#'   Must include at least columns for longitude and latitude.
#' @param full A data.frame containing reference points. Must include at least
#'   columns for longitude, latitude, and the value of interest to summarize.
#' @param value A string giving the name of the column in `full` to be summed.
#' @param lon_sub A string with the name of the longitude column in `sub`.
#'   Default is `"lon"`.
#' @param lat_sub A string with the name of the latitude column in `sub`.
#'   Default is `"lat"`.
#' @param lon_full A string with the name of the longitude column in `full`.
#'   Default is `"lon"`.
#' @param lat_full A string with the name of the latitude column in `full`.
#'   Default is `"lat"`.
#' @param radius Numeric. Radius of the circle in meters. Must be positive
#'   (default: 200).
#' @param display_progress Logical. Whether to display a progress bar.
#'   Default is `TRUE`.
#'
#' @return A data.frame equal to `sub` with an additional numeric column
#'   `radius_sum` containing the summed values from `full`.
#'
#' @details
#' This function uses a C++ backend for efficient distance calculations
#' (Haversine formula).
#'
#' @useDynLib spatialrisk
#' @importFrom Rcpp sourceCpp
#' @importFrom Rcpp evalCpp
#' @import RcppProgress
#' @importFrom rlang abort
#' @importFrom lifecycle deprecate_warn
#'
#' @author Martin Haringa
#'
#' @examples
#' sub <- data.frame(location = c("p1", "p2"),
#'                   lon = c(6.561561, 6.561398),
#'                   lat = c(53.21369, 53.21326))
#'
#' full <- data.frame(lon = c(6.5614, 6.5620, 6.5630),
#'                    lat = c(53.2132, 53.2140, 53.2150),
#'                    amount = c(10, 20, 15))
#'
#' radius_sum(sub, full, value = "amount", radius = 100)
#'
#' @export
radius_sum <- function(sub, full, value,
                       lon_sub = "lon", lat_sub = "lat",
                       lon_full = "lon", lat_full = "lat",
                       radius = 200, display_progress = TRUE) {

  if (!is.numeric(radius) || radius <= 0) {
    rlang::abort(
      c(
        paste0("Can't calculate sums with `radius = ", radius, "`."),
        "x" = "`radius` is not a positive number."
      ),
      call = NULL
    )
  }

  # Column checks
  if (!all(c(lon_sub, lat_sub) %in% names(sub))) {
    stop("`sub` does not contain columns ", lon_sub, " and ", lat_sub,
         call. = FALSE)
  }
  if (!all(c(lon_full, lat_full, value) %in% names(full))) {
    stop("`full` does not contain required columns ", lon_full, ", ",
         lat_full, ", ", value, call. = FALSE)
  }

  # Numeric checks
  if (!all(is.numeric(c(sub[[lon_sub]], sub[[lat_sub]],
                        full[[lon_full]], full[[lat_full]], full[[value]])))) {
    stop("the following variables should be numeric: ",
         paste(c(lon_sub, lat_sub, lon_full, lat_full, value), collapse = ", "),
         call. = FALSE)
  }

  sub_df <- data.frame("lon" = sub[[lon_sub]], "lat" = sub[[lat_sub]])
  full_df <- data.frame("lon" = full[[lon_full]], "lat" = full[[lat_full]],
                        "value" = full[[value]])

  res <- concentration_loop_cpp(sub_df, full_df, radius, display_progress)
  sub$radius_sum <- res$cumulation
  sub
}

#' @rdname radius_sum
#' @export
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `concentration()` was renamed to [radius_sum()].
concentration <- function(sub, full, value,
                          lon_sub = lon, lat_sub = lat,
                          lon_full = lon, lat_full = lat,
                          radius = 200, display_progress = TRUE) {
  lifecycle::deprecate_warn("0.7.5", "concentration()", "radius_sum()")

  # NSE capture
  lon_sub <- deparse(substitute(lon_sub))
  lat_sub <- deparse(substitute(lat_sub))
  lon_full <- deparse(substitute(lon_full))
  lat_full <- deparse(substitute(lat_full))
  value <- deparse(substitute(value))

  radius_sum(sub = sub, full = full, value = value,
             lon_sub = lon_sub, lat_sub = lat_sub,
             lon_full = lon_full, lat_full = lat_full,
             radius = radius, display_progress = display_progress)
}
