#' Concentration risk
#'
#' @param sub data.frame of locations to calculate concentration risk for. The data.frame should include at least columns for longitude, latitude and value of interest to summarize.
#' @param full data.frame of full portfolio. The data.frame should include at least columns for longitude, latitude and value of interest to summarize.
#' @param lon_sub Column name with longitude (lon is default)
#' @param lat_sub Column name with latitude (lat is default)
#' @param lon_full Column name with longitude in the full data (lon is default)
#' @param lat_full Column name with latitude in the full data (lat is default)
#' @param value_full Column name with value in the full data (value is default)
#' @param radius Radius (in meters) (default is 200m)
#' @param display_progress Show progress bar (TRUE/FALSE)
#'
#' @description The points (e.g. addresses) with the highest concentrations. In the context of the 'standard formula' under Solvency II,
#' this is the maximum gross sum insured of the set of buildings fully or partly located within this radius.
#'
#' @return data.frame with points with the highest concentrations.
#'
#' @useDynLib spatialrisk
#' @importFrom Rcpp sourceCpp
#' @import RcppProgress
#'
#' @author Martin Haringa
#'
#' @examples
#' df <- data.frame(location = c("p1", "p2"), lon = c(6.561561, 6.561398), lat = c(53.21369, 53.21326))
#' concentration(df, Groningen, value = amount, radius = 100)
#'
#' @export
concentration <- function(sub, full,
                          lon_sub = lon, lat_sub = lat,
                          lon_full = lon, lat_full = lat, value_full = value,
                          radius = 200, display_progress = TRUE){

  # Turn into character vector
  lon_sub <- deparse(substitute(lon_sub))
  lat_sub <- deparse(substitute(lat_sub))

  lon_full <- deparse(substitute(lon_full))
  lat_full <- deparse(substitute(lat_full))
  value_full <- deparse(substitute(value_full))

  sub_df <- data.frame("lon" = sub[[lon_sub]], "lat" = sub[[lat_sub]])
  full_df <- data.frame("lon" = full[[lon_full]], "lat" = full[[lat_full]], "value" = full[[value_full]])

  concentration_df <- concentration_loop_cpp(sub_df, full_df, radius, display_progress)

  sub$concentration <- concentration_df$cumulation

  return(sub)
}
