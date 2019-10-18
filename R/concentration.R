#' Concentration risk
#'
#' @param sub data.frame of locations to calculate concentration risk for (target points).
#' @param full data.frame to find the locations within radius \code{r} from locations in \code{sub} (reference locations).
#' @param value Column with value in \code{full}.
#' @param lon_sub Column in \code{sub} with longitude (lon is default).
#' @param lat_sub Column in \code{sub} with latitude (lat is default).
#' @param lon_full Column in \code{full} with longitude in \code{full} (lon is default).
#' @param lat_full Column in \code{full} with latitude in \code{full} (lat is default).
#' @param radius Radius (in meters) (default is 200m).
#' @param display_progress Show progress bar (TRUE/FALSE).
#'
#' @description The sum of all observations within a radius from center point(s). In particular,
#' it can be used to determine concentration risk in the context of the EU insurance regulation framework (Solvency II).
#' The function offers an effective approach to calculate the 'standard formula' under Solvency II.
#' The 'standard formula' under Solvency II asks companies to report their largest fire concentration in
#' respect of the fire peril within a radius of 200m. This is the maximum gross sum insured of the set of
#' buildings fully or partly located within this radius (Commission Delegated Regulation (EU), 2015, Article 132).
#'
#' @details The data.frame \code{sub} should include at least columns for longitude and latitude.
#' @details The data.frame \code{full} should include at least columns for longitude, latitude and value of interest to summarize.
#'
#' @references Commission Delegated Regulation (EU) (2015). Solvency II Delegated Act 2015/35. Official Journal of the European Union, 58:124.
#'
#' @return A data.frame equal to data.frame \code{sub} including an extra column \code{concentration}.
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
concentration <- function(sub, full, value,
                          lon_sub = lon, lat_sub = lat,
                          lon_full = lon, lat_full = lat,
                          radius = 200, display_progress = TRUE){

  if(!(radius > 0)) stop('radius should be positive')

  # Turn into character vector
  lon_sub <- deparse(substitute(lon_sub))
  lat_sub <- deparse(substitute(lat_sub))
  lon_full <- deparse(substitute(lon_full))
  lat_full <- deparse(substitute(lat_full))
  value <- deparse(substitute(value))

  sub_df <- data.frame("lon" = sub[[lon_sub]], "lat" = sub[[lat_sub]])
  full_df <- data.frame("lon" = full[[lon_full]], "lat" = full[[lat_full]], "value" = full[[value]])

  concentration_df <- concentration_loop_cpp(sub_df, full_df, radius, display_progress)

  sub$concentration <- concentration_df$cumulation

  return(sub)
}
