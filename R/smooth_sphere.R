#' Splines on the sphere
#'
#' @description Spline interpolation and smoothing on the sphere.
#'
#' @param df data.frame of observations.
#' @param sf_object Object of locations to calculate the smoothed values for (target points).
#' @param value Column with values in \code{df}.
#' @param lon_df Column in \code{df} with longitude (lon is default).
#' @param lat_df Column in \code{df} with latitude (lat is default).
#' @param lon_sf Column in \code{sf_object} with longitude (lon is default).
#' @param lat_sf Column in \code{sf_object} with latitude (lat is default).
#'
#' @details \code{df} should include at least columns for longitude and latitude.
#' @details \code{sf_object} should include at least columns for longitude, latitude and value of interest to interpolate and smooth.
#' @details A smooth of the general type discussed in Duchon (1977) is used: the sphere is embedded in a 3D Euclidean space, but smoothing employs a penalty based on second derivatives (so that locally as the smoothing parameter tends to zero we recover a "normal" thin plate spline on the tangent space). This is an unpublished suggestion of Jean Duchon.
#' @return Object equal to object \code{sf_object} including an extra column \code{prediction}.
#'
#' @author Martin Haringa
#'
#' @import mgcv
#' @importFrom stats as.formula
#'
#' @references \code{\link[mgcv:smooth.construct.sos.smooth.spec]{Splines on the sphere}}
#'
#' @examples
#' \dontrun{
#' pop_sf <- smooth_sphere(insurance, nl_postcode3, population_pc4)
#' choropleth(pop_sf, value = "prediction", n = 13)
#' }
#'
#' @export
smooth_sphere <- function(df, sf_object, value, lon_df = lon, lat_df = lat, lon_sf = lon, lat_sf = lat) {
  lon_sf <- deparse(substitute(lon_sf))
  lat_sf <- deparse(substitute(lat_sf))
  lon_df <- deparse(substitute(lon_df))
  lat_df <- deparse(substitute(lat_df))
  value <- deparse(substitute(value))
  sos <- deparse(substitute("sos"))

  f <- as.formula(paste0(value, "~ s(", lat_df, ",", lon_df, ",", "bs = ", sos, ", m = -1)"))
  pred_gam <- mgcv::gam(f, data = df)
  response <- as.numeric(predict.gam(pred_gam, sf_object, type = "response"))
  sf_object$prediction <- response
  return(sf_object)
}
