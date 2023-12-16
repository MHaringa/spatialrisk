#' Splines on the sphere
#'
#' @description Spline interpolation and smoothing on the sphere.
#'
#' @param observations data.frame of observations.
#' @param targets data.frame of locations to calculate the interpolated and
#' smoothed values for (target points).
#' @param value Column with values in \code{observations}.
#' @param lon_obs Column in \code{observations} with longitude (lon is default).
#' @param lat_obs Column in \code{observations} with latitude (lat is default).
#' @param lon_targets Column in \code{targets} with longitude (lon is default).
#' @param lat_targets Column in \code{targets} with latitude (lat is default).
#' @param k (default 50) is the basis dimension. For small data sets reduce
#' \code{k} manually rather than using default.
#'
#' @details \code{observations} should include at least columns for longitude
#' and latitude.
#' @details \code{targets} should include at least columns for longitude,
#' latitude and value of interest to interpolate and smooth.
#' @details A smooth of the general type discussed in Duchon (1977) is used:
#' the sphere is embedded in a 3D Euclidean space, but smoothing employs a
#' penalty based on second derivatives (so that locally as the smoothing
#' parameter tends to zero we recover a "normal" thin plate spline on the
#' tangent space). This is an unpublished suggestion of Jean Duchon.
#' @details See \code{\link[spatialrisk:interpolate_krige]{ordinary kriging}}
#' for interpolation and smoothing on the sphere by means of kriging.
#'
#' @return Object equal to object \code{targets} including an extra column
#'  with predicted values.
#'
#' @author Martin Haringa
#'
#' @importFrom stats as.formula
#'
#' @references \code{\link[mgcv:smooth.construct.sos.smooth.spec]{Splines on
#' the sphere}}
#'
#' @examples
#' \dontrun{
#' target <- sf::st_drop_geometry(nl_postcode3)
#' obs <- dplyr::sample_n(insurance, 1000)
#' pop_df <- interpolate_spline(obs, target, population_pc4, k = 20)
#' pop_sf <- left_join(nl_postcode3, pop_df)
#' choropleth(pop_sf, value = "population_pc4_pred", n = 13)
#' }
#'
#' @export
interpolate_spline <- function(observations, targets, value, lon_obs = lon,
                               lat_obs = lat, lon_targets = lon,
                               lat_targets = lat, k = 50) {

  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("mgcv is needed for this function to work.
         Install it via install.packages(\"mgcv\")", call. = FALSE)
  }

  lon_targets <- deparse(substitute(lon_targets))
  lat_targets <- deparse(substitute(lat_targets))
  lon_obs <- deparse(substitute(lon_obs))
  lat_obs <- deparse(substitute(lat_obs))
  value <- deparse(substitute(value))
  sos <- deparse(substitute("sos"))

  # Rename columns
  names(observations)[names(observations) == lon_obs] <- "lon"
  names(observations)[names(observations) == lat_obs] <- "lat"
  names(targets)[names(targets) == lon_targets] <- "lon"
  names(targets)[names(targets) == lon_targets] <- "lon"

  f <- as.formula(paste0(value, "~ s(lat, lon,", "bs = ", sos,
                         ", m = -1, k = ", k, ")"))
  pred_gam <- tryCatch(
    {
      mgcv::gam(f, data = observations)
    },
    error = function(e) {
      stop("Error: dimension k is chosen too large (default k = 50). Reduce
           dimension k manually as argument in function call.", call. = FALSE)
    })

  response <- as.numeric(mgcv::predict.gam(pred_gam, targets,
                                           type = "response"))
  targets[, paste0(value, "_pred")] <- response
  return(targets)
}
