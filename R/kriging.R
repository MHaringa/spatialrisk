#' Ordinary kriging
#'
#' @description Interpolation and smoothing on the sphere by means of ordinary
#' kriging.
#'
#' @param observations data.frame of observations.
#' @param targets data.frame of locations to calculate the interpolated and
#' smoothed values for (target points).
#' @param value Column with values in \code{observations}.
#' @param lon_obs Column in \code{observations} with longitude (lon is default).
#' @param lat_obs Column in \code{observations} with latitude (lat is default).
#' @param lon_targets Column in \code{targets} with longitude (lon is default).
#' @param lat_targets Column in \code{targets} with latitude (lat is default).
#'
#' @importFrom dplyr bind_cols
#' @importFrom methods as
#' @importFrom sf st_as_sf
#' @importFrom sf st_drop_geometry
#' @importFrom stats as.formula
#'
#' @references \code{\link[gstat:krige]{gstat::krige}}
#'
#' @details \code{observations} should include at least columns for longitude
#' and latitude.
#' @details \code{targets} should include at least columns for longitude,
#' latitude and value of interest to interpolate and smooth.
#' @details Kriging can be considered as linear regression with spatially
#' correlated residuals. Kriging is most appropriate when it is known there is
#' a spatially correlated distance or directional bias in the data. It is often
#' used in soil science and geology.
#' @details  See \code{\link[spatialrisk:interpolate_spline]{splines on the
#' sphere}} for interpolation and smoothing on the sphere by means of splines.
#'
#' @return Object equal to object \code{targets} including extra columns for
#' the predicted value and the variance.
#'
#' @author Martin Haringa
#'
#' @examples
#' \dontrun{
#' target <- sf::st_drop_geometry(nl_postcode3)
#' obs <- insurance %>% dplyr::sample_n(1000)
#' pop_df <- interpolate_krige(obs, target, population_pc4)
#' pop_sf <- left_join(nl_postcode3, pop_df)
#' choropleth(pop_sf, value = "population_pc4_pred", n = 13)
#' choropleth(pop_sf, value = "population_pc4_var", n = 13)
#' }
#'
#' @export
interpolate_krige <- function(observations, targets, value, lon_obs = lon,
                              lat_obs = lat, lon_targets = lon,
                              lat_targets = lat) {

  if (!requireNamespace("gstat", quietly = TRUE)) {
    stop("gstat is needed for this function to work.
         Install it via install.packages(\"gstat\")", call. = FALSE)
  }

  if (!requireNamespace("automap", quietly = TRUE)) {
    stop("automap is needed for this function to work.
         Install it via install.packages(\"automap\")", call. = FALSE)
  }

  value <- deparse(substitute(value))
  lon_obs <- deparse(substitute(lon_obs))
  lat_obs <- deparse(substitute(lat_obs))
  lon_targets <- deparse(substitute(lon_targets))
  lat_targets <- deparse(substitute(lat_targets))

  # Ordinary kriging
  f <- stats::as.formula(paste0(value, "~ 1"))

  observation_sf <- sf::st_as_sf(observations,
                                 coords = c(lon_obs, lat_obs), crs = 4326)
  targets_sf <- sf::st_as_sf(targets,
                             coords = c(lon_targets, lat_targets), crs = 4326)

  observation_sp <- as(observation_sf, 'Spatial')
  suppressWarnings({
    fit_variogram <- automap::autofitVariogram(f, observation_sp)$var_model
  })
  output <- gstat::krige(formula = f, observation_sf, targets_sf,
                         model = fit_variogram, debug.level = 0)

  output <- sf::st_drop_geometry(output)
  names(output)[1:2] <- c(paste0(value, "_pred"), paste0(value, "_var"))
  output <- dplyr::bind_cols(targets, output)
  return(output)
}
