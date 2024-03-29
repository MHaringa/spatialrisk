% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kriging.R
\name{interpolate_krige}
\alias{interpolate_krige}
\title{Ordinary kriging}
\usage{
interpolate_krige(
  observations,
  targets,
  value,
  lon_obs = lon,
  lat_obs = lat,
  lon_targets = lon,
  lat_targets = lat
)
}
\arguments{
\item{observations}{data.frame of observations.}

\item{targets}{data.frame of locations to calculate the interpolated and
smoothed values for (target points).}

\item{value}{Column with values in \code{observations}.}

\item{lon_obs}{Column in \code{observations} with longitude (lon is default).}

\item{lat_obs}{Column in \code{observations} with latitude (lat is default).}

\item{lon_targets}{Column in \code{targets} with longitude (lon is default).}

\item{lat_targets}{Column in \code{targets} with latitude (lat is default).}
}
\value{
Object equal to object \code{targets} including extra columns for
the predicted value and the variance.
}
\description{
Interpolation and smoothing on the sphere by means of ordinary
kriging.
}
\details{
\code{observations} should include at least columns for longitude
and latitude.

\code{targets} should include at least columns for longitude,
latitude and value of interest to interpolate and smooth.

Kriging can be considered as linear regression with spatially
correlated residuals. Kriging is most appropriate when it is known there is
a spatially correlated distance or directional bias in the data. It is often
used in soil science and geology.

See \code{\link[spatialrisk:interpolate_spline]{splines on the
sphere}} for interpolation and smoothing on the sphere by means of splines.
}
\examples{
\dontrun{
target <- sf::st_drop_geometry(nl_postcode3)
obs <- insurance \%>\% dplyr::sample_n(1000)
pop_df <- interpolate_krige(obs, target, population_pc4)
pop_sf <- left_join(nl_postcode3, pop_df)
choropleth(pop_sf, value = "population_pc4_pred", n = 13)
choropleth(pop_sf, value = "population_pc4_var", n = 13)
}

}
\references{
\code{\link[gstat:krige]{gstat::krige}}
}
\author{
Martin Haringa
}
