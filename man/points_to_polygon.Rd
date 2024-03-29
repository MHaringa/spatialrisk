% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/points_to_polygon.R
\name{points_to_polygon}
\alias{points_to_polygon}
\title{Aggregate attributes of coordinates to area level}
\usage{
points_to_polygon(sf_map, df, oper, crs = 4326, outside_print = FALSE)
}
\arguments{
\item{sf_map}{object of class sf}

\item{df}{data.frame containing coordinates (column names should be 'lon'
and 'lat')}

\item{oper}{an arithmetic operation on the polygon level}

\item{crs}{coordinate reference system: integer with the EPSG code, or
character with proj4string}

\item{outside_print}{print points that are not within a polygon (default is
FALSE).}
}
\value{
an object of class \code{sf}
}
\description{
A data.frame containing coordinates (in terms of longitude and
latitude) is joined to the polygon level. Then arithmetic operations on the
attributes of the coordinates are applied to obtain aggregated values for
each polygon.
}
\examples{
points_to_polygon(nl_postcode2, insurance, sum(amount, na.rm = TRUE))
\dontrun{
shp_read <- sf::st_read("~/path/to/file.shp")
points_to_polygon(shp_read, insurance, sum(amount, na.rm = TRUE))
}
}
\author{
Martin Haringa
}
