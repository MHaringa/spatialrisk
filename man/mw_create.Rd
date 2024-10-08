% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{mw_create}
\alias{mw_create}
\title{Create focal ("moving window") weight matrix}
\usage{
mw_create(r, radius)
}
\arguments{
\item{r}{SpatRaster.}

\item{radius}{radius of the circle (in units of the crs).}
}
\description{
Create a focal ("moving window") weight matrix for use in
\code{terra::focal()}.
}
\details{
\code{mw_create()} is a modified version of
\code{terra::focalMat()}. While \code{terra::focalMat()} creates a matrix
where the border is the distance from the center of the focal cell,
\code{mw_create()} creates a matrix where the border of the moving window
is the distance from the edge of the focal cell.
}
\author{
Martin Haringa
}
\keyword{internal}
